(ns draft-day.ingestion.pipeline
  "Resolve the player universe with a TTL disk cache and a fallout chain, mirroring
  the POC: offline-sample -> fresh-cache -> live -> stale-cache -> bundled-sample.
  The cache is Transit on disk (data/players_cache.v1.transit); the bundled sample
  is EDN on the classpath (resources/sample_players.edn).

  Every branch returns the same envelope, so a caller can always tell what it is
  looking at:

    {:schema-version 1
     :season         2026        ; the NFL season the rows were fetched for
     :fetched-at     \"...Z\"      ; when, nil for the committed sample
     :source         \"live\"      ; which rung of the fallout chain answered
     :validation     {...}       ; what id validation dropped
     :players        [...]}

  `:source` alone could not distinguish a twenty-minute-old cache from a
  fourteen-month-old one served because the network died; `:season` and
  `:fetched-at` can."
  (:require [draft-day.ingestion.sleeper :as sleeper]
            [draft-day.ingestion.fantasypros :as fantasypros]
            [draft-day.ingestion.espn :as espn]
            [draft-day.ingestion.merge :as merge]
            [draft-day.ingestion.match :as match]
            [draft-day.ingestion.player-ids :as player-ids]
            [draft-day.ingestion.validate :as validate]
            [clojure.tools.logging :as log]
            [cognitect.transit :as transit]
            [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import [java.time Instant]))

(def schema-version
  "Bumped whenever the player row shape changes. It rides in the cache filename
  as well as the payload, so an old file is simply never found rather than
  deserializing cleanly into missing columns that render as zero. The benchmark
  harness learned this the hard way — see benchmark/fetch.clj's cache-path."
  1)

(def default-cache-path (str "data/players_cache.v" schema-version ".transit"))
(def ^:private sample-resource "sample_players.edn")

(defn now-iso
  "Current instant as ISO-8601. A var so tests can pin it."
  []
  (str (Instant/now)))

(defmacro best-effort
  "Evaluate body, returning its value; on any exception log it to *err* and
  return nil. For optional enrichment steps where a failed fetch should degrade
  gracefully (leave the column absent) rather than abort ingestion."
  [& body]
  `(try ~@body
     (catch Exception e#
       (log/warn e# "best-effort failed:" (ex-message e#) (ex-data e#))
       nil)))

(defn- cache-ttl-hours []
  (Double/parseDouble (or (System/getenv "DRAFTDAY_CACHE_TTL_HOURS") "24")))

(defn offline? []
  (= "1" (System/getenv "DRAFTDAY_OFFLINE")))

(defn write-transit! [path data]
  (io/make-parents path)
  (with-open [out (io/output-stream path)]
    (transit/write (transit/writer out :json) data)))

(defn read-transit [path]
  (when (.exists (io/file path))
    (with-open [in (io/input-stream path)]
      (transit/read (transit/reader in :json)))))

(defn cache-fresh? [path ttl-hours]
  (let [f (io/file path)]
    (and (.exists f)
         (< (- (System/currentTimeMillis) (.lastModified f))
            (long (* ttl-hours 3600 1000))))))

(defn load-sample
  "The committed offline fallback universe (EDN on the classpath)."
  []
  (when-let [r (io/resource sample-resource)]
    (edn/read-string (slurp r))))

(defn checked
  "Validate a universe from a branch with nowhere better to fall back to (cache,
  bundled sample): drop unusable rows and log them, but never throw. Returns
  `{:players :validation}`."
  [label rows]
  (let [{:keys [players report]} (validate/validate-universe (or rows []))]
    (validate/log-report! label report)
    {:players players :validation report}))

(defn cached->universe
  "Coerce whatever is on disk into an envelope. Pre-versioning caches were a bare
  player vector; call those schema 0 so a hand-copied file degrades to a refetch
  instead of throwing on a map lookup."
  [x]
  (cond
    (map? x)        x
    (sequential? x) {:schema-version 0 :players (vec x)}
    :else           nil))

(def hit-rate-floor
  "Below this share of a position's *published* rows landing on a universe
  player, the join is broken rather than the source being thin."
  0.80)

(defn log-enrichment!
  "One line per source, plus a warning for any position whose published rows
  mostly failed to land. Warning on coverage instead would be pure noise: the
  FantasyPros auction list covers ~150 players by design, so it is *supposed*
  to leave most of the board untouched."
  [label {:keys [ok? rows matched hit-rate coverage by-position]}]
  (if-not ok?
    (log/warn (format "%s: unavailable, columns omitted" label))
    (do
      (log/info (format "%s: %d rows -> %d matched (%.0f%% of rows, %.0f%% of board)"
                        label rows matched (* 100.0 hit-rate) (* 100.0 coverage)))
      (doseq [[pos {:keys [n rows matched]}] (sort by-position)
              :when (and (pos? rows) (< (/ (double matched) rows) hit-rate-floor))]
        (log/warn
         (format "%s: %s published %d row(s), only %d landed (board has %d)"
                 label pos rows matched n))))))

(defn apply-enrichment
  "Left-join one best-effort enrichment source onto the accumulating universe,
  recording its match report under `label`. A nil source means the fetch failed
  (see `best-effort`) and is reported as unavailable — distinct from a source
  that answered and simply matched nothing, which is a join bug, not an outage."
  [acc label by-key]
  (if (nil? by-key)
    (do (log-enrichment! label {:ok? false})
        (assoc-in acc [:sources label] {:ok? false}))
    (let [{:keys [players report]} (merge/left-join-report (:players acc) by-key)
          report (assoc report :ok? true)]
      (log-enrichment! label report)
      (-> acc
          (assoc :players players)
          (assoc-in [:sources label] report)))))

(def enrichment-source-labels
  "Every source `enrich-universe` reports on. The bundled sample is expected to
  carry all of them; when a new one is added here and the sample is not
  recaptured, its column renders blank offline with nothing to say the column
  is structurally absent rather than merely unmatched. That is exactly what
  happened when the FantasyPros AAV and sleepers joins were introduced."
  [:sleeper/byes :fantasypros/ecr :fantasypros/aav :fantasypros/sleepers :espn])

(defn enrich-universe
  "Left-join the best-effort enrichment columns onto an already-validated
  universe, returning `{:players :sources}`. Split out from the fetch so the
  validation gate below reads as the gate it is."
  [season universe]
  (let [;; Bye weeks come from Sleeper itself (the schedule endpoint), keyed on the
        ;; :team every player already carries — complete, not just the FantasyPros
        ;; matches. Best-effort: a failed schedule fetch just leaves :bye nil.
        byes     (best-effort (sleeper/fetch-byes season))
        ;; Deliberate shortcut: the FantasyPros enrichments are baked into this
        ;; single, scoring-agnostic shared universe at ingestion, but they differ
        ;; by format — the ECR cheatsheet (:ppr/:half-ppr/:standard) and the AAV
        ;; calculator (scoring param). We always scrape PPR, so non-PPR leagues
        ;; get PPR-flavored tiers/rank-spread and market prices. See README todo
        ;; for the proper fix (store all variants, select at ranking time).
        ecr      (best-effort (fantasypros/fetch-ecr :ppr))
        aav      (best-effort (fantasypros/fetch-aav))
        sleepers (best-effort (fantasypros/fetch-sleepers))
        espn     (best-effort (espn/fetch season))]
    ;; Byes join on :team rather than a name key, so they get a row count but
    ;; none of the match-rate machinery — reporting them as a 0% join would be
    ;; a lie, not a diagnostic.
    (log/info (format ":sleeper/byes: %d team bye weeks" (count byes)))
    (-> {:players (cond-> universe (seq byes) (sleeper/assoc-byes byes))
         :sources {:sleeper/byes (if (seq byes)
                                   {:ok? true :rows (count byes)}
                                   {:ok? false})}}
        (apply-enrichment :fantasypros/ecr      (some-> ecr match/by-key))
        (apply-enrichment :fantasypros/aav      (some-> aav match/by-key))
        (apply-enrichment :fantasypros/sleepers (some-> sleepers match/by-key))
        (apply-enrichment :espn                 espn))))

(defn fetch-enriched-universe
  "The live universe: Sleeper rows, id-validated, then enriched.

  Ids are anchored first, then validated, then enriched. That order matters:
  anchoring rewrites `:player-id`, so validating afterwards is what catches two
  Sleeper ids resolving to one GSIS id — a collision that would silently drop a
  player from the board. Enrichment comes last because it joins by name key and
  could not repair a bad id anyway.

  A systemic failure throws, which `load-universe` catches into the stale-cache
  branch: serving last night's board beats serving a structurally broken one."
  [season]
  (let [anchored (player-ids/attach-ids (sleeper/fetch-universe season)
                                        (player-ids/pinned-index))
        {:keys [players report]} (validate/validate-universe anchored)]
    (validate/log-report! "sleeper universe" report)
    (when (validate/systemic-failure? report)
      (throw (ex-info "player universe failed validation" report)))
    (assoc (enrich-universe season players) :validation report)))

(defn sample-universe
  "The bundled fallback as an envelope.

  A sample captured by `draft-day.tools.snapshot` carries its own stamp — the
  season, when it was captured, and which enrichment sources contributed. The
  original hand-captured sample is a bare vector with no stamp at all, so it
  honestly reports schema 0 and nil provenance rather than borrowing the
  current version and claiming to be something it is not."
  []
  (let [env (or (cached->universe (load-sample)) {:players []})]
    (merge {:schema-version (:schema-version env)
            :season         (:season env)
            :fetched-at     (:captured-at env)
            :sources        (:sources env)
            :source         "sample"}
           ;; Anchored on read so offline dev and tests share the live id
           ;; space; a sample captured after anchoring shipped already carries
           ;; :ids and passes straight through.
           (checked "sample" (player-ids/attach-ids
                              (:players env) (player-ids/pinned-index))))))

(defn cached-universe
  "The disk cache as an envelope, or nil when absent, empty or written by a
  different schema. Provenance is read from the file, so a cached board still
  reports the season and time it was actually fetched rather than now."
  [path]
  (when-let [env (cached->universe (read-transit path))]
    (when (= schema-version (:schema-version env))
      (let [{:keys [players] :as v} (checked "cache" (:players env))]
        (when (seq players)
          (merge env v {:source "cache"}))))))

(defn live-universe
  "Fetch, validate, cache and stamp. On any failure fall back down the chain:
  a stale cache beats the committed sample, and both beat an empty board."
  [season cache-path]
  (try
    (let [season' (or season (sleeper/current-season))
          {:keys [players validation sources]} (fetch-enriched-universe season')
          env {:schema-version schema-version
               :season         season'
               :fetched-at     (now-iso)
               :validation     validation
               :sources        sources
               :players        players}]
      (write-transit! cache-path env)
      (assoc env :source "live"))
    (catch Exception _
      ;; stale > fake, but only if the stale copy still validates — a cache that
      ;; drops to nothing is worse than the sample, not better.
      (or (cached-universe cache-path)
          (let [s (sample-universe)] (when (seq (:players s)) s))
          {:schema-version schema-version :players [] :source "empty"}))))

(defn load-universe
  "Return the universe envelope (see the ns docstring). opts: :refresh (bypass a
  fresh cache), :season, :cache-path (defaults to data/...)."
  ([] (load-universe {}))
  ([{:keys [refresh season cache-path] :or {cache-path default-cache-path}}]
   (cond
     (offline?)
     (sample-universe)

     (and (not refresh) (cache-fresh? cache-path (cache-ttl-hours)))
     (or (cached-universe cache-path) (live-universe season cache-path))

     :else
     (live-universe season cache-path))))
