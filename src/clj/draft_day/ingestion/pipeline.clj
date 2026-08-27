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
            [draft-day.ingestion.nflverse :as nflverse]
            [draft-day.ingestion.match :as match]
            [draft-day.ingestion.parallel :as parallel]
            [draft-day.ingestion.player-ids :as player-ids]
            [draft-day.ingestion.validate :as validate]
            [draft-day.scoring :as scoring]
            [clojure.tools.logging :as log]
            [cognitect.transit :as transit]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.java.io :as io])
  (:import [java.time Instant]))

(def schema-version
  "Bumped whenever the player row shape changes. It rides in the cache filename
  as well as the payload, so an old file is simply never found rather than
  deserializing cleanly into missing columns that render as zero. The benchmark
  harness learned this the hard way — see benchmark/fetch.clj's cache-path.

  2: vendor columns moved under :vendor/by-format, one entry per scoring format,
  and the write-only :sleeper/pts-* fields were dropped.

  3: prior-season usage (:nflverse/prior-*) and ESPN's projected targets and
  receptions (:espn/proj-*) were added.

  4: multi-season availability (:nflverse/games-by-season, :nflverse/games-seasons)
  was added, and the nflverse label became :nflverse/player-stats now that one
  fetch answers two questions.

  5: per-season realized stat lines (:nflverse/history) were added, from the same
  window that fetch already had in hand."
  5)

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

(defn delete-cache!
  "Remove the on-disk cache file, if present. A no-op when already absent."
  [path]
  (let [f (io/file path)]
    (when (.exists f) (.delete f))))

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
  to leave most of the board untouched.

  `:expected-partial?` turns that warning off for a source whose rows cannot all
  land no matter how healthy the join is. A prior-season source is the case:
  nflverse publishes everyone who played last year, and the ones who retired or
  were cut have no row on this year's board to find. Warning about that every
  run costs the floor its only job — saying that a join which *should* land is
  not landing — by burying it under warnings that always mean 'as intended'."
  [label {:keys [ok? rows matched hit-rate coverage by-position expected-partial?]}]
  (if-not ok?
    (log/warn (format "%s: unavailable, columns omitted" label))
    (do
      (log/info (format "%s: %d rows -> %d matched (%.0f%% of rows, %.0f%% of board)"
                        label rows matched (* 100.0 hit-rate) (* 100.0 coverage)))
      (doseq [[pos {:keys [n rows matched]}] (sort by-position)
              :when (and (not expected-partial?)
                         (pos? rows)
                         (< (/ (double matched) rows) hit-rate-floor))]
        (log/warn
         (format "%s: %s published %d row(s), only %d landed (board has %d)"
                 label pos rows matched n))))))

(defn apply-enrichment
  "Left-join one best-effort enrichment source onto the accumulating universe,
  recording its match report under `label`. A nil source means the fetch failed
  (see `best-effort`) and is reported as unavailable — distinct from a source
  that answered and simply matched nothing, which is a join bug, not an outage.

  `opts` is passed straight through to `merge/left-join-report`, for a source
  that joins on something better than a name key; `:expected-partial?` is read
  here rather than there, and reaches `log-enrichment!` via the report."
  ([acc label by-key] (apply-enrichment acc label by-key {}))
  ([acc label by-key opts]
   (if (nil? by-key)
     (do (log-enrichment! label {:ok? false})
         (assoc-in acc [:sources label] {:ok? false}))
     (let [{:keys [players report]} (merge/left-join-report (:players acc) by-key opts)
           report (assoc report :ok? true
                         :expected-partial? (boolean (:expected-partial? opts)))]
       (log-enrichment! label report)
       (-> acc
           (assoc :players players)
           (assoc-in [:sources label] report))))))

(def ^{:doc "Alias for the shared `scoring/format-label` — the label scheme is
  cljc because the browser reads these keys back off /api/players."}
  format-label scoring/format-label)

(defn pos-tier-label
  "The `:sources` label for one position's expert-tier scrape. Format-varying
  positions get one label per format; the rest get a single unscoped label, so
  the report says plainly that QB was fetched once and not three times."
  ([pos] (keyword "fantasypros" (str "pos-tier-" (str/lower-case pos))))
  ([pos fmt] (format-label (pos-tier-label pos) fmt)))

(def pos-tier-tasks
  "[label position format-or-nil] for every per-position expert-tier scrape.
  Twelve of them: RB/WR/TE across three formats, plus QB/K/DST once each."
  (vec (mapcat (fn [[pos varies?]]
                 (if varies?
                   (map (fn [fmt] [(pos-tier-label pos fmt) pos fmt]) scoring/formats)
                   [[(pos-tier-label pos) pos (first scoring/formats)]]))
               (sort fantasypros/pos-formats))))

(def enrichment-source-labels
  "Every source `enrich-universe` reports on. The bundled sample is expected to
  carry all of them; when a new one is added here and the sample is not
  recaptured, its column renders blank offline with nothing to say the column
  is structurally absent rather than merely unmatched. That is exactly what
  happened when the FantasyPros AAV and sleepers joins were introduced."
  (into (into [:sleeper/byes :fantasypros/sleepers :espn :nflverse/player-stats]
              (mapcat (fn [fmt] [(format-label :fantasypros/ecr fmt)
                                 (format-label :fantasypros/aav fmt)]))
              scoring/formats)
        (map first)
        pos-tier-tasks))

(defn scoped
  "Re-key a by-key enrichment map so its columns land under
  `[:vendor/by-format fmt]` rather than at the top level."
  [fmt by-key]
  (some-> by-key (update-vals (fn [cols] {:vendor/by-format {fmt cols}}))))

(defn enrichment-tasks
  "Every enrichment fetch as a best-effort thunk, keyed by the `:sources` label
  it reports under.

  One flat map rather than a sequence of `let` bindings, because the bindings
  *were* the bug: each one blocked on a 30-second timeout before the next
  started, and singling out the FantasyPros half for concurrency just moved the
  stall to Sleeper's four sleeper pages. Everything here is independent I/O over
  three different hosts, so the honest shape is a set of tasks with no order at
  all — `parallel/all` starts them together and the joins below impose the order
  that actually matters (a deterministic `:sources` report)."
  [season]
  (into (into {:sleeper/byes         #(best-effort (sleeper/fetch-byes season))
               :fantasypros/sleepers #(best-effort (fantasypros/fetch-sleepers))
               :espn                 #(best-effort (espn/fetch season))
               ;; Last season, not this one: these are realized outcomes. One
               ;; fetch, two questions — last season's usage and the availability
               ;; window ending there. See `nflverse/fetch`.
               :nflverse/player-stats #(best-effort (nflverse/fetch (dec season)))}
              (mapcat (fn [fmt]
                        [[(format-label :fantasypros/ecr fmt)
                          #(best-effort (fantasypros/fetch-ecr fmt))]
                         [(format-label :fantasypros/aav fmt)
                          #(best-effort (fantasypros/fetch-aav fmt))]]))
              scoring/formats)
        (map (fn [[label pos fmt]]
               [label #(best-effort (fantasypros/fetch-pos-ecr pos fmt))]))
        pos-tier-tasks))

(defn enrich-universe
  "Left-join the best-effort enrichment columns onto an already-validated
  universe, returning `{:players :sources}`. Split out from the fetch so the
  validation gate below reads as the gate it is.

  FantasyPros publishes ECR and auction values per scoring format, so all three
  are fetched and stored side by side under `:vendor/by-format`; the league's
  format is chosen per request in `rankings.vendor`. Baking one format in here
  is what made a standard league read PPR tiers, PPR rank spread and PPR market
  prices — the universe cache is shared across leagues, so the choice cannot be
  made at ingestion.

  Every fetch goes out at once (`enrichment-tasks` + `parallel/all`); only the
  joins below are sequential. That does raise peak heap — ESPN's ~37MB feed is
  now in memory alongside a few cheatsheet pages instead of strictly after them,
  which is why FantasyPros caps its own concurrency rather than letting all ten
  of its pages land together.

  ESPN is deliberately *not* format-scoped: it publishes the same auction value
  under both its PPR and STANDARD rank types (checked against the live feed), so
  splitting it would invent a distinction the source does not make."
  [season universe]
  (let [fetched  (parallel/all (enrichment-tasks season))
        ;; Bye weeks come from Sleeper itself (the schedule endpoint), keyed on the
        ;; :team every player already carries — complete, not just the FantasyPros
        ;; matches. Best-effort: a failed schedule fetch just leaves :bye nil.
        byes     (:sleeper/byes fetched)
        sleepers (:fantasypros/sleepers fetched)
        espn     (:espn fetched)
        prior    (:nflverse/player-stats fetched)]
    ;; Byes join on :team rather than a name key, so they get a row count but
    ;; none of the match-rate machinery — reporting them as a 0% join would be
    ;; a lie, not a diagnostic.
    (log/info (format ":sleeper/byes: %d team bye weeks" (count byes)))
    (as-> {:players (cond-> universe (seq byes) (sleeper/assoc-byes byes))
           :sources {:sleeper/byes (if (seq byes)
                                     {:ok? true :rows (count byes)}
                                     {:ok? false})}} acc
      ;; The joins stay sequential and in `scoring/formats` order: only the
      ;; fetching is concurrent, so :sources reads the same every run.
      (reduce (fn [acc fmt]
                (let [ecr (get fetched (format-label :fantasypros/ecr fmt))
                      aav (get fetched (format-label :fantasypros/aav fmt))]
                  (-> acc
                      (apply-enrichment (format-label :fantasypros/ecr fmt)
                                        (scoped fmt (some-> ecr match/by-key)))
                      (apply-enrichment (format-label :fantasypros/aav fmt)
                                        (scoped fmt (some-> aav match/by-key))))))
              acc scoring/formats)
      ;; Per-position expert tiers. RB/WR/TE are scoped like every other
      ;; format-varying column; QB/K/DST publish one page for all three formats
      ;; and so join flat, exactly as ESPN does.
      (reduce (fn [acc [label pos fmt]]
                (let [by-key (some-> (get fetched label) match/by-key)]
                  (apply-enrichment acc label
                                    (if (get fantasypros/pos-formats pos)
                                      (scoped fmt by-key)
                                      by-key))))
              acc pos-tier-tasks)
      (apply-enrichment acc :fantasypros/sleepers (some-> sleepers match/by-key))
      (apply-enrichment acc :espn espn)
      ;; nflverse alone joins on GSIS rather than a name key — every universe
      ;; player already carries one, so there is nothing to guess. Its keys hold
      ;; no position, hence the explicit index for the per-position report.
      (apply-enrichment acc :nflverse/player-stats (:by-key prior)
                        {:key-fn            #(get-in % [:ids :gsis])
                         :key-position      (:positions prior)
                         :expected-partial? true}))))

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
