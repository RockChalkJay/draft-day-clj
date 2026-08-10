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

(defn enrich-universe
  "Left-join the best-effort enrichment columns onto an already-validated
  universe. Split out from the fetch so the validation gate below reads as the
  gate it is."
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
    (cond-> universe
      (seq byes)     (sleeper/assoc-byes byes)
      (seq ecr)      (merge/left-join (match/by-key ecr))
      (seq aav)      (merge/left-join (match/by-key aav))
      (seq sleepers) (merge/left-join (match/by-key sleepers))
      (seq espn)     (merge/left-join espn))))

(defn fetch-enriched-universe
  "The live universe: Sleeper rows, id-validated, then enriched.

  Ids are validated before any enrichment runs — it fails fast, and the
  enrichments left-join by name key so they could not repair a bad id anyway. A
  systemic failure throws, which `load-universe` catches into the stale-cache
  branch: serving last night's board beats serving a structurally broken one."
  [season]
  (let [{:keys [players report]} (validate/validate-universe
                                  (sleeper/fetch-universe season))]
    (validate/log-report! "sleeper universe" report)
    (when (validate/systemic-failure? report)
      (throw (ex-info "player universe failed validation" report)))
    {:players    (enrich-universe season players)
     :validation report}))

(defn sample-universe
  "The bundled fallback as an envelope. It carries no `:fetched-at` — it is a
  committed artifact, so its age is the repo's, not a fetch's."
  []
  (merge {:schema-version schema-version :season nil :fetched-at nil
          :source "sample"}
         (checked "sample" (load-sample))))

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
          {:keys [players validation]} (fetch-enriched-universe season')
          env {:schema-version schema-version
               :season         season'
               :fetched-at     (now-iso)
               :validation     validation
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
