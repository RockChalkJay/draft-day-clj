(ns draft-day.ingestion.pipeline
  "Resolve the player universe with a TTL disk cache and a fallout chain, mirroring
  the POC: offline-sample -> fresh-cache -> live -> stale-cache -> bundled-sample.
  The cache is Transit on disk (data/players_cache.transit); the bundled sample is
  EDN on the classpath (resources/sample_players.edn)."
  (:require [draft-day.ingestion.sleeper :as sleeper]
            [draft-day.ingestion.fantasypros :as fantasypros]
            [draft-day.ingestion.espn :as espn]
            [draft-day.ingestion.merge :as merge]
            [draft-day.ingestion.match :as match]
            [draft-day.ingestion.validate :as validate]
            [clojure.tools.logging :as log]
            [cognitect.transit :as transit]
            [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def default-cache-path "data/players_cache.transit")
(def ^:private sample-resource "sample_players.edn")

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
  bundled sample): drop unusable rows and log them, but never throw."
  [label rows]
  (let [{:keys [players report]} (validate/validate-universe (or rows []))]
    (validate/log-report! label report)
    players))

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
    (enrich-universe season players)))

(defn load-universe
  "Return {:players [...] :source \"live|cache|sample|empty\"}. opts: :refresh
  (bypass fresh cache), :season, :cache-path (defaults to data/...)."
  ([] (load-universe {}))
  ([{:keys [refresh season cache-path] :or {cache-path default-cache-path}}]
   (cond
     (offline?)
     {:players (checked "sample" (load-sample)) :source "sample"}

     (and (not refresh) (cache-fresh? cache-path (cache-ttl-hours)))
     {:players (checked "cache" (read-transit cache-path)) :source "cache"}

     :else
     (try
       (let [u (fetch-enriched-universe (or season (sleeper/current-season)))]
         (write-transit! cache-path u)
         {:players u :source "live"})
       (catch Exception _
         ;; stale > fake, but only if the stale copy still validates — a cache
         ;; that drops to nothing is worse than the sample, not better.
         (or (when-let [c (seq (checked "cache" (read-transit cache-path)))]
               {:players (vec c) :source "cache"})
             (when-let [s (seq (checked "sample" (load-sample)))]
               {:players (vec s) :source "sample"})
             {:players [] :source "empty"}))))))
