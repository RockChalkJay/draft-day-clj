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
            [cognitect.transit :as transit]
            [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def default-cache-path "data/players_cache.transit")
(def ^:private sample-resource "sample_players.edn")

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

(defn fetch-enriched-universe
  "Sleeper universe (rows) left-joined with enrichment columns: FantasyPros ECR
  (tiers/variance/bye) and AAV (auction values), plus ESPN live auction values.
  Each enrichment is best-effort — on failure the universe is returned without
  those columns and the engine falls back gracefully."
  [season]
  (let [universe (sleeper/fetch-universe season)
        ;; Deliberate shortcut: the FantasyPros enrichments are baked into this
        ;; single, scoring-agnostic shared universe at ingestion, but they differ
        ;; by format — the ECR cheatsheet (:ppr/:half-ppr/:standard) and the AAV
        ;; calculator (scoring param). We always scrape PPR, so non-PPR leagues
        ;; get PPR-flavored tiers/rank-spread and market prices. See README todo
        ;; for the proper fix (store all variants, select at ranking time).
        ecr      (try (fantasypros/fetch-ecr :ppr) (catch Exception _ nil))
        aav      (try (fantasypros/fetch-aav) (catch Exception _ nil))
        espn     (try (espn/fetch season) (catch Exception _ nil))]
    (cond-> universe
      (seq ecr)  (merge/left-join (match/by-key ecr))
      (seq aav)  (merge/left-join (match/by-key aav))
      (seq espn) (merge/left-join espn))))

(defn load-universe
  "Return {:players [...] :source \"live|cache|sample|empty\"}. opts: :refresh
  (bypass fresh cache), :season, :cache-path (defaults to data/...)."
  ([] (load-universe {}))
  ([{:keys [refresh season cache-path] :or {cache-path default-cache-path}}]
   (cond
     (offline?)
     {:players (or (load-sample) []) :source "sample"}

     (and (not refresh) (cache-fresh? cache-path (cache-ttl-hours)))
     {:players (read-transit cache-path) :source "cache"}

     :else
     (try
       (let [u (fetch-enriched-universe (or season (sleeper/current-season)))]
         (write-transit! cache-path u)
         {:players u :source "live"})
       (catch Exception _
         (or (when-let [c (read-transit cache-path)] {:players c :source "cache"})   ; stale > fake
             (when-let [s (load-sample)] {:players s :source "sample"})
             {:players [] :source "empty"}))))))
