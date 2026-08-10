(ns draft-day.benchmark.sources.ids
  "The live-fetch flavour of `draft-day.ingestion.player-ids`.

  All the identity logic — trimming, the crosswalk builders, nickname aliases,
  season-aware name disambiguation, biography — lives in `src/` now, because the
  app needs the same joins the benchmark does and two id spaces at that seam is
  friction. What remains here is where the rows come from.

  The harness deliberately keeps fetching DynastyProcess's current file rather
  than reading the app's pinned snapshot: it scores decades of seasons and wants
  the most complete identity data available, while the app wants derivation that
  is offline, reproducible and unchanged between refreshes. Same pure builders,
  two vintages."
  (:require [draft-day.benchmark.fetch :as fetch]
            [draft-day.ingestion.player-ids :as player-ids]))

(def playerids-url player-ids/playerids-url)

;; Memoized in-process on top of the disk cache. `assemble` asks for both
;; crosswalks once per season, and without this a five-season sweep re-reads and
;; re-parses a multi-megabyte transit file ten times, then rebuilds a 12k-entry
;; map each time. The disk cache stops the *downloads*; this stops the busywork.
(def dp-rows
  (memoize
   (fn []
     (fetch/cached (fetch/cache-path "dp" "playerids-rows")
                   #(if-let [body (fetch/http-get-string playerids-url)]
                      (fetch/parse-csv body)
                      [])))))

(def crosswalk
  "{sleeper-id gsis-id} from DynastyProcess."
  (memoize (fn [] (player-ids/crosswalk-from-rows (dp-rows)))))

(def fp-crosswalk
  "{fantasypros-id gsis-id} from DynastyProcess."
  (memoize (fn [] (player-ids/fp-crosswalk-from-rows (dp-rows)))))

(def name-crosswalk
  "{match-key gsis-id} from DynastyProcess, era-blind. Prefer `name-resolver`:
  this collapses same-name collisions arbitrarily and is kept only for callers
  that genuinely have no season context."
  (memoize (fn [] (player-ids/name-crosswalk-from-rows (dp-rows)))))

(def name-candidates
  "{match-key [candidate ...]} from DynastyProcess."
  (memoize (fn [] (player-ids/name-candidates-from-rows (dp-rows)))))

(def biography
  "{gsis-id {:draft-* :birth-year}} from DynastyProcess."
  (memoize (fn [] (player-ids/biography-from-rows (dp-rows)))))

(defn name-resolver
  "(fn [match-key] -> gsis-id-or-nil) for a season, over the live candidates."
  [season]
  (player-ids/name-resolver (name-candidates) season))
