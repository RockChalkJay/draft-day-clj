(ns draft-day.rankings.tiers.ecr
  "Tier strategy :ecr — FantasyPros' published expert tiers.

  Both scales are looked up, not derived. FantasyPros publishes an overall tier
  on its consensus cheatsheet and a separate, finer tier on each position's own
  cheatsheet (twelve tiers across 177 RBs, where those same RBs share only a
  handful of overall tiers), and ingestion scrapes both. Dense-ranking the
  overall tier within a position would have been an approximation of a number the
  vendor already publishes.

  Coverage is partial by construction — FantasyPros ranks roughly 475 of the ~633
  players the board carries, and the two scales are scraped from different pages
  so their coverage differs independently. A player missing from a scale is
  absent from that map and renders as the board's untiered bucket. Falling back
  to a computed tier is the failure this strategy exists to avoid: it would put
  two techniques inside one legend with nothing saying which you are reading."
  (:require [draft-day.rankings.tiers :as tiers]))

(defn expert-tier
  "`k`'s value on `p` as a positive long, or nil. Guards the vendor columns
  rather than the callers: a scrape that yields 0, a string or a negative is a
  parse failure, and 'no tier' is the honest reading of it."
  [k p]
  (let [t (get p k)]
    (when (and (number? t) (pos? t)) (long t))))

(defn- by-id [board k]
  (into {} (keep (fn [p] (when-let [t (expert-tier k p)] [(:player-id p) t]))) board))

(defmethod tiers/tier-board :ecr
  [_ _ctx board]
  {:overall  (by-id board :fantasypros/ecr-tier)
   :position (by-id board :fantasypros/ecr-pos-tier)})
