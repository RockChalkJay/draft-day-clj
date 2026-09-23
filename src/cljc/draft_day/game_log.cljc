(ns draft-day.game-log
  "Build a weekly player-detail table scored under the league's rules. Missing
  weeks remain as rows with nil values; sparse stats on a played week read as
  zero. Sleeper's realized line takes precedence over nflverse's when present."
  (:require [draft-day.scoring :as scoring]
            [draft-day.stat-lines :as sl]))

(defn- row [columns scoring entry week]
  (let [stats (:stats entry)
        value (fn [[_ ks]] (if entry (or (sl/combine stats ks) 0) nil))]
    {:week     week
     :opponent (:opponent entry)
     :played?  (some? entry)
     :values   (mapv value columns)
     :points   (when entry (scoring/player-points {:stats stats} scoring))}))

(defn table
  "nil when there is nothing to draw: a kicker, a defense, or a season that has
  not started."
  [player through-week scoring]
  (when-let [columns (get sl/position-rows (:position player))]
    (when (and through-week (pos? through-week))
      (let [by-week (into {} (map (juxt :week identity))
                          (or (:realized/game-log player)
                              (:nflverse/game-log player)))]
        {:columns columns
         :rows    (mapv #(row columns scoring (by-week %) %)
                        (range 1 (inc through-week)))}))))
