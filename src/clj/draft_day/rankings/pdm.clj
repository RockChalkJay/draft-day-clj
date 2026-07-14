(ns draft-day.rankings.pdm
  "Piece 4: Positional Demand Multiplier (live). A league-wide scarcity signal —
  how many empty slots chase how many elite (tier<=2) undrafted players at each
  position. League-wide (not per-team) because worth predicts a market price."
  (:require [draft-day.rankings.league-state :as ls]))

(def pdm-positions ["QB" "RB" "WR" "TE"])  ; K/DST never included
(def ^:private flex-positions #{"RB" "WR" "TE"})

(defn- scarcity-multiplier [needed avail-elite]
  (let [elite (if (zero? avail-elite) 0.1 avail-elite)  ; guards /0
        sr    (/ (double needed) elite)]
    (if (> sr 1.0)
      (min 1.25 (+ 1.0 (* (- sr 1.0) 0.1)))
      1.0)))

(defn calculate-pdm
  "Return {pos multiplier} for QB/RB/WR/TE. `needed` = empty slots at the position
  plus a third of the empty FLEX slots for RB/WR/TE; the multiplier rises with
  need/elite-supply, capped at 1.25."
  [board league-state]
  (let [drafted   (:drafted-player-ids league-state)
        undrafted (remove #(contains? drafted (:player-id %)) board)
        empty     (ls/empty-slots-by-pos league-state)
        flex      (get empty "FLEX" 0)
        elite     (reduce (fn [m pos]
                            (assoc m pos
                                   (count (filter #(and (= (:position %) pos)
                                                        (<= (:tier %) 2))
                                                  undrafted))))
                          {} pdm-positions)]
    (reduce (fn [m pos]
              (let [needed (+ (get empty pos 0)
                              (if (flex-positions pos) (/ flex 3.0) 0))]
                (assoc m pos (scarcity-multiplier needed (get elite pos)))))
            {} pdm-positions)))
