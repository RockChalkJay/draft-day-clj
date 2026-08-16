(ns draft-day.rankings.tcm
  "Piece 3: Tier-Cliff Multiplier (live). Recomputed over the UNDRAFTED subset so
  cliff urgency stays honest as players leave the board."
  (:require [draft-day.rankings.tiers :as tiers]))

(def DROP-THRESHOLD
  "How steep a two-slot fall has to be before it reads as a cliff.

  This is tcm's own constant and no longer has a counterpart in `tiers`, which
  now cuts a fixed number of tiers at the biggest gaps rather than thresholding
  each drop. The two still share `tiers/relative-drop` so 'a fall of x%' means
  one thing on the board; only tcm asks the threshold question, and it asks it
  across two roster slots — 'if I pass, what is still here when it is my turn
  again?' — so the same steepness shows up as a bigger number than a
  one-slot rule would want."
  0.10)

(defn- tcm-for-position
  "Seq of [player-id tcm] for one position's undrafted players: compare each
  player's points to the player two spots below; if the drop exceeds 10%,
  tcm = 1 + drop, else 1.0. The last two (nobody two below) and zero-point
  players fall back to 1.0."
  [grp]
  (let [sorted (vec (sort-by :points > grp))
        n      (count sorted)
        pts    (mapv #(double (:points %)) sorted)]
    (map-indexed
     (fn [i p]
       (let [cur    (pts i)
             below2 (when (< (+ i 2) n) (pts (+ i 2)))
             tcm    (if (and below2 (> cur 0.0))
                      (let [drop (tiers/relative-drop cur below2)]
                        (if (> drop DROP-THRESHOLD) (+ 1.0 drop) 1.0))
                      1.0)]
         [(:player-id p) tcm]))
     sorted)))

(defn with-tcm
  "Assoc :tcm on each undrafted player; drafted players get nil (never consumed)."
  [board league-state]
  (let [drafted   (:drafted-player-ids league-state)
        undrafted (remove #(contains? drafted (:player-id %)) board)
        tcm-map   (into {} (mapcat tcm-for-position (vals (group-by :position undrafted))))]
    (mapv #(assoc % :tcm (get tcm-map (:player-id %))) board)))
