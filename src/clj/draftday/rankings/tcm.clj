(ns draftday.rankings.tcm
  "Piece 3: Tier-Cliff Multiplier (live). Recomputed over the UNDRAFTED subset so
  cliff urgency stays honest as players leave the board.")

(def DROP-THRESHOLD 0.10)

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
                      (let [drop (/ (- cur below2) cur)]
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
