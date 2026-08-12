(ns draft-day.rankings.tiers
  "Piece 1: tiering by cliff detection (static). Single position in, single
  position out — the caller filters by position first.")

(def DROP-THRESHOLD
  "A cliff is a fall of more than this share of a player's own points to the next
  player at the position.

  Relative, not absolute. Ranking raw gaps and taking the biggest k put a WR tier
  break at #56 — an 18-point gap behind a 155-point WR reads as huge next to a
  26-point gap behind a 310-point WR1, even though the second is the one that
  changes a draft. It also forced exactly k tiers whether or not the position had
  k cliffs, which left one tier holding 114 of 137 RBs.

  Lower than `tcm/DROP-THRESHOLD` because that one measures across two roster
  slots and this one across one. At 0.05 the live board splits each position into
  three to six tiers; by 0.10 RB and WR collapse to two."
  0.05)

(defn- pts [player] (double (or (:points player) 0)))

(defn relative-drop
  "Fall from `from` to `to` as a share of `from`; 0.0 when `from` is not positive.
  Shared with `tcm` so a cliff means one thing across the board — the two differ
  only in threshold and lookahead, and say so."
  [from to]
  (let [f (double from)]
    (if (pos? f) (/ (- f (double to)) f) 0.0)))

(defn tiers-by-cliffs
  "Return one position's players sorted descending by :points with a 1-indexed
  :tier.

  Tiers are cut wherever the drop to the next player exceeds DROP-THRESHOLD, so
  how many there are follows the shape of the position instead of a number the
  manager has to guess at. Only players above `replacement-level` are tiered;
  the rest share the final tier, because 'worse than the player you can have for
  $1' is the only distinction that tail supports. A nil level (K and DST, which
  are never priced and so have no replacement level) tiers the whole pool."
  ([players] (tiers-by-cliffs players nil))
  ([players replacement-level]
   (let [sorted (vec (sort-by :points > players))
         n      (count sorted)
         cutoff (if (nil? replacement-level)
                  n
                  (count (take-while #(> (pts %) (double replacement-level)) sorted)))
         tiers  (if (zero? cutoff)
                  []
                  (reductions (fn [t i]
                                (if (> (relative-drop (pts (sorted (dec i))) (pts (sorted i)))
                                       DROP-THRESHOLD)
                                  (inc t)
                                  t))
                              1 (range 1 cutoff)))
         tail   (when (< cutoff n)
                  (repeat (- n cutoff) (if (seq tiers) (inc (last tiers)) 1)))]
     (mapv #(assoc %1 :tier %2) sorted (concat tiers tail)))))
