(ns draft-day.rankings.tiers
  "Piece 1: tiering by cliff detection (static). Single position in, single
  position out — the caller filters by position first.")

(defn tiers-by-cliffs
  "Return the players sorted descending by :points with a 1-indexed :tier.

  Tiers are cut at the largest score gaps, choosing only from gaps strictly > 0
  (so identical scores are never split into different tiers), using
  min(num-tiers-1, count of nonzero gaps) of them. When (count) <= num-tiers or
  num-tiers <= 1, every player lands in tier 1."
  ([players] (tiers-by-cliffs players 5))
  ([players num-tiers]
   (let [sorted (vec (sort-by :points > players))
         n      (count sorted)]
     (cond
       (zero? n) sorted
       (or (<= n num-tiers) (<= num-tiers 1))
       (mapv #(assoc % :tier 1) sorted)

       :else
       (let [scores  (mapv #(double (:points %)) sorted)
             gaps    (mapv (fn [i] (- (scores i) (scores (inc i)))) (range (dec n)))
             nonzero (filter #(> (gaps %) 0) (range (count gaps)))
             k       (min (dec num-tiers) (count nonzero))
             ;; k largest strictly-positive gaps; ties broken by earlier index.
             chosen  (set (take k (sort-by (fn [i] [(- (gaps i)) i]) nonzero)))]
         (loop [i 0, current 1, acc (transient [])]
           (if (= i n)
             (persistent! acc)
             (recur (inc i)
                    (if (chosen i) (inc current) current)
                    (conj! acc (assoc (sorted i) :tier current))))))))))
