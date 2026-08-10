(ns draft-day.benchmark.metrics
  "Score a model's ranking against realized outcomes.

  Spearman rho is the precise measure and the useless one to read: a rho gap of
  0.02 is invisible on draft day. So the headline metric here is the hit rate —
  of the top N this model ranked at a position, how many actually finished top N
  — plus median realized finish and outright busts. Those are the units a
  manager experiences, and they are what showed that projection and consensus ADP
  are effectively interchangeable despite a 0.15 rho gap.

  Everything is reported per season rather than pooled. Stability across seasons
  is the whole question; an average over five seasons hides the one where a model
  fell apart."
  (:require [draft-day.replay.metrics :as replay-metrics]))

(def default-top-n
  "Starters a 12-team league fields at each position — the slice that decides a
  season. TE and QB are single-slot, RB/WR carry flex."
  {"QB" 12 "RB" 12 "WR" 12 "TE" 6})

(defn mean [xs]
  (if (empty? xs) 0.0 (/ (reduce + 0.0 xs) (count xs))))

(defn median [xs]
  (if (empty? xs)
    0.0
    (let [v (vec (sort xs)) n (count v) m (quot n 2)]
      (if (odd? n) (double (nth v m)) (/ (+ (nth v (dec m)) (nth v m)) 2.0)))))

(defn finish-map
  "{player-id realized-finish} — 1 is the best actual finish in the group."
  [group truth-key]
  (into {}
        (map-indexed (fn [i p] [(:player-id p) (inc i)]))
        (sort-by #(- (double (or (truth-key %) 0.0))) group)))

(defn hit-metrics-meaningful?
  "Is the pool deep enough for a top-N hit rate to say anything?

  Picking 12 of 14 is nearly the whole pool, so the hit rate would flatter every
  model equally. 2N is the threshold."
  [group-size n]
  (>= group-size (* 2 n)))

(defn position-metrics
  "Metrics for one position group under one truth key (:actual/points or
  :actual/ppg). `n` is the top-N slice size.

  Rank correlation is computed for EVERY group; only the top-N family (hits,
  median finish, busts) is suppressed when the pool is too thin. Dropping the
  whole group threw away a perfectly good rho along with the hit rate, and it
  did so selectively: quarterback pools land at 20-23 in some seasons and 24 in
  others, so QB was scored in 10 of 16 seasons and those 10 were exactly the
  deeper-pool ones — a silent selection effect on the position with the least
  data to spare."
  [group n truth-key]
  (let [finish   (finish-map group truth-key)
        by-model (sort-by #(- (double (or (:points %) 0.0))) group)
        picks    (take n by-model)
        finishes (map #(finish (:player-id %)) picks)
        elite    (set (map :player-id (take n (sort-by #(- (double (or (truth-key %) 0.0))) group))))
        actuals  (mapv #(double (or (truth-key %) 0.0)) group)
        modelled (mapv #(double (or (:points %) 0.0)) group)
        ;; points-weighted rank error: missing on RB1 costs far more than missing
        ;; on WR40, which flat rho weights identically.
        m-rank   (into {} (map-indexed (fn [i p] [(:player-id p) (inc i)])) by-model)
        wsum     (reduce + 0.0 actuals)]
    (cond-> {:n        (count group)
             :spearman (replay-metrics/spearman modelled actuals)
             :weighted-rank-error
             (if (pos? wsum)
               (/ (reduce + 0.0 (map (fn [p]
                                       (* (double (or (truth-key p) 0.0))
                                          (Math/abs (double (- (m-rank (:player-id p))
                                                               (finish (:player-id p)))))))
                                     group))
                  wsum)
               0.0)}
      (hit-metrics-meaningful? (count group) n)
      (assoc :top-n         n
             :hits          (count (filter #(elite (:player-id %)) picks))
             :median-finish (median finishes)
             :busts         (count (filter #(> % (* 2 n)) finishes))))))

(defn season-metrics
  "Per-position metrics for one season's scored board.

  opts: :top-n (position -> slice size), :truth-key (default :actual/points)."
  ([players] (season-metrics players {}))
  ([players {:keys [top-n truth-key] :or {top-n default-top-n
                                          truth-key :actual/points}}]
   (into (sorted-map)
         (keep (fn [[pos group]]
                 (let [n (get top-n pos)]
                   ;; Every group with a defined slice is scored; `position-metrics`
                   ;; decides internally whether the hit-rate family is meaningful.
                   (when (and n (>= (count group) n))
                     [pos (position-metrics (vec group) n truth-key)]))))
         (group-by :position players))))

;; ---- paired comparison ----
;;
;; Two models are always scored on the SAME seasons, so comparing their pooled
;; numbers throws away the pairing and inherits the whole season-to-season
;; variance. Pairing cancels the season effect. It is free — the per-season
;; numbers are already computed — and it is not a refinement: at RB it turns a
;; difference previously reported as "interchangeable" into t=5.04.

(defn season-rho-diffs
  "Per position, [{:season :a :b :diff}] for the seasons BOTH models scored.
  `results-a`/`results-b` are `core/run` outputs."
  [results-a results-b]
  (let [idx (fn [rs] (into {} (map (juxt :season :metrics)) (remove :skipped? rs)))
        a   (idx results-a)
        b   (idx results-b)]
    (->> (for [season (sort (filter (set (keys b)) (keys a)))
               pos    (sort (distinct (concat (keys (a season)) (keys (b season)))))
               :let   [ra (get-in a [season pos :spearman])
                       rb (get-in b [season pos :spearman])]
               :when  (and ra rb)]
           {:position pos :season season :a ra :b rb :diff (- ra rb)})
         (group-by :position)
         (into (sorted-map)))))

(defn player-rank-error-diffs
  "Per position, [{:season :player-id :diff}] where diff is A's absolute rank
  error minus B's, on the players BOTH models ranked that season. Negative
  favours A.

  This is the high-power view: n runs to thousands of player-seasons rather than
  a handful of seasons. It is NOT independent — a season-wide projection miss
  moves every player together — so it must be bootstrapped by season block, never
  treated as n independent observations."
  [results-a results-b truth-key]
  (letfn [(ranks [players]
            (into {}
                  (mapcat (fn [[_ grp]]
                            (let [by-model (sort-by #(- (double (or (:points %) 0.0))) grp)
                                  by-truth (sort-by #(- (double (or (truth-key %) 0.0))) grp)
                                  mr (into {} (map-indexed (fn [i p] [(:player-id p) (inc i)])) by-model)
                                  tr (into {} (map-indexed (fn [i p] [(:player-id p) (inc i)])) by-truth)]
                              (map (fn [p] [(:player-id p)
                                            {:position (:position p)
                                             :err (Math/abs (double (- (mr (:player-id p))
                                                                       (tr (:player-id p)))))}])
                                   grp))))
                  (group-by :position players)))
          (idx [rs] (into {} (map (juxt :season (comp ranks :players))) (remove :skipped? rs)))]
    (let [a (idx results-a) b (idx results-b)]
      (->> (for [season (sort (filter (set (keys b)) (keys a)))
                 [pid ea] (get a season)
                 :let [eb (get-in b [season pid])]
                 :when eb]
             {:position (:position ea) :season season :player-id pid
              :diff (- (:err ea) (:err eb))})
           (group-by :position)
           (into (sorted-map))))))

(defn mean-of [ks] (fn [xs] (mean (map ks xs))))

(defn block-bootstrap-ci
  "Percentile CI for a statistic over rows grouped into season blocks.

  Resampling whole SEASONS rather than rows is the point: rows inside a season
  share a common shock, so resampling rows independently would understate the
  interval badly and manufacture significance."
  ([rows stat] (block-bootstrap-ci rows stat {}))
  ([rows stat {:keys [iterations alpha seed] :or {iterations 2000 alpha 0.05 seed 42}}]
   (let [blocks (vec (vals (group-by :season rows)))
         n      (count blocks)]
     (if (< n 2)
       {:point (stat rows) :lo nil :hi nil :n-blocks n :n-rows (count rows)}
       (let [rng   (java.util.Random. seed)
             draws (sort (repeatedly iterations
                                     #(stat (into [] (mapcat (fn [_] (nth blocks (.nextInt rng n))))
                                                  (range n)))))
             at    (fn [q] (nth draws (min (dec iterations)
                                           (max 0 (long (Math/floor (* q iterations)))))))]
         {:point   (stat rows)
          :lo      (at (/ alpha 2))
          :hi      (at (- 1 (/ alpha 2)))
          :n-blocks n
          :n-rows  (count rows)})))))

(defn spans-zero? [{:keys [lo hi]}]
  (or (nil? lo) (nil? hi) (and (<= lo 0.0) (>= hi 0.0))))

;; ---- statistical power ----

(defn min-detectable-difference
  "Smallest true difference detectable at ~80% power, two-sided alpha .05, given
  the observed SD of the paired per-season difference. The 2.8 is z(.975)+z(.80)."
  [sd n]
  (if (or (nil? sd) (zero? n)) nil (* 2.8 (/ sd (Math/sqrt (double n))))))

(defn seasons-needed
  "Seasons required to detect `target` given the observed paired SD."
  [sd target]
  (when (and sd (pos? target)) (long (Math/ceil (Math/pow (/ (* 2.8 sd) target) 2)))))

(defn power-summary
  "Per position: paired SD, current n, what that n can resolve, and how many
  seasons a `target` difference would need."
  [diffs-by-position target]
  (into (sorted-map)
        (map (fn [[pos rows]]
               (let [d  (map :diff rows)
                     n  (count d)
                     sd (when (> n 1) (Math/sqrt (/ (reduce + 0.0 (map #(let [x (- % (mean d))] (* x x)) d))
                                                    (dec n))))]
                 [pos {:n n
                       :mean (mean d)
                       :sd sd
                       :mdd (min-detectable-difference sd n)
                       :seasons-for-target (seasons-needed sd target)}])))
        diffs-by-position))

(defn overall-spearman
  "Rho across the whole pool. Cross-position comparability is exactly what VORP
  exists to fix, so this is a coarse summary, not the headline."
  [players truth-key]
  (replay-metrics/spearman
   (mapv #(double (or (:points %) 0.0)) players)
   (mapv #(double (or (truth-key %) 0.0)) players)))
