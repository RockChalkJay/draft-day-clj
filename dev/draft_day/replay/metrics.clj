(ns draft-day.replay.metrics
  "Aggregate replay rows into error metrics. The headline question: does the
  engine's Worth predict actual auction prices better than raw Value or MKT? If
  not, the inflation/phase-decay machinery isn't earning its keep.")

(defn- mean [xs] (if (empty? xs) 0.0 (/ (reduce + 0.0 xs) (count xs))))

(defn- avg-ranks
  "Fractional (tie-averaged) ranks of xs, in the original order."
  [xs]
  (let [groups (reduce (fn [m [i v]] (update m v (fnil conj []) (inc i)))
                       {} (map-indexed vector (sort xs)))
        rank   (into {} (map (fn [[v is]] [v (mean is)])) groups)]
    (mapv rank xs)))

(defn- pearson [xs ys]
  (let [mx (mean xs) my (mean ys)
        dx (map #(- % mx) xs) dy (map #(- % my) ys)
        cov (reduce + (map * dx dy))
        sx  (Math/sqrt (reduce + (map #(* % %) dx)))
        sy  (Math/sqrt (reduce + (map #(* % %) dy)))]
    (if (or (zero? sx) (zero? sy)) 0.0 (/ cov (* sx sy)))))

(defn spearman [a b]
  (if (< (count a) 2) 0.0 (pearson (avg-ranks a) (avg-ranks b))))

(defn metric
  "Error metrics for one predictor key (:worth/:value/:market) over rows. K/DST are
  dropped (the model prices them $0 by design); rows lacking the predictor drop too."
  [rows pred-key]
  (let [rows (->> rows
                  (remove #(#{"K" "DST"} (:position %)))
                  (filter #(number? (pred-key %))))
        errs (mapv #(- (double (pred-key %)) (double (:actual %))) rows)
        abss (mapv #(Math/abs (double %)) errs)]
    {:n        (count rows)
     :mae      (mean abss)
     :rmse     (Math/sqrt (mean (mapv #(* % %) errs)))
     :bias     (mean errs)
     :spearman (spearman (mapv pred-key rows) (mapv :actual rows))}))

(defn- phase [frac] (cond (< frac (/ 1.0 3)) :early (< frac (/ 2.0 3)) :mid :else :late))

(defn by-phase
  "Worth-vs-actual metrics per early/mid/late third (plain map; iterate in fixed
  order). Negative late bias is the signature of phase-decay over-deflation
  (audit finding #3)."
  [rows pred-key]
  (let [g (group-by (comp phase :filled-frac) rows)]
    (into {} (for [ph [:early :mid :late] :when (g ph)]
               [ph (metric (g ph) pred-key)]))))

(defn by-position [rows pred-key]
  (->> (remove #(#{"K" "DST"} (:position %)) rows)
       (group-by :position)
       (map (fn [[k rs]] [k (metric rs pred-key)]))
       (into (sorted-map))))
