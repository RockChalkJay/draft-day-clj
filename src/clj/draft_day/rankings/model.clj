(ns draft-day.rankings.model
  "The ranking-model seam: how a board of players becomes a :points score.

  A *model* is a pure function of (ctx, board) -> board with :points set. Every
  later stage (tiers, floor/ceiling, replacement/VORP, value, worth) reads
  :points and is unchanged by which model produced it, so swapping models swaps
  the whole board's ordering without touching the pipeline.

  Dispatch is a multimethod on a model keyword, mirroring the league-import
  provider convention: the multimethod lives here, each model is a `defmethod`
  (in this namespace for the baseline, or in `model/*.clj` for the rest), and a
  namespace registers its models by being `:require`d for side effects.

  `:points` is the baseline and the shipped default — raw scored projection,
  i.e. exactly what the board did before this seam existed."
  (:require [clojure.string :as str]
            [draft-day.rankings.scoring :as scoring]))

(defmulti score-board
  "Assoc :points on every player. `ctx` carries at least :scoring (the league's
  {stat-key weight} map); individual models may read more. Dispatches on `model`."
  (fn [model _ctx _board] model))

(defmulti requires
  "The inputs a model consumes, a subset of #{:projections :adp :prior-usage}.

  This exists so a benchmark can run each model over the deepest history that
  actually supports it, rather than truncating everything to the shallowest
  input. Frozen preseason projections only reach 2021, but consensus ADP reaches
  2010 and realized usage reaches 1999 — so a model that never touches a
  projection has no business being denied sixteen seasons of evidence."
  identity)

(defmethod requires :default [_] #{:projections})

(defn registered
  "Model keywords with a registered implementation, :default excluded."
  []
  (vec (sort (disj (set (keys (methods score-board))) :default))))

(defmethod score-board :points
  [_ {:keys [scoring]} board]
  (scoring/with-points board scoring))

(defmethod score-board :default
  [model _ _]
  ;; The known set goes in the *message*, not just ex-data: the default error
  ;; printer shows only the message, and "unknown ranking model" on its own
  ;; leaves the caller with nowhere to go.
  (throw (ex-info (str "unknown ranking model " (pr-str model)
                       " — registered models are "
                       (str/join ", " (map name (registered))))
                  {:model model :known (registered)})))

;; ---- helpers shared by models that re-order the board ----

(defn mean [xs]
  (let [n (count xs)]
    (if (zero? n) 0.0 (/ (reduce + 0.0 xs) n))))

(defn zscores
  "Population z-scores of xs, in order. All-equal input (sd 0) yields all zeros
  rather than dividing by zero, so a signal that is constant across the pool
  simply contributes nothing to a blend."
  [xs]
  (let [m  (mean xs)
        sd (Math/sqrt (mean (mapv #(let [d (- (double %) m)] (* d d)) xs)))]
    (if (zero? sd)
      (vec (repeat (count xs) 0.0))
      (mapv #(/ (- (double %) m) sd) xs))))

(defn rank-transfer
  "Re-order a group by `score-fn` (desc) while keeping the group's multiset of
  :points unchanged: the highest-scoring player takes the group's highest points
  value, and so on.

  This is what keeps a blended model honest downstream. Blending z-scores and
  writing the blend straight into :points would put the board on an arbitrary
  scale (and admit negatives), which floor/ceiling's `mean * (1 ± band)` and the
  VORP-share dollar split are not built for. Transferring ranks onto the existing
  points distribution changes *who* is where without changing the scale."
  [group score-fn]
  (let [pts (vec (sort > (map #(double (or (:points %) 0.0)) group)))]
    (mapv (fn [p v] (assoc p :points v))
          (sort-by score-fn > group)
          pts)))

(defn by-position
  "Apply `f` to each position's players and concat. Position order is not
  preserved; callers treat the board as a set."
  [board f]
  (into [] (mapcat (fn [[_ grp]] (f (vec grp)))) (group-by :position board)))
