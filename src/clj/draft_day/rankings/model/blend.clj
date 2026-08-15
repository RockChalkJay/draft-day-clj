(ns draft-day.rankings.model.blend
  "Candidate ranking models that re-order the board by blending the scored
  projection with other signals, per position.

  Registered here but NOT wired to the board — these exist so the benchmark
  harness has something to compare against `:points`. Requiring this namespace
  registers them.

  BOTH OF THESE LOST. Measured over 2021-2025 (`lein run -m
  draft-day.benchmark.report --compare points points+adp`), neither beats the
  plain scored projection:

    position   :points      :points+adp   :points+adp+fade
    QB          25/36        25/36         25/36
    RB          40/60        41/60         40/60
    TE          18/30        17/30         17/30   (worst-rho 0.400 -> 0.27/0.29)
    WR          31/60        31/60         32/60

  Both looked like improvements on two seasons and evaporated on five. Do not
  promote either to the live board on the strength of the weights below; re-run
  the harness first. They are kept because a candidate that has been measured and
  rejected is worth more than one nobody has tried.

  All blending happens in z-space within a position, then `rank-transfer` maps
  the new order back onto that position's existing points distribution. So a
  model changes *who* is ranked where, never the scale of :points.

  The weights below are provisional. They were fitted on two seasons (2024-2025,
  n=22-72 per position), which is not enough to trust them — measuring the
  differences is the entire reason the harness exists. Treat them as the starting
  hypothesis, not a result, and override via ctx :weights."
  (:require [draft-day.rankings.model :as model]
            [draft-day.scoring :as scoring]))

(def default-weights
  "Per-position {:points w :adp w}, falling back to :default. Projection-heavy
  for RB, consensus-heavy for WR/TE, projection-only for QB (where blending
  showed no stable gain across the two seasons measured)."
  {"RB"     {:points 0.8 :adp 0.2}
   "WR"     {:points 0.4 :adp 0.6}
   "TE"     {:points 0.4 :adp 0.6}
   "QB"     {:points 1.0 :adp 0.0}
   :default {:points 1.0 :adp 0.0}})

(def default-fade-weights
  "A deliberately uniform hypothesis rather than per-position fitted values.

  Prior-season usage correlates *negatively* with the projection's residual at
  every position (WR -0.37/-0.21, TE -0.40/-0.36 over 2024-2025): players with
  heavy prior usage tend to underperform their projection, because the projection
  has already extrapolated that usage and over-applied it. The per-position
  weights that best fit those two seasons disagreed with each other in sign,
  which is the signature of overfitting — so the shipped default is one small
  negative weight applied uniformly, for the harness to confirm or kill."
  {:default {:points 0.6 :adp 0.3 :prior-ppg -0.1}})

(defn weights-for [weights position]
  (or (get weights position) (get weights :default) {:points 1.0}))

(defn adp-column
  "ADP as a 'higher is better' signal. ADP is a draft slot, so it is negated;
  players with no ADP take the group's worst observed value rather than a
  sentinel, which keeps them last without letting an outlier distort the z-score.
  A group where nobody has an ADP yields all-equal values, which z-scores to
  zeros — the term then contributes nothing.

  Reads a canonical `:adp` first so the benchmark can supply a deeper vintage
  source (Fantasy Football Calculator reaches 2010, Sleeper only 2021), falling
  back to `:sleeper/adp` as the live pipeline populates it."
  [group]
  (let [adp-of  (fn [p] (or (:adp p) (:sleeper/adp p)))
        present (keep adp-of group)
        worst   (if (seq present) (apply max present) 0.0)]
    (mapv #(- (double (or (adp-of %) worst))) group)))

(defn blend-scores
  "Weighted sum of z-scored signals for one position group. `signals` is
  {weight-key (fn [group] -> seq of raw values)}; a weight of 0 (or absent)
  skips the signal entirely."
  [group w signals]
  (let [cols (for [[k f] signals
                   :let [weight (double (get w k 0.0))]
                   :when (not (zero? weight))]
               (mapv #(* weight %) (model/zscores (vec (f group)))))]
    (if (seq cols)
      (apply mapv + cols)
      (vec (repeat (count group) 0.0)))))

(defn blend-model
  "Build a model fn from a signal map and a default weight table."
  [signals default-w]
  (fn [{:keys [scoring weights]} board]
    (let [weights (or weights default-w)
          scored  (scoring/with-points board scoring)]
      (model/by-position
       scored
       (fn [group]
         (let [w      (weights-for weights (:position (first group)))
               blend  (blend-scores group w signals)
               by-id  (zipmap (map :player-id group) blend)]
           (model/rank-transfer group #(get by-id (:player-id %) 0.0))))))))

(def ^:private points-signal
  {:points (fn [group] (map #(double (or (:points %) 0.0)) group))})

(def ^:private adp-signal
  {:adp adp-column})

(def ^:private prior-signal
  {:prior-ppg (fn [group] (map #(double (or (:prior/ppg %) 0.0)) group))
   :prior-wopr (fn [group] (map #(double (or (:prior/wopr %) 0.0)) group))})

(defmethod model/score-board :points+adp
  [_ ctx board]
  ((blend-model (merge points-signal adp-signal) default-weights) ctx board))

(defmethod model/requires :points+adp [_] #{:projections :adp})

(defmethod model/score-board :points+adp+fade
  [_ ctx board]
  ((blend-model (merge points-signal adp-signal prior-signal) default-fade-weights)
   ctx board))

(defmethod model/requires :points+adp+fade [_] #{:projections :adp :prior-usage})

;; ---- rookie draft capital ----

(def default-rookie-weight
  "How far draft capital may move a rookie relative to other rookies.

  Untuned — a starting hypothesis for the harness, not a fitted result. 0.3 is
  deliberately modest: the claim being tested is that draft capital carries
  information a projection has not already absorbed, and a large weight would
  make a null result unreadable."
  0.3)

(defmethod model/score-board :points+rookie-capital
  [_ {:keys [scoring weights]} board]
  (let [w      (or (:rookie weights) default-rookie-weight)
        scored (scoring/with-points board scoring)]
    (model/by-position
     scored
     (fn [group]
       (let [zp      (model/zscores (mapv #(double (or (:points %) 0.0)) group))
             rookies (filterv #(and (:rookie? %) (:draft/overall %)) group)
             ;; Draft capital is z-scored among ROOKIES ONLY, so it re-orders
             ;; first-year players relative to each other without shifting
             ;; rookies as a class up or down against veterans — that would be a
             ;; different and much stronger claim than the one being tested.
             zr      (when (> (count rookies) 1)
                       (zipmap (map :player-id rookies)
                               (model/zscores (mapv #(- (double (:draft/overall %))) rookies))))
             blend   (zipmap (map :player-id group)
                             (map (fn [p z]
                                    (+ z (* w (get zr (:player-id p) 0.0))))
                                  group zp))]
         (model/rank-transfer group #(get blend (:player-id %) 0.0)))))))

(defmethod model/requires :points+rookie-capital [_] #{:projections})

;; ---- consensus-only baseline ----

(defmethod model/score-board :adp
  [_ _ctx board]
  ;; Draft the board in ADP order. :points here is ORDINAL ONLY — a monotone
  ;; decreasing transform of draft slot, not a points projection — because this
  ;; model exists to be measured, and the metrics only read the ordering. It is
  ;; not a candidate for the live board, where :points feeds VORP and dollars.
  (model/by-position
   board
   (fn [group]
     (let [worst (inc (double (apply max 0.0 (keep #(or (:adp %) (:sleeper/adp %)) group))))]
       (mapv (fn [p]
               (assoc p :points (- worst (double (or (:adp p) (:sleeper/adp p) worst)))))
             group)))))

(defmethod model/requires :adp [_] #{:adp})

(defmethod model/score-board :ecr
  [_ _ctx board]
  ;; Rank by expert consensus alone. Like :adp, :points here is ORDINAL ONLY —
  ;; a monotone decreasing transform of the ECR rank, since the metrics read only
  ;; the ordering. ECR carries no stat line, so this model is scoring-agnostic
  ;; and cannot honour a custom league; it exists to test rank-based ideas across
  ;; the fifteen seasons of archived cheatsheets.
  (model/by-position
   board
   (fn [group]
     (let [worst (inc (double (apply max 0.0 (keep :ecr group))))]
       (mapv (fn [p] (assoc p :points (- worst (double (or (:ecr p) worst))))) group)
       ))))

(defmethod model/requires :ecr [_] #{:ecr})
