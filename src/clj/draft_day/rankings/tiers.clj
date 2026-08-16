(ns draft-day.rankings.tiers
  "Piece 1: tiering (static), and the seam that makes *how* a board is tiered
  pluggable.

  Two things live here. `tiers-by-cliffs` is the cliff-detection mechanism —
  single position in, single position out, the caller filters by position first;
  it cuts a configured number of tiers (`tier-counts`) at the pool's biggest
  gaps. `tier-board` is the strategy multimethod: a *strategy* is a pure function of
  (ctx, board) -> {:overall {id tier} :position {id tier}}, and every registered
  one is computed on every request so the board can switch between them with no
  round trip.

  Dispatch mirrors `rankings.model`: the multimethod lives here, the baseline
  (`:cliffs`, which is exactly what this pipeline has always computed) is a
  `defmethod` in this namespace, and further strategies live in `tiers/*.clj` and
  register by being `:require`d for side effects."
  (:require [clojure.string :as str]))

(def tier-counts
  "How many tiers each scale is cut into. Code-level configuration; there is no
  user-facing setting.

  The two scales differ because their pools do. The overall scale ranks every
  draftable player at once, so 16 tiers is roughly the granularity a manager can
  actually hold in their head across a whole board. A position's above-replacement
  pool is a tenth that size — 16 tiers across 32 defenses would be one tier per
  two teams, which says nothing.

  These are targets, not caps: a scale is cut into exactly this many tiers unless
  the pool is too small to do it without violating `MIN-TIER-SIZE`, in which case
  `cut-points` yields as many as will fit. That is a deliberate change from the
  threshold-based rule this replaced, where the count followed the data — a
  perfectly smooth position now gets cut anyway, because a fixed count is what
  makes tiers comparable between positions and between scoring formats."
  {:overall 16 :position 8})

(def MIN-TIER-SIZE
  "No tier may hold fewer than this many players.

  A one-player tier is not a tier, it is a rank with extra styling: the whole
  claim a tier makes is 'these are interchangeable, do not pay up for the top of
  the group', and a group of one cannot make it. It also wrecks the board's
  display, since each tier spends part of a fixed hue budget."
  2)

(defn- metric-fn
  "Reader for the score a tiering pass cuts on. Missing reads as 0, so a player
  the metric was never computed for sorts to the tail instead of throwing."
  [k]
  (fn [player] (double (or (k player) 0))))

(defn relative-drop
  "Fall from `from` to `to` as a share of `from`; 0.0 when `from` is not positive.

  Used by `tcm`, which is now its only caller: tiering itself ranks absolute gaps
  (see `cut-points`). Kept here rather than moved into `tcm` because 'a fall of
  x%' is a board-wide notion, and a second copy of it is how two definitions of a
  cliff start to drift."
  [from to]
  (let [f (double from)]
    (if (pos? f) (/ (- f (double to)) f) 0.0)))

(defn cut-points
  "Indices of `scores` (descending) where a tier boundary falls: the biggest
  `(dec tier-count)` gaps that leave no segment shorter than `min-size`.
  Returns a sorted set; index i means i starts a new tier.

  Gaps are ranked *absolutely*, not by `relative-drop`. That is the reverse of
  the threshold rule this replaced, and the reason is the shape it produces:
  a relative drop is measured against the falling player, so it grows without
  bound as the metric decays toward zero. Ranking by it puts nearly every cut in
  the tail — on the sample board that came out as one 13-player top tier and four
  2-player tiers at the bottom, which is backwards. Ranking absolute gaps gives
  the shape tiers are supposed to have: small, sharply separated tiers at the top
  where the money is, wide undifferentiated ones at the bottom.

  The old objection to absolute gaps (a fat gap deep in the tail outranking a
  real one up top) is answered by *where* this is called rather than by the
  metric: the caller has already cut the pool at replacement, so there is no
  tail left to be fooled by.

  Zero gaps are never cut on, so players with identical scores cannot be split.
  Ties in gap size break toward the earlier index, so the result is a pure
  function of the scores."
  [scores tier-count min-size]
  (let [n (count scores)]
    (if (or (< n (* 2 min-size)) (< tier-count 2))
      (sorted-set)
      (loop [cands (->> (range 1 n)
                        (keep (fn [i]
                                (let [g (- (double (scores (dec i))) (double (scores i)))]
                                  (when (pos? g) [i g]))))
                        (sort-by (fn [[i g]] [(- g) i])))
             cuts  (sorted-set)]
        (if (or (empty? cands) (>= (count cuts) (dec tier-count)))
          cuts
          (let [[[i] & more] cands
                lo (or (first (rsubseq cuts <= i)) 0)
                hi (or (first (subseq cuts > i)) n)]
            (recur more (if (and (>= (- i lo) min-size) (>= (- hi i) min-size))
                          (conj cuts i)
                          cuts))))))))

(defn tiers-by-cliffs
  "Return one position's players sorted descending by its score key with a
  1-indexed :tier.

  The pool above `replacement-level` is cut into `:tier-count` tiers at its
  biggest gaps (see `cut-points`); everything at or below replacement shares the
  final tier, because 'worse than the player you can have for $1' is the only
  distinction that tail supports. A nil level (K and DST, which are never priced
  and so have no replacement level) tiers the whole pool.

  Opts: `:score-key` (default :points — `:overall` cuts on :vorp instead),
  `:tier-count` (default `(:position tier-counts)`) and `:min-size` (default
  `MIN-TIER-SIZE`)."
  ([players] (tiers-by-cliffs players nil {}))
  ([players replacement-level] (tiers-by-cliffs players replacement-level {}))
  ([players replacement-level {:keys [score-key tier-count min-size]
                               :or   {score-key  :points
                                      tier-count (:position tier-counts)
                                      min-size   MIN-TIER-SIZE}}]
   (let [pts    (metric-fn score-key)
         sorted (vec (sort-by pts > players))
         n      (count sorted)
         cutoff (if (nil? replacement-level)
                  n
                  (count (take-while #(> (pts %) (double replacement-level)) sorted)))
         cuts   (cut-points (mapv pts (subvec sorted 0 cutoff)) tier-count min-size)
         tiers  (when (pos? cutoff)
                  (reductions (fn [t i] (if (contains? cuts i) (inc t) t))
                              1 (range 1 cutoff)))
         tail   (when (< cutoff n)
                  (repeat (- n cutoff) (if (seq tiers) (inc (last tiers)) 1)))]
     (mapv #(assoc %1 :tier %2) sorted (concat tiers tail)))))

(defn tier-floor
  "Score below which a position's tail collapses into one tier.

  Valuation's replacement level where there is one. K and DST are deliberately
  absent from that map so they price at $0, but they still need a floor: kicker
  points are small enough that a 2-point drop down the tail is a 20% drop, and
  tiering the whole pool split 44 kickers into 12 tiers of mostly one. Exactly
  one of each starts, so the num-teams-th best is the same boundary the priced
  positions get.

  `sorted` is the position group already in descending :points order. It used to
  sort a second copy of the group for itself; handing the same vector to
  `tiers-by-cliffs` leaves one real sort per position, since the defensive sort
  there costs a linear pass on input that is already ordered."
  [sorted level num-teams]
  (or level
      (when (seq sorted)
        (double (:points (nth sorted (min num-teams (dec (count sorted)))))))))

;; ---- the strategy seam ----

(defmulti tier-board
  "Tier one board under `strategy`, at both scales.

  Returns {:overall {player-id tier} :position {player-id tier}} — id-keyed maps
  rather than a board, so the caller owns where the numbers land and running two
  strategies is a merge instead of a pipeline. A player absent from a scale's map
  has no tier under that strategy; a strategy must never substitute another
  strategy's number for one it lacks, which is the whole reason the two are
  separate strategies rather than one blended column.

  Tiers are 1-indexed and lower is better, on both scales. `ctx` carries
  :replacement-levels and :num-teams; individual strategies may read more.
  Dispatches on `strategy`."
  (fn [strategy _ctx _board] strategy))

(def DEFAULT-STRATEGY
  "The strategy `:tier` aliases and the board opens on. Must equal
  `db/default-tier-strategy`; a test pins the two together across the language
  boundary, because a catalog entry with no `defmethod` fails silently as
  'every player is unranked' rather than as an error."
  :cliffs)

(defn registered
  "Tier-strategy keywords with a registered implementation, :default excluded."
  []
  (vec (sort (disj (set (keys (methods tier-board))) :default))))

(defmethod tier-board :default
  [strategy _ _]
  ;; The known set goes in the *message*, not just ex-data — see rankings.model.
  (throw (ex-info (str "unknown tier strategy " (pr-str strategy)
                       " — registered strategies are "
                       (str/join ", " (map name (registered))))
                  {:strategy strategy :known (registered)})))

(defn- id->tier [tiered]
  (into {} (map (juxt :player-id :tier)) tiered))

(defmethod tier-board :cliffs
  [_ {:keys [replacement-levels num-teams]} board]
  {:position
   (into {}
         (mapcat (fn [[pos grp]]
                   (let [sorted (vec (sort-by :points > grp))]
                     (id->tier (tiers-by-cliffs
                                sorted
                                (tier-floor sorted (get replacement-levels pos)
                                            num-teams))))))
         (group-by :position board))
   :overall
   ;; Cut on VORP, the only score that compares a QB to an RB — points are on a
   ;; different scale at every position. Replacement level is 0.0 because VORP is
   ;; zero at replacement by construction, so the existing tail rule collapses
   ;; every below-replacement player (K and DST included) into one final tier.
   (id->tier (tiers-by-cliffs board 0.0
                              {:score-key  :vorp
                               :tier-count (:overall tier-counts)}))})

(defn with-tiers
  "Assoc :tiers {strategy {:overall n :position n}} on every player, running each
  strategy in `strategies` (every registered one by default).

  Both scales always exist for every strategy, which is what lets the board
  switch technique and scale without a round trip. A scale a strategy has no
  number for is simply absent, so 'untiered' survives the JSON round trip as a
  missing key rather than as a nil that JSON cannot distinguish from it anyway.

  Also assocs :tier, the back-compat alias for the default strategy's positional
  tier — exactly the number this pipeline has always written."
  ([board ctx] (with-tiers board ctx (registered)))
  ([board ctx strategies]
   (let [by-strategy (into {} (map (fn [s] [s (tier-board s ctx board)])) strategies)]
     (mapv (fn [p]
             (let [id (:player-id p)
                   ts (into {}
                            (map (fn [[s {:keys [overall position]}]]
                                   [s (cond-> {}
                                        (get overall id)  (assoc :overall (get overall id))
                                        (get position id) (assoc :position (get position id)))]))
                            by-strategy)]
               (assoc p :tiers ts :tier (get-in ts [DEFAULT-STRATEGY :position]))))
           board))))
