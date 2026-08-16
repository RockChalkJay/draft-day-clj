(ns draft-day.rankings.tiers
  "Piece 1: tiering (static). Cuts a board into groups the manager can treat as
  interchangeable, at two scales — within a position, and across the whole board.

  There is exactly one tiering technique here, deliberately. Comparing several
  providers' tiers side by side is worth doing eventually, and this namespace is
  where that seam would go (a strategy keyword dispatching to a pure
  board -> {player-id tier} function, shaped like `rankings.model`); until there
  is a second technique worth shipping, a multimethod with one implementation is
  machinery, not a seam. FantasyPros' published tiers ride along as display
  columns rather than as a rival technique.")

(def TARGET-TIER-SIZE
  "How many players a tier should hold, per scale. The count follows from this
  and the pool's depth (`tier-count`); it is not itself configured.

  Sizing beats counting because the pools differ. Measured on the sample at
  12 teams, in every scoring format: RB and WR have 30 players above replacement,
  QB, TE, K and DST have 12. One tier count across that spread means either fat
  tiers at WR or tiers of one at QB — where a *size* says the same thing about
  both, and 'about four interchangeable players' is a claim that can be checked
  against a board.

  The overall pool is one pool, ~84 deep at 12 teams, so 12 is roughly a round's
  worth of the board. It is deliberately a constant rather than `num-teams`: a
  tier count that moved with league size would stop tier 3 meaning the same thing
  between the leagues one manager runs."
  {:overall 12 :position 4})

(def MAX-TIERS
  "Ceiling on tiers per scale, whatever the pool's depth asks for.

  The board spends a fixed hue budget on tiers (see `views/board.cljs`), so past
  a dozen the stripes stop being tellable apart and the tier number stops being
  a thing you can hold in your head. Nothing on a 12-team board comes close;
  this is here for the deep-league case."
  12)

(def MIN-TIER-SIZE
  "No tier may hold fewer than this many players.

  A one-player tier is not a tier, it is a rank with extra styling: the whole
  claim a tier makes is 'these are interchangeable, do not pay up for the top of
  the group', and a group of one cannot make it. Same idea as `TARGET-TIER-SIZE`
  from the other end — the target sets the count, this is the floor `cut-points`
  refuses to breach when the target's count does not fit."
  2)

(defn tier-count
  "How many tiers a pool of `n` gets at `target` players per tier. At least 2 (a
  pool worth tiering at all has a top and a bottom), at most `MAX-TIERS`."
  [n target]
  (-> (/ (double n) (double target)) Math/round long (max 2) (min MAX-TIERS)))

(defn- metric-fn
  "Reader for the score a tiering pass cuts on. Missing reads as 0, so a player
  the metric was never computed for sorts to the tail instead of throwing."
  [k]
  (fn [player] (double (or (k player) 0))))

(defn relative-drop
  "Fall from `from` to `to` as a share of `from`; 0.0 when `from` is not positive.

  Used by `tcm`, now its only caller: tiering itself ranks absolute gaps (see
  `cut-points`). Kept here rather than moved into `tcm` because 'a fall of x%' is
  a board-wide notion, and a second copy of it is how two definitions of a cliff
  start to drift."
  [from to]
  (let [f (double from)]
    (if (pos? f) (/ (- f (double to)) f) 0.0)))

(defn cut-points
  "Indices of `scores` (descending) where a tier boundary falls: the biggest
  `(dec tier-count)` gaps that leave no segment shorter than `min-size`. Returns
  a sorted set; index i means i starts a new tier.

  Gaps are ranked *absolutely*, which reverses the relative-drop threshold this
  replaced. A relative drop is measured against the falling player, so it grows
  without bound as the metric decays toward zero and drags nearly every cut into
  the tail — on the sample board that came out as one 13-player top tier above
  four 2-player tiers, exactly backwards. Absolute gaps give tiers the shape they
  are supposed to have: small, sharply separated groups at the top where the
  money is, wide undifferentiated ones at the bottom.

  The old objection to absolute gaps (a fat gap deep in the tail outranking a
  real one up top) is answered by *where* this is called rather than by the
  metric: the caller has already truncated the pool at replacement, so there is
  no tail left to be fooled by.

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
  "Return `players` sorted descending by the score key, each with a 1-indexed
  :tier.

  The pool above `replacement-level` is cut at its biggest gaps (`cut-points`)
  into as many tiers as its depth asks for (`tier-count`); everything at or below
  replacement shares the final tier, because 'worse than the player you can have
  for $1' is the only distinction that tail supports. A nil level tiers the whole
  pool — see `tier-floor`, which is how K and DST get one anyway.

  Opts: `:score-key` (default :points; the overall scale cuts on :vorp) and
  `:target-size` (default the positional target)."
  ([players] (tiers-by-cliffs players nil {}))
  ([players replacement-level] (tiers-by-cliffs players replacement-level {}))
  ([players replacement-level {:keys [score-key target-size]
                               :or   {score-key   :points
                                      target-size (:position TARGET-TIER-SIZE)}}]
   (let [score  (metric-fn score-key)
         sorted (vec (sort-by score > players))
         n      (count sorted)
         cutoff (if (nil? replacement-level)
                  n
                  (count (take-while #(> (score %) (double replacement-level)) sorted)))
         cuts   (cut-points (mapv score (subvec sorted 0 cutoff))
                            (tier-count cutoff target-size)
                            MIN-TIER-SIZE)
         tiers  (when (pos? cutoff)
                  (reductions (fn [t i] (if (contains? cuts i) (inc t) t))
                              1 (range 1 cutoff)))
         tail   (when (< cutoff n)
                  (repeat (- n cutoff) (if (seq tiers) (inc (last tiers)) 1)))]
     (mapv #(assoc %1 :tier %2) sorted (concat tiers tail)))))

(defn tier-floor
  "Points below which a position's tail collapses into one tier.

  Valuation's replacement level where there is one. K and DST are deliberately
  absent from that map so they price at $0, but they still need a floor: without
  one, tiering the whole pool spends every tier on 44 kickers nobody drafts.
  Exactly one of each starts, so the num-teams-th best is the same boundary the
  priced positions get.

  `sorted` is the position group already in descending :points order. It used to
  sort a second copy of the group for itself; handing the same vector to
  `tiers-by-cliffs` leaves one real sort per position, since the defensive sort
  there costs a linear pass on input that is already ordered."
  [sorted level num-teams]
  (or level
      (when (seq sorted)
        (double (:points (nth sorted (min num-teams (dec (count sorted)))))))))

(defn- id->tier [tiered]
  (into {} (map (juxt :player-id :tier)) tiered))

(defn with-tiers
  "Assoc :tiers {:overall n :position n} on every player, plus :tier — the flat
  alias for the positional tier that this pipeline has always written.

  Both scales are always computed and shipped side by side, so the board picks
  the one that matches its current filter without a round trip. The positional
  scale answers 'who else is as good as this at his position'; the overall scale
  answers 'is this RB the same buy as that WR', which is a different question and
  needs a different score to answer.

  `ctx` carries :replacement-levels and :num-teams."
  [board {:keys [replacement-levels num-teams]}]
  (let [positional (into {}
                         (mapcat (fn [[pos grp]]
                                   (let [sorted (vec (sort-by :points > grp))]
                                     (id->tier
                                      (tiers-by-cliffs
                                       sorted
                                       (tier-floor sorted (get replacement-levels pos)
                                                   num-teams))))))
                         (group-by :position board))
        ;; Cut on VORP, the only score that compares a QB to an RB — points are
        ;; on a different scale at every position. Replacement level is 0.0
        ;; because VORP is zero at replacement by construction, so the tail rule
        ;; above collapses every below-replacement player (K and DST included)
        ;; into one final tier.
        overall    (id->tier (tiers-by-cliffs board 0.0
                                              {:score-key   :vorp
                                               :target-size (:overall TARGET-TIER-SIZE)}))]
    (mapv (fn [p]
            (let [id  (:player-id p)
                  pos (get positional id)]
              (assoc p :tiers {:overall (get overall id) :position pos}
                     :tier pos)))
          board)))
