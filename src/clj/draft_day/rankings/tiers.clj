(ns draft-day.rankings.tiers
  "Piece 1: tiering (static). Cuts a board into groups the manager can treat as
  interchangeable, at two scales — within a position, and across the whole
  board.

  There is exactly one tiering technique here, deliberately. Comparing several
  providers' tiers side by side is worth doing eventually, and this namespace is
  where that seam would go (a strategy keyword dispatching to a pure board ->
  {player-id tier} function, shaped like `rankings.model`); until there is a
  second technique worth shipping, a multimethod with one implementation is
  machinery, not a seam. FantasyPros' published tiers ride along as display
  columns rather than as a rival technique.

  WHY GAPS ARE RANKED ABSOLUTELY (`cut-points`). This reverses the relative-drop
  threshold it replaced. A relative drop is measured against the falling player,
  so it grows without bound as the metric decays toward zero and drags nearly
  every cut into the tail — on the sample board that came out as one 13-player
  top tier above four 2-player tiers, exactly backwards. Absolute gaps give
  tiers the shape they are supposed to have: small, sharply separated groups at
  the top where the money is, wide undifferentiated ones at the bottom. The old
  objection to absolute gaps (a fat gap deep in the tail outranking a real one
  up top) is answered by *where* it is called rather than by the metric — the
  caller has already truncated the pool at replacement, so there is no tail left
  to fool it.

  WHY TIERS ARE SIZED, NOT COUNTED (`TARGET-TIER-SIZE`). The pools differ. On
  the sample at 12 teams, in every scoring format, RB and WR have 30 players
  above replacement while QB, TE, K and DST have 12. One tier count across that
  spread means either fat tiers at WR or tiers of one at QB, where a *size* says
  the same thing about both — and 'about four interchangeable players' is a
  claim that can be checked against a board.

  WHY BOTH SCALES SHIP (`with-tiers`). The positional scale answers 'who else is
  as good as this at his position'; the overall scale answers 'is this RB the
  same buy as that WR', a different question needing a different score.
  Computing both every time is what lets the board switch on a position filter
  with no refetch.")

(def TARGET-TIER-SIZE
  "How many players a tier should hold, per scale; the count follows from this
  and the pool's depth (`tier-count`). Deliberately a constant rather than
  `num-teams` — a count that moved with league size would stop tier 3 meaning
  one thing."
  {:overall 12 :position 4})

(def MAX-TIERS
  "Ceiling on the tiers a scale renders — the board's hue budget runs out past a
  dozen. Bounds the *rendered* count, tail included: `tiers-by-cliffs` spends
  one on the below-replacement tail, so counting only cuts here would render 13.
  "
  12)

(def MIN-TIER-SIZE
  "A one-player tier is a rank with extra styling, not a tier: the claim a tier
  makes is 'these are interchangeable', and a group of one cannot make it. The
  floor `cut-points` refuses to breach when the target's count does not fit."
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
  "Fall from `from` to `to` as a share of `from`; 0.0 when `from` is not
  positive. `tcm` is now its only caller — tiering ranks absolute gaps — but it
  stays here because a second copy is how two definitions of a cliff start to
  drift."
  [from to]
  (let [f (double from)]
    (if (pos? f) (/ (- f (double to)) f) 0.0)))

(defn cut-points
  "Indices of `scores` (descending) where a tier boundary falls: the biggest
  `(dec tier-count)` *absolute* gaps leaving no segment shorter than `min-size`
  — see the namespace docstring. Zero gaps are never cut on; ties break earlier.
  "
  [scores tier-count min-size]
  (let [n        (count scores)
        max-cuts (dec tier-count)
        ;; A cut at i is legal when both segments it creates reach `min-size`.
        ;; Also what keeps a pool too small for two full tiers from being cut.
        room?    (fn [cuts i]
                   (let [lo (or (first (rsubseq cuts <= i)) 0)
                         hi (or (first (subseq cuts > i)) n)]
                     (and (>= (- i lo) min-size) (>= (- hi i) min-size))))
        by-gap   (->> (range 1 n)
                      (keep (fn [i]
                              (let [g (- (double (scores (dec i)))
                                         (double (scores i)))]
                                (when (pos? g) [i g]))))
                      (sort-by (fn [[i g]] [(- g) i]))
                      (map first))]
    (if-not (pos? max-cuts)
      (sorted-set)
      (reduce (fn [cuts i]
                (if-not (room? cuts i)
                  cuts
                  (let [cuts (conj cuts i)]
                    (if (>= (count cuts) max-cuts) (reduced cuts) cuts))))
              (sorted-set)
              by-gap))))

(defn tiers-by-cliffs
  "Sorted descending by the score key, each player with a 1-indexed :tier. The
  pool above `replacement-level` is cut at its biggest gaps; everything at or
  below it shares the final tier. A nil level tiers the whole pool."
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
         ;; The tail spends one of MAX-TIERS, so a capped pool that has one may
         ;; only cut MAX-TIERS - 1, or the ceiling is off by one where it matters.
         ceiling (if (< cutoff n) (dec MAX-TIERS) MAX-TIERS)
         cuts   (cut-points (mapv score (subvec sorted 0 cutoff))
                            (min (tier-count cutoff target-size) ceiling)
                            MIN-TIER-SIZE)
         tiers  (when (pos? cutoff)
                  (reductions (fn [t i] (if (contains? cuts i) (inc t) t))
                              1 (range 1 cutoff)))
         tail   (when (< cutoff n)
                  (repeat (- n cutoff) (if (seq tiers) (inc (last tiers)) 1)))]
     (mapv #(assoc %1 :tier %2) sorted (concat tiers tail)))))

(defn tier-floor
  "Valuation's replacement level where there is one. K and DST are absent from
  that map so they price at $0, but still need a floor or tiering spends every
  tier on 44 kickers. `sorted` is the group already in descending :points order.
  "
  [sorted level num-teams]
  (or level
      (when (seq sorted)
        (double (:points (nth sorted (min num-teams (dec (count sorted)))))))))

(defn- id->tier [tiered]
  (into {} (map (juxt :player-id :tier)) tiered))

(defn with-tiers
  "Assoc :tiers {:overall n :position n}, plus :tier as the flat alias for the
  positional one. Both scales always ship — see the namespace docstring. `ctx`
  carries :replacement-levels and :num-teams."
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
        ;; VORP is the only score comparing a QB to an RB, and it is 0.0 at
        ;; replacement by construction, so the tail rule needs no special case.
        overall    (id->tier (tiers-by-cliffs board 0.0
                                              {:score-key   :vorp
                                               :target-size (:overall TARGET-TIER-SIZE)}))]
    (mapv (fn [p]
            (let [id  (:player-id p)
                  pos (get positional id)]
              (assoc p :tiers {:overall (get overall id) :position pos}
                     :tier pos)))
          board)))
