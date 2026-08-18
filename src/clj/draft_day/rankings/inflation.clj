(ns draft-day.rankings.inflation
  "Piece 5: live auction inflation. Worth is Value scaled by these live factors.

  Conserving inflation compares money still in the room to value still on the
  board; phase decay anticipates the empirical sag in realized prices as rosters
  fill. Both are 1.0 at draft open so they compose cleanly and preserve
  opening-at-par properties. (Per-position refinements live in
  draft-day.rankings.inflation-index.)"
  (:require [draft-day.rankings.league-state :as ls]))

(def INFL-MIN 0.5)
(def INFL-MAX 1.8)
(def PHASE-DECAY 0.2)

(defn clamp-to-band
  "Hold a price multiplier inside [INFL-MIN, INFL-MAX].

  This is the *only* band in the chain, and it belongs at the end of it. The
  global factor, a position's realized tilt and the phase decay each multiply
  into the number Worth is finally scaled by, so clamping any one of them alone
  bounds a factor rather than the answer. Two things went wrong when this was
  spread out:

  - the positional band was [0.6,1.6] and described as *softer* than the global
    [0.5,1.8] \"so a positional run can register\". It is strictly narrower, so a
    global factor of 1.8 came out as 1.6 for a position with no picks at all, and
    a position that really had run 2x hot came out at 1.6 as well — below the
    global factor it was supposed to have been tilted above.
  - phase decay was applied *after* that clamp, so the multiplier Worth actually
    used ran to [0.48, 1.6] — outside both published bounds, under the floor."
  [x]
  (double (min INFL-MAX (max INFL-MIN (double x)))))

(defn draft-phase-decay
  "Anticipatory multiplier 1 - PHASE_DECAY * t^2, where t is the fraction of
  league roster slots already filled. 1.0 at open; 1 - PHASE_DECAY when full."
  [league-state]
  (let [total-slots (ls/total-slots league-state)]
    (if (<= total-slots 0)
      1.0
      (let [empty (reduce + 0 (vals (ls/empty-slots-by-pos league-state)))
            t     (- 1.0 (/ (double empty) total-slots))]
        (- 1.0 (* PHASE-DECAY t t))))))

(defn auction-inflation
  "Conserving inflation (1.0 == on-value), clamped to [0.5, 1.8]. The denominator
  sums the value premium (value - 1) over the top `slots` undrafted players — the
  ones actually expected to be drafted — so deep $0/$1 filler can't dilute it."
  [board league-state]
  (if-not (some #(contains? % :value) board)
    1.0
    (let [drafted   (:drafted-player-ids league-state)
          undrafted (remove #(contains? drafted (:player-id %)) board)
          slots     (reduce + 0 (vals (ls/empty-slots-by-pos league-state)))]
      (if (or (empty? undrafted) (<= slots 0))
        1.0
        (let [expected (take slots (sort > (map #(double (or (:value %) 0.0)) undrafted)))
              premium  (reduce + 0.0 (map #(max 0.0 (- % 1.0)) expected))]
          (if (<= premium 0.0)
            1.0
            (let [disc (- (ls/total-remaining-cash league-state) slots)]
              (clamp-to-band (/ disc premium)))))))))
