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

(defn draft-phase-decay
  "Anticipatory multiplier 1 - PHASE_DECAY * t^2, where t is the fraction of
  league roster slots already filled. 1.0 at open; 1 - PHASE_DECAY when full."
  [league-state]
  (let [total-slots (reduce + 0 (map #(count (:roster %)) (:teams league-state)))]
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
            (let [disc (- (ls/total-remaining-cash league-state) slots)
                  infl (/ disc premium)]
              (double (min INFL-MAX (max INFL-MIN infl))))))))))
