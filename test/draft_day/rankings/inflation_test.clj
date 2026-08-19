(ns draft-day.rankings.inflation-test
  (:require [clojure.test :refer [deftest is]]
            [draft-day.rankings.inflation :as infl]))

(defn- board [values]
  (mapv (fn [i v] {:player-id (str "p" i) :position "RB" :value v})
        (range (count values)) values))

(defn- state [cash slots & [drafted]]
  {:teams [{:team-id "t0" :bankroll cash
            :roster (vec (repeat slots {:pos "BENCH" :player-id nil}))}]
   :drafted-player-ids (set drafted)
   :starting-bankroll  200.0})

(defn- approx [a b] (< (Math/abs (double (- a b))) 1e-6))

(deftest inflation-is-one-when-cash-matches-value
  ;; Σ(value-1) over top-4 = 49+29+14+4 = 96; cash-slots = 100-4 = 96 -> 1.0.
  (is (approx 1.0 (infl/auction-inflation (board [50 30 15 5]) (state 100 4)))))

(deftest less-cash-deflates-board
  (is (< (infl/auction-inflation (board [50 30 15 5]) (state 80 4)) 1.0)))

(deftest more-cash-inflates-board
  (is (> (infl/auction-inflation (board [50 30 15 5]) (state 120 4)) 1.0)))

(deftest only-top-slots-counted
  ;; 6 players but 4 slots -> the two cheap extras don't dilute the denominator.
  (is (approx 1.0 (infl/auction-inflation (board [50 30 15 5 3 2]) (state 100 4)))))

(deftest drafted-players-excluded
  ;; Draft p0 ($50). Undrafted top-3 = 30/15/5 -> Σ(v-1)=47; cash-slots = 50-3 = 47 -> 1.0.
  (is (approx 1.0 (infl/auction-inflation (board [50 30 15 5]) (state 50 3 ["p0"])))))

(deftest clamped-low-and-high
  (let [df (board [50 30 15 5])]
    (is (= 0.5 (infl/auction-inflation df (state 4 4))))     ; no discretionary cash
    (is (= 1.8 (infl/auction-inflation df (state 500 4)))))) ; cash dwarfs value

(deftest no-slots-returns-one
  (is (= 1.0 (infl/auction-inflation (board [50 30]) (state 100 0)))))

(deftest no-value-premium-returns-one
  (is (= 1.0 (infl/auction-inflation (board [1 1 1]) (state 100 3)))))

(deftest missing-value-column-returns-one
  (is (= 1.0 (infl/auction-inflation [{:player-id "p0" :position "RB"}] (state 100 4)))))

;; ---- Draft-phase decay ----
(defn- phase-state [filled total]
  {:teams [{:team-id "t0" :bankroll 200.0
            :roster (mapv (fn [i] {:pos "BENCH" :player-id (when (< i filled) (str "p" i))})
                          (range total))}]})

(deftest phase-decay-is-one-at-draft-open
  (is (= 1.0 (infl/draft-phase-decay (phase-state 0 10)))))

(deftest phase-decay-quadratic-in-progress
  (is (approx (- 1.0 (* infl/PHASE-DECAY 0.25)) (infl/draft-phase-decay (phase-state 5 10))))
  (is (approx (- 1.0 infl/PHASE-DECAY) (infl/draft-phase-decay (phase-state 10 10)))))

(deftest phase-decay-handles-empty-league
  (is (= 1.0 (infl/draft-phase-decay {:teams []}))))

;; ---- one band, applied at the end -------------------------------------------

(deftest clamp-to-band-holds-the-published-bounds
  (is (= 1.8 (infl/clamp-to-band 5.09)))
  (is (= 0.5 (infl/clamp-to-band 0.42)))
  (is (= 1.0 (infl/clamp-to-band 1.0)))
  (is (= infl/INFL-MAX (infl/clamp-to-band Double/MAX_VALUE)))
  (is (= infl/INFL-MIN (infl/clamp-to-band 0.0))))

(deftest the-multiplier-worth-uses-cannot-escape-the-band
  ;; The composed factor is position tilt x phase decay. Clamping the tilt alone
  ;; left the product free to leave the band: at the old floor that was
  ;; 0.6 * 0.8 = 0.48, under a published minimum of 0.5.
  (is (= 0.48 (* 0.6 0.8)) "what the old chain produced")
  (is (= 0.5 (infl/clamp-to-band (* 0.6 0.8))) "what one band at the end produces")
  (doseq [tilt [0.1 0.5 1.0 3.0 20.0]
          heat [0.8 0.9 1.0]]
    (let [x (infl/clamp-to-band (* tilt heat))]
      (is (<= infl/INFL-MIN x infl/INFL-MAX)
          (str "tilt " tilt " x heat " heat " lands inside the band")))))
