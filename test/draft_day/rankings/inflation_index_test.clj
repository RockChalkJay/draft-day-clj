(ns draft-day.rankings.inflation-index-test
  (:require [clojure.test :refer [deftest is]]
            [draft-day.rankings.inflation-index :as idx]))

(deftest index-rises-on-overpay
  (let [board [{:player-id "a" :position "RB" :value 40}]]
    (is (= 15.0 (idx/inflation-index board [{:player-id "a" :position "RB" :price 55}])))))

(deftest index-negative-on-bargain
  (let [board [{:player-id "a" :position "RB" :value 40}]]
    (is (= -10.0 (idx/inflation-index board [{:player-id "a" :position "RB" :price 30}])))))

(deftest index-zero-with-no-picks
  (is (= 0.0 (idx/inflation-index [{:player-id "a" :position "RB" :value 40}] []))))

(deftest positional-run-tilts-that-position-up
  (let [board [{:player-id "r1" :position "RB" :value 40}
               {:player-id "r2" :position "RB" :value 30}
               {:player-id "w1" :position "WR" :value 40}]
        ls    {:picks [{:player-id "r1" :position "RB" :price 60}    ; RBs bought over par
                       {:player-id "r2" :position "RB" :price 45}
                       {:player-id "w1" :position "WR" :price 40}]}  ; WR at par
        m     (idx/per-position-inflation board ls 1.0)]
    (is (> (m "RB") (m "WR")))        ; the RB run tilts RB inflation up
    (is (= 1.0 (m "WR")))))           ; WR at par stays at the global factor

(deftest no-picks-passes-global-through
  (let [m (idx/per-position-inflation [{:player-id "a" :position "RB" :value 40}] {} 0.9)]
    (is (= 0.9 (m "RB")))
    (is (= 0.9 (m "WR")))))

(deftest minimum-bid-picks-do-not-move-a-position
  ;; Every roster slot is priced now, and the ones below replacement all price at
  ;; $1. Measured as a ratio that floor is explosive — $3 for a $1 flier reads as
  ;; a 3x overpay — so those picks are excluded rather than allowed to pin the
  ;; position at POS-MAX on nothing.
  (let [board [{:player-id "flier" :position "RB" :value 1}
               {:player-id "stud"  :position "WR" :value 40}]]
    (is (= 1.0 ((idx/per-position-inflation
                 board {:picks [{:player-id "flier" :position "RB" :price 3}]} 1.0) "RB"))
        "a $1 player bought for $3 leaves RB at the global factor")
    (is (= 1.025 ((idx/per-position-inflation
                   board {:picks [{:player-id "stud" :position "WR" :price 42}]} 1.0) "WR"))
        "the same $2 over on a real price still registers, proportionally")))

(deftest per-position-inflation-clamped-to-band
  ;; extreme overpay/underpay runs can't push a position outside [POS-MIN, POS-MAX]
  (let [board [{:player-id "r1" :position "RB" :value 40}]]
    (is (= 1.6 ((idx/per-position-inflation
                 board {:picks [{:player-id "r1" :position "RB" :price 500}]} 1.0) "RB")))
    (is (= 0.6 ((idx/per-position-inflation
                 board {:picks [{:player-id "r1" :position "RB" :price 0}]} 1.0) "RB")))))
