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
