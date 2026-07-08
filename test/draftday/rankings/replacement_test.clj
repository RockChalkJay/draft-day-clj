(ns draftday.rankings.replacement-test
  (:require [clojure.test :refer [deftest is]]
            [draftday.rankings.replacement :as rep]))

(defn- pool [pos n & {:keys [top step] :or {top 200 step 2}}]
  (mapv (fn [i] {:player-id (str pos i) :position pos :points (- top (* i step))})
        (range n)))

(deftest replacement-index-hand-computed
  ;; 12 teams, default config: repRBIdx = 12*2 + floor(12*1/2) = 30.
  (let [levels (rep/replacement-levels (pool "RB" 40 :top 200 :step 1) 12 {})]
    (is (= 170.0 (levels "RB")))))            ; points at index 30 = 200-30

(deftest replacement-qb-index-hand-computed
  (let [levels (rep/replacement-levels (pool "QB" 30 :top 400 :step 1) 12 {})]
    (is (= 388.0 (levels "QB")))))            ; 400 - 12

(deftest replacement-clamps-to-pool-size
  ;; QB pool of only 5; raw index 12 clamps to len-1 = 4.
  (let [levels (rep/replacement-levels (pool "QB" 5 :top 400 :step 10) 12 {})]
    (is (= 360.0 (levels "QB")))))            ; 400 - 4*10

(deftest k-dst-absent-from-replacement-levels
  (let [board  (concat (pool "RB" 40) (pool "K" 10) (pool "DST" 10))
        levels (rep/replacement-levels board 12 {})]
    (is (not (contains? levels "K")))
    (is (not (contains? levels "DST")))))

(deftest flex-spots-override-changes-index
  (let [board (pool "RB" 60 :top 300 :step 1)
        base  (rep/replacement-levels board 12 {:flex 1})
        more  (rep/replacement-levels board 12 {:flex 2})]
    (is (= 270.0 (base "RB")))                ; index 24+6=30
    (is (= 264.0 (more "RB")))))              ; index 24+12=36

(deftest vorp-floored-at-zero-and-zero-for-k-dst
  (let [board [{:player-id "rb1" :position "RB" :points 250}
               {:player-id "rb2" :position "RB" :points 100}   ; below replacement
               {:player-id "k1"  :position "K"  :points 180}]
        vorp  (mapv :vorp (rep/with-vorp board {"RB" 150.0}))]
    (is (= 100.0 (nth vorp 0)))               ; 250 - 150
    (is (= 0.0   (nth vorp 1)))               ; max(0, 100-150)
    (is (= 0.0   (nth vorp 2)))))             ; K not in levels
