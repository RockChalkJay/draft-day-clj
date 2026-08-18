(ns draft-day.rankings.replacement-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.rankings.replacement :as rep]))

(defn- pool [pos n & {:keys [top step] :or {top 200 step 2}}]
  (mapv (fn [i] {:player-id (str pos i) :position pos :points (- top (* i step))})
        (range n)))

(deftest replacement-index-hand-computed
  ;; 12 teams, default config. The board is running backs only, so they are the
  ;; only thing a FLEX slot can take and they win all 12: idx = 12*2 + 12 = 36.
  (let [levels (rep/replacement-levels (pool "RB" 40 :top 200 :step 1) 12 {})]
    (is (= 164.0 (levels "RB")))))            ; points at index 36 = 200-36

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
        more  (rep/replacement-levels board 12 {:flex 2})
        none  (rep/replacement-levels board 12 {:flex 0})]
    (is (= 264.0 (base "RB")))                ; index 24+12=36
    (is (= 252.0 (more "RB")))                ; index 24+24=48
    (is (= 276.0 (none "RB")))))              ; index 24+0=24

(deftest flex-goes-to-whoever-is-actually-left
  ;; The point of pooling. Two positions, 2 starters each at 12 teams, one flex.
  ;; Whichever position still has the better players once the dedicated starters
  ;; are gone wins the flex slots — it is not a 50/50 assumption.
  (let [wr-heavy (concat (pool "RB" 60 :top 300 :step 10)   ; falls off a cliff
                         (pool "WR" 60 :top 260 :step 1))   ; stays flat and deep
        claims   (rep/flex-claims wr-heavy 12 {:rb 2 :wr 2 :te 0 :flex 1} :points)]
    (is (= {"WR" 12} claims)
        "the deep position takes every flex slot when the other has fallen away"))

  (testing "and the reverse board hands them to the other position"
    (let [rb-heavy (concat (pool "RB" 60 :top 300 :step 1)
                           (pool "WR" 60 :top 260 :step 10))
          claims   (rep/flex-claims rb-heavy 12 {:rb 2 :wr 2 :te 0 :flex 1} :points)]
      (is (= {"RB" 12} claims))))

  (testing "a tight end can win a flex slot, which the old split made impossible"
    (let [board  (concat (pool "RB" 30 :top 200 :step 10)
                         (pool "WR" 30 :top 200 :step 10)
                         (pool "TE" 30 :top 400 :step 1))
          claims (rep/flex-claims board 12 {:rb 2 :wr 2 :te 1 :flex 1} :points)]
      (is (= {"TE" 12} claims)))))

(deftest flex-claims-fills-in-a-partial-config
  ;; It is public, so it cannot assume the caller already merged the defaults —
  ;; a missing :rb/:wr/:te would read as "no dedicated starters" and let whole
  ;; position pools compete for the flex slots from the top.
  (let [board (concat (pool "RB" 40 :top 300 :step 1)
                      (pool "WR" 40 :top 295 :step 1)
                      (pool "TE" 40 :top 290 :step 1))]
    (is (= (rep/flex-claims board 12 {:rb 2 :wr 2 :te 1 :flex 1} :points)
           (rep/flex-claims board 12 {:flex 1} :points))
        "a partial config resolves to the same claims as the full default")))

(deftest flex-claims-sum-to-the-league-s-flex-demand
  (let [board (concat (pool "RB" 60 :top 300 :step 2)
                      (pool "WR" 60 :top 295 :step 2)
                      (pool "TE" 60 :top 290 :step 2))]
    (doseq [flex [0 1 2 3]]
      (let [claims (rep/flex-claims board 12 {:rb 2 :wr 2 :te 1 :flex flex} :points)]
        (is (= (* 12 flex) (reduce + 0 (vals claims)))
            (str "flex " flex " allocates exactly its slots"))))))

(deftest vorp-is-signed-and-nil-where-there-is-no-opinion
  (let [board [{:player-id "rb1" :position "RB" :points 250}
               {:player-id "rb2" :position "RB" :points 100}   ; below replacement
               {:player-id "k1"  :position "K"  :points 180}]
        vorp  (mapv :vorp (rep/with-vorp board {"RB" 150.0}))]
    (is (= 100.0 (nth vorp 0)))               ; 250 - 150
    (is (= -50.0 (nth vorp 1)))               ; 100 - 150, not floored at 0
    (is (nil?    (nth vorp 2)))))             ; K takes no level -> no opinion, not a score

(deftest signed-vorp-orders-the-tail-across-positions
  ;; The reason the floor came off. Raw points are not comparable between
  ;; positions, so the below-replacement block used to sort every quarterback
  ;; above every receiver purely because quarterbacks score more. Against their
  ;; own replacement levels the same three players read in the right order.
  (let [board  [{:player-id "qb" :position "QB" :points 220.0}   ; -60 vs 280
                {:player-id "wr" :position "WR" :points 185.0}   ;  -5 vs 190
                {:player-id "rb" :position "RB" :points 140.0}]  ; -20 vs 160
        levels {"QB" 280.0 "WR" 190.0 "RB" 160.0}
        by-pts  (mapv :player-id (sort-by :points > board))
        by-vorp (mapv :player-id (sort-by :vorp > (rep/with-vorp board levels)))]
    (is (= ["qb" "wr" "rb"] by-pts)  "points rank the quarterback first")
    (is (= ["wr" "rb" "qb"] by-vorp) "signed VORP ranks him last, which is right")))
