(ns draft-day.benchmark.metrics-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.benchmark.metrics :as metrics]))

;; A four-RB fixture small enough to work out entirely by hand.
;;
;;   player   model :points   actual points   actual finish
;;   a              100             50              2
;;   b               90             10              4
;;   c               80            100              1
;;   d               70             20              3
;;
;; Model order: a b c d.  Truth order: c a d b.
(defn p [id points actual]
  {:player-id id :position "RB" :points points :actual/points actual})

(def board [(p "a" 100 50) (p "b" 90 10) (p "c" 80 100) (p "d" 70 20)])

(deftest position-metrics-are-hand-checkable
  (let [m (metrics/position-metrics board 2 :actual/points)]
    (testing "top-2 by model is [a b]; top-2 by truth is #{c a}; overlap is a"
      (is (= 1 (:hits m))))
    (testing "median realized finish of the model's picks: a->2, b->4 => 3.0"
      (is (= 3.0 (:median-finish m))))
    (testing "a bust finishes worse than 2N=4; b finishes exactly 4, so none"
      (is (= 0 (:busts m))))
    (testing "ranks are perfectly uncorrelated here, so rho is 0"
      ;; model ranks (asc) [4 3 2 1] vs actual ranks [3 1 4 2]
      ;; cov = 1.5(0.5) + 0.5(-1.5) + -0.5(1.5) + -1.5(-0.5) = 0
      (is (< (Math/abs (double (:spearman m))) 1e-9)))
    (testing "points-weighted rank error emphasises misses on high scorers"
      ;; |rank - finish|: a=1, b=2, c=2, d=1
      ;; (50*1 + 10*2 + 100*2 + 20*1) / 180 = 290/180
      (is (< (Math/abs (- (:weighted-rank-error m) (/ 290.0 180.0))) 1e-9)))
    (is (= 4 (:n m)))
    (is (= 2 (:top-n m)))))

(deftest perfect-ranking-scores-perfectly
  (let [perfect [(p "a" 100 100) (p "b" 90 90) (p "c" 80 80) (p "d" 70 70)]
        m       (metrics/position-metrics perfect 2 :actual/points)]
    (is (= 2 (:hits m)))
    (is (= 1.5 (:median-finish m)))          ; finishes 1 and 2
    (is (= 0 (:busts m)))
    (is (< (Math/abs (- 1.0 (:spearman m))) 1e-9))
    (is (= 0.0 (:weighted-rank-error m)))))

(deftest busts-count-finishes-worse-than-twice-the-slice
  ;; Model loves "x" most; it finishes dead last of 6 (finish 6 > 2*2).
  (let [b [(p "x" 100 0) (p "a" 90 50) (p "b" 80 40) (p "c" 70 30) (p "d" 60 20) (p "e" 50 10)]
        m (metrics/position-metrics b 2 :actual/points)]
    (is (= 1 (:busts m)))
    (is (= 1 (:hits m)))))

(deftest thin-groups-keep-rho-but-lose-the-hit-rate
  ;; With fewer than 2N players "top N" is most of the pool, so the hit rate is
  ;; not informative. Rho is unaffected by N, though, and dropping the whole
  ;; group discarded it too — selectively, since QB pools land at 20-23 in some
  ;; seasons and 24 in others. Suppress only what is actually compromised.
  (let [thin (take 3 board)                    ; 3 players, N=2, so 3 < 2N
        m    (get (metrics/season-metrics thin {:top-n {"RB" 2}}) "RB")]
    (is (some? m) "the group is still scored")
    (is (contains? m :spearman))
    (is (not (contains? m :hits)) "hit rate is suppressed, not faked")
    (is (not (contains? m :busts)))))

(deftest deep-enough-groups-report-the-full-family
  (let [m (get (metrics/season-metrics board {:top-n {"RB" 2}}) "RB")]
    (is (= 2 (:top-n m)))
    (is (contains? m :hits))
    (is (contains? m :median-finish))
    (is (contains? m :busts))))

(deftest a-group-smaller-than-n-is-dropped-entirely
  ;; Below N there is no top-N to take and rho on one player is meaningless.
  (is (empty? (metrics/season-metrics (take 1 board) {:top-n {"RB" 2}}))))

(deftest median-handles-both-parities
  (is (= 2.0 (metrics/median [1 2 3])))
  (is (= 2.5 (metrics/median [1 2 3 4])))
  (is (= 0.0 (metrics/median []))))
