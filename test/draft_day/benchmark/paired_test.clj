(ns draft-day.benchmark.paired-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.benchmark.metrics :as metrics]))

;; One season, one position, three players — small enough to work out by hand.
;;
;;   player   A points  B points   actual
;;   a          100        80        50
;;   b           90        90       100
;;   c           80       100        75
;;
;; A ranks:  a=1 b=2 c=3      B ranks:  c=1 b=2 a=3
;; Truth  :  b=1 c=2 a=3
;; A errors: a|1-3|=2  b|2-1|=1  c|3-2|=1
;; B errors: a|3-3|=0  b|2-1|=1  c|1-2|=1
;; diff(A-B): a=+2  b=0  c=0     -> positive means A is WORSE
(defn player [id pts actual]
  {:player-id id :position "RB" :points pts :actual/points actual})

(def season-a {:season 2021 :players [(player "a" 100 50) (player "b" 90 100) (player "c" 80 75)]})
(def season-b {:season 2021 :players [(player "a" 80 50)  (player "b" 90 100) (player "c" 100 75)]})

(deftest player-rank-error-diffs-are-hand-checkable
  (let [d (metrics/player-rank-error-diffs [season-a] [season-b] :actual/points)
        rb (get d "RB")
        by-id (into {} (map (juxt :player-id :diff)) rb)]
    (is (= 3 (count rb)))
    (is (= 2.0 (by-id "a")))
    (is (= 0.0 (by-id "b")))
    (is (= 0.0 (by-id "c")))))

(deftest pairing-uses-only-players-both-models-ranked
  ;; A model built from a different source has a different pool; scoring the
  ;; union would compare each model against players the other never saw.
  (let [only-a (update season-a :players conj (player "z" 70 10))
        d      (metrics/player-rank-error-diffs [only-a] [season-b] :actual/points)]
    (is (= #{"a" "b" "c"} (set (map :player-id (get d "RB")))))))

(deftest skipped-seasons-drop-out-of-the-pairing
  (let [skipped {:season 2022 :skipped? true :reason "no capture"}
        d (metrics/player-rank-error-diffs [season-a skipped] [season-b] :actual/points)]
    (is (= #{2021} (set (map :season (get d "RB")))))))

(deftest season-rho-diffs-pair-on-common-seasons
  (let [a [{:season 2021 :metrics {"RB" {:spearman 0.70}}}
           {:season 2022 :metrics {"RB" {:spearman 0.75}}}
           {:season 2023 :metrics {"RB" {:spearman 0.66}}}]
        b [{:season 2021 :metrics {"RB" {:spearman 0.58}}}
           {:season 2022 :metrics {"RB" {:spearman 0.55}}}]
        d (get (metrics/season-rho-diffs a b) "RB")]
    (testing "2023 has no counterpart and is excluded"
      (is (= [2021 2022] (mapv :season d))))
    (is (< (Math/abs (- 0.12 (:diff (first d)))) 1e-9))
    (is (< (Math/abs (- 0.20 (:diff (second d)))) 1e-9))))

(deftest bootstrap-resamples-seasons-not-rows
  ;; Rows inside a season share a common shock. Resampling rows independently
  ;; would shrink the interval and manufacture significance, so the block count
  ;; must equal the number of SEASONS, not the number of rows.
  (let [rows (concat (for [i (range 20)] {:season 2021 :diff 1.0})
                     (for [i (range 20)] {:season 2022 :diff -1.0}))
        ci   (metrics/block-bootstrap-ci rows (metrics/mean-of :diff))]
    (is (= 2 (:n-blocks ci)))
    (is (= 40 (:n-rows ci)))
    (is (= 0.0 (:point ci)))
    (testing "with two opposed blocks the interval must reach both extremes"
      (is (<= (:lo ci) -0.9))
      (is (>= (:hi ci) 0.9)))))

(deftest bootstrap-is-deterministic-for-a-given-seed
  (let [rows (for [s [2021 2022 2023] i (range 5)] {:season s :diff (+ 0.1 (* 0.01 i))})]
    (is (= (metrics/block-bootstrap-ci rows (metrics/mean-of :diff))
           (metrics/block-bootstrap-ci rows (metrics/mean-of :diff))))))

(deftest bootstrap-declines-to-invent-an-interval-from-one-season
  (let [ci (metrics/block-bootstrap-ci [{:season 2021 :diff 0.5}] (metrics/mean-of :diff))]
    (is (= 1 (:n-blocks ci)))
    (is (nil? (:lo ci)))
    (is (metrics/spans-zero? ci) "an interval that cannot be computed must not read as significant")))

(deftest spans-zero-decides-the-verdict
  (is (metrics/spans-zero? {:lo -0.02 :hi 0.15}))
  (is (not (metrics/spans-zero? {:lo 0.065 :hi 0.224})))
  (is (not (metrics/spans-zero? {:lo -0.30 :hi -0.05}))))

(deftest power-arithmetic-matches-the-published-table
  ;; RB's measured paired SD is 0.064 over 5 seasons.
  ;;   MDD = 2.8 * 0.064 / sqrt(5)     = 0.0801
  ;;   seasons for 0.05 = (2.8*0.064/0.05)^2 = 12.85 -> 13
  (is (< (Math/abs (- 0.0801 (metrics/min-detectable-difference 0.064 5))) 1e-4))
  (is (= 13 (metrics/seasons-needed 0.064 0.05)))
  (testing "TE's 0.215 SD needs a corpus that does not exist"
    (is (= 145 (metrics/seasons-needed 0.215 0.05))))
  (testing "more seasons shrink what is detectable"
    (is (< (metrics/min-detectable-difference 0.064 18)
           (metrics/min-detectable-difference 0.064 5)))))

(deftest power-summary-reports-n-and-sd-per-position
  (let [diffs {"RB" [{:season 2021 :diff 0.10} {:season 2022 :diff 0.20} {:season 2023 :diff 0.15}]}
        s     (get (metrics/power-summary diffs 0.05) "RB")]
    (is (= 3 (:n s)))
    (is (< (Math/abs (- 0.15 (:mean s))) 1e-9))
    (is (< (Math/abs (- 0.05 (:sd s))) 1e-9))))       ; sd of .10/.20/.15 = .05
