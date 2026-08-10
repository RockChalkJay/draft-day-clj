(ns draft-day.benchmark.normalize-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.benchmark.core :as core]))

(defn p
  ([id pos adp] (p id pos adp nil))
  ([id pos adp ecr] (cond-> {:player-id id :gsis-id id :position pos
                             :points 1.0 :actual/points 1.0}
                      adp (assoc :adp adp)
                      ecr (assoc :ecr ecr))))

(deftest default-opts-actually-carries-the-slices
  ;; Regression: `default-slices` was defined AFTER `default-opts` behind a
  ;; `declare`, so :slices held an unbound Var and slicing silently never ran.
  ;; Every number looked plausible and no test covered it.
  (is (map? (:slices core/default-opts)))
  (is (= 48 (get-in core/default-opts [:slices "RB"]))))

(deftest slice-keeps-the-top-n-per-position-by-draft-order
  (let [players (concat (for [i (range 60)] (p (str "rb" i) "RB" (inc i)))
                        (for [i (range 80)] (p (str "wr" i) "WR" (inc i))))
        out     (core/apply-position-slice players {"RB" 48 "WR" 60})
        by-pos  (group-by :position out)]
    (is (= 48 (count (by-pos "RB"))))
    (is (= 60 (count (by-pos "WR"))))
    (testing "kept players are the ones drafted earliest, not an arbitrary 48"
      (is (= (set (map #(str "rb" %) (range 48)))
             (set (map :player-id (by-pos "RB"))))))))

(deftest slice-falls-back-to-ecr-when-there-is-no-adp
  (let [players (for [i (range 40)] (p (str "te" i) "TE" nil (inc i)))
        out     (core/apply-position-slice players {"TE" 24})]
    (is (= 24 (count out)))
    (is (= (set (map #(str "te" %) (range 24))) (set (map :player-id out))))))

(deftest slice-leaves-the-pool-alone-when-draft-order-is-unknown
  ;; FFToday reaches 2008 but vintage ADP starts in 2010. Slicing on nothing
  ;; would silently pick an arbitrary subset and present it as "the top 48".
  (let [players (for [i (range 60)] (p (str "rb" i) "RB" nil))]
    (is (= 60 (count (core/apply-position-slice players {"RB" 48}))))))

(deftest slice-is-a-no-op-when-the-pool-is-already-smaller
  (let [players (for [i (range 10)] (p (str "te" i) "TE" (inc i)))]
    (is (= 10 (count (core/apply-position-slice players {"TE" 24}))))))

(deftest slice-can-be-disabled
  (let [players (for [i (range 60)] (p (str "rb" i) "RB" (inc i)))]
    (is (= 60 (count (core/apply-position-slice players nil))))))

(deftest common-pool-intersects-per-season
  ;; Two sources publish different universes; comparing them untouched compares
  ;; different sets of players and calls the difference a model result.
  (let [ra [{:season 2021 :players [(p "a" "RB" 1) (p "b" "RB" 2) (p "x" "RB" 3)]}]
        rb [{:season 2021 :players [(p "a" "RB" 1) (p "b" "RB" 2) (p "y" "RB" 3)]}]
        [ca cb] (core/common-pool ra rb :actual/points)]
    (is (= #{"a" "b"} (set (map :gsis-id (:players (first ca))))))
    (is (= #{"a" "b"} (set (map :gsis-id (:players (first cb))))))
    (testing "n is corrected to the intersection, not left at the original count"
      (is (= 2 (:n (first ca)))))))

(deftest common-pool-leaves-skipped-seasons-untouched
  (let [ra [{:season 2021 :skipped? true :reason "no capture"}]
        rb [{:season 2021 :skipped? true :reason "no capture"}]
        [ca _] (core/common-pool ra rb :actual/points)]
    (is (:skipped? (first ca)))))

(deftest common-pool-handles-a-season-only-one-side-scored
  (let [ra [{:season 2021 :players [(p "a" "RB" 1)]}
            {:season 2022 :players [(p "b" "RB" 1)]}]
        rb [{:season 2021 :players [(p "a" "RB" 1)]}]
        [ca _] (core/common-pool ra rb :actual/points)]
    (testing "the unmatched season empties rather than silently comparing to nothing"
      (is (= 1 (:n (first ca))))
      (is (= 0 (:n (second ca)))))))
