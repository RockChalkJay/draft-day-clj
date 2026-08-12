(ns draft-day.rankings.vendor-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.scoring :as scoring]
            [draft-day.rankings.vendor :as vendor]))

(defn- player [by-format]
  {:player-id "p1" :player-name "A B" :position "WR"
   :espn/auction-value 40.0                      ; flat: ESPN has no format variants
   :vendor/by-format by-format})

(def ^:private variants
  {:ppr      {:sleeper/adp 3.4 :fantasypros/ecr 1 :fantasypros/aav 59.0}
   :half-ppr {:sleeper/adp 3.7 :fantasypros/ecr 2 :fantasypros/aav 56.0}
   :standard {:sleeper/adp 6.6 :fantasypros/ecr 3 :fantasypros/aav 50.0}})

(deftest the-chosen-format-lands-on-the-flat-keys
  (let [[p] (vendor/with-format [(player variants)] :standard)]
    (is (= 6.6 (:sleeper/adp p)))
    (is (= 3 (:fantasypros/ecr p)))
    (is (= 50.0 (:fantasypros/aav p)))
    (is (= 40.0 (:espn/auction-value p)) "flat columns are untouched")))

(deftest the-bundle-never-reaches-the-client
  ;; The response carries the whole board; shipping three formats of every vendor
  ;; column would triple it for data the client cannot use.
  (let [[p] (vendor/with-format [(player variants)] :ppr)]
    (is (not (contains? p :vendor/by-format)))))

(deftest a-player-missing-the-format-keeps-what-it-has
  (testing "a source that covers ~150 players leaves the rest with no variant"
    (let [[p] (vendor/with-format [(player {:ppr {:fantasypros/aav 59.0}})] :standard)]
      (is (nil? (:fantasypros/aav p)))
      (is (= "A B" (:player-name p)))))

  (testing "and a player with no bundle at all survives"
    (let [[p] (vendor/with-format [{:player-id "x" :position "K"}] :ppr)]
      (is (= {:player-id "x" :position "K"} p)))))

(deftest for-scoring-picks-the-format-from-the-league
  (let [adp-under (fn [s] (:sleeper/adp (first (vendor/for-scoring [(player variants)] s))))]
    (is (= 6.6 (adp-under (:standard scoring/presets))))
    (is (= 3.7 (adp-under (:half-ppr scoring/presets))))
    (is (= 3.4 (adp-under (:ppr scoring/presets))))
    (is (= 3.7 (adp-under (assoc (:ppr scoring/presets) :rec 0.4)))
        "a custom config lands on its nearest published format")))
