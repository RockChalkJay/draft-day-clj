(ns draft-day.benchmark.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.benchmark.core :as core]
            [draft-day.benchmark.sources.nflverse :as nflverse]))

(defn p [gsis] {:gsis-id gsis :position "WR" :player-id gsis})

(def pool [(p "00-1") (p "00-2") (p "00-3")])

(deftest availability-filter-is-off-by-default
  (is (= pool (core/apply-availability-filter pool 2024 false)))
  (is (= pool (core/apply-availability-filter pool 2024 nil))))

(deftest availability-filter-keeps-only-week1-actives
  (with-redefs [nflverse/week1-participants (constantly #{"00-1" "00-3"})]
    (is (= ["00-1" "00-3"]
           (mapv :gsis-id (core/apply-availability-filter pool 2024 true))))))

(deftest missing-weekly-data-leaves-the-pool-intact
  ;; Failing open matters here: an empty participant set means nflverse's weekly
  ;; file was unavailable, not that nobody played. Filtering on it would silently
  ;; empty the pool and report a season as unscoreable.
  (with-redefs [nflverse/week1-participants (constantly #{})]
    (is (= pool (core/apply-availability-filter pool 2024 true)))))

(deftest filter-does-not-invent-players
  (testing "ids absent from the pool are not added back by the filter"
    (with-redefs [nflverse/week1-participants (constantly #{"00-1" "00-9"})]
      (is (= ["00-1"] (mapv :gsis-id (core/apply-availability-filter pool 2024 true)))))))
