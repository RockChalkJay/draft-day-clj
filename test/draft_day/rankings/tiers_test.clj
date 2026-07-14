(ns draft-day.rankings.tiers-test
  (:require [clojure.test :refer [deftest is]]
            [draft-day.rankings.tiers :as tiers]))

(defn- frame [scores]
  (mapv (fn [i s] {:player-id (str "p" i) :points s}) (range (count scores)) scores))

(defn- tier-seq [scores num-tiers]
  ;; tiers-by-cliffs already returns players sorted descending by points
  (mapv :tier (tiers/tiers-by-cliffs (frame scores) num-tiers)))

(deftest empty-input
  (is (empty? (tiers/tiers-by-cliffs (frame []) 5))))

(deftest num-tiers-one-forces-single-tier
  (is (= #{1} (set (tier-seq [100 50 10 1] 1)))))

(deftest len-le-num-tiers-forces-single-tier
  (is (= #{1} (set (tier-seq [100 80 60 40] 5)))))

(deftest single-big-gap-lands-break-correctly
  (is (= [1 1 1 2 2 2] (tier-seq [100 99 98 50 49 48] 2))))

(deftest adjacent-biggest-gaps-produce-singleton-tier
  (is (= [1 2 3 3 3] (tier-seq [100 50 40 39 38] 3))))

(deftest tied-scores-produce-fewer-tiers-not-split-equals
  (let [tiers (tier-seq [100 100 100 100 100 50] 5)]
    (is (= [1 1 1 1 1 2] tiers))
    (is (= 2 (reduce max tiers)))))          ; fewer tiers than requested

(deftest zero-gap-never-selected-as-break
  (let [tiers (tier-seq [100 100 99 98] 3)]
    (is (= (nth tiers 0) (nth tiers 1)))       ; the two 100s stay together
    (is (= [1 1 2 3] tiers))))
