(ns draft-day.rankings.tiers-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.rankings.tcm :as tcm]
            [draft-day.rankings.tiers :as tiers]))

(defn- frame [scores]
  (mapv (fn [i s] {:player-id (str "p" i) :points s}) (range (count scores)) scores))

(defn- tier-seq
  "tiers-by-cliffs already returns players sorted descending by points."
  ([scores] (tier-seq scores nil))
  ([scores level] (mapv :tier (tiers/tiers-by-cliffs (frame scores) level))))

(deftest empty-input
  (is (empty? (tiers/tiers-by-cliffs (frame []) nil))))

(deftest a-position-with-no-cliff-is-one-tier
  (testing "a smooth 2%-per-slot decline is not a set of tiers, it is a slope"
    (is (= #{1} (set (tier-seq [100 98 96.04 94.12 92.24]))))))

(deftest a-drop-past-the-threshold-cuts-a-tier
  ;; 100 -> 90 is exactly 10%, past the 5% threshold; 90 -> 88 is 2.2%, under it.
  (is (= [1 1 2 2] (tier-seq [100 99 90 88]))))

(deftest tier-count-follows-the-position-not-a-setting
  (testing "three cliffs make four tiers with no count passed in"
    (is (= [1 2 3 4] (tier-seq [100 80 60 40]))))

  (testing "the same call on a flat position makes one"
    (is (= [1 1 1 1] (tier-seq [100 99.5 99 98.6])))))

(deftest equal-scores-never-land-in-different-tiers
  (let [tiers (tier-seq [100 100 100 50])]
    (is (= [1 1 1 2] tiers))))

(deftest a-big-gap-low-on-the-tail-is-not-a-cliff-a-small-one-up-top-is
  ;; The regression this rule exists for. Ranking *absolute* gaps put a WR tier
  ;; break at #56: an 18-point gap behind a 155-point WR outranked a 26-point gap
  ;; behind a 310-point WR1, even though only the second changes a draft. Both
  ;; gaps below are the same 8 points; only the one that is a large share of its
  ;; own player's points counts.
  (testing "8 points behind a 300-point player is noise"
    (is (= #{1} (set (tier-seq [300 292])))))

  (testing "8 points behind a 40-point player is a cliff"
    (is (= [1 2] (tier-seq [40 32])))))

(deftest everything-at-or-below-replacement-shares-the-final-tier
  ;; Below replacement the only true statement is "worse than the player you can
  ;; have for $1", so the tail is one tier however lumpy it is.
  (let [tiers (tier-seq [100 99 60 30 10 1] 50)]
    (is (= [1 1 2 3 3 3] tiers))
    (is (= 3 (reduce max tiers)) "the tail does not keep splitting")))

(deftest a-nil-replacement-level-tiers-the-whole-pool
  ;; K and DST are never priced, so they have no replacement level.
  (is (= [1 2 3] (tier-seq [100 50 20] nil))))

(deftest nobody-above-replacement-is-a-single-tier
  (is (= #{1} (set (tier-seq [10 9 8] 100)))))

(deftest tiers-and-tcm-agree-on-what-a-cliff-is
  (testing "one shared relative-drop, so the board's stripes and its cliff marker
            cannot drift apart"
    (is (= 0.25 (tiers/relative-drop 100 75)))
    (is (= 0.0 (tiers/relative-drop 0 0)) "a zero-point player has nothing to fall from"))

  (testing "the thresholds differ on purpose — tcm looks two roster slots ahead"
    (is (> tcm/DROP-THRESHOLD tiers/DROP-THRESHOLD))))
