(ns draft-day.rankings.tcm-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.rankings.tcm :as tcm]))

;; TCM runs on every pick and had no direct assertions. It compares each
;; undrafted player's points to the player *two* spots below (same position);
;; a drop over 10% yields tcm = 1 + drop, otherwise 1.0. Drops are chosen to be
;; exactly representable as doubles (0.5, 0.25) so equality is stable.

(defn- board [& pts]
  (vec (map-indexed (fn [i p] {:player-id (str "p" i) :position "RB" :points p})
                    pts)))

(defn- tcm-of [players id]
  (some #(when (= (:player-id %) id) (:tcm %)) players))

(deftest drop-beyond-threshold-yields-one-plus-drop
  ;; p0=100, two spots below is p2=50 -> drop 0.50 -> tcm 1.50
  (let [players (tcm/with-tcm (board 100 100 50) {:drafted-player-ids #{}})]
    (is (= 1.5 (tcm-of players "p0")))
    (testing "the last two (nobody two below) fall back to 1.0"
      (is (= 1.0 (tcm-of players "p1")))
      (is (= 1.0 (tcm-of players "p2"))))))

(deftest drop-within-threshold-yields-one
  ;; p0=100 vs p2=95 -> drop 0.05 (< 10%) -> 1.0
  (let [players (tcm/with-tcm (board 100 98 95) {:drafted-player-ids #{}})]
    (is (= 1.0 (tcm-of players "p0")))))

(deftest zero-point-player-falls-back-to-one
  ;; p1=0 has a player two below (p3=0) but zero points -> guard -> 1.0
  (let [players (tcm/with-tcm (board 10 0 0 0) {:drafted-player-ids #{}})]
    (is (= 1.0 (tcm-of players "p1")))))

(deftest drafted-players-get-nil-and-drop-out-of-the-comparison
  (let [b (board 100 100 75 50)]
    (testing "undrafted: p0's two-below is p2=75 -> drop 0.25 -> 1.25"
      (is (= 1.25 (tcm-of (tcm/with-tcm b {:drafted-player-ids #{}}) "p0"))))
    (testing "drafting p1 recomputes over the undrafted subset: p0's two-below
              becomes p3=50 -> drop 0.50 -> 1.50, and p1 itself -> nil"
      (let [players (tcm/with-tcm b {:drafted-player-ids #{"p1"}})]
        (is (= 1.5 (tcm-of players "p0")))
        (is (nil? (tcm-of players "p1")))))))

(deftest tcm-is-never-below-one
  (let [players (tcm/with-tcm (board 100 90 80 70 60 50 40)
                              {:drafted-player-ids #{}})]
    (is (every? #(>= % 1.0) (keep :tcm players)))))
