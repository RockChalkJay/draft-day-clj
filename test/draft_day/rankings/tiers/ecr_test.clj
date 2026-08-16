(ns draft-day.rankings.tiers.ecr-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.rankings.tiers :as tiers]
            [draft-day.rankings.tiers.ecr :as ecr]))

(def ^:private ctx {:replacement-levels {"RB" 100.0 "WR" 100.0} :num-teams 12})

(defn- p [id pos overall pos-tier]
  (cond-> {:player-id id :position pos :points 200.0 :vorp 50.0}
    overall  (assoc :fantasypros/ecr-tier overall)
    pos-tier (assoc :fantasypros/ecr-pos-tier pos-tier)))

(deftest both-scales-are-the-vendor-numbers-verbatim
  ;; The overall tier is FantasyPros' consensus-cheatsheet tier and the
  ;; positional tier is its own per-position cheatsheet's — two published
  ;; numbers, neither derived from the other. Dense-ranking the overall tier
  ;; within a position would approximate a number the vendor already gives.
  (let [b   [(p "rb1" "RB" 1 1) (p "rb2" "RB" 3 2) (p "wr1" "WR" 3 1)]
        out (tiers/tier-board :ecr ctx b)]
    (is (= {"rb1" 1 "rb2" 3 "wr1" 3} (:overall out)))
    (is (= {"rb1" 1 "rb2" 2 "wr1" 1} (:position out)))))

(deftest the-positional-scale-is-not-the-overall-one-renumbered
  ;; Two players sharing an overall tier can sit in different positional tiers,
  ;; and vice versa — the pages are cut independently.
  (let [out (tiers/tier-board :ecr ctx [(p "rb1" "RB" 5 1) (p "wr1" "WR" 5 3)])]
    (is (= (get-in out [:overall "rb1"]) (get-in out [:overall "wr1"])))
    (is (not= (get-in out [:position "rb1"]) (get-in out [:position "wr1"])))))

(deftest coverage-differs-per-scale-and-neither-borrows
  ;; The two scales come off different pages, so a player can be ranked on one
  ;; and not the other. Absent means absent — never the cliff tier.
  (let [b   [(p "rb1" "RB" 2 nil) (p "rb2" "RB" nil 4) (p "rb3" "RB" nil nil)]
        out (tiers/tier-board :ecr ctx b)]
    (is (= {"rb1" 2} (:overall out)))
    (is (= {"rb2" 4} (:position out)))
    (is (nil? (get-in out [:overall "rb3"])))
    (is (nil? (get-in out [:position "rb3"])))))

(deftest an-unranked-player-stays-unranked-through-with-tiers
  (let [out (first (tiers/with-tiers [(p "rb3" "RB" nil nil)] ctx))]
    (is (= {} (get-in out [:tiers :ecr]))
        "no ECR tier on either scale")
    (is (some? (get-in out [:tiers :cliffs :position]))
        "and it still has a cliff tier, which it must not have inherited")))

(deftest a-junk-vendor-tier-reads-as-no-tier
  (testing "zero, negative and non-numeric are parse failures, not tier values"
    (doseq [bad [0 -1 "3" ##NaN]]
      (is (nil? (ecr/expert-tier :fantasypros/ecr-tier
                                 {:fantasypros/ecr-tier bad}))
          (str "rejected: " (pr-str bad)))))
  (is (= 3 (ecr/expert-tier :fantasypros/ecr-tier {:fantasypros/ecr-tier 3.0}))
      "a double from JSON still reads as a tier"))
