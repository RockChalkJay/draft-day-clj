(ns draft-day.rankings.scoring-propagation-test
  "The invariant this whole namespace exists for: when the league's scoring
  changes, everything derived from it changes with it.

  Nothing used to check this. Every `static-rankings` call in the suite passed
  `(:ppr scoring/presets)`, so no test could tell the difference between a board
  that responded to scoring and one that ignored it — which is exactly how Tier
  came to be frozen to a PPR vendor column and Market to a PPR scrape."
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.scoring :as scoring]
            [draft-day.rankings.engine :as engine]
            [draft-day.rankings.vendor :as vendor]))

;; Two RBs who project the same in standard and diverge sharply in PPR:
;; the receiver catches 90 balls, the grinder scores touchdowns.
;;   standard: catcher 700*0.1 + 2*6 = 82   grinder 1100*0.1 + 12*6 = 182
;;   ppr:      catcher 82 + 90 = 172        grinder 182 + 10 = 192
(defn- catcher []
  {:player-id "catcher" :player-name "Pass Catcher" :position "RB"
   :stats {:rush_yd 0 :rush_td 0 :rec 90 :rec_yd 700 :rec_td 2}})

(defn- grinder []
  {:player-id "grinder" :player-name "Goal Line" :position "RB"
   :stats {:rush_yd 1100 :rush_td 12 :rec 10 :rec_yd 80 :rec_td 0}})

(defn- filler
  "Enough depth below them that replacement level sits under both, so they both
  carry positive VORP and therefore real dollars."
  [pos n]
  (mapv (fn [i] {:player-id (str pos i) :player-name (str pos i) :position pos
                 :stats {:rush_yd (- 500 (* i 20)) :rush_td (- 4 (* i 0.1))
                         :rec (- 20 (* i 0.5)) :rec_yd (- 200 (* i 8)) :rec_td 1}})
        (range n)))

(defn- board []
  (into [(catcher) (grinder)] (concat (filler "RB" 40) (filler "WR" 40)
                                      (filler "QB" 20) (filler "TE" 20))))

(def ^:private league
  {:teams (vec (repeat 12 {:bankroll 200.0
                           :roster (vec (repeat 15 {:pos "BENCH" :player-id nil}))}))
   :drafted-player-ids #{} :starting-bankroll 200 :picks []})

(defn- valued
  "The full shipped chain — score, band, replacement, tier, VORP, dollars — under
  one scoring config, indexed by player id."
  [scoring]
  (->> (engine/live-valuation
        (engine/static-rankings (board) scoring 12 {:replacement-config {:qb 1 :rb 2 :wr 2 :te 1 :flex 1}})
        league)
       :players
       (into {} (map (juxt :player-id identity)))))

(deftest every-derived-field-moves-when-scoring-moves
  (let [std (valued (:standard scoring/presets))
        ppr (valued (:ppr scoring/presets))]
    (doseq [field [:points :floor :ceiling :vorp :value :worth]]
      (testing (str field " responds to reception scoring")
        (is (not= (get-in std ["catcher" field]) (get-in ppr ["catcher" field])))))

    (testing "tier is computed from points, not taken from a vendor column"
      (is (not= (into {} (map (juxt key (comp :tier val))) std)
                (into {} (map (juxt key (comp :tier val))) ppr))))))

(deftest reception-scoring-reorders-the-board
  (let [std (valued (:standard scoring/presets))
        ppr (valued (:ppr scoring/presets))]
    (testing "in standard the grinder is worth more than twice the catcher"
      (is (> (get-in std ["grinder" :points]) (get-in std ["catcher" :points])))
      (is (> (get-in std ["grinder" :worth]) (get-in std ["catcher" :worth]))))

    (testing "80 more catches close three quarters of that gap"
      ;; standard: grinder 1100*.1 + 12*6 + 80*.1 = 190, catcher 700*.1 + 2*6 = 82
      ;; ppr:      + 10 catches = 200,                    + 90 catches = 172
      (let [gap-std (- (get-in std ["grinder" :points]) (get-in std ["catcher" :points]))
            gap-ppr (- (get-in ppr ["grinder" :points]) (get-in ppr ["catcher" :points]))]
        (is (= 108.0 gap-std))
        (is (= 28.0 gap-ppr))))

    (testing "and the catcher gains dollars while the grinder loses them"
      (is (> (get-in ppr ["catcher" :worth]) (get-in std ["catcher" :worth])))
      (is (< (get-in ppr ["grinder" :worth]) (get-in std ["grinder" :worth]))))))

(deftest half-ppr-lands-between-the-two
  (let [pts (fn [s] (get-in (valued s) ["catcher" :points]))]
    (is (< (pts (:standard scoring/presets))
           (pts (:half-ppr scoring/presets))
           (pts (:ppr scoring/presets))))))

(deftest a-custom-config-is-not-quietly-a-preset
  ;; Custom scoring is the least-travelled path and the one a manager reaches for
  ;; when their league is unusual. A TE-premium-shaped config must actually score
  ;; differently from the PPR preset it started as.
  (let [tep (assoc (:ppr scoring/presets) :rec 1.5)]
    (is (> (get-in (valued tep) ["catcher" :points])
           (get-in (valued (:ppr scoring/presets)) ["catcher" :points])))))

(deftest vendor-columns-follow-the-league-too
  ;; Not derived from :points, but published per format — so they have their own
  ;; way of going stale, which is how Market sat frozen across every scoring
  ;; change while Worth moved underneath it.
  (let [p {:player-id "p" :player-name "A B" :position "WR"
           :vendor/by-format {:ppr      {:sleeper/adp 3.4 :fantasypros/aav 59.0}
                              :half-ppr {:sleeper/adp 3.7 :fantasypros/aav 56.0}
                              :standard {:sleeper/adp 6.6 :fantasypros/aav 50.0}}}
        under (fn [s] (first (vendor/for-scoring [p] s)))]
    (is (= 6.6 (:sleeper/adp (under (:standard scoring/presets)))))
    (is (= 3.4 (:sleeper/adp (under (:ppr scoring/presets)))))
    (is (= 50.0 (:fantasypros/aav (under (:standard scoring/presets)))))
    (is (= 59.0 (:fantasypros/aav (under (:ppr scoring/presets)))))))
