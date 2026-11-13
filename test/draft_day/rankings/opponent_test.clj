(ns draft-day.rankings.opponent-test
  (:require [clojure.test :refer [deftest is]]
            [draft-day.rankings.opponent :as opponent]))

(def ppr {:rec 1.0 :rec_yd 0.1 :rec_td 6.0 :rush_yd 0.1 :pts_allow_0 10.0})

(defn- player [pos & games]
  {:position pos
   :realized/game-log (mapv (fn [[week opp stats]] {:week week :opponent opp :stats stats}) games)})

(def wr-a (player "WR" [1 "NYG" {:rec 5.0 :rec_yd 80.0}] [2 "NYJ" {:rec 2.0 :rec_yd 20.0}]))
(def wr-b (player "WR" [1 "NYG" {:rec 3.0 :rec_yd 40.0 :rec_td 1.0}] [3 "NYG" {:rec 1.0 :rec_yd 10.0}]))
(def rb   (player "RB" [2 "NYG" {:rush_yd 100.0}]))

(deftest a-game-is-a-week-not-a-row
  (let [nyg (get (opponent/points-allowed [wr-a wr-b rb] ppr 3) "NYG")]
    (is (= 3 (get-in nyg ["WR" :games])) "two receivers in week 1 are one game")
    (is (== (+ 13.0 13.0 2.0) (get-in nyg ["WR" :points])) "13 + 13 in week 1, 2 in week 3")
    (is (== (/ 28.0 3) (get-in nyg ["WR" :per-game])))
    (is (== (/ 10.0 3) (get-in nyg ["RB" :per-game])) "the RB's one game spreads over the team's three")))

(deftest a-position-nobody-played-against-a-team-scored-nothing
  (let [nyj (get (opponent/points-allowed [wr-a rb] ppr 3) "NYJ")]
    (is (== 0.0 (get-in nyj ["RB" :per-game])))
    (is (= 1 (get-in nyj ["RB" :games])))))

(deftest the-weeks-stop-at-through-week
  (let [nyg (get (opponent/points-allowed [wr-a wr-b rb] ppr 2) "NYG")]
    (is (= 2 (get-in nyg ["WR" :games])) "week 3 is being played, not played")
    (is (== 26.0 (get-in nyg ["WR" :points]))))
  (is (= {} (opponent/points-allowed [wr-a] ppr 0)) "preseason: nothing allowed yet"))

(deftest recent-is-the-teams-last-three-games
  (let [games (map (fn [w] [w "NYG" {:rec (double w)}]) (range 1 6))
        nyg   (get (opponent/points-allowed [(apply player "WR" games)] ppr 5) "NYG")]
    (is (== 4.0 (get-in nyg ["WR" :recent-per-game])) "weeks 3, 4 and 5")
    (is (== 3.0 (get-in nyg ["WR" :per-game])))))

(deftest a-teams-line-off-rows-in-any-order
  (let [rows [{:team "NYG" :position "WR" :week 4 :points 8.0}
              {:team "NYG" :position "RB" :week 1 :points 12.0}
              {:team "NYG" :position "WR" :week 2 :points 4.0}
              {:team "NYG" :position "WR" :week 2 :points 6.0}
              {:team "NYG" :position "WR" :week 3 :points 2.0}
              {:team "NYJ" :position "WR" :week 1 :points 5.0}]]
    (is (= {"NYG" {"WR" {:games 4 :points 20.0 :per-game 5.0 :recent-per-game (/ 20.0 3)}
                   "RB" {:games 4 :points 12.0 :per-game 3.0 :recent-per-game 0.0}}
            "NYJ" {"WR" {:games 1 :points 5.0 :per-game 5.0 :recent-per-game 5.0}
                   "RB" {:games 1 :points 0.0 :per-game 0.0 :recent-per-game 0.0}}}
           (opponent/team-lines rows))
        "recent is the team's weeks 2-4 by number, not the rows' last three; the RB's week 1 falls outside it")
    (is (= {} (opponent/team-lines [])))))

(deftest a-defenses-row-is-what-its-offense-allowed
  (let [dst (player "DST" [1 "NYJ" {:pts_allow_0 1.0}])]
    (is (== 10.0 (get-in (opponent/points-allowed [dst] ppr 1) ["NYJ" "DST" :per-game]))
        "the Jets' offense gave up a shutout")))

(deftest one-is-the-team-that-allowed-the-most
  (let [table (opponent/points-allowed [wr-a wr-b] ppr 3)]
    (is (= {:rank 1 :of 2} (select-keys (get-in table ["NYG" "WR"]) [:rank :of])))
    (is (= {:rank 2 :of 2} (select-keys (get-in table ["NYJ" "WR"]) [:rank :of])))))

(deftest a-rank-says-which-way-it-runs
  (is (= "1st most" (opponent/rank-label 1 32)))
  (is (= "16th most" (opponent/rank-label 16 32)))
  (is (= "16th fewest" (opponent/rank-label 17 32)))
  (is (= "1st fewest" (opponent/rank-label 32 32)))
  (is (= "2nd most" (opponent/rank-label 2 31)))
  (is (= ["11th" "12th" "13th" "21st" "22nd" "23rd" "111th"]
         (map opponent/ordinal [11 12 13 21 22 23 111]))))

(deftest a-row-takes-its-opponents-line-at-his-position
  (let [table {"NYG" {"WR" {:rank 3 :of 32 :per-game 30.0}}}
        [wr rb bye] (opponent/with-opp-vs-pos
                      [{:position "WR" :week/opponent "NYG"}
                       {:position "RB" :kickoff/opponent "NYG"}
                       {:position "WR"}]
                      table)]
    (is (= {:rank 3 :of 32 :per-game 30.0 :team "NYG"} (:opp-vs-pos wr)))
    (is (not (contains? rb :opp-vs-pos)) "no line at his position")
    (is (not (contains? bye :opp-vs-pos)) "a bye")))
