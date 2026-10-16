(ns draft-day.benchmark.ros-recent-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.benchmark.ros-recent :as ros-recent]))

(def cfg {:rec_yd 1.0})

(def ask-line
  {:nflverse/season-to-date {:games 4
                             :stats {:rec_yd 400.0}
                             :usage {:targets 40.0}}
   :nflverse/recent {:games 2
                     :stats {:rec_yd 240.0}
                     :usage {:targets 30.0}}})

(def after-by-id
  {"player-1" [[9 {"receiving_yards" "20"}]
               [10 {"receiving_yards" "40"}]]})

(defn- weekly-row [week]
  {"player_id" "player-1"
   "position" "RB"
   "season_type" "REG"
   "week" (str week)
   "receiving_yards" "100"
   "targets" "10"})

(deftest season-asks-builds-asks-at-each-configured-as-of-week
  (let [rows (mapv weekly-row (range 1 9))
        asks (ros-recent/season-asks cfg rows)]
    (is (= [4 6] (mapv :as-of asks)))
    (is (= [4 2] (mapv :games asks)))
    (is (= [100.0 100.0] (mapv :season asks)))
    (is (= [100.0 100.0] (mapv :actual asks))))
  (testing "no weekly rows produce no asks"
    (is (= [] (ros-recent/season-asks cfg [])))))

(deftest ask-player-builds-the-recent-season-and-actual-rates
  (is (= {:id "player-1"
          :as-of 8
          :games 2
          :season 100.0
          :recent 120.0
          :ppo 10.0
          :s-opp 10.0
          :r-opp 15.0
          :trend 1.5
          :actual 30.0}
         (ros-recent/ask-player cfg {"player-1" "RB"} after-by-id 8
                               ["player-1" ask-line]))))

(deftest ask-player-rejects-players-outside-the-study-sample
  (let [args [cfg {"player-1" "RB"} after-by-id 8 ["player-1" ask-line]]
        no-future-games [cfg {"player-1" "RB"} {} 8 ["player-1" ask-line]]
        quarterback [cfg {"player-1" "QB"} after-by-id 8 ["player-1" ask-line]]
        too-few-games (assoc-in args [4 1 :nflverse/season-to-date :games] 2)
        low-scoring (assoc-in args [4 1 :nflverse/season-to-date :stats :rec_yd] 20.0)]
    (testing "the player must have subsequent games to score against"
      (is (nil? (apply ros-recent/ask-player no-future-games))))
    (testing "only receiving positions have meaningful opportunity rates"
      (is (nil? (apply ros-recent/ask-player quarterback))))
    (testing "the season sample needs at least three games"
      (is (nil? (apply ros-recent/ask-player too-few-games))))
    (testing "the season rate must clear the minimum points-per-game threshold"
      (is (nil? (apply ros-recent/ask-player low-scoring))))))
