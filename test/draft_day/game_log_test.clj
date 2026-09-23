(ns draft-day.game-log-test
  (:require [clojure.test :refer [deftest is]]
            [draft-day.game-log :as gl]
            [draft-day.scoring :as scoring]))

(def ppr (:ppr scoring/presets))

(def nacua
  {:position "WR"
   :nflverse/game-log
   [{:week 1 :opponent "SEA" :stats {:rec 6.0 :rec_yd 84.0 :rec_td 1.0}}
    {:week 2 :opponent "TB"  :stats {:rec 2.0 :rec_yd 19.0}}
    {:week 5 :opponent "PHI" :stats {}}]})

(deftest a-missed-week-keeps-its-row
  (let [rows (:rows (gl/table nacua 5 ppr))]
    (is (= [1 2 3 4 5] (mapv :week rows)))
    (is (= [true true false false true] (mapv :played? rows)))))

(deftest the-log-is-ordered-by-week
  (let [shuffled (update nacua :nflverse/game-log reverse)]
    (is (= [1 2 3 4 5] (mapv :week (:rows (gl/table shuffled 5 ppr)))))))

(deftest a-week-he-did-not-play-scores-nothing-rather-than-zero
  (let [rows (:rows (gl/table nacua 5 ppr))]
    (is (nil? (:points (nth rows 2))))
    (is (nil? (:points (nth rows 3))))))

(deftest a-week-he-played-badly-scores-zero-and-that-is-correct
  (let [wk5 (nth (:rows (gl/table nacua 5 ppr)) 4)]
    (is (true? (:played? wk5)))
    (is (= 0.0 (:points wk5)))))

(deftest a-missed-week-has-no-values-and-no-opponent
  (let [wk3 (nth (:rows (gl/table nacua 5 ppr)) 2)]
    (is (every? nil? (:values wk3)))
    (is (nil? (:opponent wk3)))))

(deftest points-follow-the-leagues-weights
  (let [pts #(:points (first (:rows (gl/table nacua 1 %))))]
    (is (= 20.4 (pts ppr)))
    (is (= 14.4 (pts (:standard scoring/presets))))))

(deftest the-columns-are-the-position-rows-verbatim
  (is (= (get @#'draft-day.stat-lines/position-rows "WR")
         (:columns (gl/table nacua 5 ppr)))))

(deftest a-quarterback-gets-his-own-columns
  (let [qb {:position "QB"
            :nflverse/game-log [{:week 1 :stats {:pass_yd 300.0}}]}]
    (is (= ["Pass Yd" "Pass TD" "Rush Yd" "Rush TD"]
           (mapv first (:columns (gl/table qb 1 ppr)))))))

(deftest a-stat-absent-from-a-week-he-played-reads-as-the-zero-it-is
  (let [qb {:position "QB"
            :nflverse/game-log [{:week 1 :stats {:pass_yd 300.0}}]}]
    (is (= [300.0 0 0 0] (:values (first (:rows (gl/table qb 1 ppr))))))))

(deftest a-week-he-missed-still-reads-as-nothing-at-all
  (let [qb {:position "QB"
            :nflverse/game-log [{:week 1 :stats {:pass_yd 300.0}}]}
        wk2 (second (:rows (gl/table qb 2 ppr)))]
    (is (false? (:played? wk2)))
    (is (= [nil nil nil nil] (:values wk2)))))

(deftest a-kicker-and-a-defense-get-no-table
  (is (nil? (gl/table {:position "K"} 5 ppr)))
  (is (nil? (gl/table {:position "DST"} 5 ppr))))

(deftest a-season-that-has-not-started-gets-no-table
  (is (nil? (gl/table nacua 0 ppr)))
  (is (nil? (gl/table nacua nil ppr))))

(deftest a-player-with-no-log-still-draws-the-weeks
  (let [rows (:rows (gl/table {:position "WR"} 3 ppr))]
    (is (= 3 (count rows)))
    (is (every? (complement :played?) rows))
    (is (every? (comp nil? :points) rows))))

(deftest sleepers-own-line-wins-where-it-arrived
  (let [p (assoc nacua :realized/game-log
                 [{:week 1 :opponent "SEA" :stats {:rec 9.0 :rec_yd 140.0
                                                   :rec_td 1.0 :rec_40p 1.0}}])
        rows (:rows (gl/table p 1 ppr))]
    (is (= 1 (count rows)))
    (is (= 9.0 (first (:values (first rows))))
        "the Rec cell reads Sleeper's count, not nflverse's six")
    (is (= (scoring/player-points {:stats {:rec 9.0 :rec_yd 140.0
                                           :rec_td 1.0 :rec_40p 1.0}} ppr)
           (:points (first rows))))))

(deftest nflverse-still-answers-when-sleeper-did-not
  (let [rows (:rows (gl/table nacua 2 ppr))]
    (is (= [6.0 2.0] (mapv #(first (:values %)) rows))))
  (is (nil? (gl/table (dissoc nacua :nflverse/game-log) nil ppr))))
