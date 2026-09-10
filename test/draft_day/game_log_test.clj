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
    ;; weeks 3 and 4 missed
    {:week 5 :opponent "PHI" :stats {}}]})

;; ---- a row per week, not a row per appearance ----

(deftest a-missed-week-keeps-its-row
  ;; A back who missed weeks 3 to 5 must read as one who missed them, not as one
  ;; whose season was three weeks shorter.
  (let [rows (:rows (gl/table nacua 5 ppr))]
    (is (= [1 2 3 4 5] (mapv :week rows)))
    (is (= [true true false false true] (mapv :played? rows)))))

(deftest the-log-is-ordered-by-week
  (let [shuffled (update nacua :nflverse/game-log reverse)]
    (is (= [1 2 3 4 5] (mapv :week (:rows (gl/table shuffled 5 ppr)))))))

;; ---- BLANK IS NOT ZERO, in the cell where the lie looks like a result ----

(deftest a-week-he-did-not-play-scores-nothing-rather-than-zero
  ;; `scoring/player-points` reads a missing stat as 0, so it scores an absent
  ;; week as a cheerful 0.0 unless the computation is gated on the row.
  (let [rows (:rows (gl/table nacua 5 ppr))]
    (is (nil? (:points (nth rows 2))))
    (is (nil? (:points (nth rows 3))))))

(deftest a-week-he-played-badly-scores-zero-and-that-is-correct
  ;; He was active and did nothing, which is a different fact from being out.
  (let [wk5 (nth (:rows (gl/table nacua 5 ppr)) 4)]
    (is (true? (:played? wk5)))
    (is (= 0.0 (:points wk5)))))

(deftest a-missed-week-has-no-values-and-no-opponent
  (let [wk3 (nth (:rows (gl/table nacua 5 ppr)) 2)]
    (is (every? nil? (:values wk3)))
    (is (nil? (:opponent wk3)))))

;; ---- scored under the league's own rules ----

(deftest points-follow-the-leagues-weights
  (let [pts #(:points (first (:rows (gl/table nacua 1 %))))]
    ;; 6 rec, 84 yds, 1 TD: 6 + 8.4 + 6 under PPR, 8.4 + 6 under standard.
    (is (= 20.4 (pts ppr)))
    (is (= 14.4 (pts (:standard scoring/presets))))))

;; ---- columns are the season table's, not a second vocabulary ----

(deftest the-columns-are-the-position-rows-verbatim
  ;; The season trend table sits directly above this one in the same modal.
  (is (= (get @#'draft-day.stat-lines/position-rows "WR")
         (:columns (gl/table nacua 5 ppr)))))

(deftest a-quarterback-gets-his-own-columns
  (let [qb {:position "QB"
            :nflverse/game-log [{:week 1 :stats {:pass_yd 300.0}}]}]
    (is (= ["Pass Yd" "Pass TD" "Rush Yd" "Rush TD"]
           (mapv first (:columns (gl/table qb 1 ppr)))))))

;; ---- the line arrives sparse ----

(deftest a-stat-absent-from-a-week-he-played-reads-as-the-zero-it-is
  ;; `nflverse-weekly` drops the zeros the file publishes in all fourteen
  ;; columns; the week's own presence is what says he was there.
  (let [qb {:position "QB"
            :nflverse/game-log [{:week 1 :stats {:pass_yd 300.0}}]}]
    (is (= [300.0 0 0 0] (:values (first (:rows (gl/table qb 1 ppr))))))))

(deftest a-week-he-missed-still-reads-as-nothing-at-all
  ;; The distinction the sparseness must not cost: absent-because-zero and
  ;; absent-because-out have to stay apart.
  (let [qb {:position "QB"
            :nflverse/game-log [{:week 1 :stats {:pass_yd 300.0}}]}
        wk2 (second (:rows (gl/table qb 2 ppr)))]
    (is (false? (:played? wk2)))
    (is (= [nil nil nil nil] (:values wk2)))))

;; ---- nothing to draw ----

(deftest a-kicker-and-a-defense-get-no-table
  (is (nil? (gl/table {:position "K"} 5 ppr)))
  (is (nil? (gl/table {:position "DST"} 5 ppr))))

(deftest a-season-that-has-not-started-gets-no-table
  ;; Preseason is `:through-week 0`, and a table of five empty rows says less
  ;; than no table at all.
  (is (nil? (gl/table nacua 0 ppr)))
  (is (nil? (gl/table nacua nil ppr))))

(deftest a-player-with-no-log-still-draws-the-weeks
  ;; On the board and has not appeared — every week absent is the answer.
  (let [rows (:rows (gl/table {:position "WR"} 3 ppr))]
    (is (= 3 (count rows)))
    (is (every? (complement :played?) rows))
    (is (every? (comp nil? :points) rows))))
