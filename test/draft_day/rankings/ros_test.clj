(ns draft-day.rankings.ros-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.scoring :as scoring]
            [draft-day.rankings.ros :as ros]))

(def ^:private ppr (:ppr scoring/presets))

(defn- ctx [through-week & {:as over}]
  (merge {:through-week through-week :season-games 17} over))

(defn- player
  "A receiver with a preseason line and, optionally, a realized one."
  [& {:keys [pre played realized bye]
      :or   {bye 7}}]
  (cond-> {:player-id "p" :position "WR" :bye bye}
    pre      (assoc :stats pre)
    played   (assoc :nflverse/season-to-date
                    {:games played :stats (or realized {})})))

;; ---- games remaining ----

(deftest weeks-left-is-not-games-left
  ;; Since 2021 a team plays 17 games across 18 weeks. Treating the two as one
  ;; number hands every player an extra game.
  (testing "preseason: the whole season, bye included"
    (is (= 17.0 (ros/games-remaining {:through-week 0 :season-games 17 :bye 7}))))
  (testing "the bye is subtracted only while it is still ahead"
    (is (= 11.0 (ros/games-remaining {:through-week 6 :season-games 17 :bye 7})))
    (is (= 11.0 (ros/games-remaining {:through-week 7 :season-games 17 :bye 7}))
        "week 7 played: 11 weeks left, bye already taken")
    (is (= 10.0 (ros/games-remaining {:through-week 8 :season-games 17 :bye 7}))))
  (testing "the season ends at zero, never below it"
    (is (= 0.0 (ros/games-remaining {:through-week 18 :season-games 17 :bye 7})))
    (is (= 0.0 (ros/games-remaining {:through-week 25 :season-games 17 :bye 7})))))

(deftest a-player-with-no-bye-on-record-gets-the-expected-one
  ;; A teamless player would otherwise quietly gain a game, every week, all
  ;; season — the error always runs the same direction.
  (is (= 17.0 (ros/games-remaining {:through-week 0 :season-games 17 :bye nil})))
  (let [mid (ros/games-remaining {:through-week 9 :season-games 17 :bye nil})]
    (is (< 8.0 mid 9.0) "nine weeks left, about half a bye still to come")))

(deftest the-sixteen-game-era-is-not-hardcoded-away
  ;; The harness reaches back past 2021; a hardcoded 17 would hand every player
  ;; in an older season a free game.
  (is (= 16.0 (ros/games-remaining {:through-week 0 :season-games 16 :bye 7}))))

;; ---- the blend ----

(deftest week-zero-is-the-preseason-board
  ;; The safe degradation, and the one that runs all August: no realized line
  ;; anywhere means the rest-of-season board *is* the draft board.
  (let [p (first (ros/with-ros [(player :pre {:rec 100.0 :rec_yd 1200.0})] ppr (ctx 0)))]
    (is (= 100.0 (get-in p [:ros/stats :rec])))
    (is (= 1200.0 (get-in p [:ros/stats :rec_yd])))
    (is (= 0 (:ros/games-played p)))
    (is (= (scoring/player-points {:stats {:rec 100.0 :rec_yd 1200.0}} ppr)
           (:ros-points p)))))

(deftest no-games-played-needs-no-special-case
  ;; A rookie who has not debuted, and every defense (nflverse publishes no DST
  ;; row at all), land here. The blend collapses to the prorated projection.
  (let [p (first (ros/with-ros [(player :pre {:rec 85.0})] ppr (ctx 8)))]
    ;; weeks 9-18 remain and the bye at 7 is already taken -> 10 games
    (is (= 10.0 (:ros/games-remaining p)))
    (is (< 49.9 (get-in p [:ros/stats :rec]) 50.1) "85 * 10/17")))

(deftest realized-production-pulls-the-rate-toward-itself
  (let [;; projected 17 catches (1.0/game); actually catching 5.0/game for 8 games
        p (first (ros/with-ros [(player :pre {:rec 17.0} :played 8 :realized {:rec 40.0})]
                               ppr (ctx 8)))
        rate (/ (get-in p [:ros/stats :rec]) (:ros/games-remaining p))]
    ;; (6*1.0 + 40) / (6+8) = 46/14 = 3.286
    (is (< 3.28 rate 3.29))
    (is (< 1.0 rate 5.0) "between the projection and the realized rate, by construction")))

(deftest the-projection-holds-the-line-early-and-yields-late
  ;; The whole point of the prior: one loud week must not reorder the board, and
  ;; a role change in September must not still be argued with in December.
  (let [rate-at (fn [played]
                  (let [p (first (ros/with-ros
                                  [(player :pre {:rec 17.0} :played played
                                           :realized {:rec (* 5.0 played)})]
                                  ppr (ctx (inc played))))]
                    (/ (get-in p [:ros/stats :rec]) (:ros/games-remaining p))))]
    (is (< (rate-at 2) (rate-at 6) (rate-at 12))
        "the realized rate wins more of the blend as evidence accumulates")
    (is (< (rate-at 2) 2.5) "two loud weeks are still mostly projection")
    (is (> (rate-at 12) 3.5) "twelve of them are mostly fact")))

(deftest a-player-with-no-preseason-line-is-valued-off-what-he-has-done
  ;; The undrafted rookie who is now the lead back — the player the whole
  ;; feature exists for. Deliberately discounted early: his total is spread over
  ;; prior-games + played, not over played.
  (let [p (first (ros/with-ros
                  [(player :played 4 :realized {:rush_yd 400.0 :rush_td 4.0})]
                  ppr (ctx 5)))
        rate (/ (get-in p [:ros/stats :rush_yd]) (:ros/games-remaining p))]
    ;; 400 / (6+4) = 40 yd/game, not the 100 he has actually been running for
    (is (< 39.9 rate 40.1))
    (is (pos? (:ros-points p)) "he is on the board, which he was not before")))

(deftest a-breakout-climbs-as-the-games-accumulate
  (let [rate-at (fn [played]
                  (let [p (first (ros/with-ros
                                  [(player :played played
                                           :realized {:rush_yd (* 100.0 played)})]
                                  ppr (ctx (inc played))))]
                    (/ (get-in p [:ros/stats :rush_yd]) (:ros/games-remaining p))))]
    (is (< (rate-at 1) (rate-at 5) (rate-at 10)))
    (is (< (rate-at 10) 100.0) "still shrunk, always — the prior never fully lets go")))

(deftest a-stat-neither-side-mentions-stays-out-of-the-line
  (let [p (first (ros/with-ros [(player :pre {:rec 50.0} :played 3 :realized {:rec 12.0})]
                               ppr (ctx 4)))]
    (is (contains? (:ros/stats p) :rec))
    (is (not (contains? (:ros/stats p) :pass_yd))
        "silence is not futility — the same rule ingestion keeps")))

(deftest a-season-with-nothing-left-projects-nothing
  (let [p (first (ros/with-ros [(player :pre {:rec 100.0})] ppr (ctx 18)))]
    (is (= 0.0 (:ros-points p)) "a score that sorts and subtracts, not a nil")
    (is (nil? (:ros/stats p)) "and no columns pretending to explain it")))

(deftest scoring-weights-reach-the-rest-of-season-line
  ;; A rest-of-season board and a draft board are the same board asked about
  ;; different games — so the league's own weights have to move it.
  (let [board [(player :pre {:rec 100.0 :rec_yd 1000.0})]
        pts   (fn [s] (:ros-points (first (ros/with-ros board s (ctx 0)))))]
    (is (> (pts ppr) (pts (:standard scoring/presets)))
        "a PPR league values the same line higher")))

(deftest season-games-must-be-supplied-rather-than-assumed
  ;; `rankings` keeps no NFL calendar of its own; a missing value should say so
  ;; rather than NPE somewhere downstream.
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"season-games"
                        (ros/with-ros [(player :pre {:rec 1.0})] ppr {:through-week 3}))))
