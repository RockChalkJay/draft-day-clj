(ns draft-day.matchup-ease-test
  (:require [clojure.test :refer [deftest is]]
            [draft-day.matchup-ease :as ease]))

(deftest a-rank-says-which-way-it-runs
  (is (= "1st most" (ease/rank-label 1 32)))
  (is (= "16th most" (ease/rank-label 16 32)))
  (is (= "16th fewest" (ease/rank-label 17 32)))
  (is (= "1st fewest" (ease/rank-label 32 32)))
  (is (= "2nd most" (ease/rank-label 2 31)))
  (is (= ["11th" "12th" "13th" "21st" "22nd" "23rd" "111th"]
         (map ease/ordinal [11 12 13 21 22 23 111]))))

(def line {:rank 8 :of 32 :per-game 14.25 :games 9})

(deftest a-line-is-found-under-either-kind-of-key
  (is (= line (ease/ease-line {"BUF" {"RB" line}} "BUF" "RB")))
  (is (= line (ease/ease-line {:BUF {:RB line}} "BUF" "RB")) "the browser's table, after JSON"))

(deftest no-line-colors-a-label-without-a-game-to-rank
  (is (nil? (ease/ease-line {"BUF" {"RB" line}} nil "RB")) "a bye")
  (is (nil? (ease/ease-line {"BUF" {"RB" line}} "BUF" "WR")))
  (is (nil? (ease/ease-line {"BUF" {"RB" (assoc line :games 2)}} "BUF" "RB"))
      "two games rank a team on one opponent's day"))

(deftest the-softest-defense-is-zero-and-the-stingiest-one
  (is (== 0.0 (ease/ease-share {:rank 1 :of 32})))
  (is (== 1.0 (ease/ease-share {:rank 32 :of 32}))))

(deftest the-middle-of-the-ramp-is-the-muted-grey
  (is (= "color-mix(in oklab, var(--good), var(--muted) 0%)" (ease/ease-color 0.0)))
  (is (= "color-mix(in oklab, var(--good), var(--muted) 100%)" (ease/ease-color 0.5)))
  (is (= "color-mix(in oklab, var(--muted), var(--warn) 100%)" (ease/ease-color 1.0))))

(deftest the-title-states-the-rank-in-words
  (is (= "BUF has allowed the 8th most points to RBs: 14.3 a game"
         (ease/ease-title line "BUF" "RB")))
  (is (= "NYJ has allowed the 1st fewest points to defenses"
         (ease/ease-title {:rank 32 :of 32} "NYJ" "DST"))))
