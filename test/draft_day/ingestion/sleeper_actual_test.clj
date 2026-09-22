(ns draft-day.ingestion.sleeper-actual-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.ingestion.sleeper-actual :as actual]
            [draft-day.scoring :as scoring]))

;; Shaped exactly like live /stats/nfl/{season}/{week} entries.
(defn- entry [id week & {:as stats}]
  {:player_id id :week week :opponent "BUF" :season_type "regular"
   :player {:position "WR"} :stats stats})

(deftest only-keys-the-engine-scores-are-carried
  (let [st (actual/scored-stats {:rec 5.0 :rec_yd 82.0 :rec_td 1.0
                                 :bonus_rec_yd_100 0.0
                                 :rec_tgt 9.0 :off_snp 44.0 :pts_ppr 19.2})]
    (is (= {:rec 5.0 :rec_yd 82.0 :rec_td 1.0} st))
    (is (nil? (:rec_tgt st)) "a usage column is not a rule")
    (is (nil? (:pts_ppr st)) "nor is the provider's own total")
    (is (nil? (:bonus_rec_yd_100 st))
        "a zero is dropped, so silence and futility stay apart")))

(deftest every-carried-key-is-one-a-league-could-state
  (is (every? (set scoring/stat-keys)
              (keys (actual/scored-stats (zipmap scoring/stat-keys (repeat 1.0)))))))

(deftest an-entry-with-nothing-to-score-is-not-an-appearance
  (is (nil? (actual/entry->row {:player_id "a" :week 2})))
  (is (nil? (actual/entry->row {:player_id "a" :stats {:rec 1.0}})))
  (is (nil? (actual/entry->row {:week 2 :stats {:rec 1.0}})))
  (is (some? (actual/entry->row (entry "a" 2 :rec 1.0)))))

(deftest a-week-with-no-entry-is-a-week-he-did-not-play
  ;; The distinction the player detail modal draws to keep a missed week from
  ;; reading as a zero: presence is the only evidence of an appearance, so
  ;; `:games` counts entries and the game log skips the gap entirely.
  (let [acc (actual/accumulate (mapv actual/entry->row
                                     [(entry "a" 1 :rec 3.0)
                                      (entry "a" 4 :rec 7.0 :rec_td 1.0)]))
        a   (get acc "a")]
    (is (= 2 (:games (:realized/season-to-date a))) "two appearances, not four weeks")
    (is (= [1 4] (mapv :week (:realized/game-log a))))
    (is (= {:rec 10.0 :rec_td 1.0} (:stats (:realized/season-to-date a))))))

(deftest the-game-log-is-ordered-oldest-first
  (let [rows (mapv actual/entry->row [(entry "a" 3 :rec 1.0)
                                      (entry "a" 1 :rec 2.0)
                                      (entry "a" 2 :rec 3.0)])]
    (is (= [1 2 3] (mapv :week (:realized/game-log (get (actual/accumulate rows) "a")))))))

(deftest the-recent-window-is-the-last-three-weeks-played
  (let [rows (mapv actual/entry->row (for [w (range 1 8)] (entry "a" w :rec 1.0)))
        a    (get (actual/accumulate rows) "a")]
    (is (= 7 (:games (:realized/season-to-date a))))
    (is (= 3 (:games (:realized/recent a))) "weeks 5, 6 and 7")
    (is (= actual/recent-window (:games (:realized/recent a)))))
  (testing "and a player with only one week played has a window of one"
    (let [a (get (actual/accumulate [(actual/entry->row (entry "a" 1 :rec 1.0))]) "a")]
      (is (= 1 (:games (:realized/recent a)))))))

(deftest a-team-defense-accumulates-like-anyone-else
  ;; The gap this module exists for: nflverse publishes no DST row at all, so a
  ;; defense's realized production was permanently empty and the tier rules had
  ;; no realized side to land on.
  (let [rows (mapv actual/entry->row
                   [{:player_id "SEA" :week 1 :stats {:sack 3.0 :pts_allow_7_13 1.0}}
                    {:player_id "SEA" :week 2 :stats {:sack 2.0 :pts_allow_14_20 1.0}}])
        sea  (get (actual/accumulate rows) "SEA")]
    (is (= {:sack 5.0 :pts_allow_7_13 1.0 :pts_allow_14_20 1.0}
           (:stats (:realized/season-to-date sea))))
    (is (= 2 (:games (:realized/season-to-date sea))))))

(deftest nothing-is-fetched-before-a-week-has-been-played
  ;; Preseason, and the normal state all August. Asking for week zero would be
  ;; a request per week that has not happened.
  (is (nil? (actual/fetch 2026 0)))
  (is (nil? (actual/fetch 2026 nil))))
