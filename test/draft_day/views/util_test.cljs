(ns draft-day.views.util-test
  "Headshot URL resolution for the On-the-block tile. Sleeper's CDN keys on the
  numeric Sleeper id, but the app's canonical :player-id is GSIS for most players.
  Run with `npx shadow-cljs compile test && node out/node-tests.js`."
  (:require [cljs.test :refer [deftest is testing]]
            [draft-day.db :as db]
            [draft-day.views.util :as util]))

(deftest headshot-url-uses-sleeper-id-from-ids
  (testing "GSIS-anchored players use [:ids :sleeper], not :player-id"
    (is (= "https://sleepercdn.com/content/nfl/players/thumb/10210.jpg"
           (util/headshot-url {:player-id "00-0038564"
                               :position  "TE"
                               :ids       {:sleeper "10210" :gsis "00-0038564"}})))))

(deftest headshot-url-dst-uses-team-logo
  (testing "DST players get the team logo from :ids :team"
    (is (= "https://sleepercdn.com/images/team_logos/nfl/bal.png"
           (util/headshot-url {:player-id "BAL"
                               :position  "DST"
                               :ids       {:sleeper "BAL" :team "BAL"}})))))

(deftest headshot-url-falls-back-to-player-id
  (testing "legacy rows without :ids still use :player-id"
    (is (= "https://sleepercdn.com/content/nfl/players/thumb/9509.jpg"
           (util/headshot-url {:player-id "9509" :position "RB"})))))

;; ---- signed differences ----
;; Barg and Edge share `sign-class` with the board's own columns, so the rule is
;; pinned here rather than in two places that could drift apart silently: a
;; wrong colour is not a crash and nothing else would catch it.

(deftest signed-carries-the-sign
  (is (= "+4" (util/signed 4)))
  (is (= "-4" (util/signed -4)))
  (is (= "+$4" (util/signed-money 4)))
  (is (= "-$4" (util/signed-money -4))
      "the unit goes between the sign and the digits, not in front of the sign"))

(deftest signed-dashes-what-is-not-a-verdict
  (doseq [f [util/signed util/signed-money]]
    (is (= "–" (f 0))
        "a difference of exactly nothing is not a verdict, and has no colour to take")
    (is (= "–" (f nil)))
    (is (= "–" (f "4")) "a string is not a number, however numeric it looks")))

(deftest sign-class-matches-the-boards-rule
  (is (= "good" (util/sign-class 4)))
  (is (= "warn" (util/sign-class -4)))
  (is (nil? (util/sign-class 0)) "zero takes neither colour")
  (is (nil? (util/sign-class nil))))

;; ---- positional label and its sort key ----
;; `db/pos-sort-key` lives in cljc but is reached only through `db/sort-accessors`,
;; which only `subs/sort-players` reads — so the browser is the one platform it
;; actually runs on, and `db_test.clj` covers the other one. The mixed-type
;; compare it does is the part worth pinning here rather than reasoning about.

(deftest pos-label-falls-back-to-the-bare-position
  (is (= "RB7" (util/pos-label {:position "RB" :pos-rank 7})))
  (is (= "RB" (util/pos-label {:position "RB"}))
      "a player the engine could not rank shows no number, not RBnil"))

(deftest pos-sort-key-orders-numerically-in-the-browser-too
  (let [rbs [{:position "RB" :pos-rank 2} {:position "RB" :pos-rank 11}
             {:position "RB" :pos-rank 1} {:position "RB" :pos-rank 10}]]
    (is (= [1 2 10 11] (mapv :pos-rank (sort-by db/pos-sort-key rbs)))))
  (testing "positions group, and an unranked row sorts last within its own"
    (let [b [{:position "WR" :pos-rank 1} {:position "RB" :pos-rank nil}
             {:position "RB" :pos-rank 1}]]
      (is (= [["RB" 1] ["RB" nil] ["WR" 1]]
             (mapv (juxt :position :pos-rank) (sort-by db/pos-sort-key b)))))))

;; ---- kickoff times ----

(deftest a-kickoff-renders-in-the-viewers-own-zone
  ;; The server ships the ISO stamp with its Z; the browser decides what that
  ;; means where the manager is sitting. Asserted loosely on purpose — the node
  ;; runner's zone is whatever the machine's is, so pinning "12:00 PM" would
  ;; make this a test of the test machine.
  (let [s (util/kickoff-label "2026-09-13T17:00Z")]
    (is (string? s))
    (is (re-find #"\d:\d\d" s) "a wall-clock time")
    (is (re-find #"(?i)sun|mon" s) "with the day attached")))

(deftest a-missing-or-broken-stamp-is-nil-not-a-dash
  ;; The caller drops the segment. A dash here would assert there is no game,
  ;; which a failed scoreboard fetch is not evidence of.
  (is (nil? (util/kickoff-label nil)))
  (is (nil? (util/kickoff-label "not a date"))))

(deftest a-scheduled-game-needs-no-status-word
  (is (nil? (util/kickoff-status-label "STATUS_SCHEDULED" "9/13 - 1:00 PM EDT"))))

(deftest a-game-that-is-over-says-so-in-espns-words
  (is (= "Final/OT" (util/kickoff-status-label "STATUS_FINAL" "Final/OT")))
  (is (= "Final" (util/kickoff-status-label "STATUS_FINAL" nil)) "a fallback word")
  (is (nil? (util/kickoff-status-label nil "Final/OT")) "no status, no claim"))
