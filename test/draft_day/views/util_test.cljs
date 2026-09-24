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

;; ---- a week's points ----

(deftest a-dash-is-not-a-zero
  ;; `:actual` is nil until kickoff, and 0.00 there would turn a Sunday morning
  ;; into nine bad performances.
  (is (= "-" (util/week-points nil)))
  (is (= "0.00" (util/week-points 0.0)) "he played and did nothing, which is a result"))

(deftest week-points-keep-two-decimals
  ;; The hosts report a score to the hundredth, and the second place prints even
  ;; when it is a zero, so a column of them lines up.
  (is (= "12.64" (util/week-points 12.644)))
  (is (= "8.40" (util/week-points 8.4)))
  (is (= "-3.46" (util/week-points -3.456)) "a defense can score below zero"))

(deftest a-half-cent-rounds-the-way-the-host-rounds-it
  ;; Both are stored a hair under the half, which `.toFixed` rounds down.
  (is (= "18.35" (util/week-points 18.345)))
  (is (= "1.01" (util/week-points 1.005)))
  (is (= "-18.35" (util/week-points -18.345)) "half away from zero, both ways"))

(deftest nothing-below-a-cent-carries-a-sign
  (is (= "0.00" (util/week-points -0.004)) "not -0.00")
  (is (zero? (util/hundredths 1e-14))
      "a lineup gain's summation drift is no gain")
  (is (zero? (util/hundredths (- (+ 0.1 0.2) 0.3)))))

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
  ;; Asserted loosely: the node runner's zone is the machine's, so pinning
  ;; "12:00 PM" would make this a test of the test machine.
  (let [s (util/kickoff-label "2026-09-13T17:00Z")]
    (is (string? s))
    (is (re-find #"\d:\d\d" s) "a wall-clock time")
    (is (re-find #"(?i)sun|mon" s) "with the day attached")))

(deftest a-missing-or-broken-stamp-is-nil-not-a-dash
  ;; A dash asserts there is no game, which a failed fetch is not evidence of.
  (is (nil? (util/kickoff-label nil)))
  (is (nil? (util/kickoff-label "not a date"))))

(deftest a-scheduled-game-needs-no-status-word
  (is (nil? (util/kickoff-status-label "STATUS_SCHEDULED" "9/13 - 1:00 PM EDT"))))

(deftest a-game-that-is-over-says-so-in-espns-words
  (is (= "Final/OT" (util/kickoff-status-label "STATUS_FINAL" "Final/OT")))
  (is (= "Q3 5:22" (util/kickoff-status-label "STATUS_IN_PROGRESS" "Q3 5:22")))
  (is (nil? (util/kickoff-status-label nil "Final/OT")) "no status, no claim"))

(deftest a-word-of-our-own-is-never-invented-for-a-status
  ;; A fallback like "Final" is eventually printed over a game still being
  ;; played, and a manager who reads Final stops considering the claim.
  (is (nil? (util/kickoff-status-label "STATUS_FINAL" nil)))
  (is (nil? (util/kickoff-status-label "STATUS_IN_PROGRESS" nil))))

(deftest projection-timestamp-cannot-rot
  ;; Absolute, not relative: this label exists to expose staleness and is
  ;; rendered once, so an age computed at render would go stale in exactly the
  ;; case it is for — a tab left open on a Sunday morning.
  (let [ago #(.toISOString (js/Date. (- (js/Date.now) (* % 60000))))]
    ;; Same instant, read twice an hour apart, reads the same both times.
    (is (= (util/fetched-at-label (ago 5))
           (util/fetched-at-label (ago 5))))
    ;; Today is a bare clock time; older carries the date so it cannot be read
    ;; as this morning.
    (is (not (re-find #"," (util/fetched-at-label (ago 1)))))
    (is (re-find #"," (util/fetched-at-label (ago (* 60 48)))))
    (is (nil? (util/fetched-at-label nil)))))
