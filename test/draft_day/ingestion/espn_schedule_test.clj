(ns draft-day.ingestion.espn-schedule-test
  (:require [clojure.test :refer [deftest is]]
            [draft-day.ingestion.espn-schedule :as sched]))

;; Shaped exactly like the live 2026 week-1 scoreboard, trimmed to the fields
;; the parser reads. The three events are chosen: an ordinary road game, the
;; Melbourne opener (neutral site, LAR nominally home), and a finished game.

(def payload
  {:events
   [{:date "2026-09-13T17:00Z"
     :shortName "TB @ CIN"
     :status {:type {:name "STATUS_SCHEDULED" :shortDetail "9/13 - 1:00 PM EDT"}}
     :competitions [{:neutralSite false
                     :venue {:fullName "Paycor Stadium"}
                     :competitors [{:homeAway "home" :team {:abbreviation "CIN"}}
                                   {:homeAway "away" :team {:abbreviation "TB"}}]}]}
    {:date "2026-09-11T00:35Z"
     :shortName "SF VS LAR"
     :status {:type {:name "STATUS_SCHEDULED" :shortDetail "9/10 - 8:35 PM EDT"}}
     :competitions [{:neutralSite true
                     :venue {:fullName "Melbourne Cricket Ground"}
                     :competitors [{:homeAway "home" :team {:abbreviation "LAR"}}
                                   {:homeAway "away" :team {:abbreviation "SF"}}]}]}
    {:date "2026-09-14T00:20Z"
     :shortName "DAL @ WSH"
     :status {:type {:name "STATUS_FINAL" :shortDetail "Final/OT"}}
     :competitions [{:neutralSite false
                     :venue {:fullName "Northwest Stadium"}
                     :competitors [{:homeAway "home" :team {:abbreviation "WSH"}}
                                   {:homeAway "away" :team {:abbreviation "DAL"}}]}]}]})

(def by-team (sched/by-team payload))

;; ---- both sides of a game ----

(deftest a-game-gives-a-kickoff-to-both-teams
  ;; Six teams from three events.
  (is (= 6 (count by-team)))
  (is (= "2026-09-13T17:00Z" (:kickoff (by-team "CIN"))))
  (is (= "2026-09-13T17:00Z" (:kickoff (by-team "TB")))))

(deftest each-side-sees-the-other-as-its-opponent
  (is (= "TB"  (:opponent (by-team "CIN"))))
  (is (= "CIN" (:opponent (by-team "TB"))))
  (is (true?  (:home? (by-team "CIN"))))
  (is (false? (:home? (by-team "TB")))))

;; ---- the time is not formatted here ----

(deftest the-iso-stamp-keeps-its-zone
  ;; Formatting here would render every manager's board in the host's timezone.
  (is (re-find #"Z$" (:kickoff (by-team "CIN")))))

;; ---- the neutral site ----

(deftest a-neutral-site-is-flagged-so-home-is-not-a-lie
  ;; ESPN lists LAR as home in Melbourne and writes the matchup "SF VS LAR".
  (is (true? (:neutral? (by-team "LAR"))))
  (is (true? (:neutral? (by-team "SF"))))
  (is (false? (:neutral? (by-team "CIN"))))
  (is (= "Melbourne Cricket Ground" (:venue (by-team "LAR")))))

;; ---- the vendor vocabulary ----

(deftest espns-washington-arrives-as-the-apps-washington
  ;; Unnormalized, every Washington player loses his kickoff at a 97% hit rate.
  (is (some? (by-team "WAS")))
  (is (nil? (by-team "WSH")))
  (is (= "WAS" (:opponent (by-team "DAL")))))

;; ---- status ----

(deftest a-finished-game-says-so
  ;; "Sun 1:00 PM" over a game that ended two hours ago is a lie.
  (is (= "STATUS_FINAL" (:status (by-team "DAL"))))
  (is (= "Final/OT" (:detail (by-team "DAL"))))
  (is (= "STATUS_SCHEDULED" (:status (by-team "CIN")))))

;; ---- degrading ----

(deftest a-team-on-bye-is-absent-rather-than-empty
  ;; Pre-filling all 32 makes "no game" and "empty fetch" the same answer.
  (is (nil? (by-team "KC"))))

(deftest a-half-parsed-game-is-dropped-whole
  ;; One side would give a team a kickoff and leave the other looking like a bye.
  (is (empty? (sched/by-team
               {:events [{:date "2026-09-13T17:00Z"
                          :status {:type {:name "STATUS_SCHEDULED"}}
                          :competitions
                          [{:competitors [{:homeAway "home"
                                           :team {:abbreviation "CIN"}}]}]}]}))))

(deftest an-event-with-no-date-yields-nothing
  (is (empty? (sched/by-team
               {:events [{:status {:type {:name "STATUS_SCHEDULED"}}
                          :competitions
                          [{:competitors [{:homeAway "home" :team {:abbreviation "CIN"}}
                                          {:homeAway "away" :team {:abbreviation "TB"}}]}]}]}))))

(deftest a-body-that-is-not-a-scoreboard-is-empty-not-a-throw
  (is (empty? (sched/by-team {})))
  (is (empty? (sched/by-team {:events []}))))

;; ---- the url ----

(deftest the-url-asks-for-the-regular-season
  (let [u (sched/scoreboard-url 2026 3)]
    (is (re-find #"dates=2026" u) "the season year, not a calendar date")
    (is (re-find #"seasontype=2" u) "regular season")
    (is (re-find #"week=3" u))))
