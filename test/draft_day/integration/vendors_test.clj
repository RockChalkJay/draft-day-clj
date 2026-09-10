(ns draft-day.integration.vendors-test
  "Live vendor contracts. Opt-in: `lein test :integration`.

  The only tests here that touch the network, and they exist for the failure a
  fixture cannot see — a vendor quietly changing shape. Every trap this repo
  has paid for was that kind: FantasyPros moving an injury badge inside the name
  cell, ESPN spelling Washington WSH, nflverse spelling the Rams LA. A fixture
  captured the day the parser was written agrees with the parser forever.

  THEY ASSERT AGAINST A COMPLETED SEASON, not the current one. Their question is
  \"did the shape change\", and pinning them to this week would make them fail
  every August when a schedule is not out yet — a test that cries wolf on the
  calendar gets muted, and then it is not a contract at all.

  Team counts are per week, not constant: byes mean a midseason week has fewer
  than thirty-two. Week 1 is the one week that always has all of them."
  (:require [clojure.test :refer [deftest is]]
            [draft-day.ingestion.espn-schedule :as sched]
            [draft-day.ingestion.nflverse :as nflverse]
            [draft-day.ingestion.nflverse-weekly :as weekly]
            [draft-day.ingestion.sleeper :as sleeper]
            [draft-day.ingestion.teams :as teams]))

(def ^:private season 2025)

;; ---- ESPN scoreboard ----

(deftest ^:integration espn-week-one-still-covers-every-team
  (let [m (sched/fetch season 1)]
    (is (= 32 (count m)) "week 1 has no byes")
    (is (teams/covers-vocabulary? :espn (keys m))
        "a renamed team, against the vendor rather than a fixture")))

(deftest ^:integration espn-still-ships-an-iso-stamp-and-a-status
  (let [e (val (first (sched/fetch season 1)))]
    (is (re-find #"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}Z$" (:kickoff e))
        "UTC, unformatted — the browser renders the wall clock")
    (is (string? (:status e)))
    (is (string? (:opponent e)))))

(deftest ^:integration a-bye-week-is-short-of-thirty-two
  ;; If this ever returns 32 the fetch has stopped being per-week, and every
  ;; kickoff would be for the wrong game.
  (is (< (count (sched/fetch season 9)) 32)))

;; ---- nflverse weekly ----

(deftest ^:integration the-weekly-file-still-has-the-columns-the-parser-needs
  (let [rows (weekly/fetch-rows season)]
    (is (seq rows))
    (is (every? (set (keys (first rows))) weekly/required-columns))
    (is (contains? (first rows) "opponent_team")
        "outside required-columns — a miss there would read as week zero")))

(deftest ^:integration nflverses-team-spelling-still-maps-onto-ours
  (let [rows (weekly/fetch-rows season)]
    (is (teams/covers-vocabulary?
         :nflverse (into #{} (keep #(get % "team")) rows))
        "the LA/LAR trap, against the vendor")))

;; ---- nflverse season ----

(deftest ^:integration the-season-file-still-has-the-stat-columns
  (let [rows (nflverse/fetch-season-rows (dec season))]
    (is (seq rows))
    (is (every? (set (keys (first rows))) (keys nflverse/line-columns)))))

;; ---- Sleeper ----

(deftest ^:integration the-schedule-still-carries-what-byes-are-derived-from
  (let [g (first (sleeper/fetch-schedule season))]
    (is (every? #(contains? g %) [:week :home :away])
        "schedule->byes reads exactly these")))

(deftest ^:integration a-weekly-projection-still-carries-its-own-opponent
  ;; The reason the matchup costs no second fetch — see `sleeper/weekly-line`.
  (let [entries (sleeper/fetch-weekly-entries season 1)
        scored  (filter #(get-in % [:stats :pts_ppr]) entries)]
    (is (seq scored) "somebody is projected in week 1")
    (is (some :opponent scored))))
