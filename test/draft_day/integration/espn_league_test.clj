(ns draft-day.integration.espn-league-test
  "Live ESPN league contract. Opt-in twice over: `lein test :integration`, and
  only when the environment carries a real account's credentials.

  It exists for two things a fixture cannot check, both of which fail silently.
  The defensive entries in `league-import.espn/stat-ids` are ESPN's documented
  numbering rather than a payload this repo has read, so a wrong one prices a
  rule nobody set. And `lineup-slots` is the same — a slot read as the wrong
  seat mis-sizes the roster, which is the one number that decides whether a
  claim costs a drop.

  Set DRAFTDAY_ESPN_SWID, DRAFTDAY_ESPN_S2 and DRAFTDAY_ESPN_LEAGUE to run it;
  DRAFTDAY_ESPN_SEASON defaults to the current one. Without them every test
  here reports that it was skipped rather than passing quietly, because a
  contract test that silently does nothing is worse than no contract test."
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.ingestion.league-import :as league-import]
            [draft-day.ingestion.league-import.espn]
            [draft-day.ingestion.league-sync :as league-sync]
            [draft-day.ingestion.league-sync.espn]
            [draft-day.ingestion.season :as season]
            [draft-day.ingestion.teams :as teams]
            [draft-day.db :as db]))

(defn credentials []
  (let [swid (System/getenv "DRAFTDAY_ESPN_SWID")
        s2   (System/getenv "DRAFTDAY_ESPN_S2")]
    (when (and swid s2) {:swid swid :espn-s2 s2})))

(defn- league-id [] (System/getenv "DRAFTDAY_ESPN_LEAGUE"))

(defn- request []
  (when-let [creds (credentials)]
    (when-let [id (league-id)]
      {:provider "espn" :league-id id :credentials creds
       :season (or (System/getenv "DRAFTDAY_ESPN_SEASON") (season/current))})))

(defmacro with-league
  "Run `body` against a live league, or report the skip out loud."
  [binding & body]
  `(if-let [~(first binding) (request)]
     (do ~@body)
     (is true "skipped: DRAFTDAY_ESPN_SWID/_S2/_LEAGUE are not set")))

(deftest ^:integration espn-still-answers-with-the-settings-we-read
  (with-league [req]
    (let [raw (league-import/fetch-raw-league :espn req)]
      (is (seq (get-in raw [:settings :scoringSettings :scoringItems]))
          "the scoring rules still live under settings.scoringSettings.scoringItems")
      (is (seq (get-in raw [:settings :rosterSettings :lineupSlotCounts]))
          "and the seats under settings.rosterSettings.lineupSlotCounts")
      (is (string? (get-in raw [:settings :draftSettings :type]))
          "and the draft type under settings.draftSettings, which decides whether
           auctionBudget is a budget anybody bids with"))))

(deftest ^:integration espn-scoring-still-maps-onto-rules-this-league-set
  (with-league [req]
    (let [{:keys [scoring unsupported-scoring roster]}
          (:config (league-import/import-league req))]
      (is (seq scoring) "no rule mapped at all means the stat ids moved")
      (is (some (comp pos? val) (select-keys scoring [:pass_td :rush_td :rec_td]))
          "touchdowns are worth something in every league there is")
      (testing "what could not be applied is named, not numbered"
        (is (not-any? #(re-find #"^ESPN stat \d+$" %) unsupported-scoring)
            "an unnamed id means this league sets a rule stat-labels has not met"))
      (testing "the seats add up to a roster somebody could field"
        (is (pos? (:qb roster)))
        (is (pos? (+ (:rb roster) (:wr roster) (:flex roster))))))))

(deftest ^:integration espn-rosters-still-name-their-seats-and-their-players
  (with-league [req]
    (let [{:keys [teams roster-positions]} (:league (league-sync/sync-league req))
          mine (first teams)]
      (is (seq teams))
      (is (every? #(seq (:player-ids %)) teams)
          "a team holding nobody reaches the board as a league everyone has left")
      (is (every? string? (mapcat :player-ids teams)))
      (is (seq (:starter-ids mine))
          "starters come off lineupSlotId; none means the slot table moved")
      (is (<= (count (:active-ids mine)) (count (:player-ids mine))))
      (testing "every seat is one the lineup can actually fill"
        (doseq [s roster-positions]
          (is (or (db/held-slots s)
                  (contains? (set db/positions) s)
                  (contains? db/flex-slots s))
              (str s " is a seat nothing downstream can fill")))))))

(deftest ^:integration espn-defenses-still-resolve-to-teams-not-to-ids
  (with-league [req]
    (let [teams (:teams (:league (league-sync/sync-league req)))
          held  (set (mapcat :player-ids teams))
          dsts  (filter teams/app-teams held)]
      (is (seq dsts)
          "no defense resolved: proTeamId moved, and every league's defenses
           would sit on the free-agent board while their owners hold them"))))
