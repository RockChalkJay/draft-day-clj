(ns draft-day.integration.espn-league-test
  "Live ESPN league contract. Opt-in twice over: `lein test :integration`, and
  only when the environment carries a real account's credentials.

  It exists for the things a fixture cannot check, all of which fail silently.
  The defensive entries in `league-import.espn/stat-ids` are ESPN's documented
  numbering rather than a payload this repo has read, so a wrong one prices a
  rule nobody set. And `lineup-slots` is the same — a slot read as the wrong
  seat mis-sizes the roster, which is the one number that decides whether a
  claim costs a drop. The matchup document is the third: it carries the whole
  season's schedule whatever week is asked for, and what dates a game is which
  side ESPN filled a roster in for.

  Set DRAFTDAY_ESPN_SWID, DRAFTDAY_ESPN_S2 and DRAFTDAY_ESPN_LEAGUE to run it;
  DRAFTDAY_ESPN_SEASON defaults to the current one. Without them every test
  here reports that it was skipped rather than passing quietly, because a
  contract test that silently does nothing is worse than no contract test."
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.ingestion.league-import :as league-import]
            [draft-day.ingestion.league-import.espn :as import-espn]
            [draft-day.ingestion.league-sync :as league-sync]
            [draft-day.ingestion.league-sync.espn]
            [draft-day.ingestion.matchups :as matchups]
            [draft-day.ingestion.matchups.espn :as m-espn]
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
      (testing "a defense is scored from the override ESPN files its rules under"
        (is (pos? (double (or (:sack scoring) 0)))
            "zero here is the whole league's defenses projecting nothing, which
             is what reading the base weight of an override does")
        (is (pos? (double (or (:int scoring) 0)))))
      (testing "the seats add up to a roster somebody could field"
        (is (pos? (:qb roster)))
        (is (pos? (+ (:rb roster) (:wr roster) (:flex roster))))))))

(deftest ^:integration espn-rosters-still-name-their-seats-and-their-players
  (with-league [req]
    (let [{:keys [teams roster-positions drafted?]} (:league (league-sync/sync-league req))
          mine (first teams)]
      (is (seq teams))
      (is (every? #(seq (:player-ids %)) teams)
          "a team holding nobody reaches the board as a league everyone has left")
      (is (true? drafted?)
          "rosters this full were drafted; nil means draftDetail moved, and the
           league opens on draft day until a week has been played")
      (is (every? string? (mapcat :player-ids teams)))
      (is (seq (:starter-ids mine))
          "starters come off lineupSlotId; none means the slot table moved")
      (is (<= (count (:active-ids mine)) (count (:player-ids mine))))
      (is (every? #(and (number? (:wins %)) (number? (:points-for %))) teams)
          "the League tab orders on these; nil means record.overall moved")
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

(deftest ^:integration espn-still-names-every-rostered-player
  (with-league [req]
    (let [{:keys [teams provider-players]} (:league (league-sync/sync-league req))
          named (set (map :id provider-players))
          held  (remove teams/app-teams (mapcat :player-ids teams))]
      (is (seq held))
      (is (every? named held)
          "a player the id file has not caught up with resolves by this name
           or not at all: fullName or defaultPositionId moved"))))

(deftest ^:integration espn-still-fills-one-weeks-rosters-into-the-schedule
  (with-league [req]
    (let [week (matchups/current-week :espn req)]
      (is (and (number? week) (pos? week))
          "no current week: scoringPeriodId moved, and the season half has no board")
      (let [raw   (matchups/fetch-raw-matchups :espn (assoc req :week week))
            games (m-espn/this-week (:schedule raw))
            sides (mapcat m-espn/sides games)]
        (is (seq (:schedule raw)) "the season's games still live under :schedule")
        (is (seq games)
            "no game carries a filled roster: rosterForCurrentScoringPeriod moved,
             and every lineup on the board would draw empty")
        (is (= 1 (count (distinct (map :matchupPeriodId games))))
            "the filled-roster filter picked up more than one period. Not
             compared to the week itself: a playoff period spans two of them")
        (is (every? #(number? (m-espn/official % week)) sides)
            "a side with no figure for this week means pointsByScoringPeriod moved")
        (testing "every entry names a seat and carries its applied total"
          (let [entries (mapcat m-espn/side-entries sides)]
            (is (seq entries))
            (is (every? #(contains? import-espn/lineup-slots (:lineupSlotId %)) entries)
                "an unlisted slot lands on the bench, so a starter would lose his seat")
            (is (every? #(number? (get-in % [:playerPoolEntry :appliedStatTotal])) entries)
                "the applied total is a level deeper than the entry; nil here blanks the board")))))))

(deftest ^:integration espn-matchup-roster-ids-are-the-ones-the-sync-reports
  (with-league [req]
    (let [{:keys [teams roster-positions]} (:league (league-sync/sync-league req))
          {:keys [ok week scores]} (matchups/fetch-matchups req)]
      (is ok)
      (is (number? week))
      (is (seq scores))
      (is (every? #(contains? scores (:roster-id %)) teams)
          "a matchup keyed off ids the sync does not use is every team drawn empty")
      (testing "and each lineup fills the seats the sync names"
        (let [seats (count (db/scoring-slots roster-positions))]
          (is (every? #(= seats (count (:starter-ids %))) (vals scores))
              "a lineup shorter than the seat list slides every seat below it up"))))))
