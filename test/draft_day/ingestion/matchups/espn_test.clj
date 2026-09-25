(ns draft-day.ingestion.matchups.espn-test
  "ESPN's half of the matchup seam, over a fixture cut down from a live league's
  own payload.

  The four traps all fail silently: a document carrying the whole season's
  schedule whatever week is asked for, a side whose `totalPoints` is 0.0 while
  the games are being played, an applied total a level deeper than the entry it
  belongs to, and a lineup that names its own seats where the shared board reads
  them by position.

  `test/draft_day/integration/espn_league_test.clj` checks the same shape
  against ESPN itself; a fixture can only hold it steady."
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.ingestion.league-import.espn :as import-espn]
            [draft-day.ingestion.matchups :as matchups]
            [draft-day.ingestion.matchups.espn :as m-espn]))

(defn- entry
  "One roster entry in ESPN's shape: a seat, an id, and its applied total on the
  `playerPoolEntry` rather than beside it."
  [slot id pos applied & [pro-team]]
  {:lineupSlotId slot
   :playerId     id
   :playerPoolEntry {:appliedStatTotal applied
                     :player {:defaultPositionId pos :proTeamId pro-team}}})

;; One of the two RB seats is deliberately left unfilled.
(def ^:private home-entries
  [(entry 0  4040715 1 8.64)
   (entry 2  4362238 2 6.9)
   (entry 4  4430878 3 0.0)
   (entry 4  4241478 3 17.6)
   (entry 6  4361307 4 12.1)
   (entry 16 -16023 16 11.0 23)
   (entry 17 2473037 5 7.0)
   (entry 23 4695883 3 5.3)
   (entry 20 4385690 4 22.5)])

(def ^:private away-entries
  [(entry 0 4038941 1 19.2)
   (entry 16 -16028 16 4.0 28)])

(def ^:private raw
  {:scoringPeriodId 2
   :status   {:currentMatchupPeriod 2 :latestScoringPeriod 2}
   :settings {:rosterSettings
              ;; Keywordized integer keys, the way `json/mapper` hands them over.
              {:lineupSlotCounts {:0 1 :2 2 :4 2 :6 1 :16 1 :17 1 :20 7 :21 1 :23 1}}}
   :schedule
   [    ;; Last week's game, shipped with its rosters emptied out.
    {:id 1 :matchupPeriodId 1
     :home {:teamId 11 :totalPoints 105.64 :pointsByScoringPeriod {:1 105.64}
            :rosterForCurrentScoringPeriod {:entries []}}
     :away {:teamId 3 :totalPoints 98.2 :pointsByScoringPeriod {:1 98.2}
            :rosterForCurrentScoringPeriod {:entries []}}}
    {:id 8 :matchupPeriodId 2
     ;; In progress: the total that is published is the live one.
     :home {:teamId 11 :totalPoints 0.0 :totalPointsLive 51.04
            :pointsByScoringPeriod {:2 51.04}
            :rosterForCurrentScoringPeriod {:entries home-entries}}
     :away {:teamId 3 :totalPoints 0.0 :totalPointsLive 23.2
            :pointsByScoringPeriod {:2 23.2}
            :rosterForCurrentScoringPeriod {:entries away-entries}}}
    {:id 9 :matchupPeriodId 2
     :home {:teamId 7 :totalPoints 0.0 :totalPointsLive 31.0
            :pointsByScoringPeriod {:2 31.0}
            :rosterForCurrentScoringPeriod {:entries away-entries}}}]})

(defn- normalized [] (matchups/normalize-matchups :espn raw))

(deftest only-the-week-that-was-asked-for-is-a-game
  (let [{:keys [matchups scores]} (normalized)]
    (is (= [8 9] (mapv :matchup-id matchups))
        "the schedule carries the whole season; the filled roster is what dates a game")
    (is (= #{11 3 7} (set (keys scores)))
        "and every side of those games is scored, byed manager included")))

(deftest a-roster-with-no-opponent-keeps-its-entry
  (let [lone (last (:matchups (normalized)))]
    (is (= {:matchup-id 9 :roster-ids [7]} lone)
        "one id, not a pair of one — dropping it would read as a sync failure")))

(deftest a-side-the-provider-did-not-fill-has-no-lineup-rather-than-empty-seats
  (is (= [] (:starter-ids (second (m-espn/roster-score {:teamId 4} 2 (m-espn/seats raw)))))
      "a vector of empty seats is seq, and would defeat week-lineup's fallback to the roster"))

(deftest the-seats-the-lineup-was-aligned-to-travel-with-it
  (is (= (m-espn/seats raw) (:slots (normalized)))
      "a board drawing this lineup against another order mislabels every seat past the disagreement"))

(deftest the-lineup-is-seated-against-the-leagues-own-seats
  (is (= ["QB" "RB" "RB" "WR" "WR" "TE" "FLEX" "K" "DST"] (m-espn/seats raw))
      "the same vector routes/matchup-slots builds from the sync, bench and IR dropped")
  (is (= ["4040715" "4362238" "0" "4430878" "4241478" "4361307" "4695883" "2473037" "PIT"]
         (get-in (normalized) [:scores 11 :starter-ids]))
      "one RB seat went unfilled, and the seats below it stay where they are"))

(deftest a-defense-is-its-team-not-its-espn-id
  (is (= "WAS" (nth (get-in (normalized) [:scores 3 :starter-ids]) 8))
      "keyed by abbreviation in the app's spelling — ESPN's own WSH is an alias")
  (is (contains? (get-in (normalized) [:scores 11 :player-points]) "PIT")
      "a defense keyed by its ESPN id resolves to nobody and goes blank"))

(deftest player-points-cover-the-whole-roster-with-string-ids
  (let [pp (get-in (normalized) [:scores 11 :player-points])]
    (is (= 9 (count pp)) "the bench too, or the optimal lineup cannot be scored")
    (is (not (contains? (m-espn/player-points [(entry 2 999 2 nil)]) "999"))
        "a player ESPN gives no number for reads as unknown, not as a zero")
    (is (every? string? (keys pp)) "the crosswalk db/provider->player-id builds is string-keyed")
    (is (= 22.5 (get pp "4385690"))
        "read off the playerPoolEntry; the roster entry's own total is nil")))

(deftest a-side-scores-whether-it-is-playing-or-long-finished
  (testing "in progress, where totalPoints is still 0.0"
    (is (= 51.04 (get-in (normalized) [:scores 11 :official]))))
  (testing "over, where there is no live total left"
    (is (= 105.64 (m-espn/official {:totalPoints 105.64 :pointsByScoringPeriod {:1 105.64}} 1))))
  (testing "a week the side has no figure for is not invented"
    (is (nil? (m-espn/official {:pointsByScoringPeriod {:1 105.64}} 2)))))

(deftest the-current-week-is-the-nfl-week
  (with-redefs [import-espn/get-json (fn [_] {:scoringPeriodId 2
                                              :status {:currentMatchupPeriod 1
                                                       :latestScoringPeriod 2}})]
    (is (= 2 (matchups/current-week :espn {}))
        "not currentMatchupPeriod, which a playoff period can stretch over two weeks"))
  (with-redefs [import-espn/get-json (fn [_] {:scoringPeriodId 0 :status {}})]
    (is (nil? (matchups/current-week :espn {}))
        "no week is 'nothing to show', never week zero")))

(deftest a-failed-fetch-keeps-its-status
  (with-redefs [import-espn/get-json (fn [_] (throw (ex-info "ESPN league not found." {:status 404})))]
    (let [{:keys [ok status]} (matchups/fetch-matchups
                               {:provider "espn" :league-id "329525708" :season "2026"
                                :credentials {:swid "{6D1F620F-4B22-468D-9FDF-E50CBA7485D6}"
                                              :espn-s2 (apply str (repeat 70 "x"))}})]
      (is (false? ok))
      (is (= 404 status) "a 404 reported as a 502 sends a manager to the wrong fix"))))
