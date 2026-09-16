(ns draft-day.ingestion.league-sync.espn-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.ingestion.league-sync :as league-sync]
            [draft-day.ingestion.league-sync.espn :as espn]
            [draft-day.ingestion.teams :as teams]))

(defn- entry
  ([pid slot] (entry pid slot 1))
  ([pid slot pos-id]
   {:playerId pid :lineupSlotId slot
    :playerPoolEntry {:player {:defaultPositionId pos-id :proTeamId 22}}}))

(def ^:private raw
  {:id 12345
   :seasonId 2026
   :settings {:name "The Big Show"
              :size 2
              :rosterSettings {:lineupSlotCounts {:0 1 :2 1 :16 1 :20 2 :21 1}}
              :acquisitionSettings {:isUsingAcquisitionBudget true
                                    :acquisitionBudget 100}}
   :teams [{:id 1 :name "Kansas Screamers" :owners ["{OWNER-1}"]
            :waiverRank 4
            :record {:overall {:wins 5 :losses 3}}
            :transactionCounter {:acquisitionBudgetSpent 30}
            :roster {:entries [(entry 4034 0)
                               (entry 6794 2)
                               ;; a defense: keyed by team, never by ESPN id
                               (entry -16022 16 espn/dst-position-id)
                               (entry 9001 espn/bench-slot)
                               (entry 9002 21)]}}
           ;; Location + nickname, the way a long-running league still sends it.
           {:id 2 :location "Sunset" :nickname "Bandits" :primaryOwner "{OWNER-2}"
            :record {:overall {:wins 8 :losses 0}}
            :roster {:entries [(entry 1234 0)]}}]})

(defn- sync-of [r] (league-sync/normalize-rosters :espn r))
(defn- mine [] (first (:teams (sync-of raw))))

(deftest every-pro-team-lands-on-a-team-the-board-knows
  ;; Both directions: a leftover misspelling and a team ESPN stopped publishing
  ;; are different bugs, and a defense that resolves to nothing sits on the
  ;; free-agent board while its owner holds it.
  (is (= 32 (count espn/pro-team-abbrev)))
  (is (teams/covers-vocabulary? :espn (vals espn/pro-team-abbrev))))

(deftest a-rostered-defense-is-keyed-by-its-team-not-by-its-espn-id
  ;; The id file `player-ids/attach-ids` reads is a player file with no defense
  ;; rows, so a defense keyed by its ESPN id resolves to nobody.
  (is (some #{"ARI"} (:player-ids (mine))))
  (is (not-any? #{"-16022"} (:player-ids (mine))))
  (testing "and ESPN's own spelling is normalized on the way"
    (is (= "WAS" (espn/entry-player-id
                  {:playerId -16028
                   :playerPoolEntry {:player {:defaultPositionId espn/dst-position-id
                                              :proTeamId 28}}})))))

(deftest roster-ids-are-strings-because-the-crosswalk-is-string-keyed
  (is (every? string? (:player-ids (mine))))
  (is (some #{"4034"} (:player-ids (mine)))))

(deftest injured-reserve-holds-a-player-without-holding-a-seat
  ;; He is rostered, so he is not a free agent — but he frees no seat a claim
  ;; could take, and counting him fills a roster that is not actually full.
  (let [t (mine)]
    (is (some #{"9002"} (:player-ids t)))
    (is (not-any? #{"9002"} (:active-ids t)))
    (is (not-any? #{"9002"} (:starter-ids t)))))

(deftest a-starter-is-named-by-his-seat-not-by-his-position-in-a-list
  (let [t (mine)]
    (is (= #{"4034" "6794" "ARI"} (set (:starter-ids t))))
    (is (not-any? #{"9001"} (:starter-ids t)) "the bench does not start")
    (is (some #{"9001"} (:active-ids t)) "but it does hold a seat")))

(deftest faab-left-is-derived-where-both-halves-are-in-hand
  (is (= 30 (:faab-used (mine))))
  (is (= 70 (:faab-left (mine))))
  (testing "and is nil outside FAAB — nothing left and nothing to have differ"
    (let [t (first (:teams (sync-of (assoc-in raw [:settings :acquisitionSettings]
                                              {:isUsingAcquisitionBudget false}))))]
      (is (nil? (:faab-left t))))))

(deftest a-team-is-named-whichever-way-espn-sends-it
  (is (= "Kansas Screamers" (:name (mine))))
  (is (= "Sunset Bandits" (:name (second (:teams (sync-of raw))))))
  (is (= "Team 9" (espn/team-name {:id 9}))))

(deftest the-owner-keeps-the-braces-that-match-him-to-his-team
  (is (= "{OWNER-1}" (:owner-id (mine))))
  (is (= "{OWNER-2}" (:owner-id (second (:teams (sync-of raw)))))))

(deftest the-sync-carries-the-leagues-own-seats-and-their-count
  (let [s (sync-of raw)]
    (is (= ["QB" "RB" "DST" "BENCH" "BENCH" "IR"] (:roster-positions s)))
    (is (= 6 (:roster-size s)))
    (is (= "12345" (:league-id s)) "so a re-sync is one click")
    (is (= "The Big Show" (:name s)))
    (is (= "2026" (:season s)))
    (is (nil? (:playoff-week-start s))
        "unread until ESPN's spelling is confirmed; a wrong week mis-sizes every bid")))

(deftest the-record-rides-along
  (is (= 5 (:wins (mine))))
  (is (= 3 (:losses (mine))))
  (is (= 4 (:waiver-position (mine)))))

;; ---- discovery ----

(deftest the-identity-is-the-credential-and-needs-no-fetch
  ;; ESPN's fan endpoint is the only undocumented thing here. Depending on it
  ;; for identity would mean a listing outage failed the whole connect, which
  ;; is the opposite of the fallback it is supposed to enable.
  (let [u (league-sync/find-user :espn {:credentials {:swid "ABC-123"}})]
    (is (= "{ABC-123}" (:user-id u)))
    (is (nil? (:display-name u))
        "the SWID is half the credential pair and must never reach the screen")))

(deftest a-fan-document-yields-the-football-leagues-and-nothing-else
  (let [doc {:preferences
             [{:metaData {:entry {:abbrev "FFL" :seasonId 2026
                                  :groups [{:groupId 12345 :groupName "The Big Show"}
                                           {:groupId 777 :groupName "Work League"}]}}}
              {:metaData {:entry {:abbrev "FBA" :seasonId 2026
                                  :groups [{:groupId 999 :groupName "Hoops"}]}}}
              {:metaData {}}]}]
    (is (= [{:league-id "12345" :name "The Big Show" :season "2026"}
            {:league-id "777" :name "Work League" :season "2026"}]
           (espn/league-entries doc)))))

(deftest a-fan-document-that-moved-finds-nothing-rather-than-throwing
  ;; An exception here would take the connect down with it; an empty list is
  ;; reported as a gap and the manager pastes a league id.
  (is (= [] (espn/league-entries {})))
  (is (= [] (espn/league-entries {:preferences [{:metaData {:entry {:abbrev "FFL"}}}]})))
  (is (= [] (espn/league-entries {:preferences "not a list"}))))
