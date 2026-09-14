(ns draft-day.ingestion.matchups-test
  "The provider seam for head-to-head, and the three traps in it: an
  `ExecutionException` swallowing a 404's status, a JSON mapper keywordizing
  player ids that are map keys, and a roster with nobody to play."
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.ingestion.matchups :as matchups]
            [draft-day.ingestion.matchups.sleeper :as m-sleeper]))

;; Shaped like the live `/league/{id}/matchups/{week}` payload, trimmed to what
;; the parser reads. Rosters 1 and 2 play each other; roster 3 has no opponent,
;; which a real odd-sized league produces every week.
(def ^:private raw
  [{:matchup_id 1 :roster_id 1 :points 96.1
    :starters ["4034" "6794" "0"]
    :players  ["4034" "6794" "9001"]
    :players_points {:4034 18.4 :6794 12.0 :9001 22.6}}
   {:matchup_id 1 :roster_id 2 :points 104.4
    :starters ["1234" "SF"]
    :players  ["1234" "SF"]
    :players_points {:1234 31.2 :SF 9.0}}
   {:matchup_id nil :roster_id 3 :points 0.0
    :starters ["7777"]
    :players  ["7777"]
    :players_points {:7777 0.0}}])

(defn- normalized [] (matchups/normalize-matchups :sleeper raw))

;; ---- pairing ----

(deftest the-pairing-is-read-off-the-matchup-id
  (let [{:keys [matchups]} (normalized)]
    (is (= {:matchup-id 1 :roster-ids [1 2]} (first matchups)))))

(deftest a-roster-with-no-opponent-keeps-its-entry
  ;; An odd league, or a provider giving a team a bye. A team with nobody to
  ;; play still has a lineup and still scores, and dropping it would make the
  ;; one manager it happens to look like a sync failure.
  (let [{:keys [matchups]} (normalized)
        lone (first (filter #(nil? (:matchup-id %)) matchups))]
    (is (some? lone) "the byed roster is present, not filtered out")
    (is (= [3] (:roster-ids lone)) "one id, not a pair of one")
    (is (= 2 (count matchups)) "one game plus one bye")))

(deftest the-games-are-ordered-by-matchup-id
  ;; So the board's order does not depend on the order the provider happened to
  ;; serialize its rosters in.
  (let [shuffled [{:matchup_id 3 :roster_id 5 :starters [] :players_points {}}
                  {:matchup_id 1 :roster_id 1 :starters [] :players_points {}}
                  {:matchup_id 3 :roster_id 6 :starters [] :players_points {}}
                  {:matchup_id 1 :roster_id 2 :starters [] :players_points {}}]]
    (is (= [1 3] (mapv :matchup-id (m-sleeper/matchup-pairs shuffled))))))

;; ---- scores ----

(deftest player-points-come-back-with-string-ids
  ;; THE TRAP. `draft-day.json/mapper` keywordizes every key it decodes, and
  ;; these keys are player ids — so `{"4034": 18.4}` arrives as `{:4034 18.4}`
  ;; while the crosswalk and every roster list hold strings. Left alone, the
  ;; join misses every player and reads like a vendor publishing nothing.
  (let [pp (get-in (normalized) [:scores 1 :player-points])]
    (is (= {"4034" 18.4 "6794" 12.0 "9001" 22.6} pp))
    (is (every? string? (keys pp)))))

(deftest a-team-defense-survives-the-same-round-trip
  ;; Its id is the team abbreviation in both id spaces, so a keyword that failed
  ;; to convert back would be the one row nothing else could catch.
  (is (= {"1234" 31.2 "SF" 9.0} (get-in (normalized) [:scores 2 :player-points]))))

(deftest the-lineup-comes-off-the-matchup-not-the-roster
  ;; They agree right up until somebody edits his lineup after the games lock,
  ;; which is exactly when a matchup board must not follow him.
  (is (= ["4034" "6794" "0"] (get-in (normalized) [:scores 1 :starter-ids]))
      "including the placeholder for an unfilled seat, which is not a player"))

(deftest the-providers-own-team-total-is-carried
  (is (= 96.1 (get-in (normalized) [:scores 1 :official])))
  (is (= 104.4 (get-in (normalized) [:scores 2 :official]))))

(deftest points-cover-the-whole-roster-not-only-the-starters
  ;; The optimal-lineup half of the board exists to say what a bench player
  ;; would have scored, and it cannot say it from the starters alone.
  (is (contains? (get-in (normalized) [:scores 1 :player-points]) "9001")
      "9001 is on the roster and not in the lineup"))

(deftest a-roster-that-has-scored-nothing-is-still-zero-not-missing
  ;; Deciding a 0.0 means "has not played" is the board's judgment, made against
  ;; the kickoff clock. A provider pre-empting it here would take away the one
  ;; signal that tells a pre-kickoff zero from a real one.
  (is (= {"7777" 0.0} (get-in (normalized) [:scores 3 :player-points]))))

(deftest an-entry-with-no-points-map-yields-an-empty-one
  (is (= {} (m-sleeper/player-points {:roster_id 9}))))

;; ---- the envelope ----

(deftest fetch-matchups-returns-the-same-envelope-the-other-pairs-do
  (with-redefs [matchups/current-week        (fn [_] 3)
                matchups/fetch-raw-matchups  (fn [_ _ _] raw)]
    (let [{:keys [ok week matchups scores]} (matchups/fetch-matchups
                                             {:provider "sleeper" :league-id "1"})]
      (is ok)
      (is (= 3 week) "asked of the provider, not derived from :through-week")
      (is (= 2 (count matchups)))
      (is (= #{1 2 3} (set (keys scores)))))))

(deftest an-explicit-week-skips-asking-the-provider
  (let [asked (atom false)]
    (with-redefs [matchups/current-week       (fn [_] (reset! asked true) 3)
                  matchups/fetch-raw-matchups (fn [_ _ wk] (is (= 7 wk)) raw)]
      (is (= 7 (:week (matchups/fetch-matchups {:provider :sleeper :league-id "1" :week 7}))))
      (is (not @asked) "there is nothing to overlap, and nothing to ask"))))

(deftest a-concurrent-fetch-does-not-cost-an-error-its-status
  ;; The same trap `league-sync` memorialises. A provider is free to fetch its
  ;; documents together, and a future's deref wraps whatever the thunk threw in
  ;; an ExecutionException carrying no ex-data — so without unwrapping, every
  ;; unknown league reports as a 502 upstream failure instead of a 404.
  (with-redefs [matchups/current-week (fn [_] 3)
                matchups/fetch-raw-matchups
                (fn [_ _ _] (throw (java.util.concurrent.ExecutionException.
                                    (ex-info "not found" {:status 404}))))]
    (let [{:keys [ok status error]} (matchups/fetch-matchups
                                     {:provider :sleeper :league-id "9"})]
      (is (not ok))
      (is (= 404 status))
      (is (= "not found" error) "the cause's message, not the wrapper's"))))

(deftest a-bare-failure-is-a-502
  (with-redefs [matchups/current-week (fn [_] 3)
                matchups/fetch-raw-matchups (fn [_ _ _] (throw (ex-info "boom" {})))]
    (is (= 502 (:status (matchups/fetch-matchups {:provider :sleeper :league-id "1"}))))))

(deftest a-provider-between-seasons-has-no-week-to-show
  ;; Not week zero, and not a crash: there is simply no matchup to draw.
  (with-redefs [matchups/current-week (fn [_] nil)]
    (let [{:keys [ok status]} (matchups/fetch-matchups {:provider :sleeper :league-id "1"})]
      (is (not ok))
      (is (= 404 status)))))

(deftest an-unknown-provider-is-a-400-on-either-multimethod
  (testing "asking what week it is"
    (is (= 400 (:status (matchups/fetch-matchups {:provider :yahoo :league-id "1"})))))
  (testing "asking for the games"
    (is (= 400 (:status (matchups/fetch-matchups {:provider :yahoo :league-id "1" :week 3}))))))

(deftest a-week-with-no-games-is-an-empty-board-not-an-error
  ;; Sleeper answers a *valid* league asked for a week outside its season with
  ;; `[]`, which is why the fetch does not treat empty as missing. Reporting a
  ;; 404 there would be "league not found" on the one league we know exists.
  (with-redefs [matchups/current-week (fn [_] 25)
                matchups/fetch-raw-matchups (fn [_ _ _] [])]
    (let [{:keys [ok matchups scores]} (matchups/fetch-matchups
                                        {:provider :sleeper :league-id "1"})]
      (is ok)
      (is (= [] matchups))
      (is (= {} scores)))))
