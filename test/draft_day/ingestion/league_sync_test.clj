(ns draft-day.ingestion.league-sync-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.ingestion.league-sync :as league-sync]
            [draft-day.ingestion.league-sync.sleeper :as sync-sleeper]
            [draft-day.ingestion.league-import.sleeper :as import-sleeper]))

(def ^:private raw
  {:rosters [{:roster_id 1 :owner_id "u1"
              :players ["4034" "6794" "SF"] :starters ["4034" "6794"]
              :settings {:waiver_budget_used 30 :waiver_position 4 :wins 5 :losses 3}}
             {:roster_id 2 :owner_id "u2"
              :players ["1234"] :starters nil
              :settings {:waiver_budget_used 0 :waiver_position 1 :wins 8 :losses 0}}
             ;; An orphan: nobody owns it, and Sleeper sends null for its players.
             {:roster_id 3 :owner_id nil :players nil :starters nil
              :settings {:waiver_budget_used 12}}]
   :users   [{:user_id "u1" :display_name "jay" :metadata {:team_name "Kansas Screamers"}}
             {:user_id "u2" :display_name "dana" :metadata {}}]
   :league  {:name "The League" :season "2026"
             :settings {:waiver_type 2 :waiver_budget 100}}})

(defn- sync-of [r] (league-sync/normalize-rosters :sleeper r))

;; ---- waiver settings ----

(deftest waiver-type-is-read-from-the-league-not-guessed
  (is (= {:type :faab :budget 100}
         (import-sleeper/waiver-settings {:settings {:waiver_type 2 :waiver_budget 100}})))
  (is (= :rolling (:type (import-sleeper/waiver-settings {:settings {:waiver_type 0}}))))
  (is (= :reverse-standings
         (:type (import-sleeper/waiver-settings {:settings {:waiver_type 1}})))))

(deftest an-unknown-waiver-type-reads-as-not-faab
  ;; The chosen direction. Suppressing a bid in a FAAB league costs a column;
  ;; inventing a dollar figure for a league that bids nothing puts a confident
  ;; number on a transaction that does not exist.
  (is (= :rolling (:type (import-sleeper/waiver-settings {:settings {:waiver_type 99}}))))
  (is (= :rolling (:type (import-sleeper/waiver-settings {:settings {}}))))
  (is (= :rolling (:type (import-sleeper/waiver-settings {}))))
  (testing "an absent budget is 0, not nil — no share rule divides by nobody's number"
    (is (= 0 (:budget (import-sleeper/waiver-settings {:settings {:waiver_type 0}}))))))

(deftest the-import-carries-the-in-season-rules-too
  (let [cfg (import-sleeper/waiver-settings (:league raw))]
    (is (= {:type :faab :budget 100} cfg))))

;; ---- roster normalization ----

(deftest a-roster-carries-who-holds-whom-and-what-is-left-to-bid
  (let [{:keys [teams waiver]} (sync-of raw)
        [t1 t2 t3] teams]
    (is (= {:type :faab :budget 100} waiver))
    (is (= 3 (count teams)))
    (is (= ["4034" "6794" "SF"] (:player-ids t1)))
    (is (= ["4034" "6794"] (:starter-ids t1)))
    (is (= 30 (:faab-used t1)))
    (is (= 70 (:faab-left t1)) "budget minus spend, derived once")
    (is (= 100 (:faab-left t2)))
    (is (= 88 (:faab-left t3)))))

(deftest a-managers-own-team-name-wins-over-his-account-name
  ;; It is what everyone in the league calls his team.
  (let [[t1 t2] (:teams (sync-of raw))]
    (is (= "Kansas Screamers" (:name t1)))
    (is (= "dana" (:name t2)) "no team name set falls back to the display name")))

(deftest an-orphan-roster-keeps-its-seat
  ;; It still holds players, and its budget can still outbid yours.
  (let [t3 (nth (:teams (sync-of raw)) 2)]
    (is (= "Roster 3" (:name t3)))
    (is (= [] (:player-ids t3)) "Sleeper's null normalized to empty, once, here")
    (is (= [] (:starter-ids t3)))
    (is (= 88 (:faab-left t3)))))

(deftest a-non-faab-league-has-no-budget-left-to-report
  ;; nil, not 0: "he has nothing left" and "there is nothing to have" are
  ;; different claims, and only one of them should reach a bid column.
  (let [rolling (assoc-in raw [:league :settings] {:waiver_type 0})
        teams   (:teams (sync-of rolling))]
    (is (every? #(nil? (:faab-left %)) teams))
    (is (= 30 (:faab-used (first teams))) "what was spent is still a fact")
    (is (= [4 1] (keep :waiver-position teams))
        "waiver order is what this league actually runs on")))

(deftest an-overspent-budget-does-not-go-negative
  (let [over (assoc-in raw [:rosters 0 :settings :waiver_budget_used] 140)]
    (is (= 0 (:faab-left (first (:teams (sync-of over))))))))

(deftest the-league-name-and-season-ride-along
  (let [s (sync-of raw)]
    (is (= "The League" (:name s)))
    (is (= "2026" (:season s)))))

;; ---- the envelope and its failure paths ----

(deftest sync-league-returns-the-same-envelope-as-an-import
  (with-redefs [league-sync/fetch-raw-rosters (fn [_ _] raw)]
    (let [{:keys [ok league]} (league-sync/sync-league {:provider "sleeper" :league-id "1"})]
      (is ok)
      (is (= 3 (count (:teams league)))))))

(deftest an-unknown-provider-is-a-400
  (let [{:keys [ok status error]} (league-sync/sync-league {:provider "yahoo" :league-id "1"})]
    (is (not ok))
    (is (= 400 status))
    (is (= "Unknown league provider" error))))

(deftest a-status-carrying-failure-keeps-its-status
  (with-redefs [league-sync/fetch-raw-rosters
                (fn [_ _] (throw (ex-info "not found" {:status 404})))]
    (is (= 404 (:status (league-sync/sync-league {:provider :sleeper :league-id "9"})))))
  (with-redefs [league-sync/fetch-raw-rosters
                (fn [_ _] (throw (ex-info "down" {:status 502})))]
    (is (= 502 (:status (league-sync/sync-league {:provider :sleeper :league-id "1"}))))))

(deftest a-concurrent-fetch-does-not-cost-an-error-its-status
  ;; The three documents are fetched together, and a future's deref wraps
  ;; whatever the thunk threw in an ExecutionException — so without unwrapping,
  ;; every unknown league id reports as a 502 upstream failure instead of a 404.
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo #"not found"
       (sync-sleeper/unwrapped
        #(throw (java.util.concurrent.ExecutionException.
                 (ex-info "not found" {:status 404}))))))
  (is (= 404 (-> (try (sync-sleeper/unwrapped
                       #(throw (java.util.concurrent.ExecutionException.
                                (ex-info "not found" {:status 404}))))
                      (catch clojure.lang.ExceptionInfo e e))
                 ex-data :status))))
