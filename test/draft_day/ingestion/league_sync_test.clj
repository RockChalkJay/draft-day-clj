(ns draft-day.ingestion.league-sync-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.ingestion.league-import :as league-import]
            [draft-day.ingestion.league-sync :as league-sync]
            [draft-day.ingestion.league-sync.sleeper :as sync-sleeper]
            [draft-day.ingestion.league-import.sleeper :as import-sleeper]))

(def ^:private raw
  {:rosters [{:roster_id 1 :owner_id "u1"
              :players ["4034" "6794" "SF" "9001" "9002"] :starters ["4034" "6794"]
              :reserve ["9001"] :taxi ["9002"]
              :settings {:waiver_budget_used 30 :waiver_position 4 :wins 5 :losses 3}}
             {:roster_id 2 :owner_id "u2"
              :players ["1234"] :starters nil
              :settings {:waiver_budget_used 0 :waiver_position 1 :wins 8 :losses 0}}
             ;; An orphan: nobody owns it, and Sleeper sends null for its players.
             {:roster_id 3 :owner_id nil :players nil :starters nil
              :settings {:waiver_budget_used 12}}]
   :users   [{:user_id "u1" :display_name "jay" :metadata {:team_name "Kansas Screamers"}}
             {:user_id "u2" :display_name "dana" :metadata {}}]
   :league  {:name "The League" :season "2026" :league_id 987654
             :roster_positions ["QB" "RB" "RB" "WR" "WR" "TE" "FLEX" "K" "DEF"
                                "BN" "BN" "BN" "BN" "BN" "BN"]
             :settings {:waiver_type 2 :waiver_budget 100 :playoff_week_start 15}}})

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

(deftest the-waiver-rules-are-read-by-the-sync-not-returned-by-the-import
  ;; They were on `normalize-league` too, which only looked tidy: the client
  ;; select-keys them away on arrival and `reconcile-config` would strip them
  ;; regardless, so it was two keys nobody read and a second place to drift.
  (is (= {:type :faab :budget 100} (import-sleeper/waiver-settings (:league raw))))
  (is (= 15 (import-sleeper/playoff-week-start (:league raw))))
  (let [cfg (league-import/normalize-league :sleeper (:league raw))]
    (is (not (contains? cfg :waiver)))
    (is (not (contains? cfg :playoff-week-start)))))

;; ---- roster normalization ----

(deftest a-roster-carries-who-holds-whom-and-what-is-left-to-bid
  (let [{:keys [teams waiver]} (sync-of raw)
        [t1 t2 t3] teams]
    (is (= {:type :faab :budget 100} waiver))
    (is (= 3 (count teams)))
    (is (= ["4034" "6794" "SF" "9001" "9002"] (:player-ids t1))
        "everyone rostered, IR and taxi included — none of them is a free agent")
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

(deftest ir-and-taxi-hold-no-active-seat
  ;; They matter in opposite directions, which is why they are split out rather
  ;; than filtered at the point of use: counted toward the roster they fill a
  ;; team that is not actually full, and offered as a drop they free no seat for
  ;; the claim being priced.
  (let [[t1 t2] (:teams (sync-of raw))]
    (is (= ["4034" "6794" "SF"] (:active-ids t1)))
    (is (= 5 (count (:player-ids t1))) "still rostered, still unavailable")
    (is (= ["1234"] (:active-ids t2)) "a roster with neither list is all active")))

(deftest the-leagues-own-seat-count-comes-back-with-it
  ;; Whether a claim costs a drop turns on this number, and the browser's
  ;; fallback is the draft config — which a manager who synced without importing
  ;; has never set to match this league.
  (is (= 15 (:roster-size (sync-of raw)))))

(deftest the-league-id-rides-back-so-a-re-sync-is-one-click
  ;; Without it the id lives only in a component-local atom that empties on
  ;; reload, and a manager returns to persisted, month-old rosters with the
  ;; re-sync button greyed out.
  (is (= "987654" (:league-id (sync-of raw)))))

(deftest the-league-name-and-season-ride-along
  (let [s (sync-of raw)]
    (is (= "The League" (:name s)))
    (is (= "2026" (:season s)))
    (is (= 15 (:playoff-week-start s)) "what bounds how many waiver runs are left")))

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
  ;; A provider fetches its documents together, and a future's deref wraps
  ;; whatever the thunk threw in an ExecutionException carrying no ex-data of
  ;; its own — so without unwrapping, every unknown league id reports as a 502
  ;; upstream failure instead of a 404.
  ;;
  ;; Exercised through `sync-league` rather than against the helper directly,
  ;; because the contract at stake is the envelope's status: a provider that
  ;; fetches concurrently must not have to remember to rescue its own 404.
  (with-redefs [league-sync/fetch-raw-rosters
                (fn [_ _] (throw (java.util.concurrent.ExecutionException.
                                  (ex-info "not found" {:status 404}))))]
    (let [{:keys [ok status error]} (league-sync/sync-league {:provider :sleeper :league-id "9"})]
      (is (not ok))
      (is (= 404 status))
      (is (= "not found" error) "the cause's message, not the wrapper's")))
  ;; A wrapper with no cause has nothing to peel, and must degrade to a 502
  ;; rather than dereferencing nil.
  (with-redefs [league-sync/fetch-raw-rosters
                (fn [_ _] (throw (java.util.concurrent.ExecutionException. "boom" nil)))]
    (is (= 502 (:status (league-sync/sync-league {:provider :sleeper :league-id "1"}))))))

(deftest unwrap-execution-peels-only-the-wrapper
  (let [inner (ex-info "not found" {:status 404})]
    (is (identical? inner (league-sync/unwrap-execution
                           (java.util.concurrent.ExecutionException. inner)))
        "an ExecutionException gives up its cause")
    (is (identical? inner (league-sync/unwrap-execution inner))
        "anything else passes through untouched")))

;; ---- connecting an account ----

(def ^:private raw-user
  ;; Shape confirmed against the live API. The nulls are Sleeper's, and three of
  ;; these keys are the reason `normalize-user` builds rather than passes through.
  {:user_id "993960010998722560" :username "rockchalkjay"
   :display_name "rockchalkjay" :avatar "fb846befd76ce7953cb9b21093d2697b"
   :email nil :phone nil :token nil :is_bot false :real_name nil})

(deftest a-user-is-narrowed-to-the-three-fields-the-app-needs
  ;; The raw document carries email, phone and a token. `/api/league/user`
  ;; returns whatever this hands back, so it is the only thing between them and
  ;; the browser.
  (let [u (sync-sleeper/normalize-user raw-user)]
    (is (= {:user-id "993960010998722560"
            :display-name "rockchalkjay"
            :avatar "fb846befd76ce7953cb9b21093d2697b"}
           u))
    (is (not-any? #{:email :phone :token :is_bot :real_name} (keys u))
        "nothing from the raw document rides along"))
  (testing "a blank display name falls back to the username"
    (is (= "handle" (:display-name (sync-sleeper/normalize-user
                                    {:user_id "1" :username "handle" :display_name ""}))))))

(deftest a-league-entry-carries-what-a-picker-shows
  (let [l (sync-sleeper/normalize-league-entry
           {:league_id 1380540443179118592 :name "RaiderNation" :season "2026"
            :total_rosters 12 :status "in_season" :avatar "abc" :draft_id "x"})]
    (is (= "1380540443179118592" (:league-id l)) "as a string, not a lossy number")
    (is (= {:name "RaiderNation" :season "2026" :num-teams 12 :status "in_season"}
           (select-keys l [:name :season :num-teams :status])))))

(deftest an-account-with-no-leagues-is-an-answer-not-a-missing-account
  ;; The trap, pinned. Sleeper answers an unknown *user* with 200 and `null`, and
  ;; a user who plays in nothing this season with 200 and `[]`. `empty?` cannot
  ;; tell them apart, so treating both as missing would tell a manager his
  ;; account does not exist because he took a year off.
  (with-redefs [league-sync/find-user    (fn [_ _] {:user-id "u1" :display-name "n"})
                league-sync/list-leagues (fn [_ _ _] [])]
    (let [{:keys [ok leagues user]} (league-sync/find-leagues
                                     {:provider :sleeper :username "n"})]
      (is ok "no leagues is a success")
      (is (= [] leagues))
      (is (= "u1" (:user-id user))))))

(deftest an-unknown-username-keeps-its-404
  (with-redefs [league-sync/find-user
                (fn [_ _] (throw (ex-info "Sleeper user not found" {:status 404})))]
    (let [{:keys [ok status error]} (league-sync/find-leagues
                                     {:provider :sleeper :username "nope"})]
      (is (not ok))
      (is (= 404 status))
      (is (= "Sleeper user not found" error)))))

(deftest an-unknown-provider-cannot-look-up-an-account
  (let [{:keys [ok status]} (league-sync/find-leagues
                             {:provider :yahoo :username "someone"})]
    (is (not ok))
    (is (= 400 status))))
