(ns draft-day.ingestion.league-sync-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.db :as db]
            [draft-day.ingestion.league-import :as league-import]
            [draft-day.ingestion.league-sync :as league-sync]
            [draft-day.ingestion.league-sync.sleeper :as sync-sleeper]
            [draft-day.ingestion.league-import.sleeper :as import-sleeper]))

(def ^:private raw
  {:rosters [{:roster_id 1 :owner_id "u1"
              :players ["4034" "6794" "SF" "9001" "9002"] :starters ["4034" "6794"]
              :reserve ["9001"] :taxi ["9002"]
              :settings {:waiver_budget_used 30 :waiver_position 4 :wins 5 :losses 3
                         :ties 1 :fpts 1043 :fpts_decimal 56}}
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
  (is (= {:type :faab :budget 100 :min-bid 0}
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

(deftest the-commissioners-minimum-bid-is-read-from-the-league
  (is (= 1 (:min-bid (import-sleeper/waiver-settings
                      {:settings {:waiver_type 2 :waiver_budget 100 :waiver_bid_min 1}})))
      "a bid below it is not one the host accepts")
  (is (= 0 (:min-bid (import-sleeper/waiver-settings {:settings {:waiver_type 2}})))
      "Sleeper's default is $0"))

(deftest the-waiver-rules-are-read-by-the-sync-not-returned-by-the-import
  ;; They were on `normalize-league` too, which only looked tidy: the client
  ;; select-keys them away on arrival, so it was two keys nobody read and a
  ;; second place to drift.
  (is (= {:type :faab :budget 100 :min-bid 0} (import-sleeper/waiver-settings (:league raw))))
  (is (= 15 (import-sleeper/playoff-week-start (:league raw))))
  (let [cfg (league-import/normalize-league :sleeper (:league raw))]
    (is (not (contains? cfg :waiver)))
    (is (not (contains? cfg :playoff-week-start)))))

;; ---- roster normalization ----

(deftest a-roster-carries-who-holds-whom-and-what-is-left-to-bid
  (let [{:keys [teams waiver]} (sync-of raw)
        [t1 t2 t3] teams]
    (is (= {:type :faab :budget 100 :min-bid 0} waiver))
    (is (= 3 (count teams)))
    (is (= ["4034" "6794" "SF" "9001" "9002"] (:player-ids t1))
        "everyone rostered, IR and taxi included — none of them is a free agent")
    (is (= ["4034" "6794"] (:starter-ids t1)))
    (is (= 30 (:faab-used t1)))
    (is (= 70 (:faab-left t1)) "budget minus spend, derived once")
    (is (= 100 (:faab-left t2)))
    (is (= 88 (:faab-left t3)))))

(deftest the-record-carries-what-standings-are-ordered-by
  (let [[t1 t2 t3] (:teams (sync-of raw))]
    (is (= [5 3 1] ((juxt :wins :losses :ties) t1)))
    (is (< (abs (- 1043.56 (:points-for t1))) 1e-9)
        "whole points and hundredths arrive as two fields")
    (is (nil? (:points-for t2)) "nothing scored yet is unknown, not zero")
    (is (nil? (:points-for t3)))))

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

(deftest the-sync-says-whether-the-league-has-drafted
  ;; `db/derived-phase` reads it: a league drafted on Sleeper is in season
  ;; before a week has been played.
  (is (true? (:drafted? (sync-of (assoc-in raw [:league :status] "in_season")))))
  (is (true? (:drafted? (sync-of (assoc-in raw [:league :status] "complete")))))
  (is (false? (:drafted? (sync-of (assoc-in raw [:league :status] "pre_draft")))))
  (is (false? (:drafted? (sync-of (assoc-in raw [:league :status] "drafting"))))
      "a live draft is not over, or a reload mid-auction lands in season")
  (is (nil? (:drafted? (sync-of raw))) "no status is the host saying nothing")
  (is (nil? (:drafted? (sync-of (assoc-in raw [:league :status] "archived"))))
      "and so is one this does not know"))

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
    (is (= "unknown provider: :yahoo" error))))

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
  {:user_id "123456789012345678" :username "test-manager"
   :display_name "test-manager" :avatar "0123456789abcdef0123456789abcdef"
   :email nil :phone nil :token nil :is_bot false :real_name nil})

(deftest a-user-is-narrowed-to-the-three-fields-the-app-needs
  ;; The raw document carries email, phone and a token. `/api/league/user`
  ;; returns whatever this hands back, so it is the only thing between them and
  ;; the browser.
  (let [u (sync-sleeper/normalize-user raw-user)]
    (is (= {:user-id "123456789012345678"
            :display-name "test-manager"
            :avatar "0123456789abcdef0123456789abcdef"}
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
                league-sync/list-leagues (fn [_ _] [])]
    (let [{:keys [ok leagues user]} (league-sync/find-leagues
                                     {:provider :sleeper :credentials {:username "n"}})]
      (is ok "no leagues is a success")
      (is (= [] leagues))
      (is (= "u1" (:user-id user))))))

(deftest an-unknown-username-keeps-its-404
  (with-redefs [league-sync/find-user
                (fn [_ _] (throw (ex-info "Sleeper user not found" {:status 404})))]
    (let [{:keys [ok status error]} (league-sync/find-leagues
                                     {:provider :sleeper :credentials {:username "nope"}})]
      (is (not ok))
      (is (= 404 status))
      (is (= "Sleeper user not found" error)))))

(deftest an-unknown-provider-cannot-look-up-an-account
  (let [{:keys [ok status]} (league-sync/find-leagues
                             {:provider :yahoo :credentials {:username "someone"}})]
    (is (not ok))
    (is (= 400 status))))

(deftest the-sync-carries-the-leagues-own-seats-in-order
  (let [{:keys [roster-positions roster-size]} (sync-of raw)]
    (is (= ["QB" "RB" "RB" "WR" "WR" "TE" "FLEX" "K" "DST"
            "BENCH" "BENCH" "BENCH" "BENCH" "BENCH" "BENCH"]
           roster-positions)
        "in order: Sleeper's `starters` array is positional against this list")
    (is (= 15 roster-size) "still the count, unchanged")
    (is (= roster-size (count roster-positions)))))

(deftest sleepers-seat-names-arrive-in-the-apps-vocabulary
  ;; DEF and BN are the two Sleeper spells differently; an IDP seat is carried
  ;; verbatim rather than dropped.
  (is (= ["DST" "BENCH" "FLEX" "SUPER_FLEX" "IR" "TAXI" "DL"]
         (sync-sleeper/normalize-positions
          ["DEF" "BN" "FLEX" "SUPER_FLEX" "IR" "TAXI" "DL"]))))

(deftest a-league-with-no-positions-yields-no-seats-rather-than-throwing
  (is (= [] (sync-sleeper/normalize-positions nil)))
  (is (= [] (sync-sleeper/normalize-positions []))))

;; ---- what the dispatcher promises on every provider's behalf ----

(deftest the-sync-passes-the-season-and-the-credentials-to-the-provider
  (let [seen (atom nil)]
    (with-redefs [league-sync/fetch-raw-rosters (fn [_ req] (reset! seen req) raw)]
      (league-sync/sync-league {:provider "sleeper" :league-id "1"
                                :season "2025" :credentials {:username "jay"}})
      (is (= "2025" (:season @seen))
          "a provider that never sees the season defaults to this one and imports
           last year's copy of the league, which looks like an empty league")
      (is (= {:username "jay"} (:credentials @seen))))))

(deftest a-season-the-caller-holds-wins-over-this-years-default
  (let [seen (atom nil)]
    (with-redefs [league-sync/fetch-raw-rosters (fn [_ req] (reset! seen req) raw)]
      (league-sync/sync-league {:provider "sleeper" :league-id "1" :season "2025"})
      (is (= "2025" (:season @seen)))
      (league-sync/sync-league {:provider "sleeper" :league-id "1"})
      (is (some? (:season @seen)) "and absent still resolves to something"))))

(deftest a-season-that-is-not-a-year-is-a-caller-with-no-opinion
  ;; It lands in a URL *path segment* on every host that puts the season in its
  ;; URL — ESPN's league document, Sleeper's league listing — so it gets the
  ;; same guard `providers/league-id-error` puts on an id, not none.
  (let [seen (atom nil)]
    (with-redefs [league-sync/fetch-raw-rosters (fn [_ req] (reset! seen req) raw)]
      (doseq [bad ["2026/../../../../evil" "20261" "" "  " "nope"]]
        (league-sync/sync-league {:provider "sleeper" :league-id "1" :season bad})
        (is (re-matches #"\d{4}" (str (:season @seen)))
            (str (pr-str bad) " reached a provider's URL unchanged"))))))

(deftest the-sync-names-the-provider-it-came-from
  (with-redefs [league-sync/fetch-raw-rosters (fn [_ _] raw)]
    (is (= :sleeper (:provider (:league (league-sync/sync-league
                                         {:provider "sleeper" :league-id "1"}))))
        "rankings.waiver picks its crosswalk off this; without it an ESPN league
         is read with the Sleeper id map and every player reads as free")))

(deftest roster-ids-come-back-as-strings-whatever-the-provider-published
  (with-redefs [league-sync/normalize-rosters
                (fn [_ _] {:teams [{:roster-id 1
                                    :player-ids [4034 nil 6794]
                                    :active-ids [4034]
                                    :starter-ids [4034]}]})
                league-sync/fetch-raw-rosters (fn [_ _] {})]
    (let [team (first (:teams (:league (league-sync/sync-league
                                        {:provider "sleeper" :league-id "1"}))))]
      (is (= ["4034" "6794"] (:player-ids team))
          "the crosswalk is string-keyed and held-ids maps a miss to itself, so an
           integer id resolves nothing and the whole league reads as free agents")
      (is (= ["4034"] (:active-ids team)))
      (is (= ["4034"] (:starter-ids team))))))

(deftest a-lineup-keeps-its-empty-seats-rather-than-closing-the-gap
  (with-redefs [league-sync/normalize-rosters
                (fn [_ _] {:teams [{:roster-id 1
                                    :player-ids [4034 6794]
                                    :starter-ids [4034 nil 6794]
                                    :starter-slots ["QB" "RB" "FLEX"]}]})
                league-sync/fetch-raw-rosters (fn [_ _] {})]
    (let [team (first (:teams (:league (league-sync/sync-league
                                        {:provider "sleeper" :league-id "1"}))))]
      (is (= ["4034" "0" "6794"] (:starter-ids team)))
      (is (= (count (:starter-slots team)) (count (:starter-ids team)))
          "the two vectors are pairs, and a repair may not desynchronize them")
      (is (= "FLEX" (get (db/starter-seats team {}) 2))
          "the seat is read at the starter's index, so a closed gap labels a
           FLEX receiver RB"))))

(deftest a-provider-that-returns-no-teams-is-not-a-league-everyone-has-left
  (with-redefs [league-sync/normalize-rosters (fn [_ _] {:waiver {:type :faab}})
                league-sync/fetch-raw-rosters (fn [_ _] {})]
    (is (nil? (:teams (:league (league-sync/sync-league
                                {:provider "sleeper" :league-id "1"}))))
        "an absent roster list must stay absent so reconcile-league-sync drops it")))

(deftest a-listing-that-broke-keeps-the-account-and-reports-the-gap
  (with-redefs [league-sync/find-user    (fn [_ _] {:user-id "u1" :display-name "n"})
                league-sync/list-leagues (fn [_ _] (throw (ex-info "upstream" {:status 502})))]
    (let [{:keys [ok user leagues leagues-error]}
          (league-sync/find-leagues {:provider :sleeper :credentials {:username "n"}})]
      (is ok "a host that knows who you are has not failed to connect")
      (is (= "u1" (:user-id user)))
      (is (= [] leagues))
      (is (= "upstream" leagues-error)
          "reported apart from [], or a discovery outage tells a manager he plays in nothing"))))

(deftest a-rejected-credential-during-listing-fails-the-connect
  (with-redefs [league-sync/find-user    (fn [_ _] {:user-id "u1"})
                league-sync/list-leagues (fn [_ _] (throw (ex-info "nope" {:status 401})))]
    (let [{:keys [ok status]} (league-sync/find-leagues
                               {:provider :sleeper :credentials {:username "n"}})]
      (is (not ok) "an expired cookie is not a discovery failure")
      (is (= 401 status)))))
