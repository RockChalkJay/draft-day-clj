(ns draft-day.sensei.context-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [draft-day.ingestion.league-import :as league-import]
            [draft-day.ingestion.league-sync :as league-sync]
            [draft-day.ingestion.season :as season]
            [draft-day.sensei.context :as context]
            [draft-day.sensei.fixtures :as fx]))

(use-fixtures :each (fn [t] (context/reset-caches!) (t) (context/reset-caches!)))

(def ^:private config
  {:scoring   {:rec 1.0 :rush_yd 0.1}
   :num-teams 10
   :roster    {:qb 1 :rb 2 :wr 2 :te 1 :flex 1 :k 1 :dst 1 :bench 5}})

(def ^:private synced
  {:provider :sleeper
   :teams    [{:roster-id 1 :owner-id "u-other" :name "Rival"}
              {:roster-id 2 :owner-id "U-ME" :name "Mine"}]
   :waiver   {:type :faab :budget 100}})

(def ^:private both
  {"DRAFTDAY_SENSEI_SLEEPER_USER" "jay"
   "DRAFTDAY_ESPN_SWID"           "{SWID-0000}"
   "DRAFTDAY_ESPN_S2"             "S2SECRET"})

;; ---- accounts ----

(deftest accounts-come-from-the-environment
  (is (= [] (context/accounts {})))
  (is (= [{:provider :sleeper :credentials {:username "jay"}}]
         (context/accounts {"DRAFTDAY_SENSEI_SLEEPER_USER" "jay"})))
  (is (= [{:provider :espn :credentials {:swid "{SWID-0000}" :espn-s2 "S2SECRET"}}]
         (context/accounts (dissoc both "DRAFTDAY_SENSEI_SLEEPER_USER"))))
  (is (= [:sleeper :espn] (map :provider (context/accounts both))) "in the order hosts are shown")
  (testing "half an ESPN pair is no account, and blank is unset"
    (is (empty? (context/accounts {"DRAFTDAY_ESPN_SWID" "{SWID-0000}"})))
    (is (empty? (context/accounts {"DRAFTDAY_SENSEI_SLEEPER_USER" ""})))))

(deftest a-secret-is-masked-in-each-spelling-an-error-echoes
  (let [swid "ABCDEF12-0000-0000-0000-000000000000"
        ss   (context/secrets-in :espn {:swid swid :espn-s2 "S2SECRET"})]
    (is (= "a •••• b •••• c ••••"
           (context/mask ss (str "a " swid " b {" swid "} c S2SECRET"))))
    (is (empty? (context/secrets-in :sleeper {:username "someone"})) "a username is public")
    (is (empty? (context/secrets-in :espn nil)))
    (is (= #{"S2SECRET" "{SWID-0000}"} (set (context/secrets (context/accounts both))))
        "every account's, Sleeper's public username excepted")
    (is (= #{"S2SECRET" "SWID-0000" "{SWID-0000}"}
           (set (context/secrets-in :espn {:swid "SWID-0000" :espn-s2 "S2SECRET"})))
        "pasted without its braces, both spellings")))

;; ---- discovery ----

(defn- fan [{:keys [provider]}]
  (case provider
    :sleeper {:ok true :user {:user-id "u-me"}
              :leagues [{:league-id "123" :name "Main League" :season "2026" :num-teams 12}
                        {:league-id "99" :name "Old League" :season "2025"}]}
    :espn    {:ok true :user {:user-id "{SWID-0000}"}
              :leagues [{:league-id "456" :name "Work League" :season "2026"}
                        {:league-id "777" :name "Undated League"}]}))

(deftest discovery-lists-every-accounts-leagues-for-this-season
  (let [seen (atom [])]
    (with-redefs [season/current (constantly 2026)
                  league-sync/find-leagues (fn [req] (swap! seen conj req) (fan req))]
      (let [{:keys [leagues errors]} (context/discover (context/accounts both))]
        (is (= ["sleeper:123" "espn:456" "espn:777"] (map :key leagues))
            "last season's league is dropped; one with no season is kept")
        (is (empty? errors))
        (is (= {:key "sleeper:123" :provider :sleeper :league-id "123" :name "Main League"
                :season "2026" :num-teams 12 :user-id "u-me" :credentials {:username "jay"}}
               (first leagues)))
        (is (= #{{:provider :sleeper :credentials {:username "jay"} :season 2026}
                 {:provider :espn :credentials {:swid "{SWID-0000}" :espn-s2 "S2SECRET"} :season 2026}}
               (set @seen)))))))

(deftest one-host-down-does-not-hide-the-other-and-says-so
  (with-redefs [season/current (constantly 2026)
                league-sync/find-leagues
                (fn [req] (if (= :espn (:provider req))
                            {:ok false :status 401 :error "rejected espn_s2=S2SECRET swid={SWID-0000}"}
                            (fan req)))]
    (let [{:keys [leagues errors]} (context/discover (context/accounts both))]
      (is (= ["sleeper:123"] (map :key leagues)))
      (is (= 1 (count errors)))
      (is (re-find #"^ESPN league lookup failed" (first errors)))
      (is (not (re-find #"S2SECRET|SWID-0000" (first errors))) "a tool result is read by a third party")))
  (testing "a host that knew the manager but could not list their leagues"
    (with-redefs [season/current (constantly 2026)
                  league-sync/find-leagues (fn [_] {:ok true :user {:user-id "u"} :leagues []
                                                    :leagues-error "ESPN would not list this account's leagues"})]
      (is (re-find #"would not list"
                   (first (:errors (context/discover (context/accounts {"DRAFTDAY_SENSEI_SLEEPER_USER" "jay"})))))))))

(deftest discovery-is-reused-until-it-fails-or-expires
  (let [runs  (atom 0)
        clock (atom 0)
        ok    {:leagues [fx/entry] :errors []}
        accts (context/accounts {"DRAFTDAY_SENSEI_SLEEPER_USER" "jay"})]
    (with-redefs [context/now-ms  (fn [] @clock)
                  context/discover (fn [_] (swap! runs inc) ok)]
      (context/leagues accts)
      (reset! clock (dec context/discovery-ttl-ms))
      (context/leagues accts)
      (is (= 1 @runs))
      (reset! clock context/discovery-ttl-ms)
      (context/leagues accts)
      (is (= 2 @runs) "expired"))
    (context/reset-caches!)
    (reset! runs 0)
    (with-redefs [context/discover (fn [_] (swap! runs inc) {:leagues [] :errors ["ESPN league lookup failed: 502"]})]
      (context/leagues accts)
      (context/leagues accts)
      (is (= 2 @runs) "a lookup that failed is tried again, not pinned for the TTL"))))

;; ---- which league ----

(def ^:private found {:leagues [fx/entry fx/entry-2] :errors []})

(deftest pick-resolves-the-league-a-call-names
  (is (= fx/entry-2 (context/pick found "espn:456")))
  (is (= fx/entry-2 (context/pick found "work league")) "a name, in any case")
  (is (= fx/entry-2 (context/pick found "  espn:456 ")))
  (is (re-find #"No league matches \"nope\".*sleeper:123 \(Main League\); espn:456 \(Work League\)"
               (:error (context/pick found "nope"))))
  (testing "two leagues of one name need the key"
    (let [twins {:leagues [fx/entry (assoc fx/entry-2 :name "Main League")] :errors []}]
      (is (:error (context/pick twins "main league")))
      (is (= "espn:456" (:key (context/pick twins "espn:456")))))))

(deftest pick-guesses-only-when-there-is-nothing-to-guess
  (is (= fx/entry (context/pick {:leagues [fx/entry] :errors []} nil)))
  (is (= fx/entry (context/pick {:leagues [fx/entry] :errors []} "  ")))
  (is (re-find #"league argument.*sleeper:123 \(Main League\); espn:456 \(Work League\)"
               (:error (context/pick found nil))))
  (testing "the only league there is is not known to be one when an account could not be listed"
    (is (re-find #"Not listed: boom" (:error (context/pick {:leagues [fx/entry] :errors ["boom"]} nil))))
    (is (= fx/entry (context/pick {:leagues [fx/entry] :errors ["boom"]} "sleeper:123"))))
  (testing "no leagues at all says how to get some"
    (is (re-find #"DRAFTDAY_SENSEI_SLEEPER_USER.*DRAFTDAY_ESPN_SWID"
                 (:error (context/pick {:leagues [] :errors []} nil))))
    (is (re-find #"boom" (:error (context/pick {:leagues [] :errors ["boom"]} "x"))))))

;; ---- the board's body ----

(deftest the-body-is-the-one-the-waivers-tab-would-send
  (let [b (context/waivers-body config synced "u-me")]
    (is (= 2 (:my-roster-id b)) "found by owner, whatever the case")
    (is (= 14 (:roster-size b)) "every seat, bench included")
    (is (= {:qb 1 :rb 2 :wr 2 :te 1 :flex 1} (:replacement-config b)))
    (is (= (:roster config) (:roster b)) "K and DST still fill starting seats")))

(deftest an-account-that-owns-no-team-gets-the-teams-there-are
  (let [{:keys [error]} (context/waivers-body config synced "stranger")]
    (is (re-find #"owns no team.*1 \(Rival\), 2 \(Mine\)" error))
    (is (not (re-find #"stranger" error)) "an ESPN user id is the SWID")))

(deftest the-context-reads-as-the-browser-posts-it
  (let [seen (atom [])]
    (with-redefs [league-import/import-league (fn [req] (swap! seen conj req) {:ok true :config config})
                  league-sync/sync-league     (fn [_] {:ok true :league synced})]
      (let [b (context/league-context fx/entry)]
        (is (= "sleeper" (get-in b [:league :provider])) "keywords cross JSON as strings")
        (is (= "faab" (get-in b [:league :waiver :type])))
        (is (= 2 (:my-roster-id b)))
        (is (= {:provider :sleeper :league-id "123" :season "2026" :credentials {:username "me"}}
               (first @seen))
            "asked with the account's own credentials")))))

(deftest a-failure-says-which-step-and-masks-the-cookie
  (with-redefs [league-import/import-league (fn [_] {:ok true :config config})
                league-sync/sync-league     (fn [_] {:ok false :status 401
                                                     :error "rejected espn_s2=S2SECRET swid={SWID-0000}"})]
    (let [{:keys [error]} (context/league-context fx/entry-2)]
      (is (re-find #"^sync failed" error))
      (is (not (re-find #"S2SECRET|SWID-0000" error)) "a tool result is read by a third party"))))

;; ---- reuse ----

(deftest parallel-calls-for-a-league-share-one-fetch-until-it-expires
  (let [fetches (atom 0)
        clock   (atom 0)]
    (with-redefs [context/now-ms              (fn [] @clock)
                  league-import/import-league (fn [_] (swap! fetches inc) (Thread/sleep 50) {:ok true :config config})
                  league-sync/sync-league     (fn [_] {:ok true :league synced})]
      (let [bodies (doall (pmap (fn [_] (context/league-context fx/entry)) (range 4)))]
        (is (= 1 @fetches) "four tools in one turn, one import")
        (is (apply = bodies)))
      (reset! clock (dec context/league-ttl-ms))
      (context/league-context fx/entry)
      (is (= 1 @fetches))
      (context/league-context (assoc fx/entry :key "sleeper:999"))
      (is (= 2 @fetches) "another league is its own fetch")
      (reset! clock context/league-ttl-ms)
      (context/league-context fx/entry)
      (is (= 3 @fetches) "expired"))))

(deftest a-failed-fetch-is-tried-again
  (let [syncs (atom 0)]
    (with-redefs [league-import/import-league (fn [_] {:ok true :config config})
                  league-sync/sync-league     (fn [_] (if (= 1 (swap! syncs inc))
                                                        {:ok false :status 502 :error "bad gateway"}
                                                        {:ok true :league synced}))]
      (is (re-find #"bad gateway" (:error (context/league-context fx/entry))))
      (is (= 2 (:my-roster-id (context/league-context fx/entry))) "not pinned for the TTL"))))
