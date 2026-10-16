(ns draft-day.sensei.context-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.ingestion.league-import :as league-import]
            [draft-day.ingestion.league-sync :as league-sync]
            [draft-day.sensei.context :as context]))

(def ^:private config
  {:scoring   {:rec 1.0 :rush_yd 0.1}
   :num-teams 10
   :roster    {:qb 1 :rb 2 :wr 2 :te 1 :flex 1 :k 1 :dst 1 :bench 5}})

(def ^:private synced
  {:provider :sleeper
   :teams    [{:roster-id 1 :name "Mine"} {:roster-id 2 :name "Rival"}]
   :waiver   {:type :faab :budget 100}})

(deftest the-body-is-the-one-the-waivers-tab-would-send
  (let [b (context/waivers-body config synced "1")]
    (is (= 1 (:my-roster-id b)) "the team's own id, not the env var's string")
    (is (= 14 (:roster-size b)) "every seat, bench included")
    (is (= {:qb 1 :rb 2 :wr 2 :te 1 :flex 1} (:replacement-config b)))
    (is (= (:roster config) (:roster b)) "K and DST still fill starting seats")))

(deftest an-unknown-roster-id-names-the-teams-there-are
  (is (re-find #"1 \(Mine\), 2 \(Rival\)"
               (:error (context/waivers-body config synced "7")))))

(deftest the-context-reads-as-the-browser-posts-it
  (let [seen (atom [])]
    (with-redefs [league-import/import-league (fn [req] (swap! seen conj req) {:ok true :config config})
                  league-sync/sync-league     (fn [_] {:ok true :league synced})]
      (let [b (context/league-context {:provider "sleeper" :league-id 123 :roster-id "2"})]
        (is (= "sleeper" (get-in b [:league :provider])) "keywords cross JSON as strings")
        (is (= "faab" (get-in b [:league :waiver :type])))
        (is (= 2 (:my-roster-id b)))
        (is (= {:provider :sleeper :league-id "123" :credentials nil}
               (select-keys (first @seen) [:provider :league-id :credentials]))
            "Sleeper is asked with no credential")))))

(deftest a-failure-says-which-step-and-masks-the-cookie
  (with-redefs [context/env-credentials     (constantly {:swid "{SW-ID}" :espn-s2 "S2SECRET"})
                league-import/import-league (fn [_] {:ok true :config config})
                league-sync/sync-league     (fn [_] {:ok false :status 401
                                                     :error "rejected espn_s2=S2SECRET swid={SW-ID}"})]
    (let [{:keys [error]} (context/league-context {:provider "espn" :league-id "9" :roster-id "1"})]
      (is (re-find #"^sync failed" error))
      (is (not (re-find #"S2SECRET|SW-ID" error)) "a tool result is read by a third party"))))

(deftest a-secret-is-masked-in-each-spelling-an-error-echoes
  (let [swid "ABCDEF12-0000-0000-0000-000000000000"
        ss   (context/secrets-in :espn {:swid swid :espn-s2 "S2SECRET"})]
    (is (= "a •••• b •••• c ••••"
           (context/mask ss (str "a " swid " b {" swid "} c S2SECRET"))))
    (is (empty? (context/secrets-in :sleeper {:username "someone"})) "a username is public")
    (is (empty? (context/secrets-in :espn nil)))))

(deftest no-league-says-which-env-vars-to-set
  (is (re-find #"DRAFTDAY_SENSEI_PROVIDER.*DRAFTDAY_SENSEI_LEAGUE_ID.*DRAFTDAY_SENSEI_ROSTER_ID"
               (:error (context/league-context nil)))))

(deftest espn-reads-its-cookies-off-the-environment-and-sleeper-none
  (testing "Sleeper serves a league to anyone with the id"
    (is (nil? (context/env-credentials :sleeper)))))
