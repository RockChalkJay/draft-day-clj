(ns draft-day.ingestion.league-import-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.ingestion.league-import :as league-import]
            [draft-day.ingestion.league-import.sleeper :as sleeper-import]))

(def ^:private raw-league
  {:name "Dynasty Dynasts" :season "2026" :total_rosters 10
   :scoring_settings {:rec 1.0 :pass_td 4.0 :pass_int -2.0 :some_future_stat 3.0}
   :roster_positions ["QB" "RB" "RB" "WR" "WR" "TE" "FLEX" "WRRB_FLEX"
                       "K" "DEF" "BN" "BN" "BN" "IDP_FLEX"]})

(deftest normalize-league-filters-scoring-and-counts-roster-slots
  (let [cfg (league-import/normalize-league :sleeper raw-league)]
    (is (= {:rec 1.0 :pass_td 4.0 :pass_int -2.0} (:scoring cfg))
        "unknown stat keys (outside scoring/stat-keys) are dropped")
    (is (= {:qb 1 :rb 2 :wr 2 :te 1 :flex 2 :k 1 :dst 1 :bench 4} (:roster cfg))
        "FLEX+WRRB_FLEX -> :flex, BN + unknown IDP_FLEX slot -> :bench")
    (is (= 10 (:num-teams cfg)))
    (is (= "Dynasty Dynasts" (:name cfg)))
    (is (= "2026" (:season cfg)))))

(deftest import-league-success
  (with-redefs [league-import/fetch-raw-league (fn [_ _] raw-league)]
    (let [{:keys [ok config]} (league-import/import-league {:provider "sleeper" :league-id "123"})]
      (is ok)
      (is (= 10 (:num-teams config))))))

(deftest sync-league-normalizes-sleeper-view-data
  (let [raw {:league {:league_id "123" :name "Dynasty Dynasts" :season "2026" :total_rosters 10}
             :users [{:user_id "u1" :username "alice" :display_name "Alice" :avatar "a.png"}
                     {:user_id "u2" :username "bob" :display_name "Bob"}]
             :rosters [{:roster_id 1 :owner_id "u1" :players ["QB:1" "RB:2"]}
                       {:roster_id 2 :owner_id "u2" :players ["WR:3"]}]}
        sync (league-import/normalize-sync :sleeper raw)]
    (is (= :sleeper (:provider sync)))
    (is (= "Dynasty Dynasts" (get-in sync [:league :name])))
    (is (= 10 (get-in sync [:league :num-teams])))
    (is (= [{:user-id "u1" :username "alice" :display-name "Alice" :avatar "a.png"}
            {:user-id "u2" :username "bob" :display-name "Bob" :avatar nil}]
           (:users sync)))
    (is (= [{:team-id 1 :owner-id "u1" :manager "Alice" :roster ["QB:1" "RB:2"]}
            {:team-id 2 :owner-id "u2" :manager "Bob" :roster ["WR:3"]}]
           (:teams sync)))
    (is (= [] (:matchups sync)))
    (is (= [] (:waiver-wire sync)))))

(deftest sync-league-success
  (with-redefs [league-import/fetch-sync (fn [_ _] {:league {:league_id "123" :name "Dynasty Dynasts" :season "2026" :total_rosters 10}
                                                   :users []
                                                   :rosters []})]
    (let [{:keys [ok league]} (league-import/sync-league {:provider "sleeper" :league-id "123"})]
      (is ok)
      (is (= "Dynasty Dynasts" (get-in league [:league :name]))))))

(deftest import-league-not-found
  (with-redefs [league-import/fetch-raw-league
                (fn [_ _] (throw (ex-info "not found" {:status 404})))]
    (let [{:keys [ok status error]} (league-import/import-league {:provider :sleeper :league-id "999"})]
      (is (not ok))
      (is (= 404 status))
      (is (= "not found" error)))))

(deftest import-league-network-error
  (with-redefs [league-import/fetch-raw-league
                (fn [_ _] (throw (ex-info "down" {:status 502})))]
    (let [{:keys [ok status]} (league-import/import-league {:provider :sleeper :league-id "1"})]
      (is (not ok))
      (is (= 502 status)))))

(deftest import-league-unknown-provider
  (let [{:keys [ok status error]} (league-import/import-league {:provider "yahoo" :league-id "1"})]
    (is (not ok))
    (is (= 400 status))
    (is (= "Unknown league provider" error))))

(deftest an-import-reports-the-rules-it-could-not-apply
  ;; Silently keeping 20 of 85 rules and reporting success hands back a config
  ;; that looks complete and scores differently from the real league. One live
  ;; league dropped every FG distance bucket (Sleeper never emits a bare `fgm`,
  ;; so kickers lost field goals outright), all DST points-allowed tiers and
  ;; every yardage bonus.
  (let [dropped (sleeper-import/unsupported-scoring
                 {:rec 1.0 :rec_yd 0.1                    ; modelled
                  :fgm_0_19 3.0 :fgm_50p 5.0              ; not modelled
                  :bonus_rec_te 0.5 :pts_allow_0 10.0
                  :def_st_ff 0.0                          ; present but off
                  :pass_2pt 2.0})]                        ; modelled
    (is (= ["bonus_rec_te" "fgm_0_19" "fgm_50p" "pts_allow_0"] dropped))
    (is (not-any? #{"rec" "rec_yd" "pass_2pt"} dropped)
        "rules we do score are not reported as dropped")
    (is (not-any? #{"def_st_ff"} dropped)
        "a rule the league has switched off costs it nothing"))

  (testing "a league with nothing exotic reports nothing"
    (is (= [] (sleeper-import/unsupported-scoring {:rec 1.0 :rush_yd 0.1})))
    (is (= [] (sleeper-import/unsupported-scoring nil)))))
