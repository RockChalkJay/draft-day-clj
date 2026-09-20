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
    (is (= "unknown provider: :yahoo" error))))

(deftest an-import-reports-the-rules-it-could-not-apply
  ;; Silently keeping 20 of 85 rules and reporting success hands back a config
  ;; that looks complete and scores differently from the real league. One live
  ;; league dropped all DST points-allowed tiers and every yardage bonus. The
  ;; field-goal buckets used to be dropped here too and are modelled now.
  (let [dropped (sleeper-import/unsupported-scoring
                 {:rec 1.0 :rec_yd 0.1                    ; modelled
                  :fgm_0_19 3.0 :fgm_50p 5.0              ; modelled now
                  :fgmiss_30_39 -2.0                      ; not modelled
                  :bonus_rec_te 0.5 :pts_allow_0 10.0
                  :def_st_ff 0.0                          ; present but off
                  :pass_2pt 2.0})]                        ; modelled
    (is (= ["bonus_rec_te" "fgmiss_30_39" "pts_allow_0"] dropped))
    (is (not-any? #{"rec" "rec_yd" "pass_2pt" "fgm_0_19" "fgm_50p"} dropped)
        "rules we do score are not reported as dropped")
    (is (not-any? #{"def_st_ff"} dropped)
        "a rule the league has switched off costs it nothing"))

  (testing "a league with nothing exotic reports nothing"
    (is (= [] (sleeper-import/unsupported-scoring {:rec 1.0 :rush_yd 0.1})))
    (is (= [] (sleeper-import/unsupported-scoring nil)))))

(deftest the-import-passes-the-season-and-the-credentials-to-the-provider
  (let [seen (atom nil)]
    (with-redefs [league-import/fetch-raw-league (fn [_ req] (reset! seen req) raw-league)]
      (league-import/import-league {:provider "sleeper" :league-id "1"
                                    :season "2025" :credentials {:username "jay"}})
      (is (= "2025" (:season @seen))
          "a provider that never sees the season imports last year's copy of the
           league, which looks like a successful import of an empty one")
      (is (= {:username "jay"} (:credentials @seen)))
      (is (= "1" (:league-id @seen))))))

(deftest a-public-host-imports-without-an-account
  (with-redefs [league-import/fetch-raw-league (fn [_ _] raw-league)]
    (is (:ok (league-import/import-league {:provider "sleeper" :league-id "1"}))
        "Sleeper serves a league to anyone with the id; demanding a username
         would refuse a case that works")))

(deftest an-auction-league-brings-its-budget
  ;; Without it the one number a connected league still needed typed by hand
  ;; was the bankroll the whole board is priced out of.
  (is (= 300 (:starting-bankroll
              (league-import/normalize-league
               :sleeper (assoc raw-league :draft {:type "auction" :settings {:budget 300}})))))
  (testing "a snake draft has no budget to bring, whatever its settings carry"
    (is (nil? (:starting-bankroll
               (league-import/normalize-league
                :sleeper (assoc raw-league :draft {:type "snake" :settings {:budget 200}}))))))
  (testing "and a league with no draft document says nothing either"
    (is (nil? (:starting-bankroll (league-import/normalize-league :sleeper raw-league))))))

(deftest the-draft-is-fetched-alongside-the-league-but-never-fails-it
  (let [asked (atom nil)]
    (with-redefs [sleeper-import/fetch-league (fn [_] (assoc raw-league :draft_id "d1"))
                  sleeper-import/fetch-draft  (fn [id] (reset! asked id) {:type "auction"
                                                                          :settings {:budget 250}})]
      (is (= 250 (-> (league-import/import-league {:provider "sleeper" :league-id "1"})
                     :config :starting-bankroll)))
      (is (= "d1" @asked) "the draft asked for is the league's own")))
  (testing "a draft fetch that throws still imports the rules"
    (with-redefs [sleeper-import/fetch-league (fn [_] (assoc raw-league :draft_id "d1"))
                  sleeper-import/fetch-draft  (fn [_] (throw (ex-info "down" {:status 502})))]
      (let [{:keys [ok config]} (league-import/import-league {:provider "sleeper" :league-id "1"})]
        (is ok)
        (is (= 10 (:num-teams config)))
        (is (nil? (:starting-bankroll config))))))
  (testing "a league with no draft scheduled asks for none"
    (with-redefs [sleeper-import/fetch-league (fn [_] raw-league)
                  sleeper-import/fetch-draft  (fn [_] (throw (AssertionError. "fetched a draft")))]
      (is (:ok (league-import/import-league {:provider "sleeper" :league-id "1"}))))))
