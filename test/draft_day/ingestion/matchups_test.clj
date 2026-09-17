(ns draft-day.ingestion.matchups-test
  "The provider seam for head-to-head, and the three traps in it: an
  `ExecutionException` swallowing a 404's status, a JSON mapper keywordizing
  player ids that are map keys, and a roster with nobody to play."
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.ingestion.matchups :as matchups]
            [draft-day.ingestion.matchups.sleeper :as m-sleeper]))

;; Rosters 1 and 2 play each other; roster 3 has no opponent.
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

(deftest the-pairing-is-read-off-the-matchup-id
  (let [{:keys [matchups]} (normalized)]
    (is (= {:matchup-id 1 :roster-ids [1 2]} (first matchups)))))

(deftest a-roster-with-no-opponent-keeps-its-entry
  (let [{:keys [matchups]} (normalized)
        lone (first (filter #(nil? (:matchup-id %)) matchups))]
    (is (some? lone)
        "a byed team still has a lineup; dropping it looks like a sync failure")
    (is (= [3] (:roster-ids lone)) "one id, not a pair of one")
    (is (= 2 (count matchups)) "one game plus one bye")))

(deftest the-games-are-ordered-by-matchup-id
  (let [shuffled [{:matchup_id 3 :roster_id 5 :starters [] :players_points {}}
                  {:matchup_id 1 :roster_id 1 :starters [] :players_points {}}
                  {:matchup_id 3 :roster_id 6 :starters [] :players_points {}}
                  {:matchup_id 1 :roster_id 2 :starters [] :players_points {}}]]
    (is (= [1 3] (mapv :matchup-id (m-sleeper/matchup-pairs shuffled)))
        "not the order the provider serialized its rosters in")))

(deftest player-points-come-back-with-string-ids
  ;; `json/mapper` keywordizes every key it decodes, and these keys are player
  ;; ids; untouched, the join misses every player on the board.
  (let [pp (get-in (normalized) [:scores 1 :player-points])]
    (is (= {"4034" 18.4 "6794" 12.0 "9001" 22.6} pp))
    (is (every? string? (keys pp)))))

(deftest a-team-defense-survives-the-same-round-trip
  (is (= {"1234" 31.2 "SF" 9.0} (get-in (normalized) [:scores 2 :player-points]))
      "its id is the abbreviation in both spaces, so a stray keyword survives"))

(deftest the-lineup-comes-off-the-matchup-not-the-roster
  (is (= ["4034" "6794" "0"] (get-in (normalized) [:scores 1 :starter-ids]))
      "including the placeholder for an unfilled seat, which is not a player"))

(deftest the-roster-comes-off-the-matchup-too
  ;; This week's, not the last sync's — a pickup since then is on it.
  (is (= ["4034" "6794" "9001"] (get-in (normalized) [:scores 1 :player-ids]))))

(deftest the-providers-own-team-total-is-carried
  (is (= 96.1 (get-in (normalized) [:scores 1 :official])))
  (is (= 104.4 (get-in (normalized) [:scores 2 :official]))))

(deftest a-commissioners-correction-is-the-score-of-record
  ;; `custom_points` is null on an ordinary matchup and set on a stat
  ;; correction; `points` still holds the computed figure.
  (let [corrected (assoc-in raw [0 :custom_points] 101.5)]
    (is (= 101.5 (get-in (matchups/normalize-matchups :sleeper corrected)
                         [:scores 1 :official]))))
  (testing "and an uncorrected score still reads off :points"
    (is (= 96.1 (get-in (matchups/normalize-matchups
                         :sleeper (assoc-in raw [0 :custom_points] nil))
                        [:scores 1 :official])))))

(deftest points-cover-the-whole-roster-not-only-the-starters
  (is (contains? (get-in (normalized) [:scores 1 :player-points]) "9001")
      "9001 is on the roster and not in the lineup; the optimal half needs him"))

(deftest a-roster-that-has-scored-nothing-is-still-zero-not-missing
  ;; Whether a 0.0 means "has not played" is the board's judgment against the
  ;; kickoff clock, not a provider's to pre-empt.
  (is (= {"7777" 0.0} (get-in (normalized) [:scores 3 :player-points]))))

(deftest an-entry-with-no-points-map-yields-an-empty-one
  (is (= {} (m-sleeper/player-points {:roster_id 9}))))

(deftest fetch-matchups-returns-the-same-envelope-the-other-pairs-do
  (with-redefs [matchups/current-week        (fn [_ _] 3)
                matchups/fetch-raw-matchups  (fn [_ _] raw)]
    (let [{:keys [ok week matchups scores]} (matchups/fetch-matchups
                                             {:provider "sleeper" :league-id "1"})]
      (is ok)
      (is (= 3 week) "asked of the provider, not derived from :through-week")
      (is (= 2 (count matchups)))
      (is (= #{1 2 3} (set (keys scores)))))))

(deftest an-explicit-week-skips-asking-the-provider
  (let [asked (atom false)]
    (with-redefs [matchups/current-week       (fn [_ _] (reset! asked true) 3)
                  matchups/fetch-raw-matchups (fn [_ {:keys [week]}] (is (= 7 week)) raw)]
      (is (= 7 (:week (matchups/fetch-matchups {:provider :sleeper :league-id "1" :week 7}))))
      (is (not @asked) "there is nothing to overlap, and nothing to ask"))))

(deftest a-concurrent-fetch-does-not-cost-an-error-its-status
  ;; A future's deref wraps the thunk's throw in an `ExecutionException`
  ;; carrying no ex-data; unwrapped, every 404 would report as a 502.
  (with-redefs [matchups/current-week (fn [_ _] 3)
                matchups/fetch-raw-matchups
                (fn [_ _] (throw (java.util.concurrent.ExecutionException.
                                    (ex-info "not found" {:status 404}))))]
    (let [{:keys [ok status error]} (matchups/fetch-matchups
                                     {:provider :sleeper :league-id "9"})]
      (is (not ok))
      (is (= 404 status))
      (is (= "not found" error) "the cause's message, not the wrapper's"))))

(deftest the-envelopes-own-keys-are-not-a-providers-to-set
  (with-redefs [matchups/current-week (fn [_ _] 3)
                matchups/fetch-raw-matchups (fn [_ _] raw)
                matchups/normalize-matchups (fn [_ _] {:matchups [] :scores {}
                                                       :ok false :week 99})]
    (let [{:keys [ok week]} (matchups/fetch-matchups {:provider :sleeper :league-id "1"})]
      (is (true? ok))
      (is (= 3 week)))))

(deftest a-bare-failure-is-a-502
  (with-redefs [matchups/current-week (fn [_ _] 3)
                matchups/fetch-raw-matchups (fn [_ _] (throw (ex-info "boom" {})))]
    (is (= 502 (:status (matchups/fetch-matchups {:provider :sleeper :league-id "1"}))))))

(deftest a-provider-between-seasons-has-no-week-to-show
  (with-redefs [matchups/current-week (fn [_ _] nil)]
    (let [{:keys [ok status]} (matchups/fetch-matchups {:provider :sleeper :league-id "1"})]
      (is (not ok) "no matchup to draw — not week zero, and not a crash")
      (is (= 404 status)))))

(deftest an-unknown-provider-is-a-400-on-either-multimethod
  (testing "asking what week it is"
    (is (= 400 (:status (matchups/fetch-matchups {:provider :yahoo :league-id "1"})))))
  (testing "asking for the games"
    (is (= 400 (:status (matchups/fetch-matchups {:provider :yahoo :league-id "1" :week 3}))))))

(deftest a-week-with-no-games-is-an-empty-board-not-an-error
  ;; Sleeper answers a valid league asked for an out-of-season week with `[]`,
  ;; which is why the fetch does not treat empty as missing.
  (with-redefs [matchups/current-week (fn [_ _] 25)
                matchups/fetch-raw-matchups (fn [_ _] [])]
    (let [{:keys [ok matchups scores]} (matchups/fetch-matchups
                                        {:provider :sleeper :league-id "1"})]
      (is ok)
      (is (= [] matchups))
      (is (= {} scores)))))

(deftest a-provider-is-asked-with-the-same-request-map-the-other-pairs-use
  ;; A host that puts the season in its URL or a cookie on its request needs
  ;; somewhere to read them from; ESPN's current week is on the league document.
  (let [asked (atom {})]
    (with-redefs [matchups/current-week       (fn [_ req] (swap! asked assoc :week-req req) 3)
                  matchups/fetch-raw-matchups (fn [_ req] (swap! asked assoc :raw-req req) raw)]
      (matchups/fetch-matchups {:provider :sleeper :league-id "1" :season 2025
                                :credentials {:username "jay"}})
      (is (= {:league-id "1" :season 2025 :credentials {:username "jay"}} (:week-req @asked)))
      (is (= 3 (get-in @asked [:raw-req :week])) "the scoreboard request adds its week")
      (is (= 2025 (get-in @asked [:raw-req :season]))))))

(deftest a-league-this-host-cannot-be-asked-about-is-refused-before-the-network
  (with-redefs [matchups/current-week (fn [_ _] (throw (ex-info "should not be asked" {})))]
    (is (= 400 (:status (matchups/fetch-matchups {:provider :espn :league-id "123"})))
        "an ESPN league with no cookie to read it with")))
