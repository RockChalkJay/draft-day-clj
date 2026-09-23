(ns draft-day.faab.crawl-test
  "No network: `crawl/fetch` is the crawler's one door to Sleeper, and every test
  stands a fake Sleeper behind it."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [draft-day.faab.crawl :as crawl]))

(defn- delete-tree [f]
  (when (.isDirectory f) (run! delete-tree (.listFiles f)))
  (.delete f))

(use-fixtures :each
  (fn [t]
    (let [dir (io/file (System/getProperty "java.io.tmpdir") (str "dd-faab-" (random-uuid)))]
      (try
        (with-redefs [crawl/cache-dir (str dir)] (t))
        (finally (delete-tree dir))))))

(defn- league
  [id & {:as over}]
  (merge {:league_id id :season "2024" :status "complete" :total_rosters 12
          :roster_positions ["QB" "RB" "RB" "WR" "WR" "TE" "FLEX" "K" "DEF" "BN"]
          :scoring_settings {:rec 1.0}
          :settings {:waiver_type 2 :waiver_budget 100 :leg 2 :type 0 :daily_waivers 1}}
         over))

(defn- claim [status roster player bid at creator]
  {:type "waiver" :status status :status_updated at :creator creator
   :adds {(keyword player) roster} :settings {:waiver_bid bid}})

(defn- week-of-auctions
  "`n` one-bidder auctions and one contested one, all decided at `at`."
  [n at]
  (into [(claim "complete" 1 "p0" 5 at "u1")
         (claim "failed" 2 "p0" 3 at "u2")
         (claim "failed" 3 "p0" 9 at "u3")]
        (map (fn [i] (claim "complete" 1 (str "q" i) 0 at "u1")) (range n))))

(defn- sleeper
  "A fake `crawl/fetch` answering from `routes` {path body}; a path mapped to a
  keyword answers that failure reason."
  [routes]
  (fn [path]
    (let [v (get routes path ::missing)]
      (cond
        (= ::missing v) {:ok? false :reason :not-found}
        (keyword? v)    {:ok? false :reason v}
        :else           {:ok? true :body v}))))

(deftest the-cheap-gate-says-why-without-a-request
  (is (nil? (crawl/season-shape (league 1))))
  (is (= :not-faab (crawl/season-shape (league 1 :settings {:waiver_type 0 :waiver_budget 100}))))
  (is (= :incomplete (crawl/season-shape (league 1 :status "in_season"))))
  (is (= :out-of-range (crawl/season-shape (league 1 :season "2020")))
      "only seasons the backtest has universes for")
  (is (= :too-small (crawl/season-shape (league 1 :total_rosters 4))) "a mock, not a market")
  (is (= :no-budget (crawl/season-shape (league 1 :settings {:waiver_type 2 :waiver_budget 0})))))

(deftest a-league-is-sliced-by-what-changes-how-it-bids
  (let [m (crawl/league-meta (league 7 :previous_league_id 6
                                     :roster_positions ["QB" "SUPER_FLEX" "RB"]
                                     :settings {:waiver_type 2 :waiver_budget 1000 :type 2}))]
    (is (= "7" (:league-id m)))
    (is (= "6" (:previous-league-id m)))
    (is (= :dynasty (:kind m)))
    (is (true? (:superflex? m)))
    (is (= 1000 (:budget m)))
    (is (= 1.0 (:scoring-rec m)))))

(deftest an-accepted-season-is-the-shipped-normalizers-reading-of-it
  (with-redefs [crawl/fetch (sleeper {"/league/1/transactions/1" (week-of-auctions 25 1000)
                                      "/league/1/transactions/2" []})]
    (let [{:keys [decision season]} (crawl/probe-season (league 1))
          contested (first (filter #(= "p0" (:player-id %)) (:auctions season)))]
      (is (:ok? decision))
      (is (= 26 (get-in decision [:meta :auctions])))
      (is (= [[1 5 true] [2 3 false]] (mapv (juxt :roster-id :amount :won?) (:bids contested)))
          "the $9 failed above the winner, so it never competed")
      (is (= "u2" (:owner-id (second (:bids contested)))) "owned by the manager who placed it"))))

(deftest a-season-that-barely-bid-is-not-a-market
  (with-redefs [crawl/fetch (sleeper {"/league/1/transactions/1" (week-of-auctions 2 1000)
                                      "/league/1/transactions/2" []})]
    (is (= :too-few-auctions (get-in (crawl/probe-season (league 1)) [:decision :reason])))))

(deftest a-throttled-week-is-not-a-verdict
  (with-redefs [crawl/fetch (sleeper {"/league/1/transactions/1" (week-of-auctions 25 1000)
                                      "/league/1/transactions/2" :throttled})]
    (let [p (crawl/probe-season (league 1))]
      (is (crawl/undecided? p) "a season missing a week would read like a quiet one"))))

(deftest a-season-already-on-disk-is-not-fetched-again
  (with-redefs [crawl/fetch (sleeper {"/league/1/transactions/1" (week-of-auctions 25 1000)
                                      "/league/1/transactions/2" []})]
    (crawl/save-season! (crawl/probe-season (league 1))))
  (let [asked (atom 0)]
    (with-redefs [crawl/fetch (fn [_] (swap! asked inc) {:ok? false :reason :throttled})]
      (is (:ok? (:decision (crawl/probe-season (league 1)))))
      (is (zero? @asked) "a resumed crawl reuses what an interrupted one saved"))))

(deftest a-league-list-that-will-not-load-is-no-list
  (with-redefs [crawl/fetch (sleeper {"/user/u/leagues/nfl/2025" [(league 1)]
                                      "/user/u/leagues/nfl/2024" :throttled})]
    (is (nil? (crawl/user-leagues "u")) "a partial list would leave seasons never seen"))
  (with-redefs [crawl/fetch (sleeper {"/user/u/leagues/nfl/2025" [(league 1)]
                                      "/user/u/leagues/nfl/2024" []
                                      "/user/u/leagues/nfl/2023" []})]
    (is (= 1 (count (crawl/user-leagues "u"))))))

(deftest one-user-adds-at-most-his-cap-and-leaves-the-rest-for-later
  (let [lgs (conj (mapv #(league (str %)) (range 1 6))
                  (league "9" :status "in_season"))
        {:keys [probes]} (with-redefs [crawl/probe-season (fn [lg] {:league-id (str (:league_id lg))
                                                                    :decision {:ok? (nil? (crawl/season-shape lg))
                                                                               :reason (or (crawl/season-shape lg) :accepted)}})]
                           (update (crawl/visit lgs {:seen-seasons #{"1"} :accepted {}}
                                                {:max-seasons-per-user 2 :expand-per-user 3})
                                   :probes vec))]
    (is (= 3 (count probes)) "two candidates under the cap, plus the free rejection")
    (is (not-any? #(= "1" (:league-id %)) probes) "a season already judged is not judged again")
    (is (some #(= :incomplete (get-in % [:decision :reason])) probes))))

(deftest the-walk-accepts-expands-and-stops
  (let [routes {"/user/a/leagues/nfl/2025" []
                "/user/a/leagues/nfl/2024" [(league "1")]
                "/user/a/leagues/nfl/2023" []
                "/user/b/leagues/nfl/2025" []
                "/user/b/leagues/nfl/2024" [(league "1") (league "2")]
                "/user/b/leagues/nfl/2023" []
                "/league/1/transactions/1" (week-of-auctions 25 1000)
                "/league/1/transactions/2" []
                "/league/2/transactions/1" (week-of-auctions 25 2000)
                "/league/2/transactions/2" []
                "/league/1/rosters" [{:owner_id "a"} {:owner_id "b"}]
                "/league/2/rosters" [{:owner_id "b"}]}
        saved  (atom [])]
    (with-redefs [crawl/fetch (sleeper routes)]
      (let [st (crawl/crawl ["a"] {:on-accept! #(swap! saved conj (:league-id %))})]
        (is (= #{"1" "2"} (set (keys (:accepted st)))))
        (is (= ["1" "2"] @saved) "each season saved as it is accepted, once")
        (is (= #{"a" "b"} (:seen-users st)) "b was found through league 1's rosters")
        (is (= #{"1" "2"} (:seen-seasons st)))))
    (testing "a cap on accepted seasons stops the walk"
      (with-redefs [crawl/fetch (sleeper routes)]
        (is (= 1 (count (:accepted (crawl/crawl ["a"] {:max-seasons 1})))))))))

(deftest a-user-whose-list-keeps-failing-is-left-for-a-later-run
  (with-redefs [crawl/fetch (fn [_] {:ok? false :reason :throttled})]
    (let [st (crawl/crawl ["a"] {:max-retries 2})]
      (is (empty? (:frontier st)) "the walk ends rather than looping on him")
      (is (not ((:seen-users st) "a")) "and he is not marked visited")
      (is (= 1 (get-in st [:reasons :user-unreadable]))))))
