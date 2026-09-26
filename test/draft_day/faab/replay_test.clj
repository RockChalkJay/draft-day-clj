(ns draft-day.faab.replay-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.bid-prior :as prior]
            [draft-day.faab.replay :as replay]
            [draft-day.rankings.faab :as faab]))

(defn- tx [at type adds drops & [bid]]
  {:status "complete" :status_updated at :type type
   :adds (update-keys adds keyword) :drops (update-keys drops keyword)
   :settings (when bid {:waiver_bid bid})})

(def ^:private weeks
  ;; Keys arrive keywordized, as `draft-day.json/mapper` decodes them.
  {1 [(tx 100 "waiver" {"a" 1} {"x" 1} 12)
      (tx 150 "free_agent" {"b" 2} {} nil)
      {:status "failed" :status_updated 120 :type "waiver" :adds {:c 2} :settings {:waiver_bid 30}}]
   2 [(tx 200 "trade" {"y" 2 "b" 1} {"y" 1 "b" 2})
      (tx 300 "waiver" {"d" 2} {} 5)]})

(def ^:private final {1 #{"a" "b"} 2 #{"y" "d"}})

(deftest moves-keep-only-what-happened-in-order
  (let [ms (replay/moves weeks)]
    (is (= [100 150 200 300] (map :at ms)) "a failed claim moved nobody")
    (is (= {"a" 1} (:adds (first ms))) "player ids back to strings")))

(deftest rosters-are-the-final-ones-with-later-moves-undone
  (let [ms (replay/moves weeks)]
    (is (= final (replay/rosters-at final ms 301)) "nothing after the last move")
    (is (= {1 #{"a" "y"} 2 #{"b"}} (replay/rosters-at final ms 200))
        "the trade and the later claim reversed")
    (is (= {1 #{"x" "y"} 2 #{}} (replay/rosters-at final ms 100))
        "back past the first claim, whose drop returns, and y is still roster 1's")))

(deftest a-manager-has-what-he-had-not-yet-spent
  (let [ms (replay/moves weeks)]
    (is (= {} (replay/spent-before ms 100)) "a claim is spent from its own run on, not before")
    (is (= {1 12} (replay/spent-before ms 101)))
    (is (= {1 12 2 5} (replay/spent-before ms 301)) "a free-agent add costs nothing")))

(deftest the-history-stops-before-the-run
  (is (= [1] (map :at (:auctions (replay/season-before {:auctions [{:at 1} {:at 2} {:at 3}]} 2))))))

(deftest the-league-at-a-run-carries-its-rosters-and-balances
  (let [synced {:waiver {:budget 100}
                :teams  [{:roster-id 1 :waiver-position 3} {:roster-id 2 :waiver-position 1}]}
        lg     (replay/league-at synced final (replay/moves weeks) 200)
        [t1 t2] (:teams lg)]
    (is (= ["a" "y"] (:player-ids t1) (:active-ids t1)) "nobody on IR: its history is gone")
    (is (= 88 (:faab-left t1)))
    (is (= 100 (:faab-left t2)))
    (is (nil? (:waiver-position t1)) "today's waiver order is not the one then")))

(defn- rival [p amounts]
  (let [pmf (double-array 101)]
    (doseq [[b v] amounts] (aset pmf b (double v)))
    {:p p :pmf pmf :cdf (faab/cumulative pmf)}))

(deftest the-top-rival-bid-counts-nobody-bidding-as-below
  (let [rs [(rival 0.5 {4 0.5 10 0.5}) (rival 0.2 {2 1.0})]]
    (is (< (Math/abs (- 0.4 (replay/top-cdf rs 1))) 1e-9) "nobody bids 40% of the time")
    (is (< (Math/abs (- 1.0 (replay/top-cdf rs 10))) 1e-9))))

(deftest a-rule-wins-by-outbidding-and-splits-a-tie
  (is (= 1.0 (replay/outcome 5 nil)) "nobody else bid")
  (is (= 1.0 (replay/outcome 6 5)))
  (is (= 0.5 (replay/outcome 5 5)) "waiver order, unknown")
  (is (= 0.0 (replay/outcome 4 5)))
  (is (= 0.0 (replay/outcome nil 5)) "no bid at all loses"))

(deftest the-median-rule-is-the-sleeper-wide-winning-bid
  (is (= 0 (replay/median-rule 1 100)) "a lone bidder's median win is $0")
  (is (= (Math/round (* 100 (get-in prior/winning-bid [4 :p50]))) (replay/median-rule 7 100))))

(deftest reliability-bins-report-what-was-predicted-and-what-happened
  (let [rows [{:p 0.05 :o 0.0} {:p 0.08 :o 1.0} {:p 0.9 :o 1.0}]]
    (is (= [[0 0.1 2 0.065 0.5] [0.5 1.01 1 0.9 1.0]]
           (map #(update % 3 (fn [x] (/ (Math/round (* 1000 x)) 1000.0)))
                (replay/reliability rows :p :o [0 0.1 0.5 1.01])))))
  (is (< (Math/abs (- 0.25 (replay/brier [0.5 0.5] [0.0 1.0]))) 1e-9)))
