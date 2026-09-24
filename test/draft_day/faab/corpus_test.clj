(ns draft-day.faab.corpus-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.faab.corpus :as corpus]
            [draft-day.faab.report :as report]))

(defn- bid [roster owner amount won?]
  {:roster-id roster :owner-id owner :amount amount :won? won?})

(def ^:private season
  {:meta   {:league-id "10" :season "2024" :budget 200 :num-teams 12 :kind :redraft
            :superflex? false :previous-league-id "9"}
   :season {:auctions [{:week 5 :at 2000 :player-id "b"
                        :bids [(bid 1 "u1" 20 true)]}
                       {:week 1 :at 1000 :player-id "a"
                        :bids [(bid 1 "u1" 50 true) (bid 2 "u2" 10 false) (bid 3 "u3" 0 false)]}
                       {:week 12 :at 3000 :player-id "c"
                        :bids [(bid 2 "u2" 0 true)]}]}})

(deftest phases-and-bidder-buckets
  (is (= [:early :early :mid :mid :late] (mapv corpus/phase [1 4 5 10 11])))
  (is (= [1 2 3 4 4] (mapv corpus/bucket [1 2 3 4 9])) "four and up are one bucket"))

(deftest an-auction-row-reads-bids-as-shares-of-the-season-budget
  (let [[a b c] (corpus/auction-rows season {"a" "RB"})]
    (is (= ["a" "b" "c"] (map :player-id [a b c])) "in the order they were decided")
    (is (= 3 (:n a)))
    (is (= 0.25 (:winner a)))
    (is (= 0.05 (:second a)) "the top rival bid")
    (is (nil? (:second b)) "a lone bidder had no rival")
    (is (= "RB" (:position a)))
    (is (= "?" (:position b)) "a player the dump does not know")
    (is (= [:early :mid :late] (map :phase [a b c])))
    (is (= "9" (:previous-league-id a)))))

(deftest what-a-manager-had-left-is-his-budget-less-his-earlier-wins
  (let [[a b] (corpus/auction-rows season {})]
    (is (= 1.0 (:remaining (first (:bids a)))) "nothing spent before his first win")
    (is (= 0.75 (:remaining (first (:bids b)))) "the $50 win came off his $200")))

(deftest a-win-does-not-come-off-a-balance-the-same-run-already-decided-against
  (let [same-run {:meta   {:budget 100}
                  :season {:auctions [{:week 2 :at 1000 :player-id "a" :bids [(bid 1 "u1" 40 true)]}
                                      {:week 2 :at 1000 :player-id "b" :bids [(bid 1 "u1" 30 true)]}
                                      {:week 3 :at 2000 :player-id "c" :bids [(bid 1 "u1" 5 true)]}]}}
        [a b c]  (corpus/auction-rows same-run {})]
    (is (= [1.0 1.0] (map #(:remaining (first (:bids %))) [a b])))
    (is (= 0.3 (:remaining (first (:bids c)))) "both of the run's wins came off before the next")))

(deftest a-manager-season-measures-how-often-and-how-hard-he-bids
  (let [rows     (corpus/auction-rows season {})
        typical  {[3 :early] 0.05 [1 :mid] 0.05}
        by-owner (into {} (map (juxt :owner-id identity)) (corpus/manager-seasons rows typical))]
    (is (= 2 (:bids (by-owner "u1"))))
    (is (= (/ 2 3.0) (:per-week (by-owner "u1"))) "two bids over the three weeks bid in")
    (is (= 0.5 (:zero-share (by-owner "u2"))))
    (is (< 0.0 (:aggression (by-owner "u1")))
        "his bids ran above what that kind of auction usually draws")
    (is (nil? (:aggression (by-owner "u3"))) "no positive bid, nothing to measure")
    (is (= "9" (:previous-league-id (by-owner "u1"))))))

(deftest quantiles-are-nearest-rank-and-nil-for-nothing
  (is (= 3 (report/quantile [5 1 3 2 4] 0.5)))
  (is (= 5 (report/quantile [1 2 3 4 5] 0.99)))
  (is (nil? (report/quantile [] 0.5))))

(deftest a-summary-counts-the-zeros-it-quantiles-over
  (let [s (report/summarize [0 0 0.1 0.2])]
    (is (= 4 (:n s)))
    (is (= 0.5 (:p-zero s)))
    (is (= 0.2 (:pos-p50 s)) "the positive half's own median")))

(deftest rank-correlation
  (is (= 1.0 (report/spearman [[1 10] [2 20] [3 30]])))
  (is (= -1.0 (report/spearman [[1 30] [2 20] [3 10]])))
  (is (nil? (report/spearman [[1 1] [2 2]])) "two pairs are not a correlation")
  (is (= [0.5 0.5 2.0] (report/ranks [7 7 9])) "tied values share their mean rank"))

(deftest a-dimension-that-separates-prices-narrows-their-spread
  (let [bids (concat (repeat 20 {:bucket 2 :share 0.02 :position "DEF"})
                     (repeat 20 {:bucket 2 :share 0.20 :position "RB"}))]
    (is (< 0.9 (report/spread-reduction bids :position)))
    (is (< (Math/abs (report/spread-reduction (map #(assoc % :flag true) bids) :flag)) 1e-9)
        "a dimension with one value tells nothing")))

(deftest habits-are-paired-across-seasons-and-across-leagues
  (let [ms [{:league-id "9" :season "2023" :owner-id "u1" :bids 12}
            {:league-id "10" :season "2024" :owner-id "u1" :bids 12 :previous-league-id "9"}
            {:league-id "20" :season "2024" :owner-id "u1" :bids 12}
            {:league-id "10" :season "2024" :owner-id "u2" :bids 12 :previous-league-id "9"}]]
    (is (= [["9" "10"]] (map (fn [[a b]] [(:league-id a) (:league-id b)]) (report/season-pairs ms)))
        "u2 has no season before in league 9")
    (is (= [["10" "20"]] (map (fn [[a b]] [(:league-id a) (:league-id b)]) (report/league-pairs ms))))))

(deftest the-backbone-carries-every-cell-it-measured-in-its-own-budget
  (let [bids [{:bucket 1 :phase :early :share 0.0 :amount 0 :budget 100}
              {:bucket 1 :phase :early :share 0.04 :amount 4 :budget 100}
              {:bucket 4 :phase :late :share 0.3 :amount 30 :budget 100}
              {:bucket 2 :phase :mid :share 0.01 :amount 10 :budget 1000}
              {:bucket 1 :phase :early :share 0.02 :amount 20 :budget 1000}]
        bb   (report/backbone bids [])]
    (is (= #{[1 :early] [4 :late]} (set (keys (:bid-share bb))))
        "a $1000 league's bids are not read as shares of the $100 scale")
    (is (= 0.5 (get-in bb [:bid-share [1 :early] :p-zero])))
    (is (= 0.04 (get-in bb [:bid-share [1 :early] :positive 0.5])))
    (is (= (Math/log 0.5) (get-in bb [:budget-shift 1 :log-shift]))
        "but its distance from that scale is kept, measured against the $100 bids directly")
    (is (not (contains? (:budget-shift bb) 2)) "a bucket with no $100 bids has nothing to sit off")))

(deftest a-win-by-exactly-five-percent-counts-as-won-by-five-percent
  (let [auction (fn [w s] {:meta   {:budget 100}
                           :season {:auctions [{:week 1 :at 1 :player-id "a"
                                                :bids [(bid 1 "u1" w true) (bid 2 "u2" s false)]}]}})
        rows    (mapcat #(corpus/auction-rows (apply auction %) {}) [[12 7] [7 3]])]
    (is (= 0.5 (:won-by-5 (report/overpay rows)))
        "$12 over $7 is a $5 margin, however 0.12 - 0.07 rounds")))

(deftest round-number-heaps-are-read-against-what-chance-would-give
  (let [bids (map (fn [a] {:budget 100 :amount a}) [5 10 15 11 7 3])
        h    (report/heaping bids 100 5 5)]
    (is (= 5 (:n h)) "bids under the floor are not asked about")
    (is (= 0.6 (:round h)))
    (is (= 0.2 (:one-over h)))
    (is (= 0.2 (:by-chance h)))))

(deftest a-position-that-draws-bigger-bids-shifts-up
  (let [bids (concat (repeat 10 {:bucket 2 :share 0.02 :position "DEF"})
                     (repeat 10 {:bucket 2 :share 0.05 :position "WR"})
                     (repeat 10 {:bucket 2 :share 0.10 :position "RB"}))
        s    (report/position-shifts bids)]
    (is (pos? (get-in s ["RB" :log-shift])))
    (is (neg? (get-in s ["DEF" :log-shift])))
    (is (zero? (get-in s ["WR" :log-shift])) "the bucket's own median shifts nothing")))
