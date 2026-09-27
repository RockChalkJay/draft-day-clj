(ns draft-day.faab.leagues-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.faab.leagues :as leagues]))

(defn- corpus-season
  "A crawled season with `auctions` auctions, `contested` of them with two
  bidders."
  [meta auctions contested]
  {:meta   (merge {:league-id "1" :season "2025" :budget 100 :previous-league-id "9"
                   :kind :redraft :superflex? false}
                  meta)
   :season {:final?   true
            :auctions (vec (concat (repeat contested {:bids [{} {}]})
                                   (repeat (- auctions contested) {:bids [{}]})))}})

(deftest a-replay-needs-a-finished-faab-season-with-a-year-behind-it
  (is (leagues/eligible? (corpus-season {} 50 20)))
  (testing "and it is not eligible"
    (is (not (leagues/eligible? (corpus-season {:season "2023"} 50 20))) "outside 2024-2025")
    (is (not (leagues/eligible? (corpus-season {:budget 0} 50 20))) "without a budget")
    (is (not (leagues/eligible? (corpus-season {:previous-league-id "0"} 50 20)))
        "continuing nothing, which Sleeper spells \"0\"")
    (is (not (leagues/eligible? (corpus-season {:previous-league-id nil} 50 20))))
    (is (not (leagues/eligible? (corpus-season {} 49 20))) "too few auctions")
    (is (not (leagues/eligible? (corpus-season {} 50 19))) "too few contested")
    (is (not (leagues/eligible? (assoc-in (corpus-season {} 50 20) [:season :final?] false)))
        "a season still running")))

(defn- metas [n]
  (map (fn [i] {:league-id (str i) :season (if (even? i) "2024" "2025") :budget (if (zero? (mod i 3)) 100 1000)
                :previous-league-id (str "p" i) :kind ([:redraft :keeper :dynasty] (mod i 3))
                :superflex? (odd? (quot i 2))})
       (range n)))

(deftest the-split-halves-every-stratum-and-never-moves
  (let [rows (leagues/assign (metas 400))]
    (is (= rows (leagues/assign (shuffle (metas 400)))) "the same leagues give the same split, in any order")
    (doseq [[s rs] (group-by :stratum rows)]
      (let [{:keys [fit score]} (frequencies (map :half rs))]
        (is (<= (Math/abs (- (or fit 0) (or score 0))) 1) (str "balanced within " s))))
    (is (= (map :league-id rows) (map :league-id (sort-by :order rows))) "in the seeded order")
    (is (every? #(= (str "p" (:league-id %)) (:previous %)) rows))))

(deftest a-smaller-sample-sits-inside-a-larger-one
  (let [rows (leagues/assign (metas 400))
        few  (set (map :league-id (leagues/pick rows :fit 20)))
        more (set (map :league-id (leagues/pick rows :fit 80)))]
    (is (= 20 (count few)))
    (is (every? more few) "nested, so a learning curve's points differ only in size")
    (is (every? #(= :fit (:half %)) (leagues/pick rows :fit nil)))
    (is (= 400 (count (leagues/pick rows :all nil))))))

(deftest the-order-is-not-the-corpus-order
  (let [ids (map :league-id (leagues/assign (metas 400)))]
    (is (not= (sort-by parse-long ids) ids) "a random order, not the crawl's")
    (is (= (leagues/order-key "123") (leagues/order-key "123")) "and a fixed one")))
