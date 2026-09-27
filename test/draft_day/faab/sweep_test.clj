(ns draft-day.faab.sweep-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.faab.sweep :as sweep]))

(deftest log-loss-scores-the-outcome-that-happened
  (is (< (Math/abs (- (Math/log 2.0) (sweep/log-loss 0.5 1.0))) 1e-12))
  (is (< (sweep/log-loss 0.9 1.0) (sweep/log-loss 0.9 0.0)))
  (testing "a certain chance that misses is bounded, not infinite"
    (is (< (sweep/log-loss 1.0 0.0) 10.0))))

(deftest value-gain-is-saving-when-kept-and-a-cost-when-lost
  (is (= 7.0 (sweep/value-gain {:value 5 :amount 12 :top 4})) "still beats the top rival")
  (is (= 12.0 (sweep/value-gain {:value 0 :amount 12 :top nil})) "nobody else bid")
  (is (= (- sweep/lost-claim) (sweep/value-gain {:value 3 :amount 12 :top 4})))
  (is (= (- (* 0.5 8) (* 0.5 sweep/lost-claim))
         (sweep/value-gain {:value 4 :amount 12 :top 4}))
      "a tie is half of each"))

(deftest who-blocks-sum-by-league-week
  (let [bs (sweep/who-blocks "L" [{:week 1 :p 0.5 :bid? true}
                                  {:week 1 :p 0.1 :bid? false}
                                  {:week 2 :p 0.2 :bid? false}])]
    (is (= #{"L:w1" "L:w2"} (set (keys bs))))
    (is (= 2 (get-in bs ["L:w1" :n])))
    (is (= 1.0 (get-in bs ["L:w1" :y])))
    (is (< (Math/abs (- 0.6 (get-in bs ["L:w1" :p]))) 1e-12))))

(deftest ratio-stat-pools-over-counts-not-blocks
  (is (= 0.25 ((sweep/ratio-stat :d :n) [{:d 1.0 :n 1} {:d 0.0 :n 3}]))
      "a block of three outweighs a block of one")
  (is (= 0.0 ((sweep/ratio-stat :d :n) [])) "nothing to pool"))

(deftest paired-blocks-difference-the-same-auctions
  (let [base {:who  {"L:w1" {:n 10 :ll 2.0}}
              :bids [{:block "L:w1" :win-ll 1.0 :gain 3.0}
                     {:block "L:w1" :win-ll 0.5 :gain nil}]}
        cand {:who  {"L:w1" {:n 10 :ll 1.5}}
              :bids [{:block "L:w1" :win-ll 0.8 :gain 4.0}
                     {:block "L:w1" :win-ll 0.5 :gain nil}]}
        [row] (sweep/paired-blocks base cand)]
    (is (= "L:w1" (:season row)) "the bootstrap's block")
    (is (= [10 -0.5] [(:who-n row) (:who-d row)]))
    (is (= 2 (:win-n row)))
    (is (< (Math/abs (- -0.2 (:win-d row))) 1e-12))
    (is (= [1 1.0] [(:gain-n row) (:gain-d row)]) "only winners the value bid priced")))

(deftest cached-rival-needs-reuses-a-team-only-on-the-same-board
  (let [calls    (atom 0)
        original (fn [teams _ _ by-id _ _ fas]
                   (swap! calls inc)
                   (mapv (fn [t] {:roster-id (:roster-id t)
                                  :needs (zipmap (map :player-id fas)
                                                 (map #(- (:ros-points %)
                                                          (get-in by-id [(first (:active-ids t)) :ros-points]))
                                                      fas))})
                         teams))
        needs    (sweep/cached-rival-needs original)
        teams    [{:roster-id 1 :active-ids ["a"]} {:roster-id 2 :active-ids ["b"]}]
        by-id    {"a" {:ros-points 10.0} "b" {:ros-points 4.0}}
        fas      [{:player-id "x" :ros-points 12.0}]]
    (reset! sweep/needs-cache {})
    (is (= [{:roster-id 2 :needs {"x" 8.0}}]
           (map #(select-keys % [:roster-id :needs]) (needs teams 1 nil by-id 9 [] fas)))
        "every team but mine")
    (is (= 1 @calls))
    (needs teams 3 nil by-id 9 [] fas)
    (is (= 2 @calls) "team 1 is new; team 2 comes from the cache")
    (needs teams 3 nil (assoc-in by-id ["a" :ros-points] 11.0) 9 [] fas)
    (is (= 3 @calls) "a board that moved his players is a new question")
    (is (= {"x" 1.0} (:needs (first (needs teams 3 nil (assoc-in by-id ["a" :ros-points] 11.0) 9 [] fas))))
        "and the answer is the new board's")))
