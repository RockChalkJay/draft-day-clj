(ns draft-day.rankings.projections-test
  (:require [clojure.test :refer [deftest is]]
            [draft-day.rankings.projections :as proj]))

(defn- one [player] (first (proj/with-floor-ceiling [player])))

(deftest floor-below-mean-ceiling-above
  (let [p (one {:position "RB" :points 200 :fantasypros/rank-std 5 :fantasypros/rank-ave 20})]
    (is (< (:floor p) 200))
    (is (> (:ceiling p) 200))))

(deftest more-disagreement-widens-band
  (let [low  (one {:position "RB" :points 200 :fantasypros/rank-std 2  :fantasypros/rank-ave 20})
        high (one {:position "RB" :points 200 :fantasypros/rank-std 10 :fantasypros/rank-ave 20})]
    (is (> (:ceiling high) (:ceiling low)))       ; wider ceiling
    (is (< (:floor high) (:floor low)))))         ; wider floor

(deftest missing-rank-uses-default-spread
  (let [p (one {:position "RB" :points 200})]
    (is (< (:floor p) 200))
    (is (> (:ceiling p) 200))))

(deftest position-volatility-scales-band
  ;; Same relative disagreement, but TE (k=0.40) bands wider than QB (k=0.20).
  (let [qb (one {:position "QB" :points 200 :fantasypros/rank-std 10 :fantasypros/rank-ave 20})
        te (one {:position "TE" :points 200 :fantasypros/rank-std 10 :fantasypros/rank-ave 20})]
    (is (> (:ceiling te) (:ceiling qb)))))
