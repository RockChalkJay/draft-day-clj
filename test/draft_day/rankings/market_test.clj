(ns draft-day.rankings.market-test
  (:require [clojure.test :refer [deftest is]]
            [draft-day.rankings.market :as market]))

;; baselines: ESPN $2000 pool, FantasyPros $2400 pool.

(deftest market-normalizes-single-source-to-league-pool
  ;; ESPN $40 of a $2000 pool = 2% -> 2% of a 12x$200 = $2400 pool = $48.
  (is (= 48 (market/market-price {:espn/auction-value 40.0} 2400.0)))
  ;; FP $48 of a $2400 pool = 2% -> 2% of a 10x$100 = $1000 pool = $20.
  (is (= 20 (market/market-price {:fantasypros/aav 48.0} 1000.0))))

(deftest market-averages-available-sources
  ;; ESPN 40/2000=2% and FP 60/2400=2.5% into a $2400 pool -> $48 and $60 -> mean $54.
  (is (= 54 (market/market-price {:espn/auction-value 40.0 :fantasypros/aav 60.0} 2400.0))))

(deftest market-nil-when-no-source-or-pool
  (is (nil? (market/market-price {} 2400.0)))
  (is (nil? (market/market-price {:espn/auction-value 0.0} 2400.0)))       ; non-positive raw
  (is (nil? (market/market-price {:espn/auction-value 40.0} 0.0))))        ; empty room

(deftest edge-is-worth-minus-market
  (is (= 8 (market/edge 40 32)))
  (is (= -5 (market/edge 20 25))))

(deftest edge-nil-without-worth-or-market
  (is (nil? (market/edge 0 32)))        ; K/DST/drafted -> worth 0
  (is (nil? (market/edge nil 32)))
  (is (nil? (market/edge 40 nil))))

(deftest with-market-assocs-both-keys
  (let [[a b] (market/with-market
                [{:worth 50 :espn/auction-value 40.0}   ; market 48, edge +2
                 {:worth 10}]                            ; no source -> nil market, nil edge
                2400.0)]
    (is (= 48 (:market a)))
    (is (= 2 (:edge a)))
    (is (nil? (:market b)))
    (is (nil? (:edge b)))))
