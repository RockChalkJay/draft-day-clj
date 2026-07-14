(ns draft-day.ingestion.espn-test
  (:require [clojure.test :refer [deftest is]]
            [draft-day.ingestion.espn :as espn]
            [draft-day.ingestion.match :as match]))

(def ^:private sample
  {:players
   [{:fullName "Bijan Robinson" :defaultPositionId 2
     :ownership {:auctionValueAverage 59.69 :averageDraftPosition 2.2}
     :draftRanksByRankType {:PPR {:auctionValue 56}}}
    {:fullName "Some Kicker" :defaultPositionId 5              ; ownership 0 -> falls back to PPR
     :ownership {:auctionValueAverage 0.0}
     :draftRanksByRankType {:PPR {:auctionValue 1}}}
    {:fullName "Deep Nobody" :defaultPositionId 2              ; no value -> excluded
     :ownership {:auctionValueAverage 0.0}
     :draftRanksByRankType {:PPR {:auctionValue 0}}}]})

(deftest enrichment-extracts-auction-values
  (let [m (espn/enrichment sample)]
    (is (= 59.69 (:espn/auction-value (get m (match/key-for "Bijan Robinson" "RB")))))
    (is (= 2.2 (:espn/adp (get m (match/key-for "Bijan Robinson" "RB")))))
    (is (= 1.0 (:espn/auction-value (get m (match/key-for "Some Kicker" "K")))))   ; PPR fallback
    (is (nil? (get m (match/key-for "Deep Nobody" "RB"))))))                        ; no value

(deftest enrichment-handles-bare-array
  ;; accepts a bare array (not just {:players ...}); Bijan + Kicker have values
  (is (= 2 (count (espn/enrichment (:players sample))))))
