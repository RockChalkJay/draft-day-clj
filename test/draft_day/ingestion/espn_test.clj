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
  (let [m (espn/enrichment 2026 sample)]
    (is (= 59.69 (:espn/auction-value (get m (match/key-for "Bijan Robinson" "RB")))))
    (is (= 2.2 (:espn/adp (get m (match/key-for "Bijan Robinson" "RB")))))
    (is (= 1.0 (:espn/auction-value (get m (match/key-for "Some Kicker" "K")))))   ; PPR fallback
    (is (nil? (get m (match/key-for "Deep Nobody" "RB"))))))                        ; no value

(deftest enrichment-handles-bare-array
  ;; accepts a bare array (not just {:players ...}); Bijan + Kicker have values
  (is (= 2 (count (espn/enrichment 2026 (:players sample))))))

;; ---- projections ----
;; ESPN ships one stats entry per (season, source, split). The current season's
;; *actual* block exists too and is all zeros until games are played, so the
;; source id is what separates a projection from a flat nothing.
;;
;; Stat ids are KEYWORDS here, not strings, because that is what
;; `draft-day.json/mapper` produces — it keywordizes every key on decode. A
;; fixture with string ids is self-consistent with string lookups and passes
;; while the real feed yields nothing, which is exactly what happened.

(def ^:private projected
  {:fullName "Ja'Marr Chase" :defaultPositionId 3
   :stats [{:seasonId 2026 :statSourceId 1 :statSplitTypeId 0
            :stats {:58 172.4 :53 119.7 :42 1508.8}}
           {:seasonId 2026 :statSourceId 0 :statSplitTypeId 0
            :stats {:58 0 :53 0 :42 0}}
           {:seasonId 2025 :statSourceId 0 :statSplitTypeId 0
            :stats {:58 185.0 :53 125.0 :42 1412.0}}
           {:seasonId 2026 :statSourceId 1 :statSplitTypeId 1
            :stats {:58 10.1 :53 7.0}}]})

(deftest projected-usage-picks-the-season-projection-block
  (let [u (espn/projected-usage 2026 projected)]
    (is (= 172.4 (:espn/proj-targets u)))
    (is (= 119.7 (:espn/proj-receptions u))))
  (is (nil? (espn/projected-usage 2027 projected))
      "a season ESPN has no projection for yields nothing, not zeros")
  (is (nil? (espn/projected-usage 2026 {:fullName "No Stats" :stats []}))))

(deftest a-projected-but-unpriced-player-is-kept
  ;; ESPN projects far more players than it prices. Gating on the auction value
  ;; alone would throw all of those projections away.
  (let [m (espn/enrichment 2026 [projected])
        e (get m (match/key-for "Ja'Marr Chase" "WR"))]
    (is (= 172.4 (:espn/proj-targets e)))
    (is (nil? (:espn/auction-value e)))))

(deftest a-priced-player-carries-both
  (let [priced (assoc projected
                      :ownership {:auctionValueAverage 61.0 :averageDraftPosition 1.2})
        e (get (espn/enrichment 2026 [priced]) (match/key-for "Ja'Marr Chase" "WR"))]
    (is (= 61.0 (:espn/auction-value e)))
    (is (= 1.2 (:espn/adp e)))
    (is (= 119.7 (:espn/proj-receptions e)))))

(deftest a-player-with-neither-price-nor-projection-is-still-excluded
  (is (empty? (espn/enrichment 2026 [{:fullName "Deep Nobody" :defaultPositionId 2
                                      :ownership {:auctionValueAverage 0.0}
                                      :draftRanksByRankType {:PPR {:auctionValue 0}}}]))))
