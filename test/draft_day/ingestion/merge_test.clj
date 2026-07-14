(ns draft-day.ingestion.merge-test
  (:require [clojure.test :refer [deftest is]]
            [draft-day.ingestion.merge :as merge]
            [draft-day.ingestion.match :as match]))

(deftest normalize-collapses-punctuation-and-suffixes
  (is (= (match/key-for "T.J. Hockenson" "TE") (match/key-for "TJ Hockenson" "TE")))
  (is (= "bijanrobinson_rb" (match/key-for "Bijan Robinson" "RB")))
  (is (= (match/key-for "Michael Pittman Jr." "WR") (match/key-for "Michael Pittman" "WR"))))

(deftest left-join-attaches-columns-only
  (let [universe   [{:player-id "9509" :player-name "Bijan Robinson" :position "RB"}
                    {:player-id "z" :player-name "Unmatched Guy" :position "WR"}]
        enrichment {(match/key-for "Bijan Robinson" "RB") {:fantasypros/ecr 2 :bye 11}}
        joined     (merge/left-join universe enrichment)]
    (is (= 2 (count joined)))                        ; universe rows preserved (no rows added/removed)
    (is (= 2 (:fantasypros/ecr (first joined))))     ; matched row gets columns
    (is (= 11 (:bye (first joined))))
    (is (nil? (:fantasypros/ecr (second joined)))))) ; unmatched row unchanged
