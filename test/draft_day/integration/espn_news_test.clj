(ns draft-day.integration.espn-news-test
  "Live ESPN news contract. Keyless, so it runs under `lein test :integration`
  with no environment. Asserts keys and types only: what a player's news says
  changes by the hour."
  (:require [clojure.test :refer [deftest is]]
            [draft-day.ingestion.espn-news :as news]))

;; Jaxon Smith-Njigba: several seasons of Rotowire items.
(def espn-id "4430878")

(deftest ^:integration feed-has-the-shape-the-parser-reads
  (let [feed  (news/fetch-feed espn-id)
        items (news/parse-feed feed)]
    (is (sequential? (:feed feed)))
    (is (<= (count items) news/max-items))
    (is (every? #(and (string? (:published %)) (string? (:headline %))) items))))

(deftest ^:integration player-news-reply-shape
  (news/reset-cache!)
  (let [r (news/player-news espn-id)]
    (is (= #{:news :fetched-at :errors} (set (keys r))))
    (is (vector? (:news r)))
    (is (vector? (:errors r)))
    (is (string? (:fetched-at r)))))
