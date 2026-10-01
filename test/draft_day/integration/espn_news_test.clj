(ns draft-day.integration.espn-news-test
  "Live ESPN news contract. Keyless, so it runs under `lein test :integration`
  with no environment. Asserts keys and types only: what a player's news says
  changes by the hour."
  (:require [clojure.test :refer [deftest is]]
            [draft-day.ingestion.espn-news :as news]))

;; Jaxon Smith-Njigba: several seasons of Rotowire items.
(def espn-id "4430878")

(deftest ^:integration feed-and-athlete-have-the-shape-the-parsers-read
  (let [feed    (news/fetch-feed espn-id)
        athlete (news/fetch-athlete espn-id)
        items   (news/parse-feed feed)
        status  (news/parse-status athlete)]
    (is (sequential? (:feed feed)))
    (is (map? (:athlete athlete)))
    (is (<= (count items) news/max-items))
    (is (every? #(and (string? (:published %)) (string? (:headline %))) items))
    (is (or (nil? status) (string? (:status status))))))

(deftest ^:integration player-news-reply-shape
  (news/reset-cache!)
  (let [r (news/player-news espn-id)]
    (is (= #{:status :news :fetched-at :errors} (set (keys r))))
    (is (vector? (:news r)))
    (is (vector? (:errors r)))
    (is (string? (:fetched-at r)))))
