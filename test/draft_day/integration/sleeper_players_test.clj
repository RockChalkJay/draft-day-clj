(ns draft-day.integration.sleeper-players-test
  (:require [clojure.test :refer [deftest is]]
            [draft-day.ingestion.sleeper-players :as sp]))

(deftest ^:integration the-live-list-has-the-shape-we-read
  (let [raw (sp/fetch-raw)
        out (sp/normalize raw)]
    (is (map? raw))
    (is (> (count raw) 1000) "a dump of every player, not a handful")
    (is (every? string? (keys out)))
    (is (every? (comp string? :status) (vals out)))))
