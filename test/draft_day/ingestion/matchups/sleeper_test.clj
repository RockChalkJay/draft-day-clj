(ns draft-day.ingestion.matchups.sleeper-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.ingestion.league-sync.sleeper :as sync-sleeper]
            [draft-day.ingestion.matchups :as matchups]
            [draft-day.ingestion.matchups.sleeper]))

(defn- week-for [state]
  (with-redefs [sync-sleeper/get-json (fn [_ _] state)]
    (matchups/current-week :sleeper {})))

(deftest a-tuesday-in-season-is-the-next-week
  (testing "display_week holds the finished week until Wednesday"
    (is (= 4 (week-for {:season_type "regular" :week 4 :display_week 3}))))
  (is (= 4 (week-for {:season_type "regular" :display_week 4}))
      "display_week when week is missing"))

(deftest outside-the-season-the-week-is-display-week
  (is (nil? (week-for {:season_type "pre" :week 2 :display_week 0}))
      "a preseason week is no week")
  (is (= 18 (week-for {:season_type "off" :week 1 :display_week 18})))
  (is (nil? (week-for {:season_type "off" :display_week 0}))))
