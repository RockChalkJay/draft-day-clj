(ns draft-day.team-names-test
  (:require [clojure.test :refer [deftest is]]
            [draft-day.ingestion.teams :as teams]
            [draft-day.team-names :as tn]))

(deftest every-team-the-app-knows-has-a-name
  (is (= teams/app-teams (set (keys tn/names)))))

(deftest an-unknown-team-reads-as-itself
  (is (= "Kansas City Chiefs" (tn/full-name "KC")))
  (is (= "XXX" (tn/full-name "XXX"))))
