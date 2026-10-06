(ns draft-day.team-colors-test
  (:require [clojure.test :refer [deftest is]]
            [draft-day.ingestion.teams :as teams]
            [draft-day.team-colors :as tc]))

(deftest every-team-the-app-knows-has-colours
  (is (= teams/app-teams (set (keys tc/colors)))))

(deftest every-colour-is-a-six-digit-hex
  (is (every? #(re-matches #"#[0-9A-F]{6}" %) (mapcat val tc/colors))))

(deftest the-darker-colour-leads
  (is (= ["#101820" "#FFB612"] (tc/pair "PIT")) "a pale lead would sink the white name")
  (is (= ["#003594" "#FFA300"] (tc/pair "LAR")))
  (is (< (tc/luminance "#000000") (tc/luminance "#FFFFFF"))))

(deftest an-unknown-team-has-no-gradient
  (is (nil? (tc/gradient nil)))
  (is (nil? (tc/gradient "XXX")))
  (is (re-find #"^linear-gradient\(110deg, #101820 0%" (tc/gradient "PIT"))))
