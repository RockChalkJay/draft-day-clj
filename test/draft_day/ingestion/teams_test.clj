(ns draft-day.ingestion.teams-test
  (:require [clojure.test :refer [deftest is]]
            [draft-day.ingestion.teams :as teams]))

;; ---- the two known deviations ----

(deftest espn-spells-washington-differently
  (is (= "WAS" (teams/normalize :espn "WSH"))))

(deftest nflverse-spells-the-rams-differently
  (is (= "LAR" (teams/normalize :nflverse "LA"))))

(deftest an-alias-belongs-to-one-vendor-only
  ;; The maps are per vendor because the vendors disagree about different teams.
  ;; Applying nflverse's rule to an ESPN payload would translate a spelling ESPN
  ;; has never used, which is an alias map inventing a join.
  (is (= "LA"  (teams/normalize :espn "LA"))     "ESPN has no LA to translate")
  (is (= "WSH" (teams/normalize :nflverse "WSH")) "nflverse has no WSH"))

;; ---- translating, not validating ----

(deftest a-team-nobody-renames-passes-through
  (is (= "KC" (teams/normalize :espn "KC")))
  (is (= "KC" (teams/normalize :nflverse "KC")))
  (is (= "KC" (teams/normalize :sleeper "KC")) "an unknown vendor is not an error"))

(deftest nothing-is-nil-and-blank-is-nothing
  ;; A source with no opinion about a team must not become a lookup for "".
  (is (nil? (teams/normalize :espn nil)))
  (is (nil? (teams/normalize :espn "")))
  (is (nil? (teams/normalize :espn "   "))))

(deftest a-csv-cell-is-not-a-json-field
  (is (= "WAS" (teams/normalize :espn " wsh ")))
  (is (= "LAR" (teams/normalize :nflverse "la"))))

;; ---- the guard the aliases exist for ----
;; Both abbreviation sets below are the vendors' real ones, captured by diffing
;; a live payload against Sleeper's rather than read out of documentation:
;; ESPN's from the 2026 scoreboard over weeks 1-3, nflverse's from the `team`
;; and `opponent_team` columns of stats_player_week_2025.csv.

(def espn-teams
  #{"ARI" "ATL" "BAL" "BUF" "CAR" "CHI" "CIN" "CLE" "DAL" "DEN" "DET" "GB"
    "HOU" "IND" "JAX" "KC" "LAC" "LAR" "LV" "MIA" "MIN" "NE" "NO" "NYG"
    "NYJ" "PHI" "PIT" "SEA" "SF" "TB" "TEN" "WSH"})

(def nflverse-teams
  #{"ARI" "ATL" "BAL" "BUF" "CAR" "CHI" "CIN" "CLE" "DAL" "DEN" "DET" "GB"
    "HOU" "IND" "JAX" "KC" "LA" "LAC" "LV" "MIA" "MIN" "NE" "NO" "NYG"
    "NYJ" "PHI" "PIT" "SEA" "SF" "TB" "TEN" "WAS"})

(deftest every-espn-team-lands-in-the-apps-vocabulary
  (is (teams/covers-vocabulary? :espn espn-teams)))

(deftest every-nflverse-team-lands-in-the-apps-vocabulary
  (is (teams/covers-vocabulary? :nflverse nflverse-teams)))

(deftest a-new-deviation-fails-rather-than-emptying-a-column
  ;; The point of the two tests above: they are assertions over the whole
  ;; vocabulary, not spot-checks of the deviations we already know about. A
  ;; vendor that renames a team next season fails here — which is the only
  ;; reason the 31-of-32 join is survivable, since a hit rate cannot see it.
  (is (false? (teams/covers-vocabulary?
               :espn (conj (disj espn-teams "KC") "KAN")))
      "a renamed team must fail")
  (is (false? (teams/covers-vocabulary? :espn (disj espn-teams "KC")))
      "a team the vendor stopped publishing must fail too"))
