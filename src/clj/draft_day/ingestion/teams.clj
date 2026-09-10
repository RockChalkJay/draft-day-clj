(ns draft-day.ingestion.teams
  "Vendor NFL team abbreviations, translated into the app's.

  The app speaks Sleeper's vocabulary, because `:team` on every player comes
  from Sleeper and every join that reaches a player by team has to land there.
  Every other source is normalized on the way in.

  WHY THIS IS A NAMESPACE AND NOT TWO INLINE `case` FORMS. Both known deviations
  are one team out of thirty-two, which is the worst possible shape: a join
  matching 31 of 32 reports a 97% hit rate, looks like an ordinary thin vendor,
  and quietly empties one team's column forever. It is the family CLAUDE.md
  describes for the FantasyPros injury badge — 'a source's hit rate is
  matched/rows'. The guard has to be a test over the *whole* vocabulary, and a
  test needs one function to point at.

  THE TWO DEVIATIONS, found by diffing each vendor's live abbreviations against
  Sleeper's rather than by reading documentation:

    ESPN      WSH   Sleeper WAS   (scoreboard, 2026 weeks 1-3, all 32 teams)
    nflverse  LA    Sleeper LAR   (stats_player_week_2025.csv, `team` and
                                   `opponent_team`; the file carries no `LAR`)

  `normalize` translates and does not validate, so an unknown abbreviation
  passes through rather than being dropped. `covers-vocabulary?` is the net:
  the tests run it over each vendor's real set, so a third deviation fails a
  test instead of blanking a column."
  (:require [clojure.string :as str]))

(def app-teams
  "Written out rather than derived from the sample universe: a bad fetch must
  not be able to move the standard it is supposed to be checked against."
  #{"ARI" "ATL" "BAL" "BUF" "CAR" "CHI" "CIN" "CLE" "DAL" "DEN" "DET" "GB"
    "HOU" "IND" "JAX" "KC" "LAC" "LAR" "LV" "MIA" "MIN" "NE" "NO" "NYG"
    "NYJ" "PHI" "PIT" "SEA" "SF" "TB" "TEN" "WAS"})

(def aliases
  "vendor -> {their-spelling ours}, per vendor rather than merged: applying
  nflverse's `LA` rule to an ESPN payload is how an alias map invents a join."
  {:espn     {"WSH" "WAS"}
   :nflverse {"LA"  "LAR"}})

(defn normalize
  "nil for blank, so a source with no opinion is not a lookup for `\"\"`. Case
  and whitespace are tolerated because a CSV cell is not a JSON field."
  [vendor team]
  (when-not (str/blank? team)
    (let [t (str/upper-case (str/trim team))]
      (get-in aliases [vendor t] t))))

(defn covers-vocabulary?
  "Both directions matter: a leftover unmapped spelling and a team the vendor
  stopped publishing are different bugs and both have to fail."
  [vendor vendor-teams]
  (= app-teams (into #{} (map #(normalize vendor %)) vendor-teams)))
