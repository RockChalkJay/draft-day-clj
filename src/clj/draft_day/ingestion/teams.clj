(ns draft-day.ingestion.teams
  "The one place a vendor's NFL team abbreviation is translated into the app's.

  The app's vocabulary is Sleeper's, because `:team` on every player comes from
  Sleeper and every join that reaches a player by team has to land in that
  space. Every other source is normalized on the way in.

  WHY THIS IS A NAMESPACE AND NOT TWO INLINE `case` FORMS. Both known
  deviations are single teams out of thirty-two, which is the worst possible
  shape: a join that matches 31 of 32 reports a 97% hit rate, looks like an
  ordinary thin vendor, and quietly empties one team's column forever. It is the
  same family as the FantasyPros injury badge CLAUDE.md describes — 'nothing
  caught it for a month because a source's hit rate is matched/rows'. A guard
  has to be a test over the *whole* vocabulary, and a test needs one function to
  point at.

  THE TWO DEVIATIONS, AND HOW THEY WERE FOUND. Both by diffing a vendor's live
  abbreviation set against Sleeper's, not by reading documentation:

    ESPN      WSH   Sleeper WAS   (scoreboard, 2026 weeks 1-3, all 32 teams)
    nflverse  LA    Sleeper LAR   (stats_player_week_2025.csv, `team` and
                                   `opponent_team`; the file contains no `LAR`)

  `normalize` leaves anything it has no alias for alone, so a vendor that agrees
  costs nothing and a *new* team abbreviation is passed through rather than
  dropped. The safety net is `covers-vocabulary?`, which the tests run over each
  vendor's real set: a third deviation fails a test instead of blanking a column."
  (:require [clojure.string :as str]))

(def app-teams
  "The thirty-two abbreviations the app speaks, which are Sleeper's.

  Written out rather than derived from the sample universe: this is the fixed
  point every vendor is measured against, and deriving it from data would let a
  bad fetch move the standard it is supposed to be checked against."
  #{"ARI" "ATL" "BAL" "BUF" "CAR" "CHI" "CIN" "CLE" "DAL" "DEN" "DET" "GB"
    "HOU" "IND" "JAX" "KC" "LAC" "LAR" "LV" "MIA" "MIN" "NE" "NO" "NYG"
    "NYJ" "PHI" "PIT" "SEA" "SF" "TB" "TEN" "WAS"})

(def aliases
  "vendor -> {their-spelling ours}, holding only the teams that actually differ.

  Deliberately per vendor rather than one merged map. The two vendors disagree
  with Sleeper about different teams, and a shared map would apply nflverse's
  `LA` rule to an ESPN payload that has never used it — translating a spelling
  the source does not have is how an alias map starts inventing joins."
  {:espn     {"WSH" "WAS"}
   :nflverse {"LA"  "LAR"}})

(defn normalize
  "`vendor`'s spelling of a team as the app spells it.

  Unknown vendors and unknown abbreviations pass through untouched — this
  translates, it does not validate. nil and blank stay nil, so a source with no
  opinion about a team is not turned into a lookup for the empty string.
  Whitespace and case are handled because a CSV cell is not a JSON field."
  [vendor team]
  (when-not (str/blank? team)
    (let [t (str/upper-case (str/trim team))]
      (get-in aliases [vendor t] t))))

(defn covers-vocabulary?
  "Does normalizing every abbreviation in `vendor-teams` land exactly on the
  app's thirty-two?

  The guard the aliases exist for, and it is the assertion rather than a
  spot-check of the known deviations: a vendor that renames a team next season,
  or a thirty-third franchise, fails here instead of silently emptying a column.
  Both directions matter — a leftover unmapped spelling and a team the vendor
  stopped publishing are different bugs and both are a failure."
  [vendor vendor-teams]
  (= app-teams (into #{} (map #(normalize vendor %)) vendor-teams)))
