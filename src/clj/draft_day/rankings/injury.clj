(ns draft-day.rankings.injury
  "Injury risk: a 1-5 durability scale from how much of the last few seasons a
  player was actually available for, floored by a serious current designation.

  DISPLAY ONLY. Like `:tcm`, and for the same reason, this never reaches Value or
  Worth — it is a column on the board, not a term in the price. Two things make
  that a deliberate line rather than an oversight. Market prices already embody
  the room's own injury opinion, so discounting Worth by risk would charge a
  fragile player twice; and the repo has been here before, with the positional
  demand multiplier that was computed on every pick and fed nothing until it was
  removed. If risk should ever move money, that is a formula change to be earned
  in `dev/draft_day/benchmark/`, not a quiet multiply here.

  It is *static* — a function of completed seasons and a preseason designation,
  neither of which a draft pick changes — so it is computed once in
  `engine/static-rankings` rather than per-pick alongside `tcm`.

  WHAT THE SCALE ACTUALLY MEASURES is availability, not injury. The evidence is
  games played (see `ingestion.nflverse` on why the weekly injury report cannot
  do this job), and a game missed for a benching, a suspension or a depth-chart
  demotion counts exactly like a game missed for a hamstring. That mostly bites
  backup quarterbacks, who look fragile here while being perfectly healthy. For a
  board whose question is \"how many games do I get for my money\" that is
  arguably the honest reading either way, but the column must not be sold as a
  medical claim, hence `:injury/reason` saying \"games missed\" and not \"injured\".

  THE DENOMINATOR IS YEARS IN THE LEAGUE, not the width of the window. Scored
  over a flat three seasons the most fragile players in football come out as last
  year's rookies, who were not in the league for the first two — measured that
  way, 2025's rookie class filled the entire top of the list. `:sleeper/years-exp`
  counts *completed* seasons (a 2021 rookie reads 5 in 2026), so
  `min(window, years-exp)` is exactly how many prior seasons a player could have
  played, and a true rookie gets 0 and therefore no opinion at all."
  (:require [clojure.string :as str]
            [draft-day.db :as db]))

(def band-thresholds
  "Games missed per season at which the scale steps up, so levels are
  1..(inc (count band-thresholds)).

  Calibrated against the *draftable* population rather than chosen for roundness:
  across the top 200 PPR scorers of 2025 the missed-game rates run p25 0.06,
  p50 0.12, p75 0.22, p90 0.37 of a season, and these cuts spread them
  29/43/57/36/35 over the five bands (32/34/51/31/29 on the committed sample's
  top 200 by ECR — the same shape). No band is starved and none swallows the
  board, which is the only property a display scale really has to have: a level
  nobody occupies teaches a manager nothing, and one half the board occupies
  teaches him less.

  The whole universe is a different distribution — roughly half of all 634 rows
  land on 5 — and that is not miscalibration. Past about ECR 300 the board is
  fringe roster bodies who genuinely were not available, and the bands are cut
  for the players a manager might actually bid on. Anyone reading the tail is
  reading the availability conflation the ns docstring describes, working as
  described.

  Expressed in games rather than rates because that is the unit the evidence is
  quoted in and the unit a manager thinks in."
  [0.5 1.5 3.0 5.0])

(def max-level (inc (count band-thresholds)))

(defn window
  "The seasons this player is actually judged on: the most recent
  `min(count fetched, years-exp)` of the seasons ingestion managed to fetch.

  Both halves matter. `years-exp` keeps a rookie from being charged for seasons
  before he existed; the fetched set keeps everyone from being charged for a
  season the network lost, since ingestion records what it got rather than
  assuming it got the lot (see `nflverse/availability`).

  `season-lengths` is nflverse's {season games-in-that-season} map, so the scale
  never needs the NFL calendar of its own."
  [season-lengths years-exp]
  (let [seasons (sort (keys season-lengths))
        n       (min (count seasons) (or years-exp 0))]
    (if (pos? n) (vec (take-last n seasons)) [])))

(defn missed-per-season
  "Average games missed per season across `seasons`, or nil when there is nothing
  to average.

  A season with no entry counts as a season missed entirely. That is right for a
  starter who lost a year to injury and wrong for a depth player who was never
  active, which is why `risk-for` refuses to score a player with no entries at
  all rather than calling him maximally fragile on the strength of never having
  played."
  [games-by-season season-lengths seasons]
  (when (seq seasons)
    (/ (reduce (fn [acc season]
                 (let [full   (double (get season-lengths season 0))
                       played (get games-by-season season 0.0)]
                   (+ acc (max 0.0 (- full played)))))
               0.0 seasons)
       (double (count seasons)))))

(defn band
  "Games missed per season -> a 1..max-level level."
  [mps]
  (inc (count (take-while #(>= mps %) band-thresholds))))

(defn- games-phrase [missed seasons]
  (format "%.1f games missed per season over %d season%s"
          missed (count seasons) (if (= 1 (count seasons)) "" "s")))

(defn risk-for
  "Pure: one player -> the injury columns, or nil when there is no opinion to
  offer. See the ns docstring for what the number means.

  Returns `{:injury-risk :injury/games-missed :injury/seasons :injury/reason}`.
  `:injury/reason` is the cell's only text — the board renders the level as a bar
  with no number — so it always names both the level and the evidence behind it."
  [{:keys [:sleeper/years-exp :sleeper/injury-status
           :nflverse/games-by-season :nflverse/games-seasons]}]
  ;; The floor exists so a season-ending situation cannot hide behind a clean
  ;; history while the Inj column is switched off. It is `db/serious-injury?` and
  ;; not "has any designation": in August 96 players carry Questionable, and
  ;; letting a tag that usually resolves by Sunday move a *durability* score
  ;; would push a fifth of the board up a band for nothing.
  (let [seasons (window games-seasons years-exp)
        seen?   (some #(contains? games-by-season %) seasons)
        mps     (when seen? (missed-per-season games-by-season games-seasons seasons))
        level   (some-> mps band)
        floored (when (db/serious-injury? injury-status) max-level)
        final   (cond (and floored level) (max floored level)
                      :else               (or floored level))]
    (when final
      (cond-> {:injury-risk final
               :injury/reason
               (str "Risk " final " of " max-level " — "
                    (cond->> (if mps (games-phrase mps seasons) "no games on record")
                      floored (str (str/trim (str injury-status)) ", ")))}
        mps (assoc :injury/games-missed (* mps (count seasons))
                   :injury/seasons      (count seasons))))))

(defn with-injury-risk
  "Assoc the injury columns on every player. A player the scale has no opinion
  about — a rookie, or anyone with no fetched season on record and no serious
  designation — is left untouched, so the board renders a dash rather than
  claiming he is the safest man in football."
  [board]
  (mapv (fn [p] (merge p (risk-for p))) board))
