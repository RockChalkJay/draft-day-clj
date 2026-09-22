(ns draft-day.ingestion.sleeper-actual
  "What each player has actually done, by Sleeper's own count and in Sleeper's
  own vocabulary. The realized half of the same house that projects the board.

  WHY A SECOND REALIZED SOURCE. nflverse already publishes a weekly line and
  `nflverse-weekly` already reads it, but it answers a different question than
  the one a scored board asks. Three things it cannot do, each measured rather
  than assumed:

  It has no team defense at all — not a sparse one, none — so a defense's
  realized production is permanently empty, `rankings.ros` blends it against
  nothing, and the tier rules that are over a third of a defense's score have
  no realized side to land on.

  It counts some stats differently from the league that pays for them. Against
  Sleeper's own 2026 weeks 1-3, nflverse's first downs read higher on a quarter
  to three quarters of the player-weeks either side reports, and it files a
  blocked field goal under `fg_blocked` where Sleeper counts it a miss. Those
  are not errors on anyone's part, they are two vocabularies — but only one of
  them is the one the standings are kept in.

  And it publishes no count of a touchdown's length, so the long-touchdown
  rules a real league states have nowhere to come from.

  Scoring Sleeper's own line under a league's own rules reproduces Sleeper's own
  per-player points exactly — 166 of 166 rostered players on the week that was
  measured — which is the property this module exists to give the board.

  ONE REQUEST PER WEEK PLAYED, and that is the cost. Sleeper has a
  season-to-date endpoint that would answer in one, but it cannot answer the
  week-by-week question the player detail modal asks, and accumulating the
  weekly answers gives both from one fetch set rather than keeping two that can
  disagree. Best-effort and parallel for `pipeline`'s usual reason: a realized
  line that fails to arrive leaves a preseason board, which is a worse board and
  not a broken one.

  PRESENCE IS AN APPEARANCE. Sleeper answers a week with an entry per player who
  was active in it, so a week with no entry is a week he did not play — the
  distinction `game-log` draws to keep a missed week from reading as a zero.
  `:games` therefore counts entries, never weeks elapsed, which is the same
  thing `nflverse-weekly/accumulate` counts and for the same reason."
  (:require [clojure.tools.logging :as log]
            [jsonista.core :as json]
            [org.httpkit.client :as http]
            [draft-day.ingestion.parallel :as parallel]
            [draft-day.json :refer [mapper]]
            [draft-day.scoring :as scoring]))

(def ^:private base "https://api.sleeper.app")

(def ^:private positions
  "The positions to ask for. Sleeper's stats endpoint requires the filter and
  answers an unfiltered request with everything it has, which is an order of
  magnitude more body for players no fantasy league seats."
  ["QB" "RB" "WR" "TE" "K" "DEF"])

(def recent-window
  "How many of the most recent weeks `:realized/recent` spans.

  Three, the same window `nflverse-weekly/recent-window` spans and a chosen
  constant for the same reason: one week is a game script and much beyond three
  reaches past the usage change a manager is trying to see. The two are
  deliberately equal — a form signal that meant a different number of weeks
  depending on which source answered would be unreadable."
  3)

(defn- week-url [season week]
  (str base "/stats/nfl/" season "/" week "?season_type=regular"
       (apply str (map #(str "&position[]=" %) positions))))

(defn fetch-week
  "Network: raw stat entries for one week of a season (throws on failure)."
  [season week]
  (let [{:keys [status body error]} @(http/get (week-url season week)
                                               {:timeout 30000})]
    (cond
      error          (throw (ex-info "Sleeper stats fetch failed"
                                     {:week week :error error}))
      (= 200 status) (json/read-value body mapper)
      :else          (throw (ex-info "Sleeper stats non-200"
                                     {:week week :status status})))))

(defn scored-stats
  "The subset of a Sleeper stats map the scoring engine reads, as doubles.

  A zero is dropped rather than carried, so a stat missing from a week he played
  reads back as the zero it is while a week he missed has no entry at all — the
  same rule `nflverse-weekly` keeps, and what lets `game-log` tell the two
  apart."
  [stats]
  (into {} (keep (fn [k]
                   (when-let [v (get stats k)]
                     (when-not (zero? v) [k (double v)]))))
        scoring/stat-keys))

(defn entry->row
  "One raw entry -> `{:id :week :opponent :stats}`, or nil when it is not an
  appearance this module can score."
  [{:keys [player_id stats week opponent]}]
  (when (and player_id stats week)
    {:id       player_id
     :week     (long week)
     :opponent opponent
     :stats    (scored-stats stats)}))

(defn- totals
  "`{:games n :stats {...}}` over a player's rows."
  [rows]
  {:games (count rows)
   :stats (reduce (fn [acc {:keys [stats]}] (merge-with + acc stats)) {} rows)})

(defn game-log
  "A player's rows as ordered per-week entries, oldest first."
  [rows]
  (mapv (fn [{:keys [week opponent stats]}]
          (cond-> {:week week :stats stats}
            opponent (assoc :opponent opponent)))
        (sort-by :week rows)))

(defn accumulate
  "Pure: rows -> the three per-player columns, keyed by *Sleeper* id.

  Sleeper's id space, not the universe's — `:player-id` there is the GSIS id
  wherever one resolves, and these arrive as Sleeper sends them. Crossing the
  two is the join's job, exactly as it is for the weekly projection line."
  [rows]
  (let [latest (reduce max 0 (map :week rows))
        floor  (- latest (dec recent-window))]
    (reduce-kv
     (fn [acc id player-rows]
       (let [recent (filterv #(>= (:week %) floor) player-rows)]
         (assoc acc id
                (cond-> {:realized/season-to-date (totals player-rows)
                         :realized/game-log       (game-log player-rows)}
                  (seq recent) (assoc :realized/recent (totals recent))))))
     {}
     (group-by :id rows))))

(def ^:private max-in-flight
  "How many weeks to ask for at once.

  Politeness lives with the vendor here as it does in `fantasypros/fetch-page`,
  and for a milder version of the same reason: a December season is eighteen
  requests of a couple of megabytes each, and asking a free keyless host for all
  of it simultaneously is how a well-behaved client starts looking like a bad
  one. Four keeps a cold load to a handful of rounds."
  4)

(defn fetch
  "Network: `{:by-key {sleeper-id columns} :through-week n}` for a season's
  played weeks, or nil when nothing arrived.

  `through-week` is asked for rather than discovered, because the caller already
  knows it and probing would mean a request per week that has not happened yet.
  A week that fails is dropped rather than failing the set: a realized line one
  week short still beats a preseason one."
  [season through-week]
  (when (and (number? through-week) (pos? through-week))
    (let [weeks (range 1 (inc (long through-week)))
          got   (mapcat (fn [batch]
                          (vals (parallel/all
                                 (into {} (map (fn [w]
                                                 [w (fn []
                                                      (try (fetch-week season w)
                                                           (catch Exception e
                                                             (log/warn e "sleeper-actual: week" w "failed")
                                                             nil)))]))
                                       batch))))
                        (partition-all max-in-flight weeks))
          rows  (into [] (comp cat (keep entry->row)) got)]
      (when (seq rows)
        {:by-key       (accumulate rows)
         :through-week (reduce max 0 (map :week rows))}))))
