(ns draft-day.ingestion.matchups.sleeper
  "Sleeper provider for head-to-head matchups.

  Two documents in sequence, because the second is addressed by what the first
  returns: `state/nfl` says which week Sleeper is showing, and
  `league/{id}/matchups/{week}` is that week's scoreboard. Both go through
  `league-sync.sleeper/get-json`, the one copy of Sleeper's habit of answering
  an unknown id with HTTP 200 and a JSON null body.

  The week is `display_week`, the one Sleeper's own app shows a manager, with
  `week` as the fallback — they diverge either side of the season, and a missing
  `display_week` is a shape change rather than an offseason.

  The scoreboard alone passes `empty-is-missing? false`: Sleeper answers a valid
  league asked for an out-of-season week with `[]`, so empty is a real board
  here and not a missing league."
  (:require [draft-day.ingestion.league-sync.sleeper :as sync-sleeper]
            [draft-day.ingestion.matchups :as matchups]))

(defmethod matchups/current-week :sleeper
  [_ _req]
  (let [state (sync-sleeper/get-json "state/nfl"
                                     {:empty-is-missing? true
                                      :not-found-msg "Sleeper season state unavailable"})]
    (let [wk (or (:display_week state) (:week state))]
      (when (and (number? wk) (pos? wk)) wk))))

(defmethod matchups/fetch-raw-matchups :sleeper
  [_ {:keys [league-id week]}]
  (sync-sleeper/get-json (str "league/" league-id "/matchups/" week)
                         {:empty-is-missing? false}))

(defn matchup-pairs
  "Pure: raw entries -> `[{:matchup-id :roster-ids [...]}]`, one per game.

  Sorted by matchup id so the board's order does not follow Sleeper's
  serialization. A roster with no `matchup_id` becomes its own single-id entry."
  [entries]
  (let [grouped (group-by :matchup_id entries)
        byes    (get grouped nil)]
    (into (vec (for [[mid es] (sort-by key (dissoc grouped nil))]
                 {:matchup-id mid :roster-ids (mapv :roster_id es)}))
          (for [e byes]
            {:matchup-id nil :roster-ids [(:roster_id e)]}))))

(defn player-points
  "Pure: one entry's `players_points` with string player ids.

  `draft-day.json/mapper` keywordizes every key it decodes and these keys are
  player ids, so `{\"4034\": 18.4}` arrives as `{:4034 18.4}` while the crosswalk
  holds strings. Untouched, the join misses the whole board and reads like a
  vendor publishing nothing."
  [entry]
  (update-keys (or (:players_points entry) {}) name))

(defn roster-score
  "Pure: one raw entry -> `[roster-id {:official :starter-ids :player-ids
  :player-points}]`.

  `:official` prefers `custom_points`, a commissioner's correction and null on
  an ordinary matchup, over the computed `points`. `:starter-ids` is the lineup
  that counted for this week, not the roster's lineup as it stands now, and
  `:player-ids` the roster as of this week rather than as of the last sync."
  [entry]
  [(:roster_id entry)
   {:official      (or (:custom_points entry) (:points entry))
    :starter-ids   (vec (:starters entry))
    :player-ids    (vec (:players entry))
    :player-points (player-points entry)}])

(defmethod matchups/normalize-matchups :sleeper
  [_ entries]
  {:matchups (matchup-pairs entries)
   :scores   (into {} (map roster-score) entries)})
