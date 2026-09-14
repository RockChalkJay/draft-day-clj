(ns draft-day.ingestion.matchups.sleeper
  "Sleeper provider for head-to-head matchups.

  Two documents, fetched one after the other because the second is addressed by
  what the first returns: `state/nfl` says which week Sleeper is showing, and
  `league/{id}/matchups/{week}` is that week's scoreboard. See
  `matchups/fetch-matchups` on why there is nothing to overlap.

  Both go through `league-sync.sleeper/get-json`, which is the one copy of
  Sleeper's habit of answering an unknown id with HTTP 200 and a JSON null body.
  A second copy of that trap is exactly what this namespace is not for."
  (:require [draft-day.ingestion.league-sync.sleeper :as sync-sleeper]
            [draft-day.ingestion.matchups :as matchups]))

(defmethod matchups/current-week :sleeper
  [_]
  (let [state (sync-sleeper/get-json "state/nfl"
                                     {:empty-is-missing? true
                                      :not-found-msg "Sleeper season state unavailable"})]
    ;; `display_week` over `week`: they diverge either side of the season, and
    ;; the one a manager means by "this week" is the one Sleeper's own app is
    ;; showing him. `week` is the fallback rather than the other way round
    ;; because a missing `display_week` is a shape change, not an offseason.
    (let [wk (or (:display_week state) (:week state))]
      (when (and (number? wk) (pos? wk)) wk))))

(defmethod matchups/fetch-raw-matchups :sleeper
  [_ league-id week]
  ;; `empty-is-missing? false`, unlike every roster fetch beside it. Sleeper
  ;; answers a *valid* league asked for a week outside its season with `[]`, and
  ;; reporting that as "league not found" would be a 404 on the one league we
  ;; already know exists — the id came off a synced league. An unknown id is
  ;; already impossible to reach here for the same reason.
  (sync-sleeper/get-json (str "league/" league-id "/matchups/" week)
                         {:empty-is-missing? false}))

(defn matchup-pairs
  "Pure: raw entries -> `[{:matchup-id :roster-ids [...]}]`, one per game.

  Sorted by matchup id so the board's order does not depend on the order Sleeper
  happened to serialize its rosters in. A roster Sleeper gives no `matchup_id` —
  an odd league, a team on a bye — becomes its own single-id entry rather than
  being dropped; see the ns docstring on `matchups`."
  [entries]
  (let [grouped (group-by :matchup_id entries)
        byes    (get grouped nil)]
    (into (vec (for [[mid es] (sort-by key (dissoc grouped nil))]
                 {:matchup-id mid :roster-ids (mapv :roster_id es)}))
          (for [e byes]
            {:matchup-id nil :roster-ids [(:roster_id e)]}))))

(defn player-points
  "Pure: one entry's `players_points` with STRING player ids.

  `draft-day.json/mapper` keywordizes every key it decodes, and these keys are
  player ids — so Sleeper's `{\"4034\": 18.4}` arrives as `{:4034 18.4}` while
  the crosswalk and every roster list are strings. Without this the join misses
  every player on the board, which looks like a vendor publishing nothing rather
  than like a bug. Same family as `pipeline/assoc-weekly`'s id trap, and the
  reason that one does not hit it is that its ids are values, not keys."
  [entry]
  (update-keys (or (:players_points entry) {}) name))

(defn roster-score
  "Pure: one raw entry -> `[roster-id {:official :starter-ids :player-points}]`.

  `:starter-ids` comes off the *matchup* document rather than the roster's,
  because they answer different questions: the roster's `starters` is the lineup
  as it stands now, and this is the lineup that actually counted for this week.
  They agree right up until somebody edits his lineup after the games lock,
  which is precisely when a matchup board must not follow him.

  `custom_points` WINS OVER `points` WHERE IT IS SET, and it is null on every
  ordinary matchup. A commissioner who corrects a score — a stat correction, a
  ruling on a scoring dispute — writes it there and leaves `points` holding the
  computed figure. The corrected one is the league's score of record, which is
  the whole meaning of `:official`; reading `points` alone would put the board
  in visible disagreement with Sleeper on exactly the weeks a manager goes
  looking for an explanation."
  [entry]
  [(:roster_id entry)
   {:official      (or (:custom_points entry) (:points entry))
    :starter-ids   (vec (:starters entry))
    :player-points (player-points entry)}])

(defmethod matchups/normalize-matchups :sleeper
  [_ entries]
  {:matchups (matchup-pairs entries)
   :scores   (into {} (map roster-score) entries)})
