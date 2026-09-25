(ns draft-day.ingestion.matchups.espn
  "ESPN provider for head-to-head matchups.

  One league document, asked for by week: `mMatchup` and `mMatchupScore` carry
  the season's whole `schedule`, and `scoringPeriodId` decides which of its
  games ESPN fills in. It goes through `league-import.espn/get-json`, the one
  place an ESPN request carries a cookie, rather than rebuilding that here — the
  arrangement `league-sync.espn` already has with the same sibling.

  Three things this namespace absorbs, all read off a live league rather than
  taken from ESPN's documentation.

  Which games are this week's. The schedule holds all of the season's games
  whatever week is asked for, and ESPN fills `rosterForCurrentScoringPeriod` on
  the requested week's alone. So the filter is the filled roster itself, not
  `matchupPeriodId` against the league's current period: those two agree only
  while the week asked for is the current one, and `fetch-matchups` lets a
  caller name a week.

  What a side scored. `totalPoints` is 0.0 on a game in progress, where
  `totalPointsLive` carries the number, and `totalPointsLive` is nil once the
  week is over — neither field alone is this week's score.
  `pointsByScoringPeriod` is right in both cases, keyed by the week and
  keywordized on the way in like every other decoded key (`{:2 51.04}`).

  Where each starter sits. An ESPN entry names its own seat on `lineupSlotId`
  while Sleeper's `starters` array is positional, and `rankings.matchup` seats
  by index. Rather than teach the shared board a second convention,
  `starter-ids` seats ESPN's entries into the league's own seat list here,
  padding an unfilled one with `db/empty-seat` — the same vector, in the same
  order, `routes/matchup-slots` builds from the sync. The seat list comes from
  the `mSettings` fetched in the same document, so the two halves of one reply
  cannot disagree about a league's shape."
  (:require [draft-day.db :as db]
            [draft-day.ingestion.league-import.espn :as import-espn]
            [draft-day.ingestion.league-sync.espn :as sync-espn]
            [draft-day.ingestion.matchups :as matchups]))

(def views
  "`mSettings` rides along because the normalizer needs the league's seats, and
  a second request for them could answer about a different league's shape."
  ["mMatchup" "mMatchupScore" "mSettings"])

(defmethod matchups/current-week :espn
  [_ {:keys [league-id season credentials]}]
  (let [doc (import-espn/get-json {:season season :league-id league-id
                                   :credentials credentials
                                   :views ["mStatus"] :require-key :status})
        ;; The NFL week, not `currentMatchupPeriod`: a playoff period can span
        ;; two of them, and the weekly line is joined by week.
        wk  (or (:scoringPeriodId doc) (get-in doc [:status :latestScoringPeriod]))]
    (when (and (number? wk) (pos? wk)) wk)))

(defmethod matchups/fetch-raw-matchups :espn
  [_ {:keys [league-id season credentials week]}]
  (import-espn/get-json {:season season :league-id league-id :credentials credentials
                         :views views :params {:scoringPeriodId week}
                         :require-key :schedule}))

(defn side-entries
  "One side's roster for the week asked for — everyone on it, not only the
  starters, or the optimal half of the board cannot say what a bench player
  scored."
  [side]
  (get-in side [:rosterForCurrentScoringPeriod :entries]))

(defn sides
  "The teams in one game, home first. One of them for a manager with no
  opponent; a side with no `teamId` is not a team."
  [game]
  (into [] (comp (keep #(get game %)) (filter :teamId)) [:home :away]))

(defn this-week
  "Pure: the schedule entries ESPN filled in for the week that was asked for.

  See the ns docstring: the filled roster is the only thing that says which week
  a game belongs to in a document that carries the whole season."
  [schedule]
  (filterv (fn [game] (some #(seq (side-entries %)) (sides game))) schedule))

(defn official
  "Pure: one side's score of record for `week`, whether the games are being
  played or long over. See the ns docstring for why neither total alone will do."
  [side week]
  (or (get (:pointsByScoringPeriod side) (keyword (str week)))
      (:totalPointsLive side)
      (:totalPoints side)))

(defn player-points
  "Pure: one side's entries -> `{player-id points}` in the board's id space.

  The applied total is on the `playerPoolEntry`; the roster entry's own
  `appliedStatTotal` is nil, and reading it publishes a board of blanks. A
  player ESPN gives no number for is left out, so he reads as unknown."
  [entries]
  (into {}
        (keep (fn [en]
                (when-let [id (sync-espn/entry-player-id en)]
                  (let [pts (get-in en [:playerPoolEntry :appliedStatTotal])]
                    (when (number? pts) [id pts])))))
        entries))

(defn slot-name
  "Pure: the seat this entry holds, in the app's spelling. An unlisted slot
  passes through as its id, as `import-espn/roster-positions` spells it, so the
  two still meet."
  [entry]
  (let [id (:lineupSlotId entry)]
    (get import-espn/lineup-slots id (str id))))

(defn starter-ids
  "Pure: the entries that started, in `seats` order, `db/empty-seat` for a seat
  nobody filled.

  Allocated by seat name rather than by sorting on the slot id, which keeps
  every other seat in place when a lineup and the settings disagree. An entry
  whose seat this league does not list stays off the lineup rather than
  shifting the rest along; `:player-ids` still holds him."
  [entries seats]
  (first
   (reduce (fn [[out left] seat]
             (let [[before [hit & after]] (split-with #(not= seat (slot-name %)) left)]
               (if hit
                 [(conj out (or (sync-espn/entry-player-id hit) db/empty-seat))
                  (into (vec before) after)]
                 [(conj out db/empty-seat) left])))
           [[] (vec entries)]
           seats)))

(defn seats
  "Pure: the seats that score, in the order a lineup is read in.

  Shipped as `:slots` rather than left for the caller to rebuild: `starter-ids`
  aligns to this list, and a board reading that lineup against another order
  mislabels every seat past the disagreement."
  [raw]
  (db/scoring-slots
   (import-espn/roster-positions (get-in raw [:settings :rosterSettings :lineupSlotCounts]))))

(defn roster-score
  "Pure: one side -> `[roster-id {:official :starter-ids :player-ids
  :player-points}]`.

  The roster id is ESPN's `teamId`, which is the integer `league-sync.espn`
  puts on `:roster-id`; a defense is its team abbreviation in both."
  [side week seat-list]
  (let [entries (side-entries side)]
    [(:teamId side)
     {:official      (official side week)
      ;; No entries is no lineup, not a lineup of empty seats — a vector of
      ;; `db/empty-seat` defeats `matchup/week-lineup`'s fallback to the roster.
      :starter-ids   (if (seq entries) (starter-ids entries seat-list) [])
      :player-ids    (into [] (keep sync-espn/entry-player-id) entries)
      :player-points (player-points entries)}]))

(defmethod matchups/normalize-matchups :espn
  [_ raw]
  ;; The week ESPN echoes, which is what its own `pointsByScoringPeriod` is
  ;; keyed by — `fetch-matchups` stamps the week it asked for on the reply.
  (let [week  (:scoringPeriodId raw)
        seat-list (seats raw)
        games (this-week (:schedule raw))]
    {:slots    seat-list
     :matchups (mapv (fn [game]
                       {:matchup-id (:id game)
                        :roster-ids (mapv :teamId (sides game))})
                     (sort-by :id games))
     :scores   (into {}
                     (map #(roster-score % week seat-list))
                     (mapcat sides games))
     ;; Off this document and not only the sync's: a player picked up since the
     ;; last sync is on this week's roster and in no stored directory.
     :provider-players (sync-espn/provider-players
                        (mapcat side-entries (mapcat sides games)))}))
