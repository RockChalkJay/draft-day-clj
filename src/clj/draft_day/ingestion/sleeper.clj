(ns draft-day.ingestion.sleeper
  "Sleeper player universe and projections. Free, keyless, and keyed to the
  scoring engine's stat vocabulary. Team defenses use their abbrev as player-id
  and map Sleeper's `DEF` position to `DST`."
  (:require [clojure.set :as set]
            [clojure.tools.logging :as log]
            [draft-day.ingestion.sleeper-http :as sleeper-http]
            [jsonista.core :as json]
            [draft-day.ingestion.season :as season]
            [draft-day.json :refer [mapper]]
            [draft-day.scoring :as scoring]))

(def ^:private base "https://api.sleeper.app")
(def fantasy-positions ["QB" "RB" "WR" "TE" "K" "DEF"])
(def ^:private fantasy-position-set (set fantasy-positions))

(defn- canon-pos [pos] (if (= pos "DEF") "DST" pos))

(def ^:private season-only-noise
  "Season-line keys that are not trustworthy, even when Sleeper sends them.

  `:yds_allow_0_100` is a one-game modal bucket on the season line and should not
  be treated like a full-season projection. The weekly line is the real source
  for that bucket; season-only noise must be stripped before scoring."
  #{:yds_allow_0_100})

(def adp-keys
  "ADP by scoring format. Sleeper publishes separate ADP values per format, and
  the chosen one is resolved at request time against the league's scoring."
  {:standard :adp_std :half-ppr :adp_half_ppr :ppr :adp_ppr})

(defn- adp
  "A single format's ADP, or nil when Sleeper marks it as absent."
  [stats k]
  (let [v (get stats k)]
    (when (and (number? v) (< v 999))
      (double v))))

(defn adp-by-format [stats]
  (into {} (keep (fn [[fmt k]] (when-let [v (adp stats k)] [fmt {:sleeper/adp v}])))
        adp-keys))

(def ^:private fgm-buckets
  "Made-field-goal buckets that appear on Sleeper's season line.

  This is a subset of the full field-goal bucket set; the season line omits
  the sub-40 bucket, so the published buckets are treated as a floor rather than
  a complete total."
  [:fgm_40_49 :fgm_50p])

(defn- summed-fgm
  "Recover a kicker's total FGM when Sleeper omits the flat total.

  The season line exposes only the bucketed makes, so we sum the published
  buckets and treat that as a floor. This only fills gaps; the weekly line's
  `:fgm` remains the authoritative total when it exists."
  [stats]
  (let [made (keep #(get stats %) fgm-buckets)]
    (when (seq made) (double (reduce + made)))))

(defn- scored-stats
  "The subset of Sleeper's stat map that the scoring engine can read, coerced to doubles."
  [stats]
  (let [base (into {} (keep (fn [k] (when-let [v (get stats k)] [k (double v)])))
                   scoring/stat-keys)]
    (if (:fgm base)
      base
      (if-let [fgm (summed-fgm stats)]
        (assoc base :fgm fgm) base))))

(def ^:private season-length
  "Maximum season-to-week stretch factor for a single player line.

  This is a cap, not a calendar fact: a ratio exceeding 17 would wildly
  overstate a player who had a tiny weekly projection."
  17.0)

(def ^:private fill-exempt
  "Keys that must not be filled from the weekly line.

  The made-FG grid is intentionally exempt because Sleeper's season and weekly
  horizons disagree on that bucketed shape, and the bucket-sum logic already
  reconciles it."
  (into #{:fgm} scoring/fg-buckets))

(defn implied-games
  "How many games the season line represents for this player, or nil.

  This is derived from the player's season `:pts_ppr` divided by the weekly
  `:pts_ppr`, capped to a modern season length. It is a per-player stretch
  factor, not a flat 17-game assumption."
  [season-stats weekly-stats]
  (let [s (:pts_ppr season-stats)
        w (:pts_ppr weekly-stats)]
    (when (and (number? s) (number? w) (pos? w) (pos? s))
      (min season-length (/ (double s) (double w))))))

(defn complete-season-line
  "Backfill sparse season stats from the weekly line, scaled to the player's
  implied games.

  Values already published on the season line are preserved; the made-FG buckets
  are intentionally skipped because Sleeper's season and weekly horizons disagree
  on that shape."
  [season-scored weekly-scored games]
  (if (and games weekly-scored)
    (reduce-kv (fn [acc k v]
                 (if (or (contains? acc k) (contains? fill-exempt k))
                   acc
                   (assoc acc k (* v games))))
               season-scored
               weekly-scored)
    season-scored))

(defn normalize-entry
  "Normalize a Sleeper projection entry into a universe player map.

  Returns nil for non-projectable players. `weekly` is the same player's week-one
  stats, used to backfill sparse season stats without overwriting values already
  published on the season line."
  ([entry] (normalize-entry entry nil))
  ([{:keys [player_id player stats team]} weekly]
   (let [pos (:position player)]
     (when (and stats (:pts_ppr stats) (fantasy-position-set pos))
       {:player-id             player_id
        :player-name           (str (:first_name player) " " (:last_name player))
        :position              (canon-pos pos)
        :team                  (or team (:team_abbr player))
        :bye                   nil
        :stats                 (complete-season-line
                                (apply dissoc (scored-stats stats) season-only-noise)
                                (some-> weekly scored-stats)
                                (implied-games stats weekly))
        :vendor/by-format      (adp-by-format stats)
        :sleeper/injury-status (:injury_status player)
        :sleeper/years-exp     (:years_exp player)}))))

(defn universe-from-entries
  "Normalize projection entries into the filtered player universe.

  `weekly-stats-by-id` backfills sparse season stats for players the feed does
  not project in a given week; players with no weekly row keep the season line as-is."
  ([entries] (universe-from-entries entries nil))
  ([entries weekly-stats-by-id]
   (into [] (keep #(normalize-entry % (get weekly-stats-by-id (:player_id %)))) entries)))

(defn- projections-url [season]
  (str base "/projections/nfl/" season "?season_type=regular"
       (apply str (map #(str "&position[]=" %) fantasy-positions))))

(defn fetch-projections
  "Fetch raw Sleeper projection entries for a season."
  [season]
  (let [{:keys [status body error]} (sleeper-http/get! (projections-url season) {:timeout 30000})]
    (cond
      error            (throw (ex-info "Sleeper projections fetch failed" {:error error}))
      (= 200 status)   (json/read-value body mapper)
      :else            (throw (ex-info "Sleeper projections non-200" {:status status})))))

;; ---- bye weeks (derived from the regular-season schedule) ----
;; Sleeper carries no bye field on players; a team's bye is simply the one
;; regular-season week it has no game. Keyed by the team abbrev that every
;; player (and team defense) already carries as :team.


(defn schedule->byes
  "Translate a regular-season schedule into a {team-abbrev bye-week} map.

  A team's bye is the single week it does not appear in any game; any team with
  no unique missing week keeps `:bye` nil."
  [games]
  (let [weeks     (keep :week games)
        all-weeks (set (range 1 (inc (apply max 0 weeks))))
        played    (reduce (fn [acc {:keys [home away week]}]
                            (cond-> acc
                              home (update home (fnil conj #{}) week)
                              away (update away (fnil conj #{}) week)))
                          {} games)]
    (into {} (keep (fn [[team wks]]
                     (let [missing (set/difference all-weeks wks)]
                       (when (= 1 (count missing))
                         [team (first missing)]))))
          played)))

(defn assoc-byes
  "Set each player's `:bye` from a {team-abbrev bye-week} map keyed by `:team`.

  Players with no known team keep their existing `:bye`."
  [universe byes]
  (mapv (fn [p] 
          (if-let [b (get byes (:team p))] 
            (assoc p :bye b) 
            p)) universe))

(defn- schedule-url [season]
  (str base "/schedule/nfl/regular/" season))

(defn fetch-schedule
  "Fetch raw regular-season schedule games for a season."
  [season]
  (let [{:keys [status body error]} (sleeper-http/get! (schedule-url season) {:timeout 30000})]
    (cond
      error          (throw (ex-info "Sleeper schedule fetch failed" {:status status :error error}))
      (= 200 status) (json/read-value body mapper)
      :else          (throw (ex-info "Sleeper schedule non-200" {:status status})))))

(defn fetch-byes
  "Fetch a {team-abbrev bye-week} map for a season."
  ([] (fetch-byes (season/current)))
  ([season] (schedule->byes (fetch-schedule season))))

;; ---- weekly projections ----

;; Sleeper's season and weekly projections are the same vendor's view
;; at different horizons. Keep the weekly :updated-at because a stale
;; projection is worse than no number. The weekly payload carries
;; matchup context, and these values are revised throughout the week.

(defn- weekly-url [season week]
  (str base "/projections/nfl/" season "/" week "?season_type=regular"
       (apply str (map #(str "&position[]=" %) fantasy-positions))))

(defn fetch-weekly-entries
  "Fetch raw weekly projection entries for one week."
  [season week]
  (let [{:keys [status body error]} (sleeper-http/get! (weekly-url season week)
                                               {:timeout 30000})]
    (cond
      error          (throw (ex-info "Sleeper weekly fetch failed"
                                     {:week week :error error}))
      (= 200 status) (json/read-value body mapper)
      :else          (throw (ex-info "Sleeper weekly non-200"
                                     {:week week :status status})))))

(defn week-one-stats
  "Fetch best-effort week-one stats keyed by player-id.

  Week one is intentionally used as the baseline for the season line; the
  in-season weekly line is handled separately so the two horizons stay distinct."
  [season]
  (try
    (into {} (keep (fn [{:keys [player_id stats]}]
                     (when (and player_id stats) [player_id stats])))
          (fetch-weekly-entries season 1))
    (catch Exception e
      (log/warn e "Sleeper week-one fetch failed; season line stands alone")
      nil)))

(defn fetch-universe
  "Fetch the normalized player universe for a season."
  ([] (fetch-universe (season/current)))
  ([season] (universe-from-entries (fetch-projections season)
                                   (week-one-stats season))))

(defn home-teams [games week]
  (into #{} (comp (filter #(= week (:week %))) (keep :home)) games))

(defn weekly-line
  "Normalize one weekly entry into the app's line shape.

  Returns nil when Sleeper does not project that player for the requested week,
  and keeps `:home?` nil when the schedule data is unavailable instead of guessing."
  [{:keys [stats opponent team updated_at]} homes]
  (when (and stats (:pts_ppr stats))
    {:stats      (scored-stats stats)
     :opponent   opponent
     ;; nil, not false, when the schedule did not arrive: an empty home set
     ;; would read as "everyone is away" and print `@ OPP` over every home
     ;; game. Not knowing the side is a state the renderer can show.
     :home?      (when homes (contains? homes team))
     :updated-at updated_at}))

(defn weekly-by-id
  "Build a {sleeper-id weekly-line} map from weekly entries."
  [entries homes]
  (into {} (keep (fn [{:keys [player_id] :as entry}]
                   (when-let [line (weekly-line entry homes)]
                     [player_id line])))
        entries))

(defn fetch-weekly
  "Fetch the weekly line keyed by Sleeper player-id for one season week.

  This uses Sleeper's id space; the universe crosswalk resolves it to the app's
  player ids elsewhere."
  [season week]
  ;; The schedule only supplies vs/@. Letting it fail the whole fetch would
  ;; trade the entire weekly projection for a two-character prefix, so it
  ;; degrades on its own: nil homes means the side is unknown.
  (let [homes (try (home-teams (fetch-schedule season) week)
                   (catch Exception e
                     (log/warn e "weekly schedule fetch failed; side unknown")
                     nil))]
    (weekly-by-id (fetch-weekly-entries season week) homes)))
