(ns draft-day.benchmark.sources.nflverse
  "nflverse: realized season outcomes (the answer key) and prior-season usage.

  Free, keyless CSV releases on GitHub, keyed by GSIS id, contiguous 1999-2025 —
  the deepest and cleanest outcome source available, and the only one here that
  is not a projection of anything.

  RATE VS COUNT — the trap in this file. `target_share`, `air_yards_share` and
  `wopr` are already **season rates** in `stats_player_reg` (Ja'Marr Chase 2024:
  target_share 0.279). Dividing them by games — the obvious-looking move next to
  the count columns — turns a rate into rate-per-game, which silently *inflates*
  the usage of players who missed time and corrupts any comparison across
  players. `targets`, `carries` and `receptions` are the opposite: raw counts
  that must be divided by games to compare. Keep the two groups straight; the
  helpers below encode which is which."
  (:require [draft-day.benchmark.fetch :as fetch]))

(def ^:private base
  "https://github.com/nflverse/nflverse-data/releases/download/stats_player")

(defn season-url [season] (str base "/stats_player_reg_" season ".csv"))

(def stat-columns
  "nflverse column -> the Sleeper stat key `rankings.scoring` already speaks.
  Deliberately limited to offensive scoring: the benchmark pool is QB/RB/WR/TE,
  and nflverse has no per-player team-defense rows to map :sack/:int/:def_td onto."
  {"passing_yards"             :pass_yd
   "passing_tds"               :pass_td
   "passing_interceptions"     :pass_int
   "passing_2pt_conversions"   :pass_2pt
   "rushing_yards"             :rush_yd
   "rushing_tds"               :rush_td
   "rushing_2pt_conversions"   :rush_2pt
   "receptions"                :rec
   "receiving_yards"           :rec_yd
   "receiving_tds"             :rec_td
   "receiving_2pt_conversions" :rec_2pt
   "fumbles_lost_total"        :fum_lost
   "fg_made"                   :fgm
   "pat_made"                  :xpm})

(defn row->stats
  "Pure: one nflverse row -> a Sleeper-keyed stat line, so realized outcomes can
  be scored by `rankings.scoring/player-points` under the league's own config."
  [row]
  (into {} (map (fn [[col k]] [k (fetch/num0 (get row col))])) stat-columns))

(defn row->outcome
  "Pure: one nflverse row -> the realized-outcome record for one player."
  [row]
  {:gsis-id  (get row "player_id")
   :position (get row "position")
   :team     (get row "recent_team")
   :games    (fetch/num0 (get row "games"))
   :stats    (row->stats row)})

(defn row->usage
  "Pure: one nflverse row -> prior-season usage signals.

  Shares are passed through untouched (already season rates); counts are
  normalized per game. `games` is carried so callers can weight a partial season."
  [row]
  (let [g (fetch/num0 (get row "games"))
        per-game (fn [col] (if (pos? g) (/ (fetch/num0 (get row col)) g) 0.0))]
    {:gsis-id          (get row "player_id")
     :position         (get row "position")
     :games            g
     ;; rates — do not divide
     :wopr             (fetch/num0 (get row "wopr"))
     :target-share     (fetch/num0 (get row "target_share"))
     :air-yards-share  (fetch/num0 (get row "air_yards_share"))
     ;; counts — per game
     :targets-per-game (per-game "targets")
     :carries-per-game (per-game "carries")
     :ppg              (per-game "fantasy_points_ppr")}))

(def season-rows
  "Raw nflverse rows for a season, disk-cached and memoized in-process.

  Memoization matters here because a sweep asks for the same season twice under
  two names: season S's outcomes and season S+1's prior-season usage are the same
  ~2000-row CSV."
  (memoize
   (fn [season]
     (fetch/cached (fetch/cache-path "nflverse" season)
                   #(if-let [body (fetch/http-get-string (season-url season))]
                      (fetch/parse-csv body)
                      [])))))

(defn outcomes
  "{gsis-id outcome-record} for a season."
  [season]
  (into {} (map (juxt :gsis-id identity)) (map row->outcome (season-rows season))))

(defn usage
  "{gsis-id usage-record} for a season."
  [season]
  (into {} (map (juxt :gsis-id identity)) (map row->usage (season-rows season))))

(defn available?
  "Whether nflverse publishes this season (used by --source-report)."
  [season]
  (or (fetch/cache-exists? (fetch/cache-path "nflverse" season))
      (some? (fetch/http-get-string (season-url season)))))

;; ---- weekly participation ----

(defn week-url [season] (str base "/stats_player_week_" season ".csv"))

(def week-rows
  "Raw weekly rows for a season, disk-cached and memoized."
  (memoize
   (fn [season]
     (fetch/cached (fetch/cache-path "nflverse" "week" season)
                   #(if-let [body (fetch/http-get-string (week-url season))]
                      (fetch/parse-csv body)
                      [])))))

(def week1-participants
  "GSIS ids with a Week 1 regular-season row — i.e. players who were active from
  the start of the season.

  Note for callers: membership is a FACT ABOUT THE OUTCOME, not about draft day.
  Filtering a pool by it removes players who were hurt or suspended in camp,
  which is information a drafter did not have. Useful as a diagnostic split, but
  it is survivorship bias if treated as the headline number."
  (memoize
   (fn [season]
     (into #{}
           (keep (fn [r]
                   (when (and (= "REG" (get r "season_type"))
                              (= "1" (str (get r "week"))))
                     (get r "player_id"))))
           (week-rows season)))))
