(ns draft-day.ingestion.sleeper
  "Sleeper JSON backbone: the projectable player universe + projections + ids.
  Free, keyless. Each projection entry carries player_id, an embedded player
  object (name/position/team), and a stats map whose keys match the scoring
  engine (`rush_yd`, `pass_td`, `rec`, ...). Team defenses use the team abbrev
  as their player_id (e.g. \"ARI\") and Sleeper's \"DEF\" maps to our \"DST\"."
  (:require [org.httpkit.client :as http]
            [jsonista.core :as json]))

(def ^:private mapper (json/object-mapper {:decode-key-fn keyword}))

(def ^:private base "https://api.sleeper.app")
(def fantasy-positions ["QB" "RB" "WR" "TE" "K" "DEF"])
(def ^:private fantasy-position-set (set fantasy-positions))

(defn- canon-pos [pos] (if (= pos "DEF") "DST" pos))

;; Stat keys carried into :stats for the scoring engine (skill + kicking/defense).
(def ^:private stat-keys
  [:pass_yd :pass_td :pass_int :pass_2pt
   :rush_yd :rush_td :rush_2pt
   :rec :rec_yd :rec_td :rec_2pt
   :fum_lost
   :fgm :xpm
   :sack :int :fum_rec :ff :def_td :safe :blk_kick])

(defn- adp
  "First real ADP (Sleeper uses 999 as a 'no ADP' sentinel), preferring PPR."
  [stats]
  (some (fn [k] (let [v (get stats k)]
                  (when (and (number? v) (< v 999)) (double v))))
        [:adp_ppr :adp_half_ppr :adp_std]))

(defn normalize-entry
  "Sleeper projection entry -> a universe player map, or nil if it is not a
  projectable, fantasy-relevant player (must have pts_ppr and a fantasy position)."
  [{:keys [player_id player stats team]}]
  (let [pos (:position player)]
    (when (and stats (:pts_ppr stats) (fantasy-position-set pos))
      {:player-id             player_id
       :player-name           (str (:first_name player) " " (:last_name player))
       :position              (canon-pos pos)
       :team                  (or team (:team_abbr player))
       :bye                   nil
       :stats                 (into {} (keep (fn [k] (when-let [v (get stats k)] [k (double v)]))
                                             stat-keys))
       :sleeper/adp           (adp stats)
       :sleeper/pts-ppr       (:pts_ppr stats)
       :sleeper/pts-half-ppr  (:pts_half_ppr stats)
       :sleeper/pts-std       (:pts_std stats)
       :sleeper/injury-status (:injury_status player)
       :sleeper/years-exp     (:years_exp player)})))

(defn universe-from-entries
  "Pure: projection entries -> the normalized, filtered player universe."
  [entries]
  (into [] (keep normalize-entry) entries))

(defn current-season []
  (.getValue (java.time.Year/now)))

(defn- projections-url [season]
  (str base "/projections/nfl/" season "?season_type=regular"
       (apply str (map #(str "&position[]=" %) fantasy-positions))))

(defn fetch-projections
  "Network: raw projection entries for a season (throws on failure)."
  [season]
  (let [{:keys [status body error]} @(http/get (projections-url season) {:timeout 30000})]
    (cond
      error            (throw (ex-info "Sleeper projections fetch failed" {:error error}))
      (= 200 status)   (json/read-value body mapper)
      :else            (throw (ex-info "Sleeper projections non-200" {:status status})))))

(defn fetch-universe
  "Network: the normalized player universe for a season (defaults to current)."
  ([] (fetch-universe (current-season)))
  ([season] (universe-from-entries (fetch-projections season))))
