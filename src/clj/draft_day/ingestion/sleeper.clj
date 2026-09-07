(ns draft-day.ingestion.sleeper
  "Sleeper JSON backbone: the projectable player universe + projections + ids.
  Free, keyless. Each projection entry carries player_id, an embedded player
  object (name/position/team), and a stats map whose keys match the scoring
  engine (`rush_yd`, `pass_td`, `rec`, ...). Team defenses use the team abbrev
  as their player_id (e.g. \"ARI\") and Sleeper's \"DEF\" maps to our \"DST\"."
  (:require [clojure.set :as set]
            [clojure.tools.logging :as log]
            [org.httpkit.client :as http]
            [jsonista.core :as json]
            [draft-day.json :refer [mapper]]))

(def ^:private base "https://api.sleeper.app")
(def fantasy-positions ["QB" "RB" "WR" "TE" "K" "DEF"])
(def ^:private fantasy-position-set (set fantasy-positions))

(defn- canon-pos [pos] (if (= pos "DEF") "DST" pos))

;; Stat keys carried into :stats for the scoring engine (skill + kicking/defense).
;; NOT the whole payload: Sleeper also projects first downs (:rec_fd on 474 players,
;; :rush_fd on 376, :pass_fd on 77), position reception premiums (:bonus_rec_te/_wr/_rb),
;; the FG distance buckets that exist (:fgm_40_49 :fgm_50p :fgm_yds :fgmiss_40_49
;; :fgmiss_50p :xpmiss), :pass_int_td, :pr_td, :def_kr_td, :pts_allow_0 and
;; :yds_allow_0_100. Dropping them is why a PPFD or TE-premium league imports lossy —
;; see docs/scoring-coverage.md.
(def ^:private stat-keys
  [:pass_yd :pass_td :pass_int :pass_2pt
   :rush_yd :rush_td :rush_2pt
   :rec :rec_yd :rec_td :rec_2pt
   :fum_lost
   :fgm :xpm
   :sack :int :fum_rec :ff :def_td :safe :blk_kick])

(def adp-keys
  "Sleeper publishes ADP per scoring format, and they diverge hard — Amon-Ra St.
  Brown went 8.1 PPR against 16.8 standard for 2026. Collapsing them to one
  PPR-preferred number meant the ADP column ignored the league's scoring, so all
  three are carried and `rankings.vendor` picks one per request."
  {:standard :adp_std :half-ppr :adp_half_ppr :ppr :adp_ppr})

(defn- adp
  "One format's ADP, or nil (Sleeper uses 999 as its 'no ADP' sentinel)."
  [stats k]
  (let [v (get stats k)]
    (when (and (number? v) (< v 999)) (double v))))

(defn adp-by-format [stats]
  (into {} (keep (fn [[fmt k]] (when-let [v (adp stats k)] [fmt {:sleeper/adp v}])))
        adp-keys))

(defn- scored-stats
  "The subset of a Sleeper stats map the scoring engine reads, as doubles."
  [stats]
  (into {} (keep (fn [k] (when-let [v (get stats k)] [k (double v)]))) stat-keys))

(defn normalize-entry
  "Sleeper projection entry -> a universe player map, or nil if it is not a
  projectable, fantasy-relevant player.

  The `pts_ppr` gate is a projectability check, not a scoring choice: Sleeper
  sets it for anyone it projects at all, so its absence means there is no
  projection to score under *any* config. The number itself is not carried —
  `:points` is always computed from `:stats` under the league's own weights."
  [{:keys [player_id player stats team]}]
  (let [pos (:position player)]
    (when (and stats (:pts_ppr stats) (fantasy-position-set pos))
      {:player-id             player_id
       :player-name           (str (:first_name player) " " (:last_name player))
       :position              (canon-pos pos)
       :team                  (or team (:team_abbr player))
       :bye                   nil
       :stats                 (scored-stats stats)
       :vendor/by-format      (adp-by-format stats)
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

;; ---- bye weeks (derived from the regular-season schedule) ----
;; Sleeper carries no bye field on players; a team's bye is simply the one
;; regular-season week it has no game. Keyed by the team abbrev that every
;; player (and team defense) already carries as :team.


(defn schedule->byes
  "Pure: regular-season schedule games -> {team-abbrev bye-week}. Each game is
  `{:home \"ATL\" :away \"TB\" :week 1 ...}`; a team's bye is the single week in
  1..(max week) it appears in no game. Teams without exactly one missing week are
  omitted (they simply keep :bye nil)."
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
  "Pure: set each player's :bye from a {team-abbrev bye-week} map, keyed on :team.
  Players with an unknown/nil team keep their existing :bye."
  [universe byes]
  (mapv (fn [p] 
          (if-let [b (get byes (:team p))] 
            (assoc p :bye b) 
            p)) universe))

(defn- schedule-url [season]
  (str base "/schedule/nfl/regular/" season))

(defn fetch-schedule
  "Network: raw regular-season schedule games for a season (throws on failure)."
  [season]
  (let [{:keys [status body error]} @(http/get (schedule-url season) {:timeout 30000})]
    (cond
      error          (throw (ex-info "Sleeper schedule fetch failed" {:status status :error error}))
      (= 200 status) (json/read-value body mapper)
      :else          (throw (ex-info "Sleeper schedule non-200" {:status status})))))

(defn fetch-byes
  "Network: {team-abbrev bye-week} for a season (defaults to current)."
  ([] (fetch-byes (current-season)))
  ([season] (schedule->byes (fetch-schedule season))))

;; ---- weekly projections ----
;; The same projections endpoint, one week at a time. The payload's own `company`
;; field says rotowire for both horizons, so a weekly number and a season number
;; are one house's opinion at two ranges rather than two houses disagreeing —
;; which is what makes showing them side by side honest.
;;
;; Three quirks. The entry carries its own `opponent`, so the matchup costs no
;; second fetch; a nil one means a bye or a player nobody projects, and only the
;; ~14% Sleeper actually projects carry one. Home/away is *not* on the entry —
;; both teams of a game share one `game_id` — so it comes from the schedule
;; `fetch-byes` already parses. And these revise through the week as injury news
;; and inactives land, so `:updated-at` is carried: a consumer that cannot say
;; how old the number is will imply it is current, and on a Sunday morning that
;; is the difference between a projection and a wrong answer.

(defn- weekly-url [season week]
  (str base "/projections/nfl/" season "/" week "?season_type=regular"
       (apply str (map #(str "&position[]=" %) fantasy-positions))))

(defn fetch-weekly-entries
  "Network: raw weekly projection entries for one week (throws on failure)."
  [season week]
  (let [{:keys [status body error]} @(http/get (weekly-url season week)
                                               {:timeout 30000})]
    (cond
      error          (throw (ex-info "Sleeper weekly fetch failed"
                                     {:week week :error error}))
      (= 200 status) (json/read-value body mapper)
      :else          (throw (ex-info "Sleeper weekly non-200"
                                     {:week week :status status})))))

(defn home-teams [games week]
  (into #{} (comp (filter #(= week (:week %))) (keep :home)) games))

(defn weekly-line
  "Pure: one entry -> its weekly line, or nil when Sleeper does not project the
  player that week. The `pts_ppr` gate is `normalize-entry`'s, for its reason."
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
  "Pure: entries -> {sleeper-id line}. See `fetch-weekly` on the id space."
  [entries homes]
  (into {} (keep (fn [{:keys [player_id] :as entry}]
                   (when-let [line (weekly-line entry homes)]
                     [player_id line])))
        entries))

(defn fetch-weekly
  "Network: {sleeper-id weekly-line} for one week of a season.

  Keyed in *Sleeper's* id space, not the universe's — `:player-id` there is the
  GSIS id wherever one resolves. `pipeline/assoc-weekly` crosses the two."
  [season week]
  ;; The schedule only supplies vs/@. Letting it fail the whole fetch would
  ;; trade the entire weekly projection for a two-character prefix, so it
  ;; degrades on its own: nil homes means the side is unknown.
  (let [homes (try (home-teams (fetch-schedule season) week)
                   (catch Exception e
                     (log/warn e "weekly schedule fetch failed; side unknown")
                     nil))]
    (weekly-by-id (fetch-weekly-entries season week) homes)))
