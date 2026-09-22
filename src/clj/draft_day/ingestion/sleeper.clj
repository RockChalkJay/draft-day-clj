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
            [draft-day.ingestion.season :as season]
            [draft-day.json :refer [mapper]]
            [draft-day.scoring :as scoring]))

(def ^:private base "https://api.sleeper.app")
(def fantasy-positions ["QB" "RB" "WR" "TE" "K" "DEF"])
(def ^:private fantasy-position-set (set fantasy-positions))

(defn- canon-pos [pos] (if (= pos "DEF") "DST" pos))

(def ^:private season-only-noise
  "Keys not to be believed on a *season* line, whatever it sends.

  Sleeper sends `yds_allow_0_100` as 1.0 for all thirty-two defenses, beside a
  `gp` of 1.0, while `sack` and `int` on the same line are real season totals.
  It is the modal bucket of one game rather than a projection of a season, and
  a league weighting that band at ten points would hand every defense the same
  spurious ten. The weekly line states a bucket the same way and means it, so
  this is the season line's problem alone — `scoring/season-projected` leaves
  the key out for the same reason."
  #{:yds_allow_0_100})

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

(def ^:private fgm-buckets
  "The made-field-goal columns Sleeper publishes on a *season* line.

  A subset of `scoring/fg-buckets` — the season line has no sub-forty bucket —
  so their sum is a floor. `every-season-bucket-is-a-scoring-bucket` is what
  keeps the two from drifting apart."
  [:fgm_40_49 :fgm_50p])

(defn- summed-fgm
  "A kicker's made field goals, when Sleeper did not publish the total.

  Its season projections carry no `:fgm` at all — 0 of 45 kickers — only these
  distance buckets and a yardage total, so a flat weight multiplied nothing and
  every kicker scored on extra points alone. Aubrey came out at 42 against
  Sleeper's own 116, and the position compressed into a 39-42 band with no
  spread in it.

  A league stating its field goals by distance never needs this: it scores the
  buckets, which reconcile to Sleeper's own total (Aubrey 118 against 116).

  Summing what *is* published recovers most of that (Aubrey 93) and keeps the
  whole line one vendor's opinion. ESPN publishes a real total and was the
  obvious alternative, but it projects him 35.5 field goals against Sleeper's
  ~25 — importing it would price kickers out of a different house than every
  other player on the board. Deliberately a floor rather than a guess: the
  sub-40 kicks are missing and no published column implies them.

  The weekly endpoint *does* send `:fgm`, so this only ever fills a gap."
  [stats]
  (let [made (keep #(get stats %) fgm-buckets)]
    (when (seq made) (double (reduce + made)))))

(defn- scored-stats
  "The subset of a Sleeper stats map the scoring engine reads, as doubles."
  [stats]
  (let [base (into {} (keep (fn [k] (when-let [v (get stats k)] [k (double v)])))
                   scoring/stat-keys)]
    (if (:fgm base)
      base
      (if-let [fgm (summed-fgm stats)] (assoc base :fgm fgm) base))))

(def ^:private season-length
  "The most games one week may be stretched over.

  A cap, not a calendar: the two projection horizons disagree hard about a
  marginal player and an unclamped ratio reached thirty, which would have
  tripled a bonus key. Seventeen is every modern season, and the 16-game era
  reaches `nflverse/games-in-season` rather than this path."
  17.0)

(def ^:private fill-exempt
  "Keys `complete-season-line` must not fill, whatever the weekly line says.

  The made-field-goal grid alone: the two horizons disagree about it rather
  than merely covering different parts of it, and `summed-fgm` already
  reconciles them. See `complete-season-line`."
  (into #{:fgm} scoring/fg-buckets))

(defn implied-games
  "How many of this week the season is, for this player, or nil.

  The vendor's own summary of each line — `pts_ppr` season over `pts_ppr` week
  — rather than a flat seventeen, so a back-up projected for a handful of games
  is stretched over a handful. It cross-checks against the ratio a player's
  shared stats imply to within about three quarters of a game."
  [season-stats weekly-stats]
  (let [s (:pts_ppr season-stats)
        w (:pts_ppr weekly-stats)]
    (when (and (number? s) (number? w) (pos? w) (pos? s))
      (min season-length (/ (double s) (double w))))))

(defn complete-season-line
  "A season line plus every scored key only the weekly line carries, stretched
  from one week to this player's own season.

  Sleeper publishes both horizons from one house — the payload's `company` says
  rotowire for each — and the season line is much the sparser: it carries no
  points- or yards-allowed bucket, no long touchdown, no completion over forty,
  no forced fumble, no safety. Left alone that is not a neutral gap but a
  *tilt*, because the receiving buckets it does carry lift receivers and tight
  ends while a quarterback and a team defense gain nothing, and replacement and
  VORP compare those directly. A defense's tier points are over a third of what
  it is projected to score.

  Deliberately an estimate and it says so, the same standing `summed-fgm` has:
  a filled key is one house's week stretched over a season rather than a season
  anybody projected. Only keys the season line is silent on are filled, so
  nothing the vendor did state is ever overwritten.

  A defense's tier buckets are the estimate's weakest part and most of its
  gain. Sleeper one-hots them — exactly one `pts_allow_*` and one
  `yds_allow_*` at 1.0, its modal week rather than a distribution, and across
  all thirty-two defenses only the 14-20, 21-27 and 28-34 points bands ever
  appear — so stretching one over a season asserts a defense lands in that
  single bucket every week, which none does. A league paying well for a shutout
  collects nothing for it here. Taken anyway, because the alternative is a
  defense scored on sacks and interceptions alone; the real-valued `pts_allow`
  Sleeper sends beside the one-hot is what a distribution would be built from
  if a benchmark ever earned one.

  A made field goal is exempt, because the two horizons do not merely differ in
  coverage there, they disagree. Aubrey's season line carries nine makes from
  40-49 and eight from 50+ and nothing under forty; his weekly line carries
  every band under fifty and no 50+ at all. Filling one from the other reads as
  thirty-one makes a season against the seventeen the season line states, and
  scored him 160 against Sleeper's own 116 — where the sparse line scores 118.
  `summed-fgm` and `scoring/fg-buckets` already reconcile that grid and the
  board agrees with Sleeper on both horizons because of it; this must not reach
  in and undo it. Misses are not exempt: a miss the season line omits is simply
  one it does not project."
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
  "Sleeper projection entry -> a universe player map, or nil if it is not a
  projectable, fantasy-relevant player. `weekly` is the same player's week-one
  stats, for `complete-season-line`; absent, the season line stands alone.

  The `pts_ppr` gate is a projectability check, not a scoring choice: Sleeper
  sets it for anyone it projects at all, so its absence means there is no
  projection to score under *any* config. The number itself is not carried —
  `:points` is always computed from `:stats` under the league's own weights."
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
  "Pure: projection entries -> the normalized, filtered player universe.

  `weekly-stats-by-id` completes the sparse season line — see
  `complete-season-line`. Sleeper projects only a fraction of the universe in
  any one week, so a player it names no week for keeps the season line alone;
  that is the deep bench, and the board it moves is the one nobody drafts off."
  ([entries] (universe-from-entries entries nil))
  ([entries weekly-stats-by-id]
   (into [] (keep #(normalize-entry % (get weekly-stats-by-id (:player_id %)))) entries)))

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
  ([] (fetch-byes (season/current)))
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

(defn week-one-stats
  "Network, best-effort: `{player-id raw-stats}` for week one of a season.

  Week one and not the current week, because `:stats` is by definition the
  *preseason* full-season line — `rankings.ros` is what corrects it for a
  season in progress, and filling it from a November week would mix the two
  horizons this exists to keep straight.

  Best-effort for `summed-fgm`'s reason: a season line that is merely sparse
  still prices a board, and failing the whole universe over the half of it that
  fills the gaps would trade a working board for a better one."
  [season]
  (try
    (into {} (keep (fn [{:keys [player_id stats]}]
                     (when (and player_id stats) [player_id stats])))
          (fetch-weekly-entries season 1))
    (catch Exception e
      (log/warn e "Sleeper week-one fetch failed; season line stands alone")
      nil)))

(defn fetch-universe
  "Network: the normalized player universe for a season (defaults to current)."
  ([] (fetch-universe (season/current)))
  ([season] (universe-from-entries (fetch-projections season)
                                   (week-one-stats season))))

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
