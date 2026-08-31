(ns draft-day.ingestion.nflverse-weekly
  "Enrichment: what a player has actually done *this* season, week by week.

  The in-season counterpart to `nflverse`. Same release, same GSIS key, same
  exact join — but a different question. `nflverse` asks what happened in
  seasons that are over; this asks how far the current one has got and what each
  player has produced inside it, which is the only thing that can tell a
  September projection from a November one.

  Two columns per player, and they are separate because they are asked by
  different consumers:

    :nflverse/season-to-date {:games n :stats {stat-key season-total}}
    :nflverse/recent         {:games n :stats {stat-key window-total}}

  `season-to-date` is the evidence `rankings.ros` blends against the preseason
  projection. `recent` is the last `recent-window` weeks only — the breakout
  signal, which is a *display* column and feeds no score (same shelf as
  `:injury-risk` and `:tcm`).

  THE STAT MAP IS WIDER THAN `nflverse/line-columns`, DELIBERATELY. That one
  carries seven columns because it exists to *show* three seasons in a tile.
  This one exists to *score* a partial season under the league's own weights, so
  it needs every column a scoring weight can reach — interceptions, two-point
  conversions, fumbles, field goals. It is keyed by Sleeper stat keys for the
  same reason `line-columns` is: a realized week and a projected season then sit
  in the same vocabulary, which is what lets `rankings.ros` blend them without
  translating between two spellings of the same stat.

  REGULAR SEASON ONLY. The file carries `season_type` POST rows too, and a
  playoff week counted as a regular-season game is a corrupted denominator under
  every per-game rate downstream — it is the same class of error as
  `nflverse/row->games` clamping an 18-game row to the season's length.

  A MISSING FILE IS WEEK ZERO, NOT AN OUTAGE. Before the season opens the asset
  does not exist (checked: `stats_player_week_2026.csv` 404s in August 2026).
  That is the normal preseason state, not a failure — the source reports
  unavailable, `rankings.ros` falls back to the prorated preseason line, and the
  board is the preseason board. Every consumer therefore has to work with these
  columns entirely absent.

  There are no DST rows here at all and a kicker's only usable columns are
  `fg_made`/`pat_made`, exactly as in `nflverse` — team defenses are a fantasy
  construct, not an nflverse player. Deciding what that *means* for a projection
  is `rankings.ros`'s job, not this namespace's."
  (:require [clojure.tools.logging :as log]
            [draft-day.ingestion.nflverse :as nflverse]))

(defn week-url [season]
  (str nflverse/base "/stats_player_week_" season ".csv"))

(def recent-window
  "How many of the most recent weeks the `:nflverse/recent` window spans.

  Three. One week is a game script, and much beyond three reaches back past the
  usage change a manager is trying to detect — a back who took over in week 9
  should read as the starter he now is, not as an average of the two roles.

  A chosen constant, not a measured one, exactly like
  `nflverse/availability-lookback`: `dev/draft_day/benchmark/` is where a number
  earns its place, and this one has not been there yet."
  3)

(def stat-columns
  "nflverse weekly column -> the Sleeper stat key `draft-day.scoring` speaks.

  Every column a `scoring/stat-keys` weight can actually reach in this file.
  The defensive keys (`:sack`, `:int`, `:fum_rec`, `:ff`, `:def_td`, `:safe`)
  are absent on purpose rather than by oversight: they score a *team* defense,
  and nflverse publishes no DST row for one — mapping them onto the individual
  defenders' columns would invent a fantasy player out of eleven real ones."
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

(def usage-columns
  "Opportunity columns carried alongside the scored line, for the trend signal.

  Volume, not production: a back who is suddenly getting the carries is a buy
  before the touchdowns arrive, and points alone cannot see that.

  Chances, not conversions — which is why `receptions` is *not* here even though
  it reads like usage. It is already in `stat-columns` as `:rec`, so carrying it
  again would put the same total on every player twice under two names; and a
  catch is an outcome of a target, so the column that answers 'is he being used
  more' is `targets`."
  {"targets" :targets
   "carries" :carries})

(def required-columns
  "Columns a response has to carry before it counts as this file at all.

  Same reasoning as `nflverse/required-columns`, and the same failure it guards:
  not a 404, but a 200 whose body is an error page or a truncated download. Such
  a body parses without throwing and yields rows carrying no week and no GSIS
  id, which would reach `accumulate` as a season in which nobody has played —
  and `rankings.ros` would then read every player in the league as having zero
  production through week zero, which is precisely the preseason state. A broken
  download must not be able to impersonate August."
  #{"player_id" "position" "week" "season_type"})

(defn as-of-week
  "The dev-only week ceiling from `DRAFTDAY_AS_OF_WEEK`, or nil.

  Rows after it are dropped, so a *completed* season replays as a season in
  progress. Without it the in-season half of the app cannot be seen working
  until games are actually played — the current season's file does not exist in
  preseason, and last season's file is 17 weeks of a season with nothing left to
  project. `DRAFTDAY_AS_OF_WEEK=8` against 2025 is a real week 8 board.

  Unparseable or non-positive values are ignored rather than throwing: this is a
  development affordance, and a typo in it should not take down ingestion."
  []
  (when-let [s (System/getenv "DRAFTDAY_AS_OF_WEEK")]
    (when-let [n (nflverse/num-or-nil s)]
      (when (pos? n) (long n)))))

(defn row-week
  "The row's regular-season week, or nil when it has none or is not REG.

  The `season_type` test lives here rather than in a separate filter so that
  every caller that asks a row for its week gets the REG gate for free — there
  is no way to hold a week from this file without having passed it."
  [row]
  (when (= "REG" (get row "season_type"))
    (some-> (nflverse/num-or-nil (get row "week")) long)))

(defn season-rows
  "Pure: raw parsed rows -> the regular-season rows this run should consider,
  each paired with its week and GSIS id: `[[gsis week row] ...]`.

  Rows with no week, no joinable id, or a week past `as-of` are dropped."
  ([rows] (season-rows rows (as-of-week)))
  ([rows as-of]
   (into []
         (keep (fn [row]
                 (let [w (row-week row)
                       g (nflverse/gsis-id row)]
                   (when (and w g (or (nil? as-of) (<= w as-of)))
                     [g w row]))))
         rows)))

(defn through-week
  "How far the season has got: the highest regular-season week present.

  Read off the data rather than computed from the calendar, which is what keeps
  this honest across the two ways it can be wrong. nflverse publishes a week a
  day or two after it is played, so a calendar week would run ahead of the
  evidence and prorate a rest-of-season line over games nobody has a stat line
  for yet; and `DRAFTDAY_AS_OF_WEEK` would be invisible to it entirely.

  0 for no rows at all, which is the preseason answer."
  [triples]
  (reduce (fn [acc [_ w _]] (max acc w)) 0 triples))

(defn- add-stats
  "Accumulate one row's columns into `acc` under `cols`.

  BLANK IS NOT ZERO, and it survives a sum: a key the source never gave this
  player a value for stays absent rather than summing to 0.0, so a consumer can
  still tell 'no opinion' from 'genuinely none'. A key present in one week and
  blank in another simply gains nothing that week, which is what a total means."
  [acc cols row]
  (reduce-kv (fn [m col k]
               (if-let [v (nflverse/num-or-nil (get row col))]
                 (update m k (fnil + 0.0) v)
                 m))
             acc cols))

(defn- totals
  "{:games n :stats {...}} over a player's [week row] pairs."
  [pairs cols]
  {:games (count pairs)
   :stats (reduce (fn [acc [_ row]] (add-stats acc cols row)) {} pairs)})

(defn accumulate
  "Pure: `season-rows` triples -> {gsis {:nflverse/season-to-date {...}
                                        :nflverse/recent {...}}}.

  `:games` is a count of the player's own regular-season rows, not of weeks
  elapsed, and that distinction is the whole point of carrying it. A player who
  has missed four games has four fewer rows; dividing his totals by weeks
  elapsed instead would charge him for the absence twice — once in the total,
  again in a per-game rate depressed by games he was never on the field for.

  The recent window is the last `recent-window` weeks *of the season*, not the
  player's own last three appearances. A back who has been inactive for a month
  should read as a back who has been inactive for a month, not as whoever he was
  the last time he played."
  [triples]
  (let [latest (through-week triples)
        floor  (- latest (dec recent-window))]
    (->> triples
         (reduce (fn [acc [gsis w row]] (update acc gsis (fnil conj []) [w row])) {})
         (reduce-kv
          (fn [acc gsis pairs]
            (let [recent (filterv (fn [[w _]] (>= w floor)) pairs)]
              (assoc acc gsis
                     (cond-> {:nflverse/season-to-date
                              (merge (totals pairs stat-columns)
                                     {:usage (:stats (totals pairs usage-columns))})}
                       (seq recent)
                       (assoc :nflverse/recent
                              (merge (totals recent stat-columns)
                                     {:usage (:stats (totals recent usage-columns))}))))))
          {}))))

(defn fetch-rows
  "Network: this season's parsed weekly rows, or nil when the release did not
  yield any.

  Every failure shape collapses to nil, and the preseason 404 is the ordinary
  one. A throw is caught locally rather than allowed to escape into
  `parallel/all`, where it would nil unrelated sources fetched alongside it; the
  message and type are logged rather than the stack, which for a read timeout is
  JDK frames all the way down."
  [season]
  (try
    (let [rows (some-> (nflverse/http-get-string (week-url season)) nflverse/parse-csv)]
      (when (and (seq rows)
                 (every? (set (keys (first rows))) required-columns))
        rows))
    (catch Exception e
      (log/warn "nflverse-weekly: season" season "unavailable:"
                (.getSimpleName (class e)) (ex-message e))
      nil)))

(defn fetch
  "Network: this season's per-player in-season columns, plus how far the season
  has got and the position index the join reports with.

  nil when the file is absent or unusable — which in preseason is simply the
  truth, and which `pipeline` reports as an unavailable source rather than as a
  season in which nobody has played."
  [season]
  (when-let [rows (fetch-rows season)]
    (let [triples (season-rows rows)]
      {:by-key       (accumulate triples)
       :through-week (through-week triples)
       :positions    (nflverse/row-positions (map (fn [[_ _ r]] r) triples))})))
