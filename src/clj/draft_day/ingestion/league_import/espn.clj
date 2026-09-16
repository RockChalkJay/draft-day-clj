(ns draft-day.ingestion.league-import.espn
  "ESPN provider for league import: a league's scoring and roster settings, and
  the cookie-bearing fetch its sibling `league-sync.espn` reuses.

  ESPN publishes no public league API. Every request carries the manager's own
  `SWID` and `espn_s2` cookies, which is why the app proxies this rather than
  calling it from the browser, and why nothing here may put a credential in a
  URL, a message or an `ex-data` — see `providers/redact`.

  It is http-kit like everything else. `ingestion.espn` uses the JDK client for
  one reason, a ~37MB player feed; a league document with all three views is
  tens of KB. That namespace is the player universe and shares only a hostname
  with this one.

  Two answers ESPN gives that are not failures at the HTTP layer and must be
  treated as ones: a rejected cookie can arrive as an HTML login page with
  status 200, and a league the account cannot see can arrive as a 200 whose
  body has no `:settings`. Reading either as an empty league is the shape
  `league-sync/normalize-rosters` warns about — a board on which the whole
  league is available.

  The stat and slot tables are ESPN's own numbering. The scoring ids are the
  ones already verified against its live feed; the rest are marked where they
  are not, because a wrong id here is silent — it prices a rule nobody set."
  (:require [clojure.string :as str]
            [jsonista.core :as json]
            [org.httpkit.client :as http]
            [draft-day.ingestion.league-import :as league-import]
            [draft-day.json :refer [mapper]]))

(def ^:private base
  "https://lm-api-reads.fantasy.espn.com/apis/v3/games/ffl/seasons/")

(defn normalize-swid
  "A SWID with its braces, whichever way it was pasted.

  ESPN's cookie carries them and so does every id in a league's `owners` array,
  so the two spellings must not diverge: a cookie authenticates either way,
  while a braceless SWID matches no team and the manager's own roster silently
  reads as somebody else's."
  [swid]
  (when-let [s (some-> swid str str/trim not-empty)]
    (if (str/starts-with? s "{") s (str "{" s "}"))))

(defn cookie-header
  "The one place a credential becomes a request. Nothing else may format one."
  [{:keys [swid espn-s2]}]
  {"Cookie" (str "SWID=" (normalize-swid swid) "; espn_s2=" espn-s2)
   "Accept" "application/json"})

(defn league-url [season league-id views]
  (str base season "/segments/0/leagues/" league-id
       "?" (str/join "&" (map #(str "view=" %) views))))

(defn status-error
  "What an ESPN status means to the manager, as `[status message]`.

  401 and 403 are kept apart on purpose. One says reconnect and the other says
  this league is not yours; collapsing them sends a manager to re-paste a
  cookie that was fine."
  [status]
  (case status
    401 [401 "ESPN rejected your credentials — reconnect your ESPN account."]
    403 [403 "This ESPN account cannot see that league."]
    404 [404 "ESPN league not found."]
    [502 (str "ESPN answered " status)]))

(defn get-json
  "Network: one league document, as parsed JSON. Throws ex-info with `:status`.

  `:require-key` is the guard against a 200 that is not the document asked for
  — a login page, or a league the account cannot read. Absent it, an empty map
  would flow on as a league nobody is rostered in."
  [{:keys [season league-id credentials views require-key]}]
  (let [url (league-url season league-id views)
        {:keys [status body error]} @(http/get url {:headers (cookie-header credentials)
                                                    :timeout 30000})]
    (cond
      error
      (throw (ex-info "ESPN league fetch failed" {:status 502}))

      (not= 200 status)
      (let [[s msg] (status-error status)] (throw (ex-info msg {:status s})))

      :else
      (let [parsed (try (json/read-value body mapper) (catch Exception _ nil))]
        (if (and (map? parsed) (get parsed require-key))
          parsed
          (throw (ex-info "ESPN did not return that league — your session may have expired."
                          {:status 502})))))))

(defmethod league-import/fetch-raw-league :espn
  [_ {:keys [league-id season credentials]}]
  (get-json {:season season :league-id league-id :credentials credentials
             :views ["mSettings"] :require-key :settings}))

(def stat-ids
  "ESPN statId -> the app's scoring key.

  Verified against ESPN's live feed. Note 23 is rushing *attempts*, not yards —
  reading the band 23/24/25 in the wrong order prices every back on carries at
  roughly a tenth of what he is worth, and nothing fails.

  The defensive ids are ESPN's documented numbering and are NOT verified here;
  `draft-day.integration` is where a real league's settings page confirms them.
  `:def_td` is deliberately absent: ESPN splits return, interception and fumble
  touchdowns across several ids and the app holds one flat weight, so summing
  them would be a number nobody set. It is reported unsupported instead."
  {3   :pass_yd
   4   :pass_td
   19  :pass_2pt
   20  :pass_int
   24  :rush_yd
   25  :rush_td
   26  :rush_2pt
   42  :rec_yd
   43  :rec_td
   44  :rec_2pt
   53  :rec
   72  :fum_lost
   83  :fgm
   86  :xpm
   95  :int
   96  :fum_rec
   97  :blk_kick
   98  :safe
   99  :sack})

(def stat-labels
  "What to call an ESPN rule the app cannot score.

  Reporting a bare integer to a manager comparing this against his league's
  settings page is no report at all. Only the ids a real league actually sets
  are named; anything else falls back to its number."
  {23  "rushing attempts"    58  "targets"
   68  "fumbles"             80  "field goals 0-39"
   81  "field goals 40-49"   82  "field goals 50+"
   84  "field goals missed"  87  "extra points attempted"
   88  "extra points missed" 89  "points allowed 0"
   90  "points allowed 1-6"  91  "points allowed 7-13"
   92  "points allowed 14-17" 93 "points allowed 18-21"
   94  "points allowed 22-27" 101 "kickoff return TD"
   102 "punt return TD"      103 "interception return TD"
   104 "fumble return TD"    105 "blocked kick return TD"
   120 "points allowed"      127 "yards allowed"})

(defn stat-label [id] (get stat-labels id (str "ESPN stat " id)))

(defn scored?
  "Does this rule move anybody's score? A rule set to zero costs the league
  nothing and is not worth reporting."
  [{:keys [points pointsOverrides]}]
  (or (and (number? points) (not (zero? points)))
      (seq pointsOverrides)))

(defn scoring-config
  "Pure: ESPN's scoring items -> `{stat-key weight}` over the keys the app
  scores. A rule carrying `pointsOverrides` contributes its base weight; the
  override itself is reported unsupported."
  [items]
  (into {}
        (keep (fn [{:keys [statId points]}]
                (when-let [k (stat-ids statId)]
                  [k (double (or points 0))])))
        items))

(defn unsupported-scoring
  "The league's own rules a flat stat-line model cannot score, sorted.

  A `pointsOverrides` rule is reported even when its statId maps, which is the
  case a `select-keys` would miss: ESPN writes a TE reception premium as a
  per-position override on an ordinary receptions rule, so taking the base
  weight and dropping the override yields a config that looks complete and
  scores differently from the league it came from."
  [items]
  (->> items
       (filter scored?)
       (keep (fn [{:keys [statId pointsOverrides]}]
               (cond
                 (seq pointsOverrides) (str (stat-label statId) " (position-specific)")
                 (nil? (stat-ids statId)) (stat-label statId))))
       distinct sort vec))

(def lineup-slots
  "ESPN lineupSlotId -> the seat name `db/flex-slots` and `db/held-slots` speak.

  ESPN's documented numbering, not a payload this repo has read — see the ns
  docstring. An unlisted slot passes through as its id so it lands on the bench
  rather than being mistaken for a seat that scores."
  {0  "QB"   1  "QB"          2  "RB"   3  "WRRB_FLEX"
   4  "WR"   5  "REC_FLEX"    6  "TE"   7  "SUPER_FLEX"
   8  "DT"   9  "DE"          10 "LB"   11 "DL"
   12 "CB"   13 "S"           14 "DB"   15 "DP"
   16 "DST"  17 "K"           18 "P"    19 "HC"
   20 "BENCH" 21 "IR"         23 "FLEX" 24 "EDR"})

(def ir-slot
  "The seat that holds a player without giving him one.

  It is why the roster config counts ESPN's slots rather than its seat total:
  counting IR toward the bench inflates `roster-size`, and that is the single
  number `waiver/drop-candidate` asks to decide whether a claim costs a drop."
  21)

(defn slot-counts
  "Pure: ESPN's `lineupSlotCounts` -> `{slot-id count}` with real numbers for
  keys. They arrive keywordized, because `draft-day.json/mapper` keywordizes
  every decoded key and these are integers."
  [counts]
  (into (sorted-map)
        (keep (fn [[k v]]
                (when (pos? (or v 0))
                  [(parse-long (name k)) v])))
        counts))

(defn roster-positions
  "Pure: ESPN's slot counts -> the league's seats, in ascending slot id.

  The order is ours, not ESPN's: unlike Sleeper, an ESPN roster entry names its
  own seat on `lineupSlotId`, so nothing downstream is positional against this
  list. It exists for the seat *vocabulary* `rankings.lineup` fills from."
  [counts]
  (into [] (mapcat (fn [[id n]] (repeat n (get lineup-slots id (str id)))))
        (slot-counts counts)))

(def ^:private config-keys
  {"QB" :qb "RB" :rb "WR" :wr "TE" :te "K" :k "DST" :dst
   "FLEX" :flex "WRRB_FLEX" :flex "REC_FLEX" :flex "SUPER_FLEX" :flex})

(defn roster-config
  "Pure: ESPN's slot counts -> the app's `{:qb :rb ... :bench}` roster config.

  Every flex spelling pools into `:flex` and anything the app cannot express —
  an IDP seat, a head coach — counts as bench depth, which is what it is. IR is
  the one seat that counts as neither; see `ir-slot`."
  [counts]
  (reduce (fn [cfg [id n]]
            (if (= ir-slot id)
              cfg
              (let [k (config-keys (get lineup-slots id))]
                (update cfg (or k :bench) + n))))
          {:qb 0 :rb 0 :wr 0 :te 0 :flex 0 :k 0 :dst 0 :bench 0}
          (slot-counts counts)))

(defn waiver-settings
  "Pure: a raw ESPN league -> `{:type :budget}`.

  Anything not plainly FAAB reads `:rolling`, the direction
  `league-import.sleeper/waiver-types` argues for: suppressing a bid costs a
  column, while inventing one puts a confident number on a transaction that
  does not exist."
  [raw]
  (let [a (get-in raw [:settings :acquisitionSettings])]
    {:type   (if (:isUsingAcquisitionBudget a) :faab :rolling)
     :budget (or (:acquisitionBudget a) 0)}))

(defmethod league-import/normalize-league :espn
  [_ raw]
  (let [items  (get-in raw [:settings :scoringSettings :scoringItems])
        counts (get-in raw [:settings :rosterSettings :lineupSlotCounts])]
    {:scoring             (scoring-config items)
     :unsupported-scoring (unsupported-scoring items)
     :roster              (roster-config counts)
     :num-teams           (or (get-in raw [:settings :size]) (count (:teams raw)))
     :name                (get-in raw [:settings :name])
     :season              (str (:seasonId raw))}))
