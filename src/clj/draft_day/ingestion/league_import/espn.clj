(ns draft-day.ingestion.league-import.espn
  "ESPN provider for league import: a league's scoring and roster settings, and
  the cookie-bearing fetch its sibling `league-sync.espn` reuses.

  ESPN publishes no public league API. Every request carries the manager's own
  `SWID` and `espn_s2` cookies, which is why the app proxies this rather than
  calling it from the browser, and why nothing here may put a credential in a
  URL, a message or an `ex-data` — see `providers/redact`.

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
            [draft-day.db :as db]
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
  "The one place a credential becomes a request. Nothing else may format one.

  Trimmed here because `providers/field-error` validates the *trimmed* value
  while the browser stores what was pasted: a cookie copied out of DevTools
  with a trailing newline passes the form and then rides into the header
  verbatim, and ESPN answers 401 — telling the manager to re-paste a credential
  that was correct."
  [{:keys [swid espn-s2]}]
  {"Cookie" (str "SWID=" (normalize-swid swid)
                 "; espn_s2=" (some-> espn-s2 str str/trim))
   "Accept" "application/json"})

(defn league-url
  "The league document's URL: its views, plus whatever `params` addresses it by.

  A view names a slice of the document; a param names which document —
  `scoringPeriodId` is how the boxscore asks for one week rather than today's."
  ([season league-id views] (league-url season league-id views nil))
  ([season league-id views params]
   (str base season "/segments/0/leagues/" league-id
        "?" (str/join "&" (concat (map #(str "view=" %) views)
                                  (map (fn [[k v]] (str (name k) "=" v)) params))))))

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
  [{:keys [season league-id credentials views require-key params]}]
  (let [url (league-url season league-id views params)
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

  The defensive ids came from ESPN's documented numbering rather than a payload;
  `draft-day.integration` is what confirms them against a real league.
  `:def_td` is deliberately absent: ESPN splits return, interception and fumble
  touchdowns across several ids and the app holds one flat weight, so summing
  them would be a number nobody set. It is reported unsupported instead.

  Every defensive id here is also in `defense-only-stats`, which is what lets
  its per-position override be read; adding one here means adding it there.

  A value may be a *vector*, for a rule whose grid is coarser than the stat
  line's: one ESPN weight for every kick under forty covers three buckets, and
  its total-FG-missed rule covers both miss buckets. The kicking ids were read
  off a live league's scoring page; 201 (FG 60+) is unmapped on purpose, since
  folding it into `:fgm_50p` pays a fifty-yarder at the sixty-yard rate."
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
   77  :fgm_40_49
   80  [:fgm_0_19 :fgm_20_29 :fgm_30_39]
   83  :fgm
   85  [:fgmiss_40_49 :fgmiss_50p]
   86  :xpm
   88  :xpmiss
   95  :int
   96  :fum_rec
   97  :blk_kick
   98  :safe
   99  :sack
   198 :fgm_50p})

(def stat-labels
  "What to call an ESPN rule the app cannot score.

  Reporting a bare integer to a manager comparing this against his league's
  settings page is no report at all. Only the ids a real league actually sets
  are named; anything else falls back to its number.

  Three kicking labels that used to sit here named the wrong rules, and a wrong
  name is worse than a number: a live league puts those bands on 77, 198 and
  201, and its misses on 85."
  {23  "rushing attempts"    58  "targets"
   68  "fumbles"             87  "extra points attempted"
   201 "field goals 60+"     89  "points allowed 0"
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

(def dst-position-id
  "ESPN's `defaultPositionId` for a team defense, and the key its per-position
  scoring overrides are filed under. `league-sync.espn` reads it from here so
  the number has one home."
  16)

(def ^:private dst-override-key
  "The same id as `pointsOverrides` carries it: `draft-day.json/mapper`
  keywordizes every decoded key, and these keys are position ids."
  (keyword (str dst-position-id)))

(def defense-only-stats
  "The stat ids nobody but a team defense can accrue.

  ESPN files their weights as a D/ST override on a rule whose base is 0.0, so
  here the override is the entire rule rather than a premium over one. Kept
  narrow on purpose: a rule a skill player can earn too needs two weights, and
  belongs in `unsupported-scoring` beside the reception premium.

  Every defense-only id `stat-ids` maps belongs here. One that does not imports
  at 0.0 with nothing failing to say so, which is the bug this set exists for."
  #{95 96 97 98 99})

(defn stat-weight
  "Pure: one scoring item -> the weight the app scores it at. The D/ST override
  wins for a defense-only stat, being the only weight ESPN states for it;
  everything else takes the base a premium would be a premium over."
  [{:keys [statId points pointsOverrides]}]
  (double (or (when (defense-only-stats statId)
                (get pointsOverrides dst-override-key))
              points
              0)))

(defn stat-keys-for
  "The app's scoring keys for one ESPN stat id, as a seq. Empty for an id the
  app cannot score. The one place `stat-ids`' scalar-or-vector value is
  unwrapped."
  [id]
  (when-let [k (stat-ids id)]
    (if (vector? k) k [k])))

(defn scoring-config
  "Pure: ESPN's scoring items -> `{stat-key weight}` over the keys the app
  scores, each weighted by `stat-weight`. One id may carry several keys — see
  `stat-ids`."
  [items]
  (into {}
        (mapcat (fn [{:keys [statId] :as item}]
                  (let [w (stat-weight item)]
                    (map (fn [key] [key w]) (stat-keys-for statId)))))
        items))

(defn dropped-overrides
  "Pure: the per-position overrides on one item the import could not apply.

  Empty for a defense-only stat whose sole override `stat-weight` took, so
  Settings stops warning about scoring it imported correctly. A second position
  on one of those still reports: one weight cannot hold both."
  [{:keys [statId pointsOverrides]}]
  (cond-> pointsOverrides
    (defense-only-stats statId) (dissoc dst-override-key)))

(defn unsupported-scoring
  "The league's own rules a flat stat-line model cannot score, sorted.

  An override is reported even when its statId maps, the case a `select-keys`
  would miss: ESPN writes a TE reception premium as a per-position override on
  an ordinary receptions rule, so keeping the base alone yields a config that
  looks complete and scores differently. `dropped-overrides` separates that from
  an override the import does apply."
  [items]
  (->> items
       (filter scored?)
       (keep (fn [{:keys [statId] :as item}]
               (cond
                 (seq (dropped-overrides item)) (str (stat-label statId) " (position-specific)")
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
  "Pure: ESPN's slot counts -> the league's seats, in `db/seat-order` rather
  than ESPN's slot numbering, since the matchup board draws one row per seat in
  this order. Seats it does not rank — IDP, the bench, IR — keep ESPN's order
  behind the rest."
  [counts]
  (->> (slot-counts counts)
       (mapcat (fn [[id n]] (repeat n (get lineup-slots id (str id)))))
       (sort-by db/slot-rank)
       vec))

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

(defn auction-budget
  "Pure: what each team starts the draft with, or nil for anything but an
  auction. ESPN fills `auctionBudget` in a snake league too, so the draft type
  is what decides, the way `league-import.sleeper/auction-budget` reads it."
  [raw]
  (let [d (get-in raw [:settings :draftSettings])]
    (when (= "AUCTION" (:type d))
      (:auctionBudget d))))

(defmethod league-import/normalize-league :espn
  [_ raw]
  (let [items  (get-in raw [:settings :scoringSettings :scoringItems])
        counts (get-in raw [:settings :rosterSettings :lineupSlotCounts])]
    {:scoring             (scoring-config items)
     :unsupported-scoring (unsupported-scoring items)
     :roster              (roster-config counts)
     ;; No fallback to `(count (:teams raw))`: this fetch asks for `mSettings`
     ;; alone, so `:teams` is never in the document and the count would always
     ;; be 0 — which `:apply-config` writes straight into the live config and
     ;; `db/make-teams` turns into a league with no teams. nil is what
     ;; `league-import.sleeper` answers when the field is missing, and it leaves
     ;; the manager's own team count standing.
     :num-teams           (get-in raw [:settings :size])
     :starting-bankroll   (auction-budget raw)
     :name                (get-in raw [:settings :name])
     :season              (str (:seasonId raw))}))
