(ns draft-day.sensei.tools
  "Sensei's tools: what a model may ask Draft Day, each answered off the board
  the Waivers tab draws.

  A tool states the engine's numbers and never computes one of its own, so
  Sensei and the screen cannot disagree. The rows are the waiver board's columns
  read through `db/waiver-sort-accessors`, and their legend is the column
  catalog's tooltips, so a column's meaning is written once.

  What the screen holds back, the tools hold back: the bid model's predictions
  while `db/bid-predictions?` is off, and raw rest-of-season points, which no
  surface shows. A row is compact on purpose, since every token of it is spent
  against the manager's Claude plan.

  A manager may play in several leagues, so a tool that is about one takes a
  `league` argument, resolved by `context/pick`. The tools read the server
  through `deps` rather than by requiring `routes`, which mounts them."
  (:require [clojure.string :as str]
            [draft-day.db :as db]
            [draft-day.ingestion.match :as match]
            [draft-day.providers :as providers]
            [draft-day.rankings.lineup :as lineup]
            [draft-day.rankings.waiver :as waiver]
            [draft-day.sensei.context :as context]))

(defn per-league
  "`f` of a league entry, computed at most once per league for as long as the
  returned fn lives: one request, so a batch that asks after the same league
  twice prices its board once, and one that asks after two prices two."
  [f]
  (let [seen (atom {})]
    (fn [entry]
      @(get (swap! seen #(cond-> % (not (contains? % (:key entry))) (assoc (:key entry) (delay (f entry)))))
            (:key entry)))))

(defn deps
  "What the tools read for one request, each computed at most once and only
  when a tool asks: `:found`, the accounts' leagues; `:body` and `:reply`, fns of
  a league entry giving its /api/waivers body and the waiver board for it;
  `:players`, the universe; `:news`, a fn of an ESPN id; and `:accounts`, whose
  secrets the reply is masked against. The server's half is
  `routes/sensei-server`."
  [{:keys [waivers-reply matchup-reply players news env]}]
  (let [accounts (context/accounts (env))
        body     (per-league context/league-context)]
    {:accounts accounts
     :found    (delay (context/leagues accounts))
     :body     body
     :reply    (per-league (fn [entry] (let [b (body entry)] (if (:error b) b (waivers-reply b)))))
     ;; Credentials go to the host with the request, never to the model.
     :matchup  (per-league (fn [entry]
                             (let [b (body entry)]
                               (if (:error b)
                                 b
                                 (matchup-reply
                                  (merge (select-keys entry [:provider :league-id :season :credentials])
                                         (select-keys b [:scoring :league :roster :my-roster-id])))))))
     :players  (delay (players))
     :news     news}))

(defn tool-error
  "A failure the model should read, as opposed to a bug."
  [msg]
  (ex-info msg {:sensei/tool-error true}))

(defn checked
  "`v`, or a tool error when it is an `{:error msg}`."
  [v]
  (if-let [e (and (map? v) (:error v))]
    (throw (tool-error e))
    v))

(defn need
  "The value behind one of `deps`' delays; a tool error when it is an error."
  [deps k]
  (checked @(get deps k)))

(defn the-league
  "The league entry a call is about, off its `league` argument."
  [deps {:keys [league]}]
  (checked (context/pick (need deps :found) league)))

(defn named
  "A league as a reply states it, so every answer says which league it is about."
  [entry]
  (select-keys entry [:key :name]))

(def row-columns
  "The waiver board's columns a row carries, by `db/waiver-column-catalog` key.
  Name, position, team and opponent lead every row outside this list, because
  their cells are formatted rather than read."
  [:bye :week :week-rank :pts :avg :gp :form :trend :tgt :car :rec :yds :td
   :adds :lineup :upgrade :ros-vorp :risk])

(defn columns
  "`row-columns`, and the bid model's while it is on."
  []
  (cond-> row-columns
    db/bid-predictions? (into [:typical :bid :rivals])))

(defn rounded
  "A double to one place, or two for the trend ratio, which lives near 1.0."
  [k v]
  (if (and (number? v) (not (integer? v)))
    (let [scale (if (= k :trend) 100.0 10.0)]
      (/ (Math/round (* scale (double v))) scale))
    v))

(defn compact
  "`[k v]` pairs as a map in their order, nil values dropped and numbers
  `rounded`."
  [pairs]
  (->> pairs
       (keep (fn [[k v]] (when (some? v) [k (rounded k v)])))
       (apply concat)
       (apply array-map)))

(defn opponent
  "`vs SEA` or `@ SEA`, off either source `util/week-matchup` reads; nil on a
  bye or before a schedule."
  [p]
  (when-let [opp (or (:week/opponent p) (:kickoff/opponent p))]
    (let [home? (if (some? (:week/home? p)) (:week/home? p) (:kickoff/home? p))]
      (str (case home? true "vs " false "@ " "") opp))))

(defn injury
  "Sleeper's designation with the body part, e.g. `Questionable (Hamstring)`."
  [p]
  (when-let [status (:sleeper/injury-status p)]
    (cond-> status
      (:sleeper/injury-body-part p) (str " (" (:sleeper/injury-body-part p) ")"))))

(defn ros-ecr
  "The experts' rest-of-season rank at his position, with the spread of their
  individual ranks: `RB24 (24-29)`. Nil for a player they do not rank."
  [{:fantasypros/keys [ros-pos-rank ros-rank-min ros-rank-max]}]
  (when ros-pos-rank
    (cond-> ros-pos-rank
      (and ros-rank-min ros-rank-max) (str " (" ros-rank-min "-" ros-rank-max ")"))))

(defn percent
  "A share as a whole percent, or nil."
  [share]
  (when (number? share) (Math/round (* 100.0 (double share)))))

(defn whole
  "A count as a whole number, or nil."
  [n]
  (when (number? n) (Math/round (double n))))

(defn role
  "`since wk 3, De'Von Achane IR` for a player who took over a vacated role."
  [{:role/keys [since vacated-by vacated-status]}]
  (when (and since vacated-by)
    (str "since wk " since ", " vacated-by (some->> vacated-status (str " ")))))

(defn row
  "One player as the tools state him: a few keys in a fixed order, nothing nil,
  numbers rounded, and no column the board would hide at his position (a
  quarterback's targets read 0.0, which says nothing)."
  [p]
  (let [pos  (:position p)
        lead [[:name (:player-name p)]
              [:player-id (:player-id p)]
              [:position pos]
              [:pos-rank (db/season-rank p)]
              [:team (:team p)]
              [:slot (:slot p)]
              [:opp (opponent p)]
              [:inj (injury p)]
              [:outlook (:outlook-ppg p)]
              [:ros-ecr (ros-ecr p)]
              [:snap (percent (:snap-share p))]
              [:snap-season (percent (:snap-share-season p))]
              [:role (role p)]
              [:rz-rush (whole (:rz-rush p))]
              [:rz-tgt (whole (:rz-tgt p))]
              [:rz-pass (whole (:rz-pass p))]
              [:drop-candidate (when (:drop? p) true)]
              [:drop-cost (:drop-cost p)]
              [:unvalued (when (:unvalued? p) true)]]
        cols (keep (fn [k] (when (db/applies? (db/waiver-columns-by-key k) pos)
                             [k ((db/waiver-sort-accessors k) p)]))
                   (columns))]
    (compact (concat lead cols))))

(defn legend
  "What each row key means, off the column catalog's tooltips."
  []
  (->> (columns)
       (keep (fn [k] (when-let [tip (:tooltip (db/waiver-columns-by-key k))]
                       (str "- " (name k) ": " tip))))
       (into ["- pos-rank: his rank at his position on fantasy points so far (preseason rank before week 1)"
              "- opp: this week's opponent"
              "- inj: Sleeper's injury designation"
              "- outlook: Sleeper's projected points a game over the weeks he has left, under this league's scoring"
              "- ros-ecr: FantasyPros' experts' rest-of-season rank at his position in this league's scoring format, with the range of their ranks; absent when they do not rank him"
              "- snap: percent of his team's offensive snaps over his last three weeks played; snap-season is the season's"
              "- rz-rush, rz-tgt, rz-pass: his carries, targets and passes inside the 20 this season through the last week played, as Sleeper counts them; absent means none recorded"
              "- role: set when a teammate at his position who held the role went on injured reserve and he has taken it over, with the week it changed"
              "- drop-cost: rest-of-season points the starting lineup loses if he is dropped; 0 on a bench that never plays"])
       (str/join "\n")))

(defn find-team
  "The team of `teams` that `q` names, by roster id or case-folded name, or a
  tool error listing the choices."
  [teams q]
  (let [q    (str/lower-case (str/trim (str q)))
        hits (filter #(or (= q (str/lower-case (str (:roster-id %))))
                          (= q (str/lower-case (str (:name %)))))
                     teams)]
    (if (= 1 (count hits))
      (first hits)
      (throw (tool-error (str (if (seq hits) "More than one team matches " "No team matches ")
                              (pr-str q) ". Choices: "
                              (str/join "; " (map #(str (:roster-id %) " " (:name %)) teams))))))))

(defn roster-of
  "A team in one league as the tools read it: the league's entry, waiver reply
  and lineup `:slots`, and the roster split by `db/team-roster` (`:starters`,
  `:bench`, `:parked`) and the board-spaced `:active-ids`. The manager's own
  unless `args` names a `team`, whose rows are the reply's
  `:league-roster-players`."
  [deps {:keys [team] :as args}]
  (let [entry   (the-league deps args)
        {:keys [league my-roster-id roster]} (checked ((:body deps) entry))
        reply   (checked ((:reply deps) entry))
        players (need deps :players)
        mine    (some #(when (= my-roster-id (:roster-id %)) %) (:teams league))
        team    (if (str/blank? team) mine (find-team (:teams league) team))
        xwalk   (db/provider->player-id players (:provider league) (:provider-players league))
        priced  (if (= team mine)
                  (:my-roster-players reply)
                  (get (:league-roster-players reply) (:roster-id team)))
        ;; As the `:league-rosters` sub builds it: the waiver reply's rows over
        ;; the universe's, so a player the board did not value still shows.
        by-id   (into (into {} (map (juxt :player-id identity)) players)
                      (map (juxt :player-id identity))
                      priced)]
    (assoc (db/team-roster team league xwalk by-id)
           :entry entry :reply reply :team team :mine? (= team mine)
           ;; As `routes/waivers-reply` seats the board.
           :slots (some-> roster db/starting-slots)
           :active-ids (set (db/held-ids team xwalk :active-ids)))))

(defn my-team
  "The manager's roster split as My Team draws it, through `db/team-roster`,
  with the seat a claim would cost marked."
  [deps args]
  (let [{:keys [entry reply team starters bench parked]} (roster-of deps args)
        drop-id (get-in reply [:drop-candidate :player-id])
        costs   (:drop-costs reply)
        ;; Flagged before `row`, as the `:my-roster` sub flags it, so the row
        ;; keeps its key order.
        rows    (fn [ps] (mapv #(row (assoc % :drop?     (= drop-id (:player-id %))
                                                :drop-cost (get costs (:player-id %))))
                               ps))]
    {:league          (named entry)
     :team            (:name team)
     :record          (select-keys team [:wins :losses])
     :week            (:week reply)
     :through-week    (:through-week reply)
     :faab            (select-keys (:faab reply) [:type :budget :left])
     :claims-left     (:claims-left reply)
     :drop-candidate  (some-> (:drop-candidate reply) (select-keys [:player-id :player-name :position]))
     :starters        (rows starters)
     :bench           (rows bench)
     :injured-reserve (rows parked)}))

(defn team-roster
  "Any team in the league, split as My Team draws it. Its rows lack the
  columns about the manager's own lineup (`lineup`, `upgrade`, `drop-cost`),
  since those are priced against his seats."
  [deps args]
  (let [{:keys [entry reply team starters bench parked mine?]} (roster-of deps args)]
    (compact [[:league          (named entry)]
              [:team            (:name team)]
              [:roster-id       (:roster-id team)]
              [:mine            (when mine? true)]
              [:record          (select-keys team [:wins :losses :ties])]
              [:points-for      (:points-for team)]
              [:week            (:week reply)]
              [:through-week    (:through-week reply)]
              [:starters        (mapv row starters)]
              [:bench           (mapv row bench)]
              [:injured-reserve (mapv row parked)]])))

(defn league-teams
  "Every team in the league in standings order, mine marked."
  [deps args]
  (let [entry (the-league deps args)
        {:keys [league my-roster-id]} (checked ((:body deps) entry))
        reply (checked ((:reply deps) entry))]
    {:league       (named entry)
     :week         (:week reply)
     :through-week (:through-week reply)
     :teams        (mapv (fn [t]
                           (compact [[:team       (:name t)]
                                     [:roster-id  (:roster-id t)]
                                     [:mine       (when (= my-roster-id (:roster-id t)) true)]
                                     [:wins       (:wins t)]
                                     [:losses     (:losses t)]
                                     [:ties       (:ties t)]
                                     [:points-for (:points-for t)]
                                     [:faab-left  (:faab-left t)]]))
                         (db/record-order (:teams league)))}))

(defn seat-row
  "One lineup seat of a matchup: the slot, the player and this week's
  projection, with the score once he has played."
  [{:keys [slot player-name position team week-points actual empty? unvalued?] :as p}]
  (compact [[:slot slot]
            [:name (when-not empty? player-name)]
            [:position position]
            [:team team]
            [:opp (opponent p)]
            [:inj (injury p)]
            [:proj week-points]
            [:actual actual]
            [:empty (when empty? true)]
            [:unvalued (when unvalued? true)]]))

(defn side
  "One team's side of a matchup: projected total, lineup and best lineup."
  [t]
  (let [best (get-in t [:optimal :projected])]
    (compact [[:team      (:name t)]
              [:roster-id (:roster-id t)]
              [:record    (select-keys t [:wins :losses :ties])]
              [:projected (:projected t)]
              [:actual    (:actual t)]
              [:best-lineup-gain (:gain best)]
              [:starters  (mapv seat-row (:starters t))]
              [:bench     (mapv seat-row (:bench t))]])))

(defn matchup
  "The manager's game this week: both lineups with the week's projections."
  [deps args]
  (let [entry (the-league deps args)
        reply (checked ((:matchup deps) entry))
        mine  (str (:my-roster-id reply))
        pair  (some #(when (some #{mine} (map str (:roster-ids %))) %) (:matchups reply))
        opp   (some->> pair :roster-ids (map str) (remove #{mine}) first)
        team  (fn [id] (some #(when (= id (str (:roster-id %))) %) (:teams reply)))]
    (if-not opp
      {:league (named entry) :week (:week reply) :opponent nil
       :note   "no opponent this week (a bye); the lineups are not compared"}
      {:league   (named entry)
       :week     (:week reply)
       :mine     (side (team mine))
       :opponent (side (team opp))})))

(def max-targets 25)

(def claim-positions
  "What a waiver claim is about. A kicker or a defense is streamed for a week,
  and a list that led with one buried the quarterback, back and receiver a
  manager came to ask about; they come back when named."
  (filterv (complement #{"K" "DST"}) db/positions))

(defn position-arg
  "The `position` argument as the board spells it, or nil for none. Checked
  before any league is read, so a bad one costs no board."
  [position]
  (let [pos (some-> position str str/upper-case)]
    (when (and pos (not (some #{pos} db/positions)))
      (throw (tool-error (str "position must be one of " (str/join ", " db/positions)))))
    pos))

(defn waiver-targets
  "The free agents ranked as the Waivers tab's # column ranks them
  (`db/waiver-rank-key`), at one position or all. The tab opens sorted by Adds
  instead, which is a sort and not a claim order."
  [deps {:keys [position limit] :as args}]
  (let [pos       (position-arg position)
        positions (if pos [pos] claim-positions)
        n         (-> (if (number? limit) (long limit) 10) (max 1) (min max-targets))
        entry     (the-league deps args)
        reply     (checked ((:reply deps) entry))]
    {:league         (named entry)
     :week           (:week reply)
     :through-week   (:through-week reply)
     :positions      positions
     :drop-candidate (get-in reply [:drop-candidate :player-name])
     :players        (->> (:players reply)
                          (filter (comp (set positions) :position))
                          (sort-by db/waiver-rank-key)
                          (take n)
                          (mapv row))}))

(defn find-players
  "Universe players matching `q`: by player id exactly, else by name through
  `match/normalize-name`, the same key every join uses. A query that normalizes
  to nothing, such as `Jr.`, names nobody, since a row with no name shares that
  key."
  [players q]
  (let [q (str/trim (str q))
        k (match/normalize-name q)]
    (or (when-not (str/blank? q) (seq (filter #(= q (:player-id %)) players)))
        (when-not (str/blank? k) (filter #(= k (match/normalize-name (:player-name %))) players)))))

(defn the-player
  "The one row of `players` that `q` names (`find-players`), or a tool error
  saying there is none, or listing the ids to ask again by. `what` names the
  pool, as in `no free agent matches`."
  [players q what]
  (let [[p & more :as hits] (find-players players q)]
    (cond
      (empty? hits) (throw (tool-error (str "no " what " matches " (pr-str q))))
      more          (throw (tool-error (str (pr-str q) " matches more than one " what "; ask again by id: "
                                            (str/join "; " (map #(str (:player-id %) " " (:player-name %) " "
                                                                      (:position %) " " (:team %))
                                                                hits)))))
      :else         p)))

(defn this-week-seat
  "What claiming `cand` for `drop-p` does to `roster`'s best lineup by this
  week's projection: `:gain` in points, plus the `:seat` he takes and whom he
  `:displaces` when he starts. Players with no projection (a bye) are not
  seated, as `lineup/best-lineup` skips them."
  [roster cand drop-p slots]
  (let [kept   (remove #(= (:player-id %) (:player-id drop-p)) roster)
        before (lineup/best-lineup roster slots :week-points)
        after  (lineup/best-lineup (conj (vec kept) cand) slots :week-points)
        seated (into #{} (map (comp :player-id second)) after)
        seat   (some (fn [[slot p]] (when (= (:player-id cand) (:player-id p)) slot)) after)]
    (cond-> {:gain (- (lineup/seated-points after :week-points)
                      (lineup/seated-points before :week-points))}
      seat (assoc :seat      seat
                  :displaces (some (fn [[_ p]]
                                     (when (and (not (contains? seated (:player-id p)))
                                                (not= (:player-id drop-p) (:player-id p)))
                                       p))
                                   before)))))

(defn evaluate-claim
  "What claiming `add` and dropping `drop` does to the manager's lineup, off the
  waiver reply's own rows. `lineup` and `upgrade` are over the rest of the
  season, as the board states them. `lineup-this-week` is the change in the best
  lineup by this week's projections, which is not the add's own projection (his
  is `add.week`); `starts-this-week` and `displaces` say where he would play and
  whom he would push out. `drop` defaults to the board's drop candidate, and
  with no seat to give up there is none."
  [deps {:keys [add drop] :as args}]
  (let [{:keys [entry reply team slots starters bench active-ids]} (roster-of deps args)
        _      (cond
                 (nil? team)    (throw (tool-error "the manager's team is not in this league's sync, so a claim cannot be priced"))
                 (empty? slots) (throw (tool-error "this league's lineup seats are unknown, so a claim cannot be priced")))
        ;; The board prices from the active ids, so a starter id the host lists
        ;; but does not hold actively cannot make this tool disagree with it.
        roster (filterv #(contains? active-ids (:player-id %)) (concat starters bench))
        cand   (the-player (:players reply) add "free agent")
        drop-p (if-not (str/blank? drop)
                 (the-player roster drop "player on the active roster")
                 (let [id (get-in reply [:drop-candidate :player-id])]
                   (some #(when (= id (:player-id %)) %) roster)))
        week   (this-week-seat roster cand drop-p slots)]
    (compact [[:league  (named entry)]
              [:week    (:week reply)]
              [:add     (select-keys (row cand) [:name :player-id :position :team :opp :inj :week])]
              [:drop    (some-> drop-p row (select-keys [:name :player-id :position :slot :inj]))]
              [:lineup  (first (lineup/upgrades roster [cand] drop-p slots :ros-points))]
              [:upgrade (:upgrade (first (waiver/with-upgrade [cand] drop-p)))]
              [:lineup-this-week (:gain week)]
              [:starts-this-week (:seat week)]
              [:displaces        (some-> (:displaces week) row (select-keys [:name :player-id :position :week]))]])))

(def default-news-items
  "Five, not the feed's ten: a note runs to a paragraph, and the older half
  rarely changes a call."
  5)

(defn player-news
  "A player's latest Rotowire notes off ESPN, newest first, and Sleeper's
  designation."
  [deps {:keys [player limit]}]
  (let [p     (the-player (need deps :players) player "player")
        n     (-> (if (number? limit) (long limit) default-news-items) (max 1) (min 10))
        espn  (get-in p [:ids :espn])
        reply (when espn ((:news deps) (str espn)))]
    (cond-> {:player (select-keys (row p) [:player-id :name :position :team :inj])
             :news   (vec (take n (:news reply)))}
      (:sleeper/injury-notes p) (assoc :injury-notes (:sleeper/injury-notes p))
      (seq (:errors reply))     (assoc :errors (mapv :message (:errors reply)))
      (nil? espn)               (assoc :reason "no ESPN id for this player, so no news feed"))))

(defn league-row
  "One league as `leagues` states it. The entry also holds the account's user id
  and credentials; neither is copied, since an ESPN user id is the SWID and so
  half the cookie pair."
  [l]
  (compact [[:league (:key l)]
            [:name (:name l)]
            [:provider (providers/label (:provider l))]
            [:season (:season l)]
            [:teams (:num-teams l)]]))

(defn leagues
  "The leagues the manager plays in, by the key every league tool takes, and
  any account that could not be listed."
  [deps _args]
  (let [{:keys [leagues errors]} (need deps :found)]
    (when (empty? leagues)
      (throw (tool-error (context/no-leagues errors))))
    (cond-> {:leagues (mapv league-row leagues)}
      (seq errors) (assoc :errors errors))))

(def league-property
  {:type        "string"
   :description (str "Which league: a key or name from the `leagues` tool. Optional when the "
                     "manager plays in only one.")})

(def tools
  "Every tool, in the order a client lists them."
  [{:name         "leagues"
    :title        "Leagues"
    :description  (str "The leagues the manager plays in, each with the key that the league argument of "
                       "my_team, team_roster, matchup and waiver_targets takes. Call this first when the question does not "
                       "name a league.")
    :input-schema {:type "object" :properties {} :additionalProperties false}
    :handler      leagues}
   {:name         "my_team"
    :title        "My team"
    :description  (str "The manager's roster this week in one league: starters (with the seat each fills), "
                       "bench and injured reserve, FAAB left, claims left, and the drop a waiver claim "
                       "would cost (marked drop-candidate).\n\nRow keys:\n" (legend))
    :input-schema {:type                 "object"
                   :properties           {:league league-property}
                   :additionalProperties false}
    :handler      my-team}
   {:name         "league_teams"
    :title        "League teams"
    :description  "Every team in one league in standings order: record, points for and FAAB left, the manager's marked mine. Use for the league as a whole, then team_roster for any one team."
    :input-schema {:type                 "object"
                   :properties           {:league league-property}
                   :additionalProperties false}
    :handler      league-teams}
   {:name         "team_roster"
    :title        "Team roster"
    :description  (str "Any team's roster in one league: starters (with seats), bench and injured reserve, "
                       "to compare the manager's team with a rival's. Omit team for the manager's own. "
                       "Rows carry no lineup, upgrade or drop-cost, which are priced against the "
                       "manager's seats.\n\nRow keys:\n" (legend))
    :input-schema {:type                 "object"
                   :properties           {:league league-property
                                          :team   {:type        "string"
                                                   :description "A team name or roster id from league_teams"}}
                   :additionalProperties false}
    :handler      team-roster}
   {:name         "matchup"
    :title        "This week's matchup"
    :description  (str "The manager's game this week: both teams' lineups seat by seat with this week's "
                       "projection (proj) and score once played (actual), each side's projected total "
                       "and what its best lineup would gain. Live from the host.")
    :input-schema {:type                 "object"
                   :properties           {:league league-property}
                   :additionalProperties false}
    :handler      matchup}
   {:name         "waiver_targets"
    :title       "Waiver targets"
    :description  (str "Free agents in one league, best claim first: ordered by lineup, then upgrade, "
                       "both measured against the drop-candidate. Quarterbacks, backs, receivers and "
                       "tight ends unless a position is named, which is how to ask for a kicker or "
                       "a defense.\n\nRow keys:\n" (legend))
    :input-schema {:type                 "object"
                   :properties           {:league   league-property
                                          :position {:type "string" :enum db/positions
                                                     :description "Only this position"}
                                          :limit    {:type "integer" :minimum 1 :maximum max-targets
                                                     :description "How many players (default 10)"}}
                   :additionalProperties false}
    :handler      waiver-targets}
   {:name         "evaluate_claim"
    :title        "Evaluate a claim"
    :description  (str "What claiming one free agent and dropping one player does to the manager's "
                       "starting lineup: lineup (rest of season) and upgrade as waiver_targets states "
                       "them, and lineup-this-week: how much the best lineup by this week's projections "
                       "changes, not the add's own projection (add.week is that). starts-this-week "
                       "names his seat and displaces the player he pushes out; both are absent when he "
                       "would not play this week. drop defaults to the drop candidate; name another "
                       "to compare drops.")
    :input-schema {:type                 "object"
                   :properties           {:league league-property
                                          :add    {:type "string"
                                                   :description "The free agent: full name, or the player-id a row carries"}
                                          :drop   {:type "string"
                                                   :description "The player to drop, from the active roster (default: the drop candidate)"}}
                   :required             ["add"]
                   :additionalProperties false}
    :handler      evaluate-claim}
   {:name         "player_news"
    :title        "Player news"
    :description  (str "A player's latest news notes (Rotowire, via ESPN), newest first, and his "
                       "injury designation. Use before leaning on an injury or a role change.")
    :input-schema {:type                 "object"
                   :properties           {:player {:type        "string"
                                                   :description "Full name, or the player-id a row carries"}
                                          :limit  {:type "integer" :minimum 1 :maximum 10
                                                   :description "How many notes (default 5)"}}
                   :required             ["player"]
                   :additionalProperties false}
    :handler      player-news}])

(defn by-name [tool-name] (some #(when (= tool-name (:name %)) %) tools))

(defn run
  "`{:result data}`, or `{:error msg}` for a failure the model should read.
  Anything else is a bug and is thrown."
  [tool deps args]
  (try
    {:result ((:handler tool) deps (or args {}))}
    (catch clojure.lang.ExceptionInfo e
      (if (:sensei/tool-error (ex-data e))
        {:error (ex-message e)}
        (throw e)))))
