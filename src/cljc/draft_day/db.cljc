(ns draft-day.db
  "app-db shape, the column catalog, and roster/league helpers. No reagent here —
  pure data + functions so it can be required from events and views alike."
  (:require [clojure.string :as str]
            [draft-day.scoring :as scoring]))

;; ---- roster / teams ----

(def roster-order
  "Ordered [slot-label config-key] pairs used to expand a roster config into a
  concrete slot list."
  [["QB" :qb] ["RB" :rb] ["WR" :wr] ["TE" :te] ["FLEX" :flex] ["K" :k] ["DST" :dst] ["BENCH" :bench]])

(def default-roster {:qb 1 :rb 2 :wr 2 :te 1 :flex 1 :k 1 :dst 1 :bench 6})

(defn roster-template [roster-cfg]
  (vec (mapcat (fn [[label k]] (repeat (get roster-cfg k 0) label)) roster-order)))

(defn- default-name [i] (if (zero? i) "You" (str "Team " (inc i))))

(def persist-keys [:config :teams :drafted :picks :columns :my-team-id :watchlist])

(defn make-teams-named
  "Build `(count names)` fresh (empty-roster, full-bankroll) teams with the given
  names; a blank name falls back to the default (\"You\"/\"Team N\")."
  [names roster-cfg bankroll]
  (let [tmpl (roster-template roster-cfg)]
    (vec (map-indexed
          (fn [i nm]
            {:team-id  (str "t" i)
             :name     (if (str/blank? nm) (default-name i) nm)
             :bankroll bankroll
             :roster   (mapv (fn [p] {:pos p :player-id nil}) tmpl)})
          names))))

(defn make-teams [num-teams roster-cfg bankroll]
  (make-teams-named (map default-name (range num-teams)) roster-cfg bankroll))

(def priced-positions
  "Positions the model puts a dollar on.

  The one copy. `rankings.value` and `rankings.inflation-index` read this too —
  `src/cljc` is on the backend classpath — because there were five hand-written
  copies of this set before, one of them a *vector* under the same name, so
  `(priced-positions pos)` threw where it read as a membership test everywhere
  else. `conflict-positions` below happens to hold the same four strings for an
  unrelated reason and is deliberately not aliased to this.

  K and DST are absent because the engine ranks them on no replacement level and
  gives them no VORP (see `rankings.replacement/with-vorp`). They are still
  *priced*, at the league minimum, because they fill roster slots."
  #{"QB" "RB" "WR" "TE"})

(defn vorp-sort-key
  "Sortable :vorp, or nil for a position the model has no opinion about.

  The engine now sends nil for K/DST rather than a placeholder 0.0, so this is
  belt and braces — but the braces are the reason the belt exists. A 0.0 that
  read as *at replacement* floated all 76 specialists above every
  below-replacement skill player once, and a column accessor is exactly where
  that resurfaces if the engine ever spells 'no opinion' as a number again. nil
  falls to `sort-players`' nil-last rule, which holds in both directions."
  [p]
  (when (priced-positions (:position p)) (:vorp p)))

(defn pos-sort-key
  "Sortable Pos column: `[position ordinal]`, grouping by position and ordering
  by :pos-rank inside each group.

  Not the \"RB25\" string the cell renders — `compare` on a string reads it
  digit by digit, so the column would run RB1, RB10, RB11, RB2. A vector
  compares elementwise, which is what the column is actually asking for.

  A row the engine could not rank sorts to the back of its own position rather
  than to the back of the board: it is still an RB, and burying it under the
  kickers would be a worse lie than showing it last among its peers."
  [p]
  [(str (:position p)) (or (:pos-rank p) ##Inf)])

(defn rank-key
  "Descending sort key for the board's overall rank: Worth, then skill positions
  ahead of K/DST, then VORP, then projected points.

  Worth alone is not a total order. Everything past the last roster slot prices
  at $0, and the minimum-bid tail all prices at $1, so a stable sort left those
  blocks in whatever order the server emitted, which is position grouping. VORP
  and points still separate those players even where dollars cannot.

  The K/DST term is what keeps signed VORP honest. The engine gives those two no
  replacement level, so their :vorp is nil — and `(or nil 0)` below would put them
  right back at the top of the tail, ahead of 469 players actually worth
  drafting. They now carry the same $1 as the minimum-bid tail, so Worth no longer
  separates them either; this term is the only thing that does.

  Missing values read as 0 rather than nil: the model leaves :vorp and :points
  off players it never scored, and a nil inside a vector sort key throws instead
  of sorting last.

  It lives here rather than in `subs.cljs`, where it started, because it is now
  the tiebreak for two orderings — the board's sorted column and the watch
  list's one-shot re-sort — and only one of them runs in a subscription."
  [p]
  [(- (double (or (:worth p) 0)))
   (if (priced-positions (:position p)) 0 1)
   (- (double (or (:vorp p) 0)))
   (- (double (or (:points p) 0)))])

;; ---- bye-week conflict detection ----
;; Staged by draft phase. Only QB/RB/WR/TE matter (K/DST are streamed weekly, so
;; bye stacking there is a non-issue; FLEX is ignored too):
;;  - while the starting lineup is still filling, the board pulses a candidate's
;;    Bye red if I already start that same position on that bye week;
;;  - once every starting slot is filled, the board goes quiet and My Roster marks
;;    a starter's bye amber when no bench player at that position covers the week.

(def conflict-positions
  "Positions whose bye stacking matters — QB/RB/WR/TE only (K/DST streamed, FLEX
  ignored). Only these drive the red board pulse and the amber roster marker."
  #{"QB" "RB" "WR" "TE"})

(defn roster-exposure
  "My roster split for bye-conflict detection, resolving each filled slot's player
  through `by-id`:
    :starters       [{:player-id :position :bye}…] — filled QB/RB/WR/TE slots
    :bench          [{:position :bye}…]            — filled BENCH slots (cover pool)
    :open-non-bench count                          — unfilled non-BENCH slots
  A filled FLEX/K/DST slot is ignored; an unfilled one still counts toward
  `:open-non-bench`, so `lineup-full? = (zero? open-non-bench)`."
  [team by-id]
  (reduce (fn [acc {:keys [pos player-id]}]
            (cond
              (and player-id (conflict-positions pos))
              (let [p (get by-id player-id)]
                (update acc :starters conj {:player-id player-id :position (:position p) :bye (:bye p)}))

              (and player-id (= pos "BENCH"))
              (let [p (get by-id player-id)]
                (update acc :bench conj {:position (:position p) :bye (:bye p)}))

              (and (nil? player-id) (not= pos "BENCH"))
              (update acc :open-non-bench inc)

              :else acc))
          {:starters [] :bench [] :open-non-bench 0}
          (:roster team)))

(defn board-bye-clash?
  "Phase B board signal: true when an undrafted `position`/`bye` player should
  pulse red — the starting lineup isn't full yet and I already start a player at
  the same position on the same bye week (same-position only)."
  [position bye {:keys [starters open-non-bench]}]
  (boolean
   (and bye
        (pos? open-non-bench)
        (some #(and (= (:position %) position) (= (:bye %) bye)) starters))))

(defn uncovered-starter-ids
  "Roster signal: the set of starter `:player-id`s whose bye week has no bench
  cover — shown throughout the draft (the moment a starter lacks cover), not only
  once the lineup is full. Cover is a bench player at the same position on a
  *different* bye; each starter needs its own. Per position+bye group the first
  `covers` starters are covered and the rest are uncovered (`max 0, starters−covers`).
  A single bench body covers starters across different bye weeks freely — the
  shortfall only bites within one week."
  [{:keys [starters bench]}]
  (->> (group-by (juxt :position :bye) starters)
       (mapcat (fn [[[position bye] grp]]
                 (let [covers (count (filter #(and (= (:position %) position)
                                                   (not= (:bye %) bye))
                                             bench))]
                   (map :player-id (drop covers grp)))))
       set))

(defn covers-starter?
  "True when a candidate at `position`/`bye` would cover one of my currently
  *uncovered* starters — same position, a *different* bye (so it plays that
  starter's bye week). Pairs the tile's green signal with the roster's amber.

  `bye-coverage` is the roster bye coverage summary returned by
  `roster-exposure` (starters/bench/open-non-bench)."
  [position bye bye-coverage]
  (boolean
   (and bye
        (let [uncovered (uncovered-starter-ids bye-coverage)]
          (some #(and (contains? uncovered (:player-id %))
                      (= (:position %) position)
                      (not= (:bye %) bye))
                (:starters bye-coverage))))))

;; ---- budget plan ----
;; The manager's own $ allocation, one bucket per slot type (K and DST share).
;; Spend is attributed to the bucket of the slot a player fills, so a WR who
;; lands in FLEX charges the FLEX budget, not the WR budget.

(def budget-order
  "Ordered [label bucket-key] pairs for the Settings budget editor."
  [["QB" :qb] ["RB" :rb] ["WR" :wr] ["TE" :te] ["FLEX" :flex] ["K/DST" :kdst] ["Bench" :bench]])

(def slot->budget-key
  {"QB" :qb "RB" :rb "WR" :wr "TE" :te "FLEX" :flex "K" :kdst "DST" :kdst "BENCH" :bench})

(def default-budget-plan (into {} (map (fn [[_ k]] [k 0])) budget-order))

(def default-config
  {:num-teams 12 :starting-bankroll 200 :scoring :ppr :roster default-roster
   :budget-plan default-budget-plan})

;; ---- tiers ----
;; The server ships both scales on every player, so which one the board reads is
;; a pure display decision made here.

(defn tier-scale
  "Which tier scale the board reads: positional while filtered to one position,
  overall otherwise. The manager never picks this separately — choosing a
  position filter has already said which question they are asking."
  [pos-filter]
  (if pos-filter :position :overall))

(defn player-tier
  "`p`'s tier at `scale`. Every player is tiered on both scales, so this is total
  — there is no untiered bucket to fall into."
  [p scale]
  (get-in p [:tiers scale]))

(defn fp-tier
  "FantasyPros' published expert tier at `scale`, so the FP T column tracks the
  board's own tier rather than answering a different question beside it. Unlike
  `player-tier` this is partial: FantasyPros ranks about three quarters of the
  board, and the rest render as a dash."
  [p scale]
  (if (= :position scale)
    (:fantasypros/ecr-pos-tier p)
    (:fantasypros/ecr-tier p)))

;; ---- injury ----

(def serious-injury-statuses
  "Designations that read as serious: the ones that cost a manager games he has
  already paid for. IR and PUP are multi-week by rule, a suspension is served
  whatever the player's health, and NA/DNR mean he is not with the team at all.
  Sleeper spells them tersely and inconsistently across seasons, so membership is
  tested case-insensitively against a set of spellings rather than by equality.

  The one copy, in cljc for the same reason `priced-positions` is: the server
  floors the risk scale on this set (`rankings.injury`) and the board colours the
  Inj cell on it, and a second hand-written copy would drift. Drift here is
  especially loud — it puts a Risk of 5 next to a calm, plain-black Inj cell
  saying the opposite.

  MEMBERSHIP IS DECIDED BY DURATION, NOT BY SEVERITY, because the only thing
  this set does is override a multi-season durability score. A designation
  earns a place here by costing weeks *by rule* — which is the same test that
  keeps Questionable and Doubtful out, and, less obviously, Out.

  Out sounds like the serious one and is not: it is a gameday call covering the
  next game, and this is a preseason auction board, where it is either absent or
  a week old by kickoff. Floored on it, a 17/17/17 veteran with a one-week knock
  rendered `Risk 5 of 5 — Out, 0.0 games missed per season over 3 seasons` — a
  cell that states its own evidence and then contradicts it. COV is gone for the
  same reason plus a simpler one: the COVID list itself is defunct."
  #{"ir" "ir-r" "pup" "nfi" "sus" "susp" "na" "dnr"})

(defn serious-injury?
  "Is this `:sleeper/injury-status` one of the serious ones?"
  [status]
  (boolean (some-> status str/trim str/lower-case not-empty serious-injury-statuses)))

;; ---- board columns ----
;; The board is data-driven: :columns is an ordered vector of {:key :visible?},
;; so a column can be hidden/shown and drag-reordered. Rendering + sort accessors
;; live in views.board keyed by :key.

(def column-catalog
  "Ordered column definitions; :default? seeds initial visibility."
  [{:key :rank     :label "#"      :tooltip "Rank by live Worth"        :default? true}
   {:key :ecr      :label "ECR"    :tooltip "FantasyPros expert rank"   :default? true}
   {:key :name     :label "Player" :tooltip "Player"                    :default? true}
   {:key :team     :label "Tm"     :tooltip "NFL team"                  :default? true}
   {:key :bye      :label "Bye"    :tooltip "Bye week"                  :default? true}
   {:key :position :label "Pos"    :tooltip "Position and rank within it — RB1 is the top RB on the board. Fixed for the whole draft; it does not renumber as players go" :default? true}
   {:key :worth    :label "Worth"  :tooltip "Live auction price"        :default? true}
   {:key :value    :label "Value"  :tooltip "Stable VBD dollars"        :default? true}
   {:key :market   :label "Mkt"    :tooltip "Market price — ESPN + FantasyPros consensus, scaled to your league" :default? true}
   {:key :espn-value :label "ESPN" :tooltip "ESPN live auction value ($, raw)" :default? true}
   {:key :fp-aav   :label "FP$"    :tooltip "FantasyPros auction value ($, raw)" :default? true}
   {:key :bargain  :label "Barg"   :tooltip "Value − Worth (green target / red reach)" :default? true}
   {:key :vorp     :label "VORP"   :tooltip "Value over replacement"    :default? true}
   {:key :risk     :label "Risk"   :tooltip "Injury risk — games missed per season over the last three, 1 (durable) to 5 (fragile); a serious designation forces 5. Blank where there is no history to judge" :default? true}
   {:key :inj      :label "Inj"    :tooltip "Current injury status"     :default? false}
   {:key :edge     :label "Edge"   :tooltip "Worth − Market (green: model likes more than the market)" :default? false}
   {:key :adp      :label "ADP"    :tooltip "Sleeper average draft position" :default? false}
   {:key :tier     :label "Tier"   :tooltip "Tier — within the position while filtered to one, across the whole board otherwise" :default? false}
   {:key :fp-tier  :label "FP T"   :tooltip "FantasyPros' expert tier at the same scale as Tier — within the position while filtered, overall otherwise; blank where FantasyPros has no match" :default? false}
   {:key :proj     :label "Proj"   :tooltip "Projected fantasy points"  :default? false}
   {:key :ceiling  :label "Ceil"   :tooltip "Ceiling projection (p90)"  :default? false}
   {:key :floor    :label "Floor"  :tooltip "Floor projection (p10)"    :default? false}
   ;; Usage. What a player actually did last season, and what ESPN expects this
   ;; one — blank where the source has no row, which for the prior-season three
   ;; is exactly the rookies.
   {:key :prior-tgt     :label "Tgt"  :tooltip "Targets last season — blank for rookies" :default? false}
   {:key :prior-rec     :label "Rec"  :tooltip "Receptions last season — blank for rookies" :default? false}
   {:key :prior-tgt-pct :label "Tgt%" :tooltip "Share of his team's targets last season — blank for rookies" :default? false}
   {:key :proj-tgt      :label "pTgt" :tooltip "Projected targets this season (ESPN)" :default? false}
   {:key :proj-rec      :label "pRec" :tooltip "Projected receptions this season (ESPN)" :default? false}])

(def columns-by-key (into {} (map (juxt :key identity)) column-catalog))

;; ---- scoring catalog ----
;; Grouped presentational metadata for the custom scoring editor: each group is
;; rendered as a section of numeric weight inputs, in this order.

(def scoring-catalog
  [{:group "Passing"   :stats [[:pass_yd "Pass Yd"] [:pass_td "Pass TD"]
                               [:pass_int "Pass INT"] [:pass_2pt "Pass 2PT"]]}
   {:group "Rushing"   :stats [[:rush_yd "Rush Yd"] [:rush_td "Rush TD"] [:rush_2pt "Rush 2PT"]]}
   {:group "Receiving" :stats [[:rec "Reception"] [:rec_yd "Rec Yd"]
                               [:rec_td "Rec TD"] [:rec_2pt "Rec 2PT"]]}
   {:group "Misc"      :stats [[:fum_lost "Fumble Lost"]]}
   {:group "Kicking"   :stats [[:fgm "FG Made"] [:xpm "XP Made"] [:blk_kick "Blocked Kick"]]}
   {:group "Defense"   :stats [[:sack "Sack"] [:int "INT"] [:fum_rec "Fumble Rec"]
                               [:ff "Forced Fumble"] [:def_td "Def/ST TD"] [:safe "Safety"]]}])

(def sort-accessors
  "column key -> fn player -> sortable value. :rank is attached in the sub."
  {:rank     :rank
   :name     :player-name
   :team     :team
   ;; [position ordinal], not the "RB25" string the board renders: sorted as a
   ;; string that reads RB1, RB10, RB2. Unranked rows sort last either way.
   :position pos-sort-key
   :worth    :worth
   :value    :value
   :espn-value :espn/auction-value
   :fp-aav   :fantasypros/aav
   :market   :market
   :edge     :edge
   :bargain  :bargain
   :adp      :sleeper/adp
   :proj     :points
   :ceiling  :ceiling
   :floor    :floor
   :vorp     vorp-sort-key
   :ecr      :fantasypros/ecr
   ;; resolved to the active scale in the :board-players sub
   :tier     :tier
   :fp-tier  :fantasypros/ecr-tier
   :inj      :sleeper/injury-status
   :risk     :injury-risk
   :bye      :bye
   :prior-tgt     :nflverse/prior-targets
   :prior-rec     :nflverse/prior-receptions
   :prior-tgt-pct :nflverse/prior-target-share
   :proj-tgt      :espn/proj-targets
   :proj-rec      :espn/proj-receptions})

(defn reconcile-config
  "Reconcile a persisted config with the current shape: drop keys the app no
  longer has, fill in ones added since the blob was written, and when :scoring is
  a custom map, give it a 0 for any stat key it predates.

  localStorage carries no schema stamp, so — exactly like `reconcile-columns` —
  every shape the app has ever persisted has to be repairable in place. A blob
  written before :scoring existed (or one poisoned by the old
  `:enable-custom-scoring` race, which could store nil) otherwise reaches the
  Settings page as a nil scoring config and throws."
  [stored]
  (let [cfg (merge default-config stored)
        s   (:scoring cfg)]
    (-> cfg
        (select-keys (keys default-config))
        (assoc :roster      (merge default-roster (:roster cfg))
               :budget-plan (merge default-budget-plan (:budget-plan cfg))
               :scoring     (cond
                              (map? s) (merge (zipmap scoring/stat-keys (repeat 0)) s)
                              (contains? scoring/presets s) s
                              :else (:scoring default-config))))))

(defn default-columns []
  (mapv (fn [c] {:key (:key c) :visible? (boolean (:default? c))}) column-catalog))

(defn reconcile-columns
  "Reconcile a persisted column config with the current catalog: keep the stored
  order for keys that still exist, drop unknown keys (e.g. a removed :tier), and
  append any new catalog columns at their default visibility."
  [stored]
  (let [valid   (set (map :key column-catalog))
        kept    (filterv #(valid (:key %)) (or stored []))
        present (set (map :key kept))
        added   (->> column-catalog
                     (remove #(present (:key %)))
                     (map (fn [c] 
                            {:key (:key c) 
                             :visible? (boolean (:default? c))})))]
    (vec (concat kept added))))

(defn move-onto
  "Drop the element `key-fn` identifies as `from-k` onto the one it identifies as
  `to-k`: remove it, then insert it at `to-k`'s index *in the original vector*.

  Taking the target index before the removal rather than after is what makes the
  drag read the way it looks. Dragging rightwards, the removal shifts the target
  left by one, so the old index lands the column just past it; dragging
  leftwards nothing before the target moves, so it lands just before it. Both
  ends of the board stay reachable — measuring after the removal instead costs
  you the last slot, since no surviving column has an index to aim at.

  Keyed rather than indexed because the two places that reorder columns hold
  different vectors — the picker lists every column, the board header only the
  visible ones — so an index means two different things depending on where it
  came from. A key means one thing either way, and hidden columns keep their
  slots. The watch list has the same problem for a different reason: what it
  renders is the undrafted subset of what it stores, so a row's screen index is
  not its index in the vector. An absent key, or a drop onto itself, is a no-op."
  [v from-k to-k key-fn]
  (let [v        (vec v)
        index-of (fn [k] (first (keep-indexed #(when (= k (key-fn %2)) %1) v)))
        from     (index-of from-k)
        to       (index-of to-k)]
    (if (or (nil? from) (nil? to) (= from-k to-k))
      v
      (let [without (vec (concat (subvec v 0 from) (subvec v (inc from))))]
        (vec (concat (subvec without 0 to) [(nth v from)] (subvec without to)))))))

(defn move-column-onto
  "`move-onto` over the column vector, whose elements are keyed by :key."
  [cols from-k to-k]
  (move-onto cols from-k to-k :key))

(defn move-watch-onto
  "`move-onto` over the watch list, whose elements are bare player-ids."
  [ids from-id to-id]
  (move-onto ids from-id to-id identity))

(defn index-by-id
  "`{player-id player}` over a ranked board, for `sort-watchlist`'s caller: the
  `:watch-sort` event holds raw db and cannot reach the `:players-by-id` sub."
  [players]
  (into {} (map (juxt :player-id identity)) players))

(def watch-sort-keys
  "Watch-list sort key -> `player -> comparable`, direction baked in.

  Fixed direction rather than the board's toggleable `dir`, because these are
  one-shot actions and not a sort mode: there is no active column to click a
  second time, so there is nowhere for a reversal to live. Each key ends in
  `rank-key` so ties fall back to the board's own total order rather than to
  whatever order the manager's vector happened to be in — the same discipline
  `subs/sort-players` enforces on the board.

  Rank and Worth will usually agree, since `rank-key` leads with Worth. They part
  company across the $0 and minimum-bid tails, which is exactly where Worth stops
  being an ordering at all."
  {:rank     rank-key
   :worth    (fn [p] [(- (double (or (:worth p) 0))) (rank-key p)])
   :position (fn [p] [(pos-sort-key p) (rank-key p)])})

(defn sort-watchlist
  "Reorder watch-list `ids` by `k`, resolving each through `by-id`.

  A one-shot rewrite of the stored order, not a view: the list stays the
  manager's, hand-draggable, and nothing re-sorts it out from under him after a
  pick.

  Ids `by-id` cannot resolve keep their relative order at the back rather than
  being dropped. Two of them are routine, not corrupt state: a watched player who
  has been drafted stays in the vector (he only falls out of the sub, so an undo
  restores him to his place), and before the first `/api/rankings` reply `by-id`
  is empty for every id there is. Sorting them by a nil player would silently
  shuffle a list the manager built by hand.

  An unknown `k` is a no-op for the same reason: a re-ordered list is not a safe
  guess at what was meant."
  [ids by-id k]
  (if-let [keyfn (get watch-sort-keys k)]
    (let [{known true unknown false} (group-by #(contains? by-id %) (vec ids))]
      (into (vec (sort-by (comp keyfn by-id) known)) unknown))
    (vec ids)))

(defn reconcile-watchlist
  "Reconcile a persisted watch list with the current shape: an ordered vector of
  distinct player-ids.

  The list was a set until it became orderable, and localStorage carries no
  schema stamp — so, exactly like `reconcile-columns` and `reconcile-config`,
  every shape the app has ever written has to be repairable in place. A set
  reaching the ordered code unrepaired is the worst kind of wrong: `conj` puts a
  new id wherever the hash says, and a drag would silently do nothing."
  [stored]
  (into [] (distinct) (or stored [])))

;; ---- player-id migration ----
;; :player-id used to be Sleeper's id verbatim; it is now the GSIS id wherever
;; one resolves. Draft state saved before that change is keyed by the old value,
;; so it is remapped once the universe arrives — the crosswalk needed to do it
;; rides on each player as :ids, which is the whole reason that envelope exists.

(defn sleeper->player-id
  "{sleeper-id canonical-player-id} from a loaded universe.

  Ids that were never remapped (team defenses, players absent from the pinned
  crosswalk) map to themselves, so applying this to already-migrated state is a
  no-op. That is what makes it safe to run on every load rather than gating it
  behind a version stamp."
  [players]
  (into {}
        (keep (fn [p] (when-let [s (get-in p [:ids :sleeper])]
                        [s (:player-id p)])))
        players))

(defn remap-draft-ids
  "Rewrite every player-id held in draft state through `xwalk`.

  An id with no entry is left exactly as it was. An unknown id is not evidence
  that it is wrong — the universe may simply be a stale cache or the offline
  sample — and dropping a pick would destroy a real record of what a manager
  paid."
  [db xwalk]
  (let [->id  #(get xwalk % %)
        slot  (fn [s] (cond-> s (:player-id s) (update :player-id ->id)))]
    (-> db
        (update :drafted #(into {} (map (fn [[k v]] [(->id k) v])) %))
        (update :picks #(mapv (fn [p] (update p :player-id ->id)) %))
        (update :watchlist #(into [] (comp (map ->id) (distinct)) %))
        (update :nominated-id #(some-> % ->id))
        (update :teams
                (fn [teams]
                  (mapv (fn [t] (update t :roster #(mapv slot %))) teams))))))

;; ---- initial db ----

(defn default-db []
  (let [cfg default-config]
    {:players     []            ; raw universe from /api/players
     :ranked      nil           ; last /api/rankings response
     :recompute-seq 0           ; newest /api/rankings request; older replies are dropped
     :status      nil
     :universe-status nil       ; "N players · source", restored after a recompute error
     :universe    nil           ; /api/players provenance: season, fetched-at, per-source :ok?
     :recompute-error nil       ; the failure message, while it is still on :status
     :import-report nil         ; {:name :season :unsupported-scoring [...]}
     :config      cfg
     :teams       (make-teams (:num-teams cfg) (:roster cfg) (:starting-bankroll cfg))
     :my-team-id  "t0"
     :drafted     {}            ; player-id -> {:price :team-id}
     :picks       []            ; [{:player-id :position :price :team-id}]
     :nominated-id nil
     :watchlist    []           ; player-ids the manager is tracking, in his own order
     :modal        nil
     :sort        {:key :worth :dir -1}
     :pos-filter  nil
     :search      ""
     :view        :board
     :columns     (default-columns)}))
