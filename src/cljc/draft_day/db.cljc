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
   {:key :position :label "Pos"    :tooltip "Position"                  :default? true}
   {:key :worth    :label "Worth"  :tooltip "Live auction price"        :default? true}
   {:key :value    :label "Value"  :tooltip "Stable VBD dollars"        :default? true}
   {:key :market   :label "Mkt"    :tooltip "Market price — ESPN + FantasyPros consensus, scaled to your league" :default? true}
   {:key :espn-value :label "ESPN" :tooltip "ESPN live auction value ($, raw)" :default? true}
   {:key :fp-aav   :label "FP$"    :tooltip "FantasyPros auction value ($, raw)" :default? true}
   {:key :bargain  :label "Barg"   :tooltip "Value − Worth (green target / red reach)" :default? true}
   {:key :vorp     :label "VORP"   :tooltip "Value over replacement"    :default? true}
   {:key :inj      :label "Inj"    :tooltip "Injury status"             :default? false}
   {:key :edge     :label "Edge"   :tooltip "Worth − Market (green: model likes more than the market)" :default? false}
   {:key :adp      :label "ADP"    :tooltip "Sleeper average draft position" :default? false}
   {:key :fp-tier  :label "FP T"   :tooltip "FantasyPros expert tier (PPR reference — does not follow your scoring)" :default? false}
   {:key :proj     :label "Proj"   :tooltip "Projected fantasy points"  :default? false}
   {:key :ceiling  :label "Ceil"   :tooltip "Ceiling projection (p90)"  :default? false}
   {:key :floor    :label "Floor"  :tooltip "Floor projection (p10)"    :default? false}])

(def columns-by-key (into {} (map (juxt :key identity)) column-catalog))

;; ---- scoring catalog ----
;; Grouped presentational metadata for the custom scoring editor: each group is
;; rendered as a section of numeric weight inputs, in this order.

(def unprojected-stats
  "Stat keys we score but that Sleeper's projections never carry — checked
  against both the live universe and the bundled sample, where all four appear
  zero times. Kickers are projected for extra points but not field goals, and
  team defenses for sacks/interceptions/fumble recoveries but not forced fumbles,
  defensive touchdowns or safeties. A weight on these cannot move any player's
  points, so the editor shows them but will not pretend they are editable."
  #{:fgm :ff :def_td :safe})

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
   :position :position
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
   :vorp     :vorp
   :ecr      :fantasypros/ecr
   :fp-tier  :fantasypros/ecr-tier
   :inj      :sleeper/injury-status
   :bye      :bye})

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
        (update :watchlist #(into #{} (map ->id) %))
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
     :recompute-error? false
     :import-report nil         ; {:name :season :unsupported-scoring [...]}
     :config      cfg
     :teams       (make-teams (:num-teams cfg) (:roster cfg) (:starting-bankroll cfg))
     :my-team-id  "t0"
     :drafted     {}            ; player-id -> {:price :team-id}
     :picks       []            ; [{:player-id :position :price :team-id}]
     :nominated-id nil
     :watchlist    #{}          ; player-ids the manager is tracking
     :modal        nil
     :sort        {:key :worth :dir -1}
     :pos-filter  nil
     :search      ""
     :view        :board
     :columns     (default-columns)}))

;; slice persisted to localStorage
(def persist-keys [:config :teams :drafted :picks :columns :my-team-id :watchlist])
