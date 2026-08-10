(ns draft-day.db
  "app-db shape, the column catalog, and roster/league helpers. No reagent here —
  pure data + functions so it can be required from events and views alike."
  (:require [clojure.string :as str]
            [draft-day.rankings.tiers :as tiers]))

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
  starter's bye week). Pairs the tile's green signal with the roster's amber."
  [position bye exposure]
  (boolean
   (and bye
        (let [uncovered (uncovered-starter-ids exposure)]
          (some #(and (contains? uncovered (:player-id %))
                      (= (:position %) position)
                      (not= (:bye %) bye))
                (:starters exposure))))))

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

;; A column may also carry a :tier spec (see draft-day.rankings.tiers), which is
;; what the board bands rows by while that column is the one sorted on. Specs
;; resolve against a scope: :position when the board is filtered to one position,
;; :overall when it is not.

(def column-catalog
  "Ordered column definitions; :default? seeds initial visibility."
  [{:key :rank     :label "#"      :tooltip "Rank by live Worth"        :default? true}
   {:key :ecr      :label "ECR"    :tooltip "FantasyPros expert rank"   :default? true
    ;; FantasyPros publishes both tierings; read whichever matches the scope.
    :tier {:source :field
           :field  {:overall  :fantasypros/ecr-tier    ; overall cheatsheet
                    :position :fantasypros/pos-tier}}} ; per-position cheatsheets
   {:key :name     :label "Player" :tooltip "Player"                    :default? true}
   {:key :team     :label "Tm"     :tooltip "NFL team"                  :default? true}
   {:key :bye      :label "Bye"    :tooltip "Bye week"                  :default? true}
   {:key :position :label "Pos"    :tooltip "Position"                  :default? true}
   {:key :worth    :label "Worth"  :tooltip "Live auction price"        :default? true
    ;; Fixed dollar cuts rather than derived ones, so a band means the same money
    ;; in either scope. `pos?` leaves the $0 tail (and all of K/DST) unstriped.
    :tier {:source :breaks :breaks [60 40 25 15 8 3] :value? pos?}}
   {:key :value    :label "Value"  :tooltip "Stable VBD dollars"        :default? true}
   {:key :market   :label "Mkt"    :tooltip "Market price — ESPN + FantasyPros consensus, scaled to your league" :default? true}
   {:key :espn-value :label "ESPN" :tooltip "ESPN live auction value ($, raw)" :default? true}
   {:key :fp-aav   :label "FP$"    :tooltip "FantasyPros auction value ($, raw)" :default? true}
   {:key :bargain  :label "Barg"   :tooltip "Value − Worth (green target / red reach)" :default? true}
   {:key :vorp     :label "VORP"   :tooltip "Value over replacement"    :default? true}
   {:key :inj      :label "Inj"    :tooltip "Injury status"             :default? false}
   {:key :edge     :label "Edge"   :tooltip "Worth − Market (green: model likes more than the market)" :default? false}
   {:key :adp      :label "ADP"    :tooltip "Sleeper average draft position" :default? false}
   {:key :proj     :label "Proj"   :tooltip "Projected fantasy points"  :default? false}
   {:key :ceiling  :label "Ceil"   :tooltip "Ceiling projection (p90)"  :default? false}
   {:key :floor    :label "Floor"  :tooltip "Floor projection (p10)"    :default? false}])

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
   :inj      :sleeper/injury-status
   :bye      :bye})

(defn tier-spec
  "The sorted column's :tier spec resolved for `scope`, or nil when that column
  doesn't band in that scope. The measurement defaults to the accessor the column
  already sorts by, so there's no second source of truth."
  [col-key scope bankroll]
  (tiers/resolve-spec (:tier (columns-by-key col-key)) scope
                      (get sort-accessors col-key) bankroll))

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

;; ---- initial db ----

(defn default-db []
  (let [cfg default-config]
    {:players     []            ; raw universe from /api/players
     :ranked      nil           ; last /api/rankings response
     :status      nil
     :scoring-presets nil       ; {:presets {...} :stat-keys [...]}, fetched at boot
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
