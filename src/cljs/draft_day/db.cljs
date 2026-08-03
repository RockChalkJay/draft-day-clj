(ns draft-day.db
  "app-db shape, the column catalog, and roster/league helpers. No reagent here —
  pure data + functions so it can be required from events and views alike."
  (:require [clojure.string :as str]))

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
;; Only dedicated position slots (not FLEX/BENCH) drive the board's bye-conflict
;; indicator: two starters at the same position on the same bye is the costly
;; mistake, and a bench player at that position on a different bye is the cover.

(def dedicated-positions
  "Starting-slot labels that map to a single position (everything but FLEX/BENCH).
  Only these participate in bye-conflict detection."
  #{"QB" "RB" "WR" "TE" "K" "DST"})

(defn roster-exposure
  "My roster split for bye-conflict detection, resolving each filled slot's player
  through `by-id`:
    :starters         [{:position :bye}…] — filled dedicated slots (FLEX excluded)
    :bench            [{:position :bye}…] — filled BENCH slots (the cover pool)
    :open-start-slots #{slot-label…}      — unfilled dedicated slots
  FLEX slots are ignored entirely."
  [team by-id]
  (reduce (fn [acc {:keys [pos player-id]}]
            (let [entry (fn [] (let [p (get by-id player-id)]
                                 {:position (:position p) :bye (:bye p)}))]
              (cond
                (and player-id (dedicated-positions pos)) (update acc :starters conj (entry))
                (and player-id (= pos "BENCH"))           (update acc :bench conj (entry))
                (and (nil? player-id) (dedicated-positions pos)) (update acc :open-start-slots conj pos)
                :else acc)))
          {:starters [] :bench [] :open-start-slots #{}}
          (:roster team)))

(defn bye-conflict
  "Severity of drafting a `position`/`bye` player onto my roster, judged on the
  roster as it would be *after* the pick. Both severities require that I already
  start a player at this position+bye, so nothing fires on a first pick:
    :clash   (red)   — two or more starters would share this position+bye
    :caution (amber) — I already start one here and have no bench cover that week,
                       and this pick wouldn't create a second starter
    nil              — no meaningful conflict (or `bye` unknown)
  `exposure` is the map from `roster-exposure`. FLEX is not considered."
  [position bye {:keys [starters bench open-start-slots]}]
  (when bye
    (let [cand-starter?   (contains? open-start-slots position)
          on-bye          (count (filter #(and (= (:position %) position)
                                               (= (:bye %) bye))
                                         starters))
          starters-on-bye (cond-> on-bye cand-starter? inc)
          cover?          (boolean (some #(and (= (:position %) position)
                                               (not= (:bye %) bye))
                                         bench))]
      (cond
        (>= starters-on-bye 2)             :clash
        (and (pos? on-bye) (not cover?))   :caution
        :else                              nil))))

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
  {:num-teams 12 :num-tiers 5 :starting-bankroll 200 :scoring :ppr :roster default-roster
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
