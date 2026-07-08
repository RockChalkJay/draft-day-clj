(ns draftday.db
  "app-db shape, the column catalog, and roster/league helpers. No reagent here —
  pure data + functions so it can be required from events and views alike.")

;; ---- roster / teams ----

(def roster-order
  "Ordered [slot-label config-key] pairs used to expand a roster config into a
  concrete slot list."
  [["QB" :qb] ["RB" :rb] ["WR" :wr] ["TE" :te] ["FLEX" :flex] ["K" :k] ["DST" :dst] ["BENCH" :bench]])

(def default-roster {:qb 1 :rb 2 :wr 2 :te 1 :flex 1 :k 1 :dst 1 :bench 6})

(defn roster-template [roster-cfg]
  (vec (mapcat (fn [[label k]] (repeat (get roster-cfg k 0) label)) roster-order)))

(defn make-teams [num-teams roster-cfg bankroll]
  (let [tmpl (roster-template roster-cfg)]
    (vec (for [i (range num-teams)]
           {:team-id (str "t" i)
            :name    (if (zero? i) "You" (str "Team " (inc i)))
            :bankroll bankroll
            :roster  (mapv (fn [p] {:pos p :player-id nil}) tmpl)}))))

(def default-config
  {:num-teams 12 :num-tiers 5 :starting-bankroll 200 :scoring :ppr :roster default-roster})

;; ---- board columns ----
;; The board is data-driven: :columns is an ordered vector of {:key :visible?},
;; so a column can be hidden/shown and drag-reordered. Rendering + sort accessors
;; live in views.board keyed by :key.

(def column-catalog
  "Ordered column definitions; :default? seeds initial visibility."
  [{:key :rank     :label "#"      :tooltip "Rank by live Worth"        :default? true}
   {:key :name     :label "Player" :tooltip "Player"                    :default? true}
   {:key :team     :label "Tm"     :tooltip "NFL team"                  :default? true}
   {:key :position :label "Pos"    :tooltip "Position"                  :default? true}
   {:key :tier     :label "Tier"   :tooltip "Per-position tier (🚨 = live cliff)" :default? true}
   {:key :worth    :label "Worth"  :tooltip "Live auction price (active profile)"  :default? true}
   {:key :value    :label "Value"  :tooltip "Stable VBD dollars"        :default? true}
   {:key :bargain  :label "Barg"   :tooltip "Value − Worth (green target / red reach)" :default? true}
   {:key :adp      :label "ADP"    :tooltip "Sleeper average draft position" :default? true}
   {:key :proj     :label "Proj"   :tooltip "Projected fantasy points"  :default? true}
   {:key :ceiling  :label "Ceil"   :tooltip "Ceiling projection (p90)"  :default? false}
   {:key :floor    :label "Floor"  :tooltip "Floor projection (p10)"    :default? false}
   {:key :vorp     :label "VORP"   :tooltip "Value over replacement"    :default? false}
   {:key :ecr      :label "ECR"    :tooltip "FantasyPros expert rank"   :default? false}
   {:key :inj      :label "Inj"    :tooltip "Injury status"             :default? false}
   {:key :bye      :label "Bye"    :tooltip "Bye week"                  :default? false}])

(def columns-by-key (into {} (map (juxt :key identity)) column-catalog))

(def sort-accessors
  "column key -> fn player -> sortable value. :rank is attached in the sub."
  {:rank     :rank
   :name     :player-name
   :team     :team
   :position :position
   :tier     :tier
   :worth    :worth
   :value    :value
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

;; ---- initial db ----

(defn default-db []
  (let [cfg default-config]
    {:players     []            ; raw universe from /api/players
     :source      nil
     :ranked      nil           ; last /api/rankings response
     :loading?    false
     :status      nil
     :config      cfg
     :profile     :balanced
     :teams       (make-teams (:num-teams cfg) (:roster cfg) (:starting-bankroll cfg))
     :my-team-id  "t0"
     :drafted     {}            ; player-id -> {:price :team-id}
     :picks       []            ; [{:player-id :position :price :team-id}]
     :nominated-id nil
     :bid          ""
     :bid-team     "t0"
     :sort        {:key :worth :dir -1}
     :pos-filter  nil
     :search      ""
     :view        :board
     :columns     (default-columns)}))

;; slice persisted to localStorage
(def persist-keys [:config :profile :teams :drafted :picks :columns :my-team-id])
