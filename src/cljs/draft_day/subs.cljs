(ns draft-day.subs
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [draft-day.db :as db]))

;; ---- simple extracts ----
(doseq [k [:view :status :config :teams :my-team-id
           :nominated-id :sort :pos-filter :search :columns :drafted :ranked :modal
           :watchlist]]
  (rf/reg-sub k (fn [dbv _] (get dbv k))))

;; :custom when :scoring is a full {stat weight} map (hand-edited or imported),
;; otherwise the active preset keyword itself (:standard/:half-ppr/:ppr).
(rf/reg-sub :scoring-mode :<- [:config]
  (fn [cfg _] (let [s (:scoring cfg)] (if (map? s) :custom s))))

(rf/reg-sub :ranked-players :<- [:ranked] (fn [r _] (:players r)))

(rf/reg-sub :players-by-id :<- [:ranked]
  (fn [r _] (into {} (map (juxt :player-id identity)) (:players r))))

(rf/reg-sub :market :<- [:ranked]
  (fn [r _] (select-keys r [:inflation :inflation-index :market-heat])))

(rf/reg-sub :visible-columns :<- [:columns]
  (fn [cols _] (filterv :visible? cols)))

;; ---- board: filter to undrafted, apply pos/search, rank by worth, sort ----

(defn- matches-search? [p q]
  (or (str/blank? q)
      (str/includes? (str/lower-case (or (:player-name p) "")) q)
      (str/includes? (str/lower-case (or (:team p) "")) q)))

(defn- sort-players [players key dir]
  (let [acc (get db/sort-accessors key :worth)]
    (sort (fn [a b]
            (let [va (acc a) vb (acc b)]
              (cond
                (and (nil? va) (nil? vb)) 0
                (nil? va) 1
                (nil? vb) -1
                :else (* dir (compare va vb)))))
          players)))

;; undrafted, unfiltered by position/search — the pool the board and watch list
;; draw from, so they don't collapse when the board is filtered by pos/search.
(rf/reg-sub :undrafted-players
  :<- [:ranked-players]
  :<- [:drafted]
  (fn [[players drafted] _]
    (let [drafted-ids (set (keys drafted))]
      (remove #(contains? drafted-ids (:player-id %)) players))))

(rf/reg-sub :board-players
  :<- [:undrafted-players]
  :<- [:sort]
  :<- [:pos-filter]
  :<- [:search]
  (fn [[players sort pos-filter search] _]
    (let [q           (str/lower-case (or search ""))
          filtered    (->> players
                           (filter #(or (nil? pos-filter) (= (:position %) pos-filter)))
                           (filter #(matches-search? % q)))
          ;; live overall rank by Worth (independent of the active sort column)
          rank-map    (into {} (map-indexed (fn [i p] [(:player-id p) (inc i)])
                                            (sort-by #(- (or (:worth %) 0)) filtered)))
          ranked      (map #(assoc % :rank (rank-map (:player-id %))) filtered)]
      (vec (sort-players ranked (:key sort) (:dir sort))))))

;; ---- my team / roster ----

(rf/reg-sub :my-team :<- [:teams] :<- [:my-team-id]
  (fn [[teams id] _] (first (filter #(= (:team-id %) id) teams))))

;; My roster's bye exposure (starters/bench/open non-bench slot count). Drives the
;; board's red pulse and the roster's amber marker. Recomputes after every
;; pick/undo since it derives from :teams and :ranked.
(rf/reg-sub :my-bye-exposure :<- [:my-team] :<- [:players-by-id]
  (fn [[team by-id] _] (db/roster-exposure team by-id)))

;; Starter player-ids whose bye week has no bench cover, once the lineup is full
;; (empty before that). Colors the My Roster Bye column amber.
(rf/reg-sub :my-uncovered-starters :<- [:my-bye-exposure]
  (fn [exposure _] (db/uncovered-starter-ids exposure)))

(defn- open-slots [team] (count (filter #(nil? (:player-id %)) (:roster team))))

(rf/reg-sub :my-max-bid :<- [:my-team]
  (fn [team _] (when team (max 1 (- (:bankroll team) (dec (open-slots team)))))))

;; Pooled per-bucket availability for MY ROSTER open slots: each open slot in a
;; budget bucket shows (plan − spent) ÷ open slots, floored. Spend counts against
;; the bucket of the slot a player actually fills (a WR parked in FLEX charges
;; FLEX). Buckets with no plan set get no entry, so the column stays blank.
(rf/reg-sub :budget-avail
  :<- [:my-team]
  :<- [:drafted]
  :<- [:config]
  (fn [[team drafted cfg] _]
    (let [plan          (:budget-plan cfg)
          price-of      (fn [player-id] (get-in drafted [player-id :price] 0))
          ;; tally spent + open slots per budget bucket
          bucket-totals (reduce (fn [acc {:keys [pos player-id]}]
                                  (let [budget (db/slot->budget-key pos)]
                                    (if player-id
                                      (update-in acc [budget :spent] (fnil + 0) (price-of player-id))
                                      (update-in acc [budget :open]  (fnil inc 0)))))
                                {} (:roster team))]
      ;; (planned − spent) ÷ open, per bucket that has a plan and open slots
      (reduce-kv (fn [acc budget {:keys [spent open]}]
                   (let [planned (get plan budget 0)]
                     (if (and (pos? planned) 
                              (pos? (or open 0)))
                       (assoc acc budget (js/Math.floor (/ (- planned (or spent 0)) open)))
                       acc)))
                 {} bucket-totals))))

;; ---- watch list ----

;; The raw set, for the board's star membership test.
(rf/reg-sub :watch-set :<- [:watchlist] (fn [w _] (or w #{})))

;; Watched players, richest first. Drafted players fall out here rather than
;; through an event, so a pick (or its undo) is reflected automatically.
(rf/reg-sub :watchlist-players
  :<- [:players-by-id]
  :<- [:watchlist]
  :<- [:drafted]
  (fn [[by-id watchlist drafted] _]
    (->> watchlist
         (remove #(contains? drafted %))
         (keep by-id)
         (sort-by #(- (or (:worth %) 0)))
         vec)))
