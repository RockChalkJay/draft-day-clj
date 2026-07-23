(ns draft-day.subs
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [draft-day.db :as db]))

;; ---- simple extracts ----
(doseq [k [:view :status :source :loading? :profile :config :teams :my-team-id
           :nominated-id :bid :bid-team :sort :pos-filter :search :columns :drafted :ranked :modal
           :scoring-presets]]
  (rf/reg-sub k (fn [dbv _] (get dbv k))))

;; :custom when :scoring is a full {stat weight} map (hand-edited or imported),
;; otherwise the active preset keyword itself (:standard/:half-ppr/:ppr).
(rf/reg-sub :scoring-mode :<- [:config]
  (fn [cfg _] (let [s (:scoring cfg)] (if (map? s) :custom s))))

(rf/reg-sub :ranked-players :<- [:ranked] (fn [r _] (:players r)))

(rf/reg-sub :players-by-id :<- [:ranked]
  (fn [r _] (into {} (map (juxt :player-id identity)) (:players r))))

(rf/reg-sub :market :<- [:ranked]
  (fn [r _] (select-keys r [:inflation :inflation-index :market-heat :position-inflation :pdm-map])))

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

;; undrafted, unfiltered by position/search — the pool suggestions/budget
;; targets draw from, so they don't collapse when the board is filtered.
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

;; ---- suggestions ----
(def ^:private suggestion-positions #{"QB" "RB" "WR" "TE"})

(rf/reg-sub :top-overall :<- [:undrafted-players]
  (fn [players _]
    (->> players (filter #(pos? (or (:worth %) 0)))
         (sort-by #(- (:worth %))) (take 5) vec)))

(rf/reg-sub :top-by-position :<- [:undrafted-players]
  (fn [players _]
    (into {} (map (fn [pos]
                    [pos (->> players
                              (filter #(and (= (:position %) pos) 
                                            (pos? (or (:worth %) 0))))
                              (sort-by #(- (:worth %))) (take 3) vec)])
                  ["QB" "RB" "WR" "TE"]))))

(rf/reg-sub :needs
  :<- [:my-team]
  (fn [team _]
    (->> (:roster team)
         (filter #(and (nil? (:player-id %)) (suggestion-positions (:pos %))))
         (map :pos) distinct vec)))

;; best available player for each open starter need (blue "Best Value" cards)
(rf/reg-sub :best-value-for-needs
  :<- [:needs]
  :<- [:top-by-position]
  (fn [[needs by-pos] _]
    (mapv (fn [pos] {:pos pos :player (first (get by-pos pos))}) needs)))
