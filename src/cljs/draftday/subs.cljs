(ns draftday.subs
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [draftday.db :as db]))

;; ---- simple extracts ----
(doseq [k [:view :status :source :loading? :profile :config :teams :my-team-id
           :nominated-id :bid :bid-team :sort :pos-filter :search :columns :drafted :ranked :modal]]
  (rf/reg-sub k (fn [dbv _] (get dbv k))))

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

(rf/reg-sub :board-players
  :<- [:ranked-players]
  :<- [:drafted]
  :<- [:sort]
  :<- [:pos-filter]
  :<- [:search]
  (fn [[players drafted sort pos-filter search] _]
    (let [drafted-ids (set (keys drafted))
          q           (str/lower-case (or search ""))
          filtered    (->> players
                           (remove #(contains? drafted-ids (:player-id %)))
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

;; best available Worth per position (for MY ROSTER open-slot budget targets)
(rf/reg-sub :best-worth-by-pos
  :<- [:board-players]
  (fn [players _]
    (reduce (fn [m p]
              (let [pos (:position p) w (or (:worth p) 0)]
                (if (> w (get m pos 0)) (assoc m pos w) m)))
            {} players)))

;; ---- suggestions ----
(def ^:private suggestion-positions #{"QB" "RB" "WR" "TE"})

(rf/reg-sub :top-overall :<- [:board-players]
  (fn [players _]
    (->> players (filter #(pos? (or (:worth %) 0)))
         (sort-by #(- (:worth %))) (take 5) vec)))

(rf/reg-sub :top-by-position :<- [:board-players]
  (fn [players _]
    (into {} (for [pos ["QB" "RB" "WR" "TE"]]
               [pos (->> players
                         (filter #(and (= (:position %) pos) (pos? (or (:worth %) 0))))
                         (sort-by #(- (:worth %))) (take 3) vec)]))))

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
