(ns draftday.events
  (:require [re-frame.core :as rf]
            [draftday.db :as db]
            [draftday.fx :as fx]))

;; Persist a whitelisted slice to localStorage after any mutating event.
(def persist
  (rf/->interceptor
   :id :persist
   :after (fn [ctx]
            (let [new-db (or (get-in ctx [:effects :db]) (get-in ctx [:coeffects :db]))]
              (assoc-in ctx [:effects :persist!] (select-keys new-db db/persist-keys))))))

;; ---- boot / data loading ----

(rf/reg-event-fx
 :boot
 (fn [_ _]
   {:db (merge (db/default-db) (fx/load-persisted))
    :fx [[:dispatch [:fetch-players]]]}))

(rf/reg-event-fx
 :fetch-players
 (fn [{:keys [db]} [_ refresh?]]
   {:db   (assoc db :loading? true :status "Loading players…")
    :http {:method :get
           :url (str "/api/players" (when refresh? "?refresh=true"))
           :on-success [:players-loaded]
           :on-failure [:load-failed]}}))

(rf/reg-event-fx
 :players-loaded
 (fn [{:keys [db]} [_ resp]]
   {:db (assoc db :players (:players resp) :source (:source resp) :loading? false
               :status (str (:count resp) " players · " (:source resp)))
    :fx [[:dispatch [:recompute]]]}))

(rf/reg-event-db :load-failed (fn [db [_ err]] (assoc db :loading? false :status (str "Load failed: " err))))

;; ---- rankings recompute ----

(defn- replacement-config [roster]
  (select-keys roster [:qb :rb :wr :te :flex]))

(defn- league-state [db]
  {:teams              (:teams db)
   :drafted-player-ids (vec (keys (:drafted db)))
   :starting-bankroll  (get-in db [:config :starting-bankroll])
   :picks              (:picks db)})

(rf/reg-event-fx
 :recompute
 (fn [{:keys [db]} _]
   (if (seq (:players db))
     {:http {:method :post :url "/api/rankings"
             :body {:num-teams          (get-in db [:config :num-teams])
                    :num-tiers          (get-in db [:config :num-tiers])
                    :scoring            (get-in db [:config :scoring])
                    :replacement-config (replacement-config (get-in db [:config :roster]))
                    :profile            (:profile db)
                    :league-state       (league-state db)}
             :on-success [:ranked-loaded]}}
     {})))

(rf/reg-event-db :ranked-loaded (fn [db [_ resp]] (assoc db :ranked resp)))

;; ---- UI state ----

(rf/reg-event-db :set-view      (fn [db [_ v]] (assoc db :view v)))
(rf/reg-event-db :set-search    (fn [db [_ q]] (assoc db :search q)))
(rf/reg-event-db :set-pos-filter (fn [db [_ p]] (assoc db :pos-filter (if (= p (:pos-filter db)) nil p))))
(rf/reg-event-db :set-nominated (fn [db [_ id]] (assoc db :nominated-id id)))
(rf/reg-event-db :set-bid       (fn [db [_ v]] (assoc db :bid v)))
(rf/reg-event-db :set-bid-team  (fn [db [_ v]] (assoc db :bid-team v)))
(rf/reg-event-db :set-status    (fn [db [_ s]] (assoc db :status s)))

(rf/reg-event-db
 :set-sort
 (fn [db [_ k]]
   (update db :sort
           (fn [{:keys [key dir]}]
             (if (= key k)
               {:key k :dir (- dir)}
               {:key k :dir (if (#{:name :team :position :rank :adp :ecr} k) 1 -1)})))))

(rf/reg-event-fx :set-profile [persist]
  (fn [{:keys [db]} [_ p]] {:db (assoc db :profile p) :fx [[:dispatch [:recompute]]]}))

;; ---- columns ----

(rf/reg-event-db :toggle-column [persist]
  (fn [db [_ k]]
    (update db :columns (fn [cols] (mapv #(if (= (:key %) k) (update % :visible? not) %) cols)))))

(rf/reg-event-db :move-column [persist]
  (fn [db [_ from to]]
    (update db :columns
            (fn [cols]
              (let [item    (nth cols from)
                    without (vec (concat (subvec cols 0 from) (subvec cols (inc from))))]
                (vec (concat (subvec without 0 to) [item] (subvec without to))))))))

;; ---- draft actions ----

(defn- eligible? [slot-pos position]
  (or (= slot-pos position)
      (and (= slot-pos "FLEX") (#{"RB" "WR" "TE"} position))
      (= slot-pos "BENCH")))

(defn- fill-slot [roster position player-id]
  (if-let [idx (first (keep-indexed (fn [i s] (when (and (nil? (:player-id s))
                                                         (eligible? (:pos s) position)) i))
                                    roster))]
    (assoc-in roster [idx :player-id] player-id)
    roster))

(rf/reg-event-fx :record-pick [persist]
  (fn [{:keys [db]} [_ {:keys [player-id price team-id position]}]]
    (let [price (js/parseInt price 10)
          teams (mapv (fn [t]
                        (if (= (:team-id t) team-id)
                          (-> t
                              (update :roster fill-slot position player-id)
                              (update :bankroll - price))
                          t))
                      (:teams db))]
      {:db (-> db
               (assoc :teams teams)
               (update :drafted assoc player-id {:price price :team-id team-id})
               (update :picks conj {:player-id player-id :position position :price price :team-id team-id})
               (assoc :nominated-id nil :bid ""))
       :fx [[:dispatch [:recompute]]]})))

(rf/reg-event-fx :undo-pick [persist]
  (fn [{:keys [db]} [_ player-id]]
    (let [{:keys [price team-id]} (get-in db [:drafted player-id])
          teams (mapv (fn [t]
                        (if (= (:team-id t) team-id)
                          (-> t
                              (update :roster (fn [r] (mapv #(if (= (:player-id %) player-id)
                                                               (assoc % :player-id nil) %) r)))
                              (update :bankroll + price))
                          t))
                      (:teams db))]
      {:db (-> db
               (assoc :teams teams)
               (update :drafted dissoc player-id)
               (update :picks (fn [ps] (vec (remove #(= (:player-id %) player-id) ps)))))
       :fx [[:dispatch [:recompute]]]})))

;; ---- config / Sleeper import ----

(rf/reg-event-fx :apply-config [persist]
  (fn [{:keys [db]} [_ new-cfg]]
    (let [cfg   (merge (:config db) new-cfg)
          teams (if (empty? (:picks db))
                  (db/make-teams (:num-teams cfg) (:roster cfg) (:starting-bankroll cfg))
                  (:teams db))]
      {:db (assoc db :config cfg :teams teams)
       :fx [[:dispatch [:recompute]]]})))

(rf/reg-event-fx :import-sleeper
  (fn [{:keys [db]} [_ league-id]]
    {:db   (assoc db :status "Importing from Sleeper…")
     :http {:method :get :url (str "https://api.sleeper.app/v1/league/" league-id)
            :on-success [:sleeper-loaded] :on-failure [:sleeper-failed]}}))

(defn- sleeper->config [data current]
  (let [s         (:scoring_settings data)
        positions (:roster_positions data)
        rec       (or (:rec s) 0)
        preset    (cond (>= rec 1) :ppr (>= rec 0.5) :half-ppr :else :standard)
        cnt       (fn [p] (count (filter #(= % p) positions)))
        known     #{"QB" "RB" "WR" "TE" "K" "DEF" "BN" "FLEX" "WRRB_FLEX" "REC_FLEX"}
        flex      (+ (cnt "FLEX") (cnt "WRRB_FLEX") (cnt "REC_FLEX"))
        bench     (+ (cnt "BN") (count (remove known positions)))]
    {:scoring   preset
     :num-teams (or (:total_rosters data) (:num-teams current))
     :roster    {:qb (cnt "QB") :rb (cnt "RB") :wr (cnt "WR") :te (cnt "TE")
                 :flex flex :k (cnt "K") :dst (cnt "DEF") :bench bench}}))

(rf/reg-event-fx :sleeper-loaded
  (fn [{:keys [db]} [_ data]]
    {:fx [[:dispatch [:apply-config (sleeper->config data (:config db))]]
          [:dispatch [:set-status (str "✓ Imported \"" (:name data) "\" (" (:season data) ")")]]]}))

(rf/reg-event-db :sleeper-failed (fn [db [_ err]] (assoc db :status (str "Sleeper import failed: " err))))
