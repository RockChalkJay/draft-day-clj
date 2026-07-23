(ns draft-day.events
  (:require [re-frame.core :as rf]
            [draft-day.db :as db]
            [draft-day.fx :as fx]))

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
   {:db (-> (merge (db/default-db) (fx/load-persisted))
            (update :columns db/reconcile-columns))   ; drop removed cols, add new ones
    :fx [[:dispatch [:fetch-players]]
         [:dispatch [:fetch-scoring-presets]]]}))

(rf/reg-event-fx
 :fetch-scoring-presets
 (fn [_ _]
   {:http {:method :get :url "/api/scoring/presets"
           :on-success [:scoring-presets-loaded]}}))

(rf/reg-event-db :scoring-presets-loaded
  (fn [db [_ resp]] (assoc db :scoring-presets resp)))

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

;; ---- start-draft modal ----

(rf/reg-event-db :show-modal  (fn [db [_ m]] (assoc db :modal m)))
(rf/reg-event-db :close-modal (fn [db _] (assoc db :modal nil)))

(rf/reg-event-fx :start-draft [persist]
  (fn [{:keys [db]} [_ {:keys [num-teams starting-bankroll num-tiers team-names]}]]
    (let [num-teams (max 2 (min 20 (or num-teams 12)))
          bankroll  (max 1 (or starting-bankroll 200))
          num-tiers (max 1 (min 12 (or num-tiers 5)))
          cfg   (assoc (:config db)
                       :num-teams num-teams
                       :starting-bankroll bankroll
                       :num-tiers num-tiers)
          teams (db/make-teams-named (take num-teams (concat team-names (repeat "")))
                                     (:roster cfg) bankroll)]
      {:db (-> db
               (assoc :config cfg :teams teams)
               ;; reset ALL in-progress draft state
               (assoc :drafted {} :picks [] :nominated-id nil :bid "" :modal nil)
               (assoc :my-team-id (:team-id (first teams))))
       :fx [[:dispatch [:recompute]]]})))

(rf/reg-event-fx :apply-config [persist]
  (fn [{:keys [db]} [_ new-cfg]]
    (let [cfg   (merge (:config db) new-cfg)
          teams (if (empty? (:picks db))
                  (db/make-teams (:num-teams cfg) (:roster cfg) (:starting-bankroll cfg))
                  (:teams db))]
      {:db (assoc db :config cfg :teams teams)
       :fx [[:dispatch [:recompute]]]})))

;; Manager's per-position budget plan — client-only tracking, so no team
;; rebuild and no :recompute; just persist the :config slice.
(rf/reg-event-db :set-position-budget [persist]
  (fn [db [_ bucket v]]
    (let [v (if (and (number? v) (not (js/isNaN v))) (max 0 v) 0)]
      (assoc-in db [:config :budget-plan bucket] v))))

(rf/reg-event-fx :select-scoring-preset [persist]
  (fn [{:keys [db]} [_ preset]]
    {:db (assoc-in db [:config :scoring] preset)
     :fx [[:dispatch [:recompute]]]}))

(rf/reg-event-fx :enable-custom-scoring [persist]
  (fn [{:keys [db]} _]
    (let [scoring (get-in db [:config :scoring])]
      (if (map? scoring)
        {}
        {:db (assoc-in db [:config :scoring]
                        (get-in db [:scoring-presets :presets scoring]))
         :fx [[:dispatch [:recompute]]]}))))

(rf/reg-event-fx :set-scoring-weight [persist]
  (fn [{:keys [db]} [_ stat-key v]]
    {:db (assoc-in db [:config :scoring stat-key] v)
     :fx [[:dispatch [:recompute]]]}))

(rf/reg-event-fx :import-league
  (fn [{:keys [db]} [_ {:keys [provider league-id]}]]
    {:db   (assoc db :status "Importing league…")
     :http {:method :post :url "/api/league/import"
            :body {:provider provider :league-id league-id}
            :on-success [:league-import-loaded]
            :on-failure [:league-import-failed]}}))

(rf/reg-event-fx :league-import-loaded
  (fn [_ [_ resp]]
    (if (:error resp)
      {:fx [[:dispatch [:league-import-failed (:error resp)]]]}
      {:fx [[:dispatch [:apply-config (select-keys resp [:scoring :roster :num-teams])]]
            [:dispatch [:set-status (str "✓ Imported \"" (:name resp) "\" (" (:season resp) ")")]]]})))

(rf/reg-event-db :league-import-failed
  (fn [db [_ err]] (assoc db :status (str "League import failed: " err))))
