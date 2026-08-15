(ns draft-day.events
  (:require [re-frame.core :as rf]
            [draft-day.db :as db]
            [draft-day.scoring :as scoring]
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
            (update :columns db/reconcile-columns)    ; drop removed cols, add new ones
            (update :config db/reconcile-config))     ; repair a config from an older shape
    :fx [[:dispatch [:fetch-players]]]}))

(rf/reg-event-fx
 :fetch-players
 (fn [{:keys [db]} [_ refresh?]]
   {:db   (assoc db :status "Loading players…")
    :http {:method :get
           :url (str "/api/players" (when refresh? "?refresh=true"))
           :on-success [:players-loaded]
           :on-failure [:load-failed]}}))

(rf/reg-event-fx
 :players-loaded
 (fn [{:keys [db]} [_ resp]]
   ;; Draft state is migrated here rather than in :boot because the crosswalk
   ;; it needs travels on the players, which have not arrived at boot time.
   ;; Remapping is idempotent, so it runs on every load; the persist only fires
   ;; when something actually moved.
   (let [players  (:players resp)
         migrated (db/remap-draft-ids db (db/sleeper->player-id players))
         slice    (select-keys migrated db/persist-keys)
         changed? (not= (select-keys db db/persist-keys) slice)
         status   (str (:count resp) " players · " (:source resp))]
     (cond-> {:db (assoc migrated
                         :players players
                         :status status
                         :universe-status status)
              :fx [[:dispatch [:recompute]]]}
       changed? (assoc :persist! slice)))))

(rf/reg-event-db :load-failed (fn [db [_ err]] (assoc db :status (str "Load failed: " err))))

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
   ;; No players yet means the universe is still in flight; :players-loaded
   ;; dispatches :recompute once it lands, so a config change made in the
   ;; meantime is picked up rather than lost.
   (if-not (seq (:players db))
     {}
     (let [n (inc (:recompute-seq db 0))]
       {:db   (assoc db :recompute-seq n)
        :http {:method :post :url "/api/rankings"
               :body {:num-teams          (get-in db [:config :num-teams])
                      :scoring            (get-in db [:config :scoring])
                      :replacement-config (replacement-config (get-in db [:config :roster]))
                      :league-state       (league-state db)}
               :on-success [:ranked-loaded n]
               :on-failure [:recompute-failed]}}))))

(rf/reg-event-db :ranked-loaded
  (fn [db [_ n resp]]
    ;; Overlapping requests can answer out of order, and each one re-ranks the
    ;; whole universe, so latency varies. Only the newest may write the board —
    ;; otherwise a slow reply computed under the previous scoring config wins and
    ;; stays there until something else triggers a recompute.
    (if-not (= n (:recompute-seq db))
      db
      (cond-> (assoc db :ranked resp)
        ;; Clear only an error this handler's own failure path put up; other
        ;; flows (a league import's "✓ Imported …") own the status line too.
        (:recompute-error? db)
        (assoc :status (:universe-status db) :recompute-error? false)))))

(rf/reg-event-db :recompute-failed
  (fn [db [_ err]]
    ;; Leave :ranked alone — the previous board is stale but readable, which
    ;; beats blanking it. The status line is what says it is stale.
    (assoc db :status (str "Rankings update failed: " err) :recompute-error? true)))

;; ---- UI state ----

(rf/reg-event-db :set-view      (fn [db [_ v]] (assoc db :view v)))
(rf/reg-event-db :set-search    (fn [db [_ q]] (assoc db :search q)))
(rf/reg-event-db :set-pos-filter (fn [db [_ p]] (assoc db :pos-filter (if (= p (:pos-filter db)) nil p))))
(rf/reg-event-db :set-nominated (fn [db [_ id]] (assoc db :nominated-id id)))
(rf/reg-event-db :set-status    (fn [db [_ s]] (assoc db :status s)))

;; ---- watch list ----
;; Client-only tracking state: it feeds no valuation input, so these persist
;; but deliberately skip :recompute (same as :set-position-budget).

(rf/reg-event-db :watch-toggle [persist]
  (fn [db [_ id]]
    (update db :watchlist #(if (contains? % id) (disj % id) (conj % id)))))

(rf/reg-event-db :watch-remove [persist]
  (fn [db [_ id]] (update db :watchlist disj id)))

(rf/reg-event-db
 :set-sort
 (fn [db [_ k]]
   (update db :sort
           (fn [{:keys [key dir]}]
             (if (= key k)
               {:key k :dir (- dir)}
               ;; Ascending first for the columns where a lower number is better
               ;; (ranks, tiers, ADP) and alphabetical for the text ones;
               ;; everything else is a dollar or a point total, best-first.
               {:key k :dir (if (#{:name :team :position :rank :adp :ecr :fp-tier} k) 1 -1)})))))

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
               (assoc :nominated-id nil))
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
  (fn [{:keys [db]} [_ {:keys [num-teams starting-bankroll team-names]}]]
    (let [num-teams (max 2 (min 20 (or num-teams 12)))
          bankroll  (max 1 (or starting-bankroll 200))
          cfg   (assoc (:config db)
                       :num-teams num-teams
                       :starting-bankroll bankroll)
          teams (db/make-teams-named (take num-teams (concat team-names (repeat "")))
                                     (:roster cfg) bankroll)]
      {:db (-> db
               (assoc :config cfg :teams teams)
               ;; reset ALL in-progress draft state
               (assoc :drafted {} :picks [] :nominated-id nil :modal nil)
               (assoc :my-team-id (:team-id (first teams))))
       :fx [[:dispatch [:recompute]]]})))

;; Debounced for the same reason as :set-scoring-weight — the League and Roster
;; fields dispatch this per keystroke, and each one re-ranks the whole universe.
(rf/reg-event-fx :apply-config [persist]
  (fn [{:keys [db]} [_ new-cfg]]
    (let [cfg   (merge (:config db) new-cfg)
          teams (if (empty? (:picks db))
                  (db/make-teams (:num-teams cfg) (:roster cfg) (:starting-bankroll cfg))
                  (:teams db))]
      {:db (assoc db :config cfg :teams teams)
       :debounce {:id :recompute :event [:recompute]}})))

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

;; Seeded from the shared preset table rather than a fetched one: picking Custom
;; before an async /api/scoring/presets reply landed used to write nil into the
;; config, which the server read as PPR and the Settings page died on.
(rf/reg-event-fx :enable-custom-scoring [persist]
  (fn [{:keys [db]} _]
    (let [s (get-in db [:config :scoring])]
      (if (map? s)
        {}
        {:db (assoc-in db [:config :scoring]
                       (get scoring/presets s (:ppr scoring/presets)))
         :fx [[:dispatch [:recompute]]]}))))

(rf/reg-event-fx :set-scoring-weight [persist]
  (fn [{:keys [db]} [_ stat-key v]]
    ;; `usable-weight` is the same guard the server applies, so a NaN from a
    ;; cleared input box can never reach the request body — it used to serialize
    ;; as null, 400 the rankings call, and blank the board until localStorage was
    ;; cleared by hand. Debounced because each edit re-ranks the whole universe.
    {:db (assoc-in db [:config :scoring stat-key] (scoring/usable-weight v))
     :debounce {:id :recompute :event [:recompute]}}))

(rf/reg-event-fx :import-league
  (fn [{:keys [db]} [_ {:keys [provider league-id]}]]
    {:db   (assoc db :status "Importing league…")
     :http {:method :post :url "/api/league/import"
            :body {:provider provider :league-id league-id}
            :on-success [:league-import-loaded]
            :on-failure [:league-import-failed]}}))

;; A failed import now arrives at :league-import-failed, because the :http effect
;; routes any non-2xx there; this handler only ever sees a real config.
(rf/reg-event-fx :league-import-loaded
  (fn [_ [_ resp]]
    {:fx [[:dispatch [:apply-config (select-keys resp [:scoring :roster :num-teams])]]
          [:dispatch [:set-import-report (select-keys resp [:name :season :unsupported-scoring])]]
          [:dispatch [:set-status (str "✓ Imported \"" (:name resp) "\" (" (:season resp) ")")]]]}))

(rf/reg-event-db :set-import-report (fn [db [_ r]] (assoc db :import-report r)))

(rf/reg-event-db :league-import-failed
  (fn [db [_ err]] (assoc db :status (str "League import failed: " err))))

;; ---- cache reset ----

(rf/reg-event-fx
 :reset-cache
 (fn [{:keys [db]} _]
   {:db   (assoc db :status "Resetting player cache…" :modal nil)
    :http {:method :post :url "/api/cache/reset"
           :on-success [:cache-reset-done]
           :on-failure [:load-failed]}}))

(rf/reg-event-fx
 :cache-reset-done
 (fn [_ _]
   {:fx [[:dispatch [:fetch-players true]]]}))
