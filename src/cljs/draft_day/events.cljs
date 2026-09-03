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
            (update :config db/reconcile-config)      ; repair a config from an older shape
            (update :watchlist db/reconcile-watchlist)  ; a set, back when it had no order
            (update :waiver-columns db/reconcile-waiver-columns)
            (update :league-sync db/reconcile-league-sync))
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
                         :universe (:universe resp)
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
      (let [err (:recompute-error db)]
        (cond-> (assoc db :ranked resp :recompute-error nil)
          ;; Take the status line back only while the error is still what is on
          ;; it. Other flows (a league import's "✓ Imported …") own it too, and
          ;; between the failure and this reply one of them may have spoken.
          (and err (= err (:status db)))
          (assoc :status (:universe-status db)))))))

(rf/reg-event-db :recompute-failed
  (fn [db [_ err]]
    ;; Leave :ranked alone — the previous board is stale but readable, which
    ;; beats blanking it. The status line is what says it is stale.
    (let [msg (str "Rankings update failed: " err)]
      (assoc db :status msg :recompute-error msg))))

;; ---- UI state ----

(rf/reg-event-fx :set-view
  (fn [{:keys [db]} [_ v]]
    ;; Opening the Waivers tab loads its board, because it is a second full rank
    ;; of the universe and a manager who never opens the tab should not pay for
    ;; one on every page load. Only when there is nothing to show: after that a
    ;; refresh is a button, not a side effect of navigation, or every glance at
    ;; the tab re-ranks the league.
    (cond-> {:db (assoc db :view v)}
      (and (= v :waivers) (nil? (:waivers db)))
      (assoc :fx [[:dispatch [:fetch-waivers]]]))))
(rf/reg-event-db :set-search    (fn [db [_ q]] (assoc db :search q)))
(rf/reg-event-db :set-pos-filter (fn [db [_ p]] (assoc db :pos-filter (if (= p (:pos-filter db)) nil p))))
(rf/reg-event-db :set-nominated (fn [db [_ id]] (assoc db :nominated-id id)))
(rf/reg-event-db :set-status    (fn [db [_ s]] (assoc db :status s)))

;; ---- watch list ----
;; Client-only tracking state: it feeds no valuation input, so these persist
;; but deliberately skip :recompute (same as :set-position-budget).

(rf/reg-event-db :watch-toggle [persist]
  (fn [db [_ id]]
    (update db :watchlist
            (fn [ids]
              (if (some #{id} ids)
                (vec (remove #{id} ids))
                ;; appended, never inserted: the order is the manager's, and a
                ;; new star is a guess about a later nomination, not a jump
                ;; over the ones already ranked.
                (conj (vec ids) id))))))

(rf/reg-event-db :watch-remove [persist]
  (fn [db [_ id]] (update db :watchlist #(vec (remove #{id} %)))))

;; Keyed by player-id rather than by row index, for the same reason
;; :move-column-onto is: the rows on screen are the *undrafted* watch list, so a
;; row's index there is not its index in the stored vector.
(rf/reg-event-db :move-watch-onto [persist]
  (fn [db [_ from-id to-id]]
    (update db :watchlist db/move-watch-onto from-id to-id)))

;; A one-shot rewrite of the stored order, not a sort mode: there is no
;; `:watch-sort` key in db to consult afterwards, the rows stay draggable, and
;; nothing re-sorts the list out from under the manager after a pick. That is the
;; whole reason `:watchlist-players` can keep promising an order he can trust.
(rf/reg-event-db :watch-sort [persist]
  (fn [db [_ k]]
    (update db :watchlist db/sort-watchlist
            (db/index-by-id (get-in db [:ranked :players])) k)))

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
               {:key k :dir (if (#{:name :team :position :rank :adp :ecr :tier :fp-tier} k) 1 -1)})))))

;; ---- columns ----

(rf/reg-event-db :toggle-column [persist]
  (fn [db [_ k]]
    (update db :columns (fn [cols] (mapv #(if (= (:key %) k) (update % :visible? not) %) cols)))))

;; Reorder is keyed, not indexed: the picker drags against every column while the
;; board header drags against the visible ones only, and both dispatch this.
(rf/reg-event-db :move-column-onto [persist]
  (fn [db [_ from-k to-k]]
    (update db :columns db/move-column-onto from-k to-k)))

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
        {:db (assoc-in db [:config :scoring] (scoring/resolve-config s))
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

;; ---- in-season: league sync + waivers ----
;; The same statelessness the draft board runs on: the browser owns the synced
;; league and re-POSTs it, and the server holds nothing between requests.

(rf/reg-event-fx :set-my-roster-id [persist]
  (fn [{:keys [db]} [_ id]]
    ;; Re-fetches, because almost everything on the board is measured *from*
    ;; this. The sync fires :fetch-waivers while it is still nil, so the first
    ;; board comes back with no drop, no budget and every bid blank; without a
    ;; refetch here, picking your team changed a dropdown and nothing else until
    ;; you happened to press Refresh.
    {:db  (assoc db :my-roster-id id)
     :fx  [[:dispatch [:fetch-waivers]]]}))

(rf/reg-event-fx :sync-league
  (fn [{:keys [db]} [_ {:keys [provider league-id]}]]
    {:db   (assoc db :waiver-status "Syncing rosters…")
     :http {:method :post :url "/api/league/sync"
            :body {:provider provider :league-id league-id}
            :on-success [:league-synced]
            :on-failure [:league-sync-failed]}}))

(defn my-roster-id-for
  "Which roster in this league belongs to `user-id`, or nil.

  The point of connecting an account. Without it the manager picks his own team
  out of a list of twelve before the board can name a drop, price a bid or show
  him his roster — and until he does, all three read as blank rather than as
  unanswered, which is what made the dropdown look like it did nothing.

  nil rather than a guess when nothing matches. A co-managed team, an orphan
  roster and a second account are all real, and the dropdown still handles them."
  [teams user-id]
  (when (not-empty (str user-id))
    (some (fn [t] (when (= (str (:owner-id t)) (str user-id)) (:roster-id t)))
          teams)))

(rf/reg-event-fx :league-synced [persist]
  (fn [{:keys [db]} [_ resp]]
    ;; The reply is repaired on the way *in*, not only at boot. It is the same
    ;; shape localStorage will hand back next session, and a provider that grew a
    ;; field or dropped one should fail here — where the status line can say so —
    ;; rather than a session later with no way to tell what changed.
    (let [league (db/reconcile-league-sync resp)
          ;; Only when it is still unset. A manager who corrected the dropdown —
          ;; he co-manages, or plays under a second account — must not have that
          ;; correction undone by the next re-sync.
          mine   (or (:my-roster-id db)
                     (my-roster-id-for (:teams league) (:sleeper-user-id db)))]
      {:db (assoc db :league-sync league
                  :my-roster-id mine
                  :waiver-status (if league
                                   (str "✓ Synced " (count (:teams league)) " rosters")
                                   "Sync returned nothing usable"))
       :fx [[:dispatch [:fetch-waivers]]]})))

(rf/reg-event-fx :league-connect
  (fn [{:keys [db]} [_ username]]
    {:db   (assoc db :waiver-status (str "Looking up " username "…"))
     :http {:method :get
            :url (str "/api/league/user?provider=sleeper&username="
                      (js/encodeURIComponent username))
            :on-success [:league-user-loaded]
            :on-failure [:league-user-failed]}}))

(rf/reg-event-fx :league-user-loaded [persist]
  (fn [{:keys [db]} [_ {:keys [user leagues]}]]
    (let [db' (assoc db :sleeper-username (:display-name user)
                     :sleeper-user-id (:user-id user)
                     :league-choices  (vec leagues))]
      (cond
        (empty? leagues)
        {:db (assoc db' :waiver-status
                    (str (:display-name user) " has no leagues this season."))}

        ;; One league is not a choice. Making the manager pick it out of a list
        ;; of one is a step that asks him to confirm the only possible answer.
        (= 1 (count leagues))
        {:db db'
         :fx [[:dispatch [:league-choose (:league-id (first leagues))]]]}

        :else
        {:db (assoc db' :waiver-status
                    (str "Pick one of " (count leagues) " leagues."))}))))

(rf/reg-event-db :league-user-failed
  (fn [db [_ err]] (assoc db :waiver-status (str "Lookup failed: " err))))

(rf/reg-event-fx :league-choose
  (fn [_ [_ league-id]]
    ;; Both, because they answer different questions off the same id: the sync is
    ;; who is rostered, the import is what the league's rules are. A manager who
    ;; synced without importing gets a board priced under the draft config's
    ;; scoring rather than his league's.
    {:fx [[:dispatch [:sync-league {:provider "sleeper" :league-id league-id}]]
          [:dispatch [:import-league {:provider "sleeper" :league-id league-id}]]]}))

(rf/reg-event-db :league-sync-failed
  (fn [db [_ err]] (assoc db :waiver-status (str "League sync failed: " err))))

(defn- waiver-request
  "The body of an /api/waivers call. `roster-size` is what the manager's league
  actually gives each team — the waiver board needs it to know whether a claim
  costs a drop, and `db/roster-template` is already the one place that expands a
  roster config into seats."
  [db]
  {:scoring            (get-in db [:config :scoring])
   :num-teams          (get-in db [:config :num-teams])
   :replacement-config (replacement-config (get-in db [:config :roster]))
   :league             (:league-sync db)
   :my-roster-id       (:my-roster-id db)
   :roster-size        (count (db/roster-template (get-in db [:config :roster])))})

(rf/reg-event-fx :fetch-waivers
  (fn [{:keys [db]} _]
    ;; Stamped and checked exactly as :recompute is, and for the identical
    ;; hazard: a full re-rank of the universe takes long enough that overlapping
    ;; requests answer out of order, and a reply computed against the *previous*
    ;; roster would otherwise win and stick — telling the manager a player he
    ;; just claimed is still available.
    (let [n (inc (:waiver-seq db 0))]
      {:db   (assoc db :waiver-seq n :waiver-status "Loading waiver board…")
       :http {:method :post :url "/api/waivers"
              :body (waiver-request db)
              :on-success [:waivers-loaded n]
              :on-failure [:waivers-failed]}})))

(rf/reg-event-db :waivers-loaded
  (fn [db [_ n resp]]
    (if-not (= n (:waiver-seq db))
      db
      (assoc db :waivers resp :waiver-status nil))))

(rf/reg-event-db :waivers-failed
  (fn [db [_ err]]
    ;; Leave :waivers alone — the previous board is stale but readable, which
    ;; beats blanking it. Same call as :recompute-failed makes.
    (assoc db :waiver-status (str "Waiver board failed: " err))))

(rf/reg-event-db :set-waiver-sort
  (fn [db [_ k]]
    (update db :waiver-sort
            (fn [{:keys [key dir]}]
              (if (= key k)
                {:key k :dir (- dir)}
                ;; Ascending first where a lower number is better or the column
                ;; is text; everything else is points or dollars, best-first.
                {:key k :dir (if (#{:name :team :position :rank :ecr :bye :inj} k) 1 -1)})))))

(rf/reg-event-db :toggle-waiver-column [persist]
  (fn [db [_ k]]
    (update db :waiver-columns
            (fn [cols] (mapv #(if (= (:key %) k) (update % :visible? not) %) cols)))))

(rf/reg-event-db :move-waiver-column-onto [persist]
  (fn [db [_ from-k to-k]]
    (update db :waiver-columns db/move-column-onto from-k to-k)))

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
