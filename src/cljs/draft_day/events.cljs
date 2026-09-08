(ns draft-day.events
  "Every mutating event, and the guards that keep a reply from writing into the
  wrong board.

  A REPLY MUST PROVE IT IS STILL WANTED. Two things can be true by the time one
  lands: the request has been superseded, or the manager has switched leagues
  under it. So `:recompute`/`:fetch-waivers` stamp a monotonic sequence number
  and only the newest may write, and every league-scoped request threads
  `db/league-key` through `:on-success` so the answer goes to the league it was
  asked for. A full re-rank takes long enough that overlapping requests
  routinely answer out of order; without the stamp a board computed under the
  *previous* scoring config wins and stays.

  A BOARD IS STAMPED, NOT CLEARED. `:ranked-loaded` records the
  `db/rules-stamp` its request carried, so the board can *say* it belongs to
  another league rather than blanking. Clearing could not: two leagues on one
  format would blank for nothing, and a recompute that never lands would leave
  an empty table explaining itself no better than a wrong one. `:waivers` is
  the exception: it states facts about a league (who is rostered, your FAAB,
  the drop), and under another league's name those are not stale, they are
  false.

  PERSISTENCE IS ALL-OR-NOTHING. Saved state either matches the current
  `fx/storage-version` or is not loaded at all, so no event here repairs one.
  Archived drafts live under their own key and version and are unaffected by a
  bump.

  The in-season half runs on the same statelessness as the draft board: the
  browser owns the synced league and re-POSTs it, and the server holds nothing
  between requests."
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
   ;; Archived drafts are read separately — see the ns docstring.
   {:db (assoc (merge (db/default-db) (fx/load-persisted))
               :drafts (fx/read-drafts))
    :fx [[:dispatch [:fetch-players]]]}))

;; The archive is written by an effect; this is the one place it is read back
;; into db, so there is a single reader rather than two `conj`s that can drift.
(rf/reg-event-db :refresh-drafts
  (fn [db _] (assoc db :drafts (fx/read-drafts))))

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
   (let [status (str (:count resp) " players · " (:source resp))]
     {:db (assoc db
                 :players (:players resp)
                 :universe (:universe resp)
                 :status status
                 :universe-status status)
      :fx [[:dispatch [:recompute]]]})))

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
   ;; Universe still in flight; :players-loaded re-dispatches once it lands,
   ;; so a config change made in the meantime is picked up rather than lost.
   (if-not (seq (:players db))
     {}
     (let [n (inc (:recompute-seq db 0))]
       {:db   (assoc db :recompute-seq n)
        :http {:method :post :url "/api/rankings"
               :body {:num-teams          (get-in db [:config :num-teams])
                      :scoring            (get-in db [:config :scoring])
                      :replacement-config (replacement-config (get-in db [:config :roster]))
                      :league-state       (league-state db)}
               ;; Rules ride along so the board can be compared against the
               ;; rules in force when it is *read* — see the ns docstring.
               :on-success [:ranked-loaded n (db/rules-stamp (:config db))]
               :on-failure [:recompute-failed]}}))))

(rf/reg-event-db :ranked-loaded
  (fn [db [_ n rules resp]]
    ;; Only the newest request may write the board — see the ns docstring.
    (if-not (= n (:recompute-seq db))
      db
      (let [err (:recompute-error db)]
        (cond-> (assoc db :ranked resp :ranked-rules rules :recompute-error nil)
          ;; Reclaim the status line only while the error is still on it —
          ;; a league import owns it too and may have spoken since.
          (and err (= err (:status db)))
          (assoc :status (:universe-status db)))))))

(rf/reg-event-db :recompute-failed
  (fn [db [_ err]]
    ;; Leave :ranked alone: stale but readable beats blank, and the status
    ;; line plus `:ranked-rules` are what say so. See the ns docstring.
    (let [msg (str "Rankings update failed: " err)]
      (assoc db :status msg :recompute-error msg))))

;; ---- UI state ----

(rf/reg-event-fx :set-view
  (fn [{:keys [db]} [_ v]]
    ;; A second full rank of the universe, so it loads on first open only:
    ;; after that a refresh is a button, not a side effect of navigation.
    (cond-> {:db (assoc db :view v)}
      (and (= v :waivers) (nil? (:waivers db)))
      (assoc :fx [[:dispatch [:fetch-waivers]]]))))
(rf/reg-event-db :set-search    (fn [db [_ q]] (assoc db :search q)))
(rf/reg-event-db :set-pos-filter (fn [db [_ p]] (assoc db :pos-filter (if (= p (:pos-filter db)) nil p))))
(rf/reg-event-db :set-nominated (fn [db [_ id]] (assoc db :nominated-id id)))
(rf/reg-event-db :set-status    (fn [db [_ s]] (assoc db :status s)))

;; ---- watch list ----
;; Client-only tracking state, feeding no valuation input — so these persist
;; but deliberately skip :recompute (as :set-position-budget does).

(rf/reg-event-db :watch-toggle [persist]
  (fn [db [_ id]]
    (update db :watchlist
            (fn [ids]
              (if (some #{id} ids)
                (vec (remove #{id} ids))
                ;; Appended, never inserted: the order is the manager's, and a
                ;; new star does not jump the ones already ranked.
                (conj (vec ids) id))))))

(rf/reg-event-db :watch-remove [persist]
  (fn [db [_ id]] (update db :watchlist #(vec (remove #{id} %)))))

;; Keyed by player-id, not row index — the rows on screen are the *undrafted*
;; watch list, so an index there is not one into the stored vector.
(rf/reg-event-db :move-watch-onto [persist]
  (fn [db [_ from-id to-id]]
    (update db :watchlist db/move-watch-onto from-id to-id)))

;; A one-shot rewrite of the stored order, not a sort mode: nothing re-sorts
;; the list afterwards — which is what lets `:watchlist-players` promise one.
(rf/reg-event-db :watch-sort [persist]
  (fn [db [_ k]]
    ;; The one place a stale read would *write*: sorting between a switch and
    ;; its reply would persist the previous league's ranking. So it refuses.
    (if (not= (:ranked-rules db) (db/rules-stamp (:config db)))
      db
      (update db :watchlist db/sort-watchlist
              (db/index-by-id (get-in db [:ranked :players])) k))))

(rf/reg-event-db
 :set-sort
 (fn [db [_ k]]
   (update db :sort
           (fn [{:keys [key dir]}]
             (if (= key k)
               {:key k :dir (- dir)}
               ;; Ascending where lower is better (ranks, tiers, ADP) and for
               ;; text; everything else is dollars or points, best-first.
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

;; One copy of the FLEX rule, in `db` so both sides of the wire read the same
;; one — this file's private version was the fourth spelling of it.
(def ^:private eligible? db/slot-accepts?)

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

(rf/reg-event-fx :archive-draft
  (fn [{:keys [db]} _]
    ;; Refuses an empty shell, and a repeat press against unchanged state —
    ;; that would list one draft as several. Only the first applies on start.
    (when (and (db/drafted-anything? db)
               (not= (:picks db) (:picks (last (fx/read-drafts)))))
      {:archive-draft! (db/archive-entry db (.toISOString (js/Date.)))
       :fx [[:dispatch [:refresh-drafts]]]})))

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
               (db/set-config cfg)
               (assoc :teams teams)
               ;; reset ALL in-progress draft state
               (assoc :drafted {} :picks [] :nominated-id nil :modal nil)
               (assoc :my-team-id (:team-id (first teams))))
       ;; The one place a completed draft is destroyed. `db` is still the
       ;; outgoing draft: `:db` above is a value, not an assignment.
       :fx [(when (db/drafted-anything? db)
              [:archive-draft! (db/archive-entry db (.toISOString (js/Date.)))])
            [:dispatch [:refresh-drafts]]
            [:dispatch [:recompute]]]})))

;; Debounced for the same reason as :set-scoring-weight — the League and Roster
;; fields dispatch this per keystroke, and each one re-ranks the whole universe.
(rf/reg-event-fx :apply-config [persist]
  (fn [{:keys [db]} [_ new-cfg]]
    (let [cfg   (merge (:config db) new-cfg)
          teams (if (empty? (:picks db))
                  (db/make-teams (:num-teams cfg) (:roster cfg) (:starting-bankroll cfg))
                  (:teams db))]
      {:db (-> db (db/set-config cfg) (assoc :teams teams))
       :debounce {:id :recompute :event [:recompute]}})))

;; Manager's per-position budget plan — client-only tracking, so no team
;; rebuild and no :recompute; just persist the :config slice.
(rf/reg-event-db :set-position-budget [persist]
  (fn [db [_ bucket v]]
    (let [v (if (and (number? v) (not (js/isNaN v))) (max 0 v) 0)]
      (db/update-config db assoc-in [:budget-plan bucket] v))))

(rf/reg-event-fx :select-scoring-preset [persist]
  (fn [{:keys [db]} [_ preset]]
    {:db (db/update-config db assoc :scoring preset)
     :fx [[:dispatch [:recompute]]]}))

;; Seeded from the shared preset table, never a fetched one: picking Custom
;; before an async reply landed used to write nil, which the server read as PPR.
(rf/reg-event-fx :enable-custom-scoring [persist]
  (fn [{:keys [db]} _]
    (let [s (get-in db [:config :scoring])]
      (if (map? s)
        {}
        {:db (db/update-config db assoc :scoring (scoring/resolve-config s))
         :fx [[:dispatch [:recompute]]]}))))

(rf/reg-event-fx :set-scoring-weight [persist]
  (fn [{:keys [db]} [_ stat-key v]]
    ;; `usable-weight` is the server's own guard; a NaN used to 400 the call.
    ;; Debounced, because each edit re-ranks the whole universe.
    {:db (db/update-config db assoc-in [:scoring stat-key] (scoring/usable-weight v))
     :debounce {:id :recompute :event [:recompute]}}))

(rf/reg-event-fx :import-league
  (fn [{:keys [db]} [_ {:keys [provider league-id]}]]
    ;; The key rides along so imported rules land in the league they were
    ;; fetched for — see the ns docstring.
    {:db   (assoc db :status "Importing league…")
     :http {:method :post :url "/api/league/import"
            :body {:provider provider :league-id league-id}
            :on-success [:league-import-loaded (db/league-key provider league-id)]
            :on-failure [:league-import-failed]}}))

;; A failed import now arrives at :league-import-failed, because the :http effect
;; routes any non-2xx there; this handler only ever sees a real config.
(rf/reg-event-fx :league-import-loaded [persist]
  (fn [{:keys [db]} [_ k resp]]
    (let [cfg    (select-keys resp [:scoring :roster :num-teams])
          known? (contains? (:leagues db) k)
          ;; Only the active league's rules may touch the board.
          live?  (or (nil? k) (= k (:active-league db)))]
      ;; Status and report describe *the board*, so they are gated with
      ;; `:apply-config` — a late reply would otherwise announce the old league.
      {:db (cond-> db
             known? (-> (update-in [:leagues k :config] merge cfg)
                        (update-in [:leagues k] merge (select-keys resp [:name :season]))))
       :fx (if live?
             [[:dispatch [:apply-config cfg]]
              [:dispatch [:set-import-report (select-keys resp [:name :season :unsupported-scoring])]]
              [:dispatch [:set-status (str "✓ Imported \"" (:name resp) "\" (" (:season resp) ")")]]]
             [])})))

(rf/reg-event-db :set-import-report (fn [db [_ r]] (assoc db :import-report r)))

(rf/reg-event-db :league-import-failed
  (fn [db [_ err]]
    ;; Both status lines: Settings renders `:waiver-status`, the header
    ;; `:status`, and only one put the failure on the unwatched tab.
    (let [msg (str "League import failed: " err)]
      (assoc db :status msg :waiver-status msg))))

;; ---- in-season: league sync + waivers ----

(rf/reg-event-fx :set-my-roster-id [persist]
  (fn [{:keys [db]} [_ id]]
    ;; Almost everything on the board is measured *from* this, and the first
    ;; board came back while it was nil — so picking a team must refetch.
    (if-let [k (:active-league db)]
      {:db  (assoc-in db [:leagues k :my-roster-id] id)
       :fx  [[:dispatch [:fetch-waivers]]]}
      {})))

;; ---- the active league ----

(defn activate
  "Make `k` the league everything on screen is about, swapping in its config,
  its teams and whatever the last league's board asserted about *its* league.

  `default-config` underneath, because a league whose import never landed has
  scoring and roster but no bankroll, and a nil bankroll reaches the rankings
  request as null."
  [db k]
  (let [cfg (merge db/default-config (get-in db [:leagues k :config]))]
    (cond-> (assoc db
                   :active-league k
                   :config cfg
                   ;; Dropped, while `:ranked` is only stamped — ns docstring.
                   :waivers nil
                   ;; Goes with the board it was asked about: a free agent in
                   ;; one league is rostered in another.
                   :compare [])
      ;; `:teams` is built from the three values that just moved, so it is
      ;; rebuilt or every dollar is wrong. Picks keep their teams; docs/TODO.md.
      (empty? (:picks db))
      (assoc :teams (db/make-teams (:num-teams cfg) (:roster cfg) (:starting-bankroll cfg))))))

(rf/reg-event-fx :set-active-league [persist]
  (fn [{:keys [db]} [_ k]]
    ;; Both boards, because both are priced under this league's rules — a
    ;; switch that moved one would leave Worth priced under the league you left.
    (if (contains? (:leagues db) k)
      {:db (activate db k)
       :fx [[:dispatch [:recompute]]
            [:dispatch [:fetch-waivers]]]}
      {})))

(rf/reg-event-fx :sync-league
  (fn [{:keys [db]} [_ {:keys [provider league-id]}]]
    ;; `db/league-key` calls `name` on the provider, and `(name nil)` throws in
    ;; ClojureScript — killing the event rather than reporting anything.
    (if-not (and provider league-id)
      {:db (assoc db :waiver-status "Nothing to sync — no league is selected.")}
      {:db   (assoc db :waiver-status "Syncing rosters…")
       :http {:method :post :url "/api/league/sync"
              :body {:provider provider :league-id league-id}
              :on-success [:league-synced (db/league-key provider league-id)]
              :on-failure [:league-sync-failed]}})))

(defn my-roster-id-for
  "Which roster in this league belongs to `user-id`, or nil."
  [teams user-id]
  (when (not-empty (str user-id))
    (some (fn [t] (when (= (str (:owner-id t)) (str user-id)) (:roster-id t)))
          teams)))

(rf/reg-event-fx :league-synced [persist]
  (fn [{:keys [db]} [_ k resp]]
    ;; Repaired on the way *in*, not only at boot: a provider that grew or
    ;; dropped a field should fail here, where the status line can say so.
    (let [league   (db/reconcile-league-sync resp)
          entry    (get (:leagues db) k)
          provider (:provider entry)
          ;; Only when unset: a manager who corrected the dropdown must not
          ;; have that undone by the next re-sync.
          mine     (or (:my-roster-id entry)
                       (my-roster-id-for (:teams league)
                                         (get-in db [:accounts provider :user-id])))]
      {:db (-> db
               (update-in [:leagues k] merge
                          (cond-> {:sync league :my-roster-id mine}
                            ;; Name and season ride along so a league synced
                            ;; by pasted id still reads in the switcher.
                            (:name league)   (assoc :name (:name league))
                            (:season league) (assoc :season (:season league))))
               (assoc :waiver-status (if league
                                       (str "✓ Synced " (count (:teams league)) " rosters")
                                       "Sync returned nothing usable")))
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
    ;; Keyed by provider, so a second Sleeper account replaces the first while
    ;; an ESPN one lands beside it. Persisted, or it is a login we hide.
    (let [provider "sleeper"
          db' (-> db
                  (assoc-in [:accounts provider]
                            {:provider provider
                             :user-id  (:user-id user)
                             :username (:display-name user)})
                  (assoc :league-choices (vec leagues)))]
      (cond
        (empty? leagues)
        {:db (assoc db' :waiver-status
                    (str (:display-name user) " has no leagues this season."))}

        ;; One league is not a choice. Making the manager pick it out of a list
        ;; of one is a step that asks him to confirm the only possible answer.
        (= 1 (count leagues))
        {:db db'
         :fx [[:dispatch [:league-choose (first leagues)]]]}

        :else
        {:db (assoc db' :waiver-status
                    (str "Pick one of " (count leagues) " leagues."))}))))

(rf/reg-event-db :league-user-failed
  (fn [db [_ err]] (assoc db :waiver-status (str "Lookup failed: " err))))

(rf/reg-event-fx :league-choose [persist]
  (fn [{:keys [db]} [_ league]]
    ;; Takes the picker's league map or a bare typed id. The map carries a name
    ;; and season the sync supplies only on reply, and the switcher wants both.
    (let [league    (if (map? league) league {:league-id league})
          league-id (:league-id league)
          provider  "sleeper"
          k         (db/league-key provider league-id)
          ;; What we knew wins over the seed — a re-chosen league keeps its
          ;; team and rules — but the picker's name and season are freshest.
          entry     (merge {:provider provider :league-id league-id
                            :config   (:config db)}
                           (get (:leagues db) k)
                           (select-keys league [:name :season]))]
      ;; Sync is who is rostered, import is the rules; `:recompute` because
      ;; until the import answers this league seeds the previous one's config.
      {:db (-> db (assoc-in [:leagues k] entry) (activate k))
       :fx [[:dispatch [:sync-league {:provider provider :league-id league-id}]]
            [:dispatch [:import-league {:provider provider :league-id league-id}]]
            [:dispatch [:recompute]]]})))

(rf/reg-event-db :league-sync-failed
  (fn [db [_ err]] (assoc db :waiver-status (str "League sync failed: " err))))

(defn- waiver-request
  "The body of an /api/waivers call. `roster-size` is what the manager's league
  actually gives each team — the waiver board needs it to know whether a claim
  costs a drop, and `db/roster-template` is already the one place that expands a
  roster config into seats."
  [db]
  ;; Both halves come off the *active* league entry, so the payload the server
  ;; sees is unchanged and `rankings.waiver` knows nothing about a leagues map.
  (let [lg (db/active-league db)]
    {:scoring            (get-in db [:config :scoring])
     :num-teams          (get-in db [:config :num-teams])
     :replacement-config (replacement-config (get-in db [:config :roster]))
     :league             (:sync lg)
     :my-roster-id       (:my-roster-id lg)
     ;; The whole roster config, not `replacement-config` — that drops K and
     ;; DST, which fill starting slots. See `waiver/with-lineup-upgrade`.
     :roster             (get-in db [:config :roster])
     :roster-size        (count (db/roster-template (get-in db [:config :roster])))}))

(rf/reg-event-fx :fetch-waivers
  (fn [{:keys [db]} _]
    ;; Stamped exactly as :recompute is — see the ns docstring. Worse symptom
    ;; here: a stale board says a player you just claimed is still free.
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

;; ---- comparison ----

(rf/reg-event-db :compare-toggle
  (fn [db [_ id]]
    ;; Two slots, left then right; a third evicts the *older* rather than being
    ;; refused, so one player is held while the board is clicked through.
    (let [c (vec (:compare db))]
      (assoc db :compare
             (cond
               (some #{id} c)   (vec (remove #{id} c))
               (< (count c) 2)  (conj c id)
               :else            [(second c) id])))))

(rf/reg-event-db :compare-clear
  (fn [db _] (assoc db :compare [])))

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
