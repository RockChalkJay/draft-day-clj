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
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [draft-day.db :as db]
            [draft-day.providers :as providers]
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
   (let [status (str (:count resp) " players · " (:source resp))
         db'    (assoc db
                       :players (:players resp)
                       :universe (:universe resp)
                       :status status
                       :universe-status status)
         ;; The universe carries `:through-week`, so this is the first moment
         ;; the app knows whether the season has started — and so which half of
         ;; the app to open on. Through `:set-view`, which loads that tab's board.
         v      (db/view-for (db/phase db') (:view db'))]
     {:db db'
      :fx (cond-> [[:dispatch [:recompute]]]
            (not= v (:view db')) (conj [:dispatch [:set-view v]]))})))

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
  (fn [{:keys [db]} [_ v section]]
    ;; A second full rank of the universe, so it loads on first open only:
    ;; after that a refresh is a button, not a side effect of navigation.
    ;;
    ;; `section` deep-links into Settings — "Connect a league" means the
    ;; accounts section, not whichever one was open last.
    ;;
    ;; My Team reads the waiver reply for its rows and the matchup's week for
    ;; the header, so opening it — which is where the season half opens — loads
    ;; both.
    (let [fx (cond-> []
               (and (#{:waivers :team} v) (nil? (:waivers db)))
               (conj [:dispatch [:fetch-waivers]])
               (and (#{:matchup :team} v) (nil? (:matchup db)))
               (conj [:dispatch [:fetch-matchup]]))]
      (cond-> {:db (cond-> (assoc db :view v)
                     section (assoc :settings-section section))}
        (seq fx) (assoc :fx fx)))))
;; ---- phase ----

(defn with-phase
  "Store `p` as the phase override for league `k` — or, with `k` nil, for when
  no league is active. `p` is stored exactly: nil is automatic."
  [db k p]
  (if k (assoc-in db [:leagues k :phase] p) (assoc db :phase p)))

(rf/reg-event-fx :set-phase [persist]
  (fn [{:keys [db]} [_ k p]]
    ;; Only a change to the league on screen moves the view, and only off a tab
    ;; the new phase does not have.
    (let [db' (with-phase db k p)
          v   (db/view-for (db/phase db') (:view db'))]
      (cond-> {:db db'}
        (and (= k (:active-league db)) (not= v (:view db')))
        (assoc :fx [[:dispatch [:set-view v]]])))))

(rf/reg-event-fx :switch-mode [persist]
  (fn [{:keys [db]} [_ target]]
    ;; The header's "Go to …" link. Choosing what the data already says stores
    ;; no override, so a manager who went back to the board to fix a pick is
    ;; returned to automatic by coming forward again. It always lands on a tab
    ;; of the target mode — from Settings as well.
    ;;
    ;; The override is written here rather than through `:set-phase`, whose own
    ;; view resolution would queue behind this one and keep Settings on screen.
    (let [override (when (not= target (db/derived-phase db)) target)
          view     (:view db)]
      {:db (with-phase db (:active-league db) override)
       :fx [[:dispatch [:set-view (if (= target (db/view-mode view))
                                    view
                                    (first (db/mode-views target)))]]]})))

(rf/reg-event-db :set-settings-section (fn [db [_ k]] (assoc db :settings-section k)))
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

;; A one-shot rewrite, not a sort mode — there is no `:watch-sort` key in db to
;; consult afterwards, which is what lets `:watchlist-players` promise an order.
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

;; ---- modals ----
;; `:modal` names what is open. A modal with no argument is a bare keyword; one
;; that needs an argument is `{:kind ... :player-id ...}`. `db/modal-kind` is
;; the one reader that knows both shapes, so nothing here has to.

(rf/reg-event-db :show-modal  (fn [db [_ m]] (assoc db :modal m)))
(rf/reg-event-db :close-modal (fn [db _] (assoc db :modal nil)))

(rf/reg-event-db :escape-pressed
  ;; One Escape, one effect, and the modal wins because it is on top of the
  ;; tile. Closing the modal AND clearing the comparison underneath it would
  ;; take away the pair the manager was holding, invisibly, behind the thing he
  ;; just shut.
  ;;
  ;; It is one event rather than a handler per surface because two document-level
  ;; listeners are order-dependent: whichever ran second would see `:modal`
  ;; already nil and clear `:compare` anyway. The precedence has to be a rule in
  ;; one place, which is also what makes it testable.
  ;;
  ;; The comparison is cleared only from the tab it is on. The listener lives in
  ;; `core/app` now and so is attached for the life of the app, where it used to
  ;; unmount with the tile — without this the manager could hold a pair, switch
  ;; to the draft board, press Escape for some unrelated reason and come back to
  ;; find it gone, which is the same silent loss the paragraph above is about.
  ;; The modal is not view-scoped: it opens over whatever is on screen.
  (fn [db _]
    (cond
      (:modal db)         (assoc db :modal nil)
      (and (= :waivers (:view db))
           (seq (:compare db))) (assoc db :compare [])
      :else               db)))

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

(defn league-request
  "The body of an import or a sync: which league, and what authorizes asking.

  The season is the *league entry's* and not this year's. A host that puts the
  season in its URL — ESPN does — would otherwise be asked in January for next
  season's copy of a league still being played, and answer with an empty shell
  rather than an error."
  [db {:keys [provider league-id] :as league}]
  (let [stored (get-in db [:leagues (db/league-key provider league-id)])
        league (merge stored league)]
    {:provider    provider
     :league-id   league-id
     :season      (:season league)
     :credentials (db/credentials-for db league)}))

(rf/reg-event-fx :import-league
  (fn [{:keys [db]} [_ {:keys [provider league-id] :as league}]]
    ;; The key rides along so imported rules land in the league they were
    ;; fetched for — see the ns docstring.
    {:db   (assoc db :status "Importing league…")
     :http {:method :post :url "/api/league/import"
            :body (league-request db league)
            :on-success [:league-import-loaded (db/league-key provider league-id)]
            :on-failure [:league-import-failed (db/league-key provider league-id)]}}))

;; A failed import now arrives at :league-import-failed, because the :http effect
;; routes any non-2xx there; this handler only ever sees a real config.
(rf/reg-event-fx :league-import-loaded [persist]
  (fn [{:keys [db]} [_ k resp]]
    ;; Nils dropped, not merged: a provider that omits a field has no opinion
    ;; about it, and `merge`ing the nil over what the manager already has is
    ;; how a missing `total_rosters`/`settings.size` reaches `db/make-teams` as
    ;; a team count of nothing.
    (let [cfg    (into {} (remove (comp nil? val))
                       (select-keys resp [:scoring :roster :num-teams]))
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
              [:dispatch [:set-import-report (assoc (select-keys resp [:name :season :unsupported-scoring])
                                                   :league-key k)]]
              [:dispatch [:set-status (str "✓ Imported \"" (:name resp) "\" (" (:season resp) ")")]]]
             [])})))

(rf/reg-event-db :set-import-report (fn [db [_ r]] (assoc db :import-report r)))

(defn mark-stale
  "Flag the account a league is read through as needing a reconnect.

  On the account and not on the app, because the flag has to say *which* one:
  a manager with a Sleeper league and an ESPN league whose cookie expired must
  be sent to the ESPN card and left alone on the other."
  [db k stale?]
  (if-let [ak (:account-key (get-in db [:leagues k]))]
    (assoc-in db [:accounts ak :credentials-stale?] stale?)
    db))

(rf/reg-event-db :league-import-failed [persist]
  (fn [db [_ k err status]]
    ;; Both status lines: Settings renders `:waiver-status`, the header
    ;; `:status`, and only one put the failure on the unwatched tab.
    (let [msg (str "League import failed: " err)]
      (cond-> (assoc db :status msg :waiver-status msg)
        (= 401 status) (mark-stale k true)))))

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
                   ;; Who you are playing and what he scored: under another
                   ;; league's name those are false, not stale.
                   :matchup nil
                   :matchup-pick nil
                   :matchup-status nil
                   ;; And any reply still in flight about it: a switch only
                   ;; refetches on the matchup tab, so without the bump the old
                   ;; league's reply still matches and lands under this one.
                   :matchup-seq (inc (:matchup-seq db 0))
                   ;; Goes with the board it was asked about: a free agent in
                   ;; one league is rostered in another.
                   :compare [])
      ;; `:teams` is built from `:num-teams`, `:starting-bankroll` and the
      ;; roster template, all of which just moved. Picks keep theirs — TODO.md.
      (empty? (:picks db))
      (assoc :teams (db/make-teams (:num-teams cfg) (:roster cfg) (:starting-bankroll cfg))))))

(rf/reg-event-fx :set-active-league [persist]
  (fn [{:keys [db]} [_ k]]
    ;; Both boards, because both are priced under this league's rules — a
    ;; switch that moved one would leave Worth priced under the league you left.
    (if-let [entry (get (:leagues db) k)]
      ;; Leagues can be in different phases, so the tab on screen may not exist
      ;; in this one. Resolved here rather than through `:set-view`: both boards
      ;; are already being fetched below.
      (let [db'  (activate db k)
            view (db/view-for (db/phase db') (:view db'))]
        {:db (assoc db' :view view)
         :fx (cond-> [[:dispatch [:recompute]]
                      [:dispatch [:fetch-waivers]]]
               ;; A league added by pasted id has rosters nobody has fetched, and
               ;; a waiver board built on no league says everyone is free.
               (nil? (:sync entry))
               (conj [:dispatch [:sync-league (select-keys entry [:provider :league-id])]])
               ;; Only when it is the tab on screen; every other tab picks it up
               ;; from `:set-view`'s first-open fetch, and this one is live. Not
               ;; before a first sync, which asks for it once there are rosters.
               ;; My Team shows the matchup's week in the header.
               (and (#{:matchup :team} view) (:sync entry))
               (conj [:dispatch [:fetch-matchup]]))})
      {})))

(rf/reg-event-fx :sync-league
  (fn [{:keys [db]} [_ {:keys [provider league-id] :as league}]]
    ;; `db/league-key` calls `name` on the provider, and `(name nil)` throws in
    ;; ClojureScript — killing the event rather than reporting anything.
    (if-not (and provider league-id)
      {:db (assoc db :waiver-status "Nothing to sync — no league is selected.")}
      {:db   (assoc db :waiver-status "Syncing rosters…")
       :http {:method :post :url "/api/league/sync"
              :body (league-request db league)
              :on-success [:league-synced (db/league-key provider league-id)]
              :on-failure [:league-sync-failed (db/league-key provider league-id)]}})))

(defn my-roster-id-for
  "Which roster in this league belongs to `user-id`, or nil.

  Case-folded, which costs a numeric Sleeper id nothing and is the difference
  between finding the manager's own team and not on ESPN: his user id *is* his
  SWID, ESPN publishes it uppercase in a team's `owners` array, and a manager
  who pasted his cookie in lower case would otherwise land on a board with no
  roster, no budget and no drop until he found the dropdown."
  [teams user-id]
  (when (not-empty (str user-id))
    (let [mine (str/lower-case (str user-id))]
      (some (fn [t] (when (= (str/lower-case (str (:owner-id t))) mine)
                      (:roster-id t)))
            teams))))

(rf/reg-event-fx :league-synced [persist]
  (fn [{:keys [db]} [_ k resp]]
    ;; Repaired on the way *in*, not only at boot: a provider that grew or
    ;; dropped a field should fail here, where the status line can say so.
    (let [league   (db/reconcile-league-sync resp)
          entry    (get (:leagues db) k)
          ;; Only when unset: a manager who corrected the dropdown must not
          ;; have that undone by the next re-sync.
          mine     (or (:my-roster-id entry)
                       (my-roster-id-for (:teams league)
                                         (:user-id (db/league-account db entry))))]
      {:db (-> db
               (update-in [:leagues k] merge
                          ;; Client clock: it answers "how old is what I am
                          ;; looking at", which is a question about this browser.
                          (cond-> {:sync league :my-roster-id mine
                                   :synced-at (.toISOString (js/Date.))}
                            ;; Name and season ride along so a league synced
                            ;; by pasted id still reads in the switcher.
                            (:name league)   (assoc :name (:name league))
                            (:season league) (assoc :season (:season league))))
               (assoc :waiver-status (if league
                                       (str "✓ Synced " (count (:teams league)) " rosters")
                                       "Sync returned nothing usable")))
       :fx (cond-> [[:dispatch [:fetch-waivers]]]
             ;; The matchup board asked with no rosters is every team empty.
             (and (#{:matchup :team} (:view db)) (= k (:active-league db)))
             (conj [:dispatch [:fetch-matchup]]))})))

(rf/reg-event-fx :connect-account
  (fn [{:keys [db]} [_ provider credentials]]
    ;; POST, not GET: an ESPN espn_s2 is a live session token and a query
    ;; string reaches browser history, proxy logs and Referer headers.
    {:db   (assoc db :waiver-status (str "Connecting to " (providers/label provider) "…"))
     :http {:method :post :url "/api/account/connect"
            :body {:provider (name provider) :credentials credentials}
            :on-success [:account-connected provider credentials]
            :on-failure [:account-connect-failed provider]}}))

(rf/reg-event-fx :account-connected [persist]
  (fn [{:keys [db]} [_ provider credentials {:keys [user leagues leagues-error]}]]
    ;; Credentials are stored, or this is a login the manager has to repeat on
    ;; every reload. They are the account's, not the app's: every league read
    ;; through it sends these and nothing else does.
    (let [ak  (db/account-key provider (:user-id user))
          db' (-> db
                  (assoc-in [:accounts ak]
                            {:provider    (name provider)
                             :user-id     (:user-id user)
                             :username    (:display-name user)
                             :avatar      (:avatar user)
                             :credentials credentials})
                  ;; Scoped to the account, so connecting a second host does not
                  ;; blow away the first one's list.
                  (update-in [:accounts ak] dissoc :credentials-stale?)
                  (assoc-in [:league-choices ak] (vec leagues))
                  (assoc-in [:league-choices-error ak] leagues-error))]
      (cond
        leagues-error
        {:db (assoc db' :waiver-status
                    (str (providers/label provider)
                         " is connected, but its leagues could not be listed. Paste a league ID."))}

        (empty? leagues)
        {:db (assoc db' :waiver-status
                    (str "That " (providers/label provider)
                         " account plays in no leagues this season."))}

        ;; One league is not a choice. Making the manager pick it out of a list
        ;; of one is a step that asks him to confirm the only possible answer.
        (= 1 (count leagues))
        {:db db' :fx [[:dispatch [:league-choose ak (first leagues)]]]}

        :else
        {:db (assoc db' :waiver-status (str "Pick one of " (count leagues) " leagues."))}))))

(rf/reg-event-db :account-connect-failed [persist]
  (fn [db [_ provider err status]]
    ;; Marked on whatever account this host already has, if any. A first
    ;; connection has no account to flag and the status line is the whole
    ;; report — there is nothing yet to reconnect.
    (cond-> (assoc db :waiver-status
                   (str (providers/label provider) " connection failed: " err))
      (= 401 status)
      (update :accounts
              #(reduce-kv (fn [m ak a]
                            (assoc m ak (cond-> a
                                          (= (name provider) (:provider a))
                                          (assoc :credentials-stale? true))))
                          {} %)))))

(rf/reg-event-fx :disconnect-account [persist]
  (fn [{:keys [db]} [_ ak]]
    ;; The leagues go with it. They are read through this account's credentials
    ;; and nothing else can refresh them, so leaving them behind would leave a
    ;; board naming rosters nobody can re-sync.
    (let [gone   (into #{} (comp (filter (fn [[_ e]] (= ak (:account-key e)))) (map key))
                       (:leagues db))
          db'    (-> db
                     (update :accounts dissoc ak)
                     (update :league-choices dissoc ak)
                     (update :league-choices-error dissoc ak)
                     (update :leagues #(apply dissoc % gone)))
          moved? (contains? gone (:active-league db))
          next-k (when moved? (first (sort (keys (:leagues db')))))]
      (if-not moved?
        {:db db'}
        {:db (cond-> (assoc db' :active-league nil :waivers nil :compare [])
               next-k (activate next-k))
         :fx (if next-k
               [[:dispatch [:recompute]] [:dispatch [:fetch-waivers]]]
               [])}))))

(defn choose-league
  "Store this league under its account, make it active, and go ask both
  questions about it: who is rostered, and what the rules are."
  [db ak provider league league-id]
  (let [k     (db/league-key provider league-id)
        ;; What we knew wins over the seed — a re-chosen league keeps its team
        ;; and rules — but the picker's name and season are freshest.
        entry (merge {:provider provider :league-id league-id
                      :account-key ak :config (:config db)}
                     (get (:leagues db) k)
                     (select-keys league [:name :season])
                     {:account-key ak})
        req   {:provider provider :league-id league-id}]
    ;; Sync is who is rostered, import is the rules; `:recompute` because until
    ;; the import answers this league seeds the previous one's config.
    {:db (-> db (assoc-in [:leagues k] entry) (activate k))
     :fx [[:dispatch [:sync-league req]]
          [:dispatch [:import-league req]]
          [:dispatch [:recompute]]]}))

(rf/reg-event-fx :league-choose [persist]
  (fn [{:keys [db]} [_ ak league]]
    ;; Takes the picker's league map or a bare typed id. The map carries a name
    ;; and season the sync supplies only on reply, and the switcher wants both.
    (let [league    (if (map? league) league {:league-id league})
          league-id (str (:league-id league))
          provider  (or (:provider league) (get-in db [:accounts ak :provider]))]
      (if-not provider
        ;; `db/league-key` calls `name` on the provider, and `(name nil)` throws
        ;; in ClojureScript — killing the event rather than reporting anything.
        {:db (assoc db :waiver-status "Connect an account before adding a league.")}
        (choose-league db ak provider league league-id)))))

(rf/reg-event-db :league-sync-failed [persist]
  (fn [db [_ k err status]]
    ;; A 401 is not an outage, it is an instruction: the host refused the
    ;; credentials and the card must offer a reconnect rather than a retry.
    (cond-> (assoc db :waiver-status (str "League sync failed: " err))
      ;; A matchup on a never-synced league waits for this sync, so it is the
      ;; one place that can say why the board never arrived.
      (= k (:active-league db)) (assoc :matchup-status (str "League sync failed: " err))
      (= 401 status) (mark-stale k true))))

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


(defn- matchup-request
  "The body of an /api/matchup call.

  `:provider` and `:league-id` ride along because unlike the other two boards
  this one takes a *live* fetch server-side, so it needs to know whose
  scoreboard to read. Everything else is the same active-league copy the waiver
  request uses."
  [db]
  (let [lg (db/active-league db)]
    {:provider  (:provider lg)
     :league-id (:league-id lg)
     :scoring   (get-in db [:config :scoring])
     :league    (:sync lg)
     :roster    (get-in db [:config :roster])
     :my-roster-id (:my-roster-id lg)}))

(rf/reg-event-fx :fetch-matchup
  (fn [{:keys [db]} _]
    (let [lg (db/active-league db)]
      (if-not (and (:provider lg) (:league-id lg))
        {:db (assoc db :matchup-status "No league connected — nothing to look up.")}
        ;; Stamped like the other two boards, and the worst race of the three
        ;; to lose: a stale scoreboard nothing on screen contradicts.
        (let [n (inc (:matchup-seq db 0))]
          {:db   (assoc db :matchup-seq n :matchup-status "Loading this week's matchup…")
           :http {:method :post :url "/api/matchup"
                  :body (matchup-request db)
                  :on-success [:matchup-loaded n]
                  :on-failure [:matchup-failed]}})))))

(rf/reg-event-db :matchup-loaded
  (fn [db [_ n resp]]
    (if-not (= n (:matchup-seq db))
      db
      (assoc db :matchup resp :matchup-status nil))))

(rf/reg-event-db :matchup-failed
  (fn [db [_ err]]
    ;; The previous board stays, as `:waivers-failed` does: a scoreboard a few
    ;; minutes old still answers who is winning.
    (assoc db :matchup-status (str "Matchup failed: " err))))

(rf/reg-event-db :set-optimal-basis
  (fn [db [_ basis]] (assoc db :optimal-basis basis)))

(rf/reg-event-db :set-matchup-pick
  ;; No refetch: every team came back in one reply. See `matchup-board`.
  (fn [db [_ roster-id]] (assoc db :matchup-pick roster-id)))

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
