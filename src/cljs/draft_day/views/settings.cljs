(ns draft-day.views.settings
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [re-frame.core :as rf]
            [draft-day.db :as db]
            [draft-day.providers :as providers]
            [draft-day.scoring :as scoring]))

(defn- numeric-field
  "A number input that keeps what you typed until you leave it.

  Binding a controlled input straight to the parsed value cannot hold an
  intermediate `-`, `0.` or empty string — the round-trip through app-db rewrites
  the DOM mid-keystroke, and parsing those yields NaN. A NaN weight used to
  serialize as null and 400 the rankings call, blanking the board. So local state
  holds the raw text, only a successful parse is dispatched, and blur re-syncs the
  field with whatever app-db actually holds.

  What you typed only outranks app-db while app-db still holds what this field
  put there. `sent` is the last value dispatched from here, so a value that moved
  for any other reason — a league import, a reset — shows through immediately
  rather than waiting for the field to lose focus."
  [_label _value _opts]
  (let [raw  (r/atom nil)
        sent (r/atom nil)]
    (fn [label value {:keys [step min-v parse on-change]}]
      [:label.field
       [:span label]
       [:input (cond-> {:type      "number"
                        :step      step
                        :value     (if (and (some? @raw) (= value @sent)) @raw (str value))
                        :on-change (fn [e]
                                     (let [s (.. e -target -value)
                                           v (parse s)]
                                       (reset! raw s)
                                       (when-not (js/isNaN v)
                                         (reset! sent v)
                                         (on-change v))))
                        :on-blur   (fn [_] (reset! raw nil))}
                 min-v (assoc :min min-v))]])))

(defn- num-field [label value on-change]
  [numeric-field label value
   {:step "1" :min-v 0 :parse #(js/parseInt % 10) :on-change on-change}])

(defn- weight-field [label value on-change]
  [numeric-field label value
   {:step "0.01" :parse js/parseFloat :on-change on-change}])

(defn- league-row
  "One league under its account: what it is, whose team is whose, and the two
  buttons that act on it."
  [[k entry] active-key]
  (let [active? (= k active-key)
        teams   (vec (get-in entry [:sync :teams]))
        mine    (:my-roster-id entry)]
    [:div.league-row {:class (when active? "on")}
     [:div.league-row-head
      [:button.league-pick {:class (when active? "on")
                            :disabled active?
                            :on-click #(rf/dispatch [:set-active-league k])}
       (if active? "Active" "Make active")]
      [:b (or (:name entry) (:league-id entry))]
      [:span.muted (str " · " (or (:season entry) "—")
                        (when (seq teams) (str " · " (count teams) " teams")))]]
     [:div.league-row-actions
      (when (seq teams)
        [:label.field.inline
         [:span "My team"]
         [:select {:value (str mine)
                   :on-change (fn [e]
                                (let [v (.. e -target -value)]
                                  ;; The roster id round-trips through the DOM as
                                  ;; a string; the synced league keys on the
                                  ;; number the provider sent, so it has to go
                                  ;; back as one or nothing matches and the board
                                  ;; silently reports no drop and no budget.
                                  (rf/dispatch [:set-my-roster-id
                                                (when-not (str/blank? v)
                                                  (js/parseInt v 10))])))
                   ;; Only the active league's roster id is writable, because
                   ;; `:set-my-roster-id` writes to whichever league is active.
                   ;; A dropdown that silently retargeted another league is
                   ;; worse than one that asks you to switch first.
                   :disabled (not active?)}
          [:option {:value ""} "— pick a team —"]
          (for [t teams]
            ^{:key (:roster-id t)}
            [:option {:value (str (:roster-id t))} (:name t)])]])
      ;; Rosters only, not the rules. `:league-choose` re-imports as well, which
      ;; would silently overwrite a hand-edited scoring config every time the
      ;; manager pressed a button labelled Re-sync.
      [:button {:disabled (not active?)
                :on-click #(rf/dispatch [:sync-league (select-keys entry [:provider :league-id])])}
       "Re-sync"]]]))

(defn- credential-field
  "One field of a provider's connect form, drawn from the catalog.

  A secret is masked until asked for. The toggle is per field rather than per
  form: a manager checking a mistyped SWID has no reason to put his session
  cookie on screen beside it."
  [{:keys [key label secret? placeholder]} draft]
  (let [shown (r/atom false)]
    (fn [{:keys [key label secret? placeholder]} draft]
      [:label.field {:class (when secret? "secret")}
       [:span label]
       [:input {:type (if (and secret? (not @shown)) "password" "text")
                :placeholder placeholder
                :auto-complete "off"
                :spell-check false
                :value (get @draft key "")
                :on-change #(swap! draft assoc key (.. % -target -value))}]
       (when secret?
         [:button.reveal {:type "button"
                          :title (if @shown "Hide" "Show")
                          :on-click #(swap! shown not)}
          (if @shown "hide" "show")])])))

(defn- connect-form
  "The whole of connecting to a host, rendered from `providers/catalog`.

  Nothing here names a provider. A second host is an entry in that catalog and
  two defmethods on the server; it costs no markup, which is the difference
  between supporting one more and supporting them one at a time."
  [picked draft on-done]
  (let [p     @picked
        creds (into {} (filter (comp not-empty str val)) @draft)
        bad   (providers/credential-errors p creds)]
    [:<>
     [:div.provider-tabs
      (for [x (providers/providers)]
        ^{:key x}
        [:button.provider-tab {:class (when (= x p) "on")
                               :on-click #(do (reset! picked x) (reset! draft {}))}
         (providers/label x)])]
     [:div.connect-form
      (for [f (providers/fields p)]
        ^{:key (:key f)} [credential-field f draft])]
     [:div.connect-actions
      [:button.primary {:disabled (some? bad)
                        :on-click #(do (rf/dispatch [:connect-account p creds])
                                       (reset! draft {})
                                       (on-done))}
       "Connect"]
      [:button {:on-click #(do (reset! draft {}) (on-done))} "Cancel"]
      (when-let [{:keys [url]} (providers/help p)]
        [:a.connect-help {:href url :target "_blank" :rel "noreferrer"}
         "Where do I find these?"])]
     (when-let [{:keys [text]} (providers/help p)]
       [:p.connect-note text])
     ;; Only once something has been typed: an empty form is not a mistake.
     (when (and bad (seq creds))
       [:div.connect-error (:error bad)])]))

(defn unadded-choices
  "The leagues this account plays in that are not stored yet.

  They shrink out of this list as they are added rather than doubling with the
  rows below, which is what the two disconnected lists used to do."
  [provider choices leagues]
  (let [stored (into #{} (map first) leagues)]
    (remove #(stored (db/league-key provider (:league-id %))) choices)))

(defn- add-by-id
  "The paste-a-league-ID row. Per account, never global: an ESPN league needs
  *this* account's cookie to sync at all, so an id with no account beside it
  would name a league nothing could fetch."
  [ak provider typed]
  (let [v   (get @typed ak "")
        bad (providers/league-id-error provider v)]
    [:div.row.league-id-row
     [:input {:type "text" :placeholder (providers/league-id-label provider)
              :value v
              :on-change #(swap! typed assoc ak (.. % -target -value))}]
     [:button {:disabled (some? bad)
               :on-click #(do (rf/dispatch [:league-choose ak (str/trim v)])
                              (swap! typed dissoc ak))}
      "Add league"]
     [:span.muted "For a league this account is not in."]]))

(defn- account-group
  "One account and everything read through it: its stored leagues, the ones it
  plays in that are not stored yet, and the row for an id it does not list."
  [[ak acct leagues] {:keys [active-key reconnect! typed-id]}]
  (let [provider (:provider acct)
        {:keys [choices error]} (when ak @(rf/subscribe [:account-choices ak]))]
    [:section.account-group {:class (cond (nil? ak) "no-account"
                                          (:credentials-stale? acct) "stale")}
     [:div.account-head
      [:span.account-provider (if ak (providers/label provider) "No account")]
      ;; Omitted rather than defaulted to the provider's name: ESPN publishes no
      ;; display name, and "ESPN · ESPN" says the label twice. The SWID is never
      ;; a fallback — it is half the credential pair.
      (when-let [nm (and ak (not-empty (:username acct)))]
        [:b.account-name nm])
      (when (:credentials-stale? acct)
        [:span.account-stale "Session expired — reconnect"])
      (when ak
        [:div.account-actions
         (when (:credentials-stale? acct)
           [:button.link {:on-click #(reconnect! provider)} "Reconnect"])
         [:button.link {:on-click #(rf/dispatch [:disconnect-account ak])} "Disconnect"]])]

     (when error
       [:div.discovery-warn
        [:b (str "Couldn't list this account's leagues.")]
        [:div.muted
         (str (providers/label provider) " publishes no supported way to do it, so this "
              "can stop working without notice. Paste a league ID instead.")]])

     (if (seq leagues)
       [:div.league-rows
        (for [[k _ :as row] leagues] ^{:key k} [league-row row active-key])]
       (when-not (seq choices)
         [:p.muted "No leagues from this account yet."]))

     (when-let [unadded (seq (unadded-choices provider choices leagues))]
       [:div.league-choices
        (for [{:keys [league-id name num-teams status] :as choice} unadded]
          ^{:key league-id}
          [:button.league-choice {:on-click #(rf/dispatch [:league-choose ak choice])}
           [:span.league-choice-name name]
           [:span.muted (str/join " · " (cons "" (remove nil? [(when num-teams (str num-teams "-team"))
                                                               status])))]
           [:span.league-choice-add "Add"]])])

     (when (and ak (= [] choices) (empty? leagues) (not error))
       [:div.sync-empty "That account plays in no leagues this season."])

     (when ak [add-by-id ak provider typed-id])]))

(defn connected-accounts
  "The one place an account, a league and a team are set.

  Everything that identifies the manager lives here and every other view reads
  it: the header switcher and the Waivers strip only display. The connect form
  folds away once there is an account, because connecting is rare and this card
  is read far more often than it is used."
  []
  (let [picked   (r/atom (first (providers/providers)))
        draft    (r/atom {})
        typed-id (r/atom {})
        adding?  (r/atom false)]
    (fn []
      (let [groups     @(rf/subscribe [:leagues-by-account])
            active-key @(rf/subscribe [:active-league-key])
            status     @(rf/subscribe [:waiver-status])
            any?       (seq @(rf/subscribe [:accounts]))
            reconnect! (fn [p] (reset! picked (keyword p)) (reset! draft {}) (reset! adding? true))
            open?      (or @adding? (not any?))]
        [:section.settings-card.accounts
         [:h3 "Accounts"]
         [:p.muted "Connect a fantasy account to pull its leagues. Everything on the
                    board — scoring, rosters, waivers — follows whichever league is
                    active. Credentials are kept in this browser and sent only to
                    read your leagues."]
         (if open?
           [connect-form picked draft #(reset! adding? false)]
           [:button.link {:on-click #(reset! adding? true)} "+ Add account"])

         (when (seq groups)
           [:div.account-groups
            (for [[ak acct leagues] groups]
              ^{:key (or ak "orphans")}
              [account-group [ak acct leagues]
               {:active-key active-key :reconnect! reconnect! :typed-id typed-id}])])
         (when (and (empty? groups) any?)
           [:p.muted "No leagues yet — pick one above, or paste a league ID."])
         (when status [:div.sync-status status])]))))

(defn- league-config []
  (let [cfg @(rf/subscribe [:config])]
    [:section.settings-card
     [:h3 "League"]
     [:div.fields
      [num-field "Teams" (:num-teams cfg) #(rf/dispatch [:apply-config {:num-teams %}])]
      [num-field "Budget $" (:starting-bankroll cfg) #(rf/dispatch [:apply-config {:starting-bankroll %}])]]]))

(defn- budget-config []
  (let [cfg      @(rf/subscribe [:config])
        plan     (:budget-plan cfg)
        bankroll (:starting-bankroll cfg)
        total    (reduce + (map (fn [[_ k]] (get plan k 0)) db/budget-order))
        over?    (> total bankroll)]
    [:section.settings-card
     [:h3 "Budget Plan"]
     [:p.muted (str "Split your $" bankroll " across positions. "
                    "My Roster tracks your spend against it live.")]
     [:div.fields
      (map (fn [[label k]]
             ^{:key k}
             [num-field label (get plan k 0) #(rf/dispatch [:set-position-budget k %])])
           db/budget-order)]
     [:div.budget-tally {:class (when over? "over")}
      "Allocated " [:b (str "$" total)] (str " of $" bankroll)
      (cond
        over?              (str " · $" (- total bankroll) " over budget")
        (< total bankroll) (str " · $" (- bankroll total) " unallocated"))]]))

(def ^:private unprojected-note
  "Sleeper publishes no projection for this stat, so a weight here cannot change any player's points.")

(defn- unprojected-field
  "Shows the weight — an imported league may well set it — but will not pretend
  it is editable, because nothing in the projections can respond to it."
  [label value]
  [:label.field.unprojected {:title unprojected-note}
   [:span label [:i.not-projected "not projected"]]
   [:input {:type "number" :value (str value) :disabled true :read-only true}]])

(defn- custom-scoring-editor [weights]
  [:div.scoring-groups
   (map (fn [{:keys [group stats]}]
          ^{:key group}
          [:div.scoring-group
           [:h4 group]
           [:div.fields
            ;; The key rides on each branch's vector, not on the `if` — metadata
            ;; on a special form is dropped at compile time, which left every row
            ;; keyless and reconciled by index.
            (map (fn [[stat-key label]]
                   (if (contains? scoring/unprojected-stats stat-key)
                     ^{:key stat-key} [unprojected-field label (get weights stat-key 0)]
                     ^{:key stat-key} [weight-field label (get weights stat-key 0)
                                       #(rf/dispatch [:set-scoring-weight stat-key %])]))
                 stats)]])
        db/scoring-catalog)])

(defn- import-warning
  "What the last league import could not apply. An import that quietly drops most
  of a league's rules while reporting success is the failure this exists to
  prevent."
  []
  (let [{:keys [unsupported-scoring]} @(rf/subscribe [:import-report])]
    (when (seq unsupported-scoring)
      [:div.scoring-warning
       [:b (str (count unsupported-scoring) " scoring rules were not applied.")]
       [:p.muted "Draft Day scores a flat stat line, so these are not modelled and
                  your board will differ from your league where they matter:"]
       [:code (str/join ", " unsupported-scoring)]])))

(def vendor-gap-copy
  "What each missing FantasyPros half is called, and what the board actually
  loses without it. The two are not interchangeable: ECR sets tiers and the rank
  spread the Floor/Ceiling band is built from, while AAV is the market price. A
  notice that blames prices when the *ranks* failed points the manager at the
  one number still worth trusting."
  {:fantasypros/ecr {:what "expert ranks"
                     :cost "tiers and the Floor/Ceiling band fall back to projections alone"}
   :fantasypros/aav {:what "auction values"
                     :cost "market prices lean on ESPN alone"}})

(defn vendor-gap-message
  "Headline + consequence for the FantasyPros halves missing at this league's
  format, or nil when there are none. Pure, so what the notice claims is
  testable without rendering it."
  [gaps]
  (when-let [entries (seq (keep vendor-gap-copy gaps))]
    {:headline (str "FantasyPros " (str/join " and " (map :what entries))
                    " are missing for your scoring format.")
     :detail   (str "The board still values every player, but "
                    (str/join ", and " (map :cost entries))
                    " until the next cache refresh.")}))

(defn vendor-gap-warning
  "What FantasyPros did not publish *for this league's format*. Each scrape is
  independently best-effort and the cache is served for a day, so a standard
  league can silently be pricing off ESPN alone while a PPR league in the same
  universe sees the full consensus."
  []
  (when-let [{:keys [headline detail]} (vendor-gap-message @(rf/subscribe [:vendor-gaps]))]
    [:div.scoring-warning
     [:b headline]
     [:p.muted detail]]))

(defn- scoring-config []
  (let [cfg    @(rf/subscribe [:config])
        mode   @(rf/subscribe [:scoring-mode])
        mode-s (name mode)]
    [:section.settings-card
     [:h3 "Scoring"]
     [:label.field
      [:span "Preset"]
      [:select {:value mode-s
                :on-change #(let [v (.. % -target -value)]
                              (if (= v "custom")
                                (rf/dispatch [:enable-custom-scoring])
                                (rf/dispatch [:select-scoring-preset (keyword v)])))}
       [:option {:value "standard"} "Standard"]
       [:option {:value "half-ppr"} "Half PPR"]
       [:option {:value "ppr"} "PPR"]
       [:option {:value "custom"} "Custom"]]]
     [import-warning]
     [vendor-gap-warning]
     (when (= mode :custom)
       [custom-scoring-editor (:scoring cfg)])]))

(defn- roster-config []
  (let [cfg    @(rf/subscribe [:config])
        roster (:roster cfg)
        set-r  (fn [k v] (rf/dispatch [:apply-config {:roster (assoc roster k v)}]))]
    [:section.settings-card
     [:h3 "Roster"]
     [:div.fields
      (map (fn [[k label]]
             ^{:key k}
             [num-field label (get roster k 0) #(set-r k %)])
           [[:qb "QB"] [:rb "RB"] [:wr "WR"] [:te "TE"]
            [:flex "FLEX"] [:k "K"] [:dst "DST"] [:bench "Bench"]])]]))

(defn draft-archive
  "Completed drafts, kept because the board destroys one every time a new draft
  starts and a season's prices are worth more than the state that produced them.

  Read-only. Restoring a draft into the live board is deliberately not offered:
  it would overwrite whatever draft is running with one priced under a different
  config, and there is no sensible answer to what should happen to the league
  synced in between."
  []
  (let [drafts (reverse @(rf/subscribe [:drafts]))
        live?  @(rf/subscribe [:draft-has-picks?])]
    [:section.settings-card.draft-archive
     [:h3 "Draft Archive"]
     [:p.muted "Completed drafts are archived automatically when you start a new one. 
                They are kept separately from the rest of your saved state, so an app 
                update cannot discard them."]
     (if (seq drafts)
       [:ul.archive-list
        (for [{:keys [archived-at season league picks teams config]} drafts]
          ^{:key archived-at}
          [:li.archive-entry
           [:div.archive-head
            [:b (or league "Draft")]
            (when season [:span.muted (str " \u00b7 " season)])]
           [:div.muted
            (str (count picks) " picks \u00b7 " (count teams) " teams \u00b7 $"
                 (:starting-bankroll config) " each \u00b7 "
                 (subs (str archived-at) 0 10))]])]
       [:p.muted "No drafts archived yet."])
     (when live?
       [:div.row
        [:button {:on-click #(rf/dispatch [:archive-draft])}
         "Archive current draft"]
        [:span.muted "Keeps a copy without starting a new draft."]])]))

(defn- danger-zone []
  [:section.settings-card.danger-zone
   [:h3 "Danger Zone"]
   [:p.muted "Force the server to drop its cached player data and re-fetch live prices from Sleeper, FantasyPros and ESPN. Does not affect your draft or league settings."]
   [:button.danger {:on-click #(rf/dispatch [:show-modal {:kind :reset-cache}])} "Reset Player Cache"]])

(defn settings []
  [:div.settings
   [connected-accounts]
   [draft-archive]
   [league-config]
   [budget-config]
   [scoring-config]
   [roster-config]
   [danger-zone]])
