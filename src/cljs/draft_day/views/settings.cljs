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
  "One league under its account: what it is, whose team is whose, and the
  button that refreshes it."
  [[k entry] active-key]
  (let [active? (= k active-key)
        teams   (vec (get-in entry [:sync :teams]))
        mine    (:my-roster-id entry)]
    [:div.league-row {:class (when active? "on")}
     [:button.league-pick {:class (when active? "on")
                           :disabled active?
                           :on-click #(rf/dispatch [:set-active-league k])}
      (if active? "Active" "Make active")]
     [:div.league-who
      [:b (or (:name entry) (:league-id entry))]
      [:span.muted (str (or (:season entry) "—")
                        (when (seq teams) (str " · " (count teams) " teams")))
       ;; On the row, not only on the status line: Re-sync runs the sync and the
       ;; import together, and a sync that answers second overwrites the line.
       (when (= :failed (get-in entry [:rules :status]))
         [:span.warn {:title (get-in entry [:rules :error])} " · settings not imported"])]]
     (if (seq teams)
       [:select {:value (str mine)
                 :aria-label "My team"
                 :title "Which team is yours"
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
                 :disabled (not active?)}
        [:option {:value ""} "— pick a team —"]
        (map (fn [t]
               ^{:key (:roster-id t)}
               [:option {:value (str (:roster-id t))} (:name t)])
             teams)]
       [:span])
     ;; Writable for any league, unlike the team picker: it is a fact about
     ;; that league, and `:set-phase` names the league it writes to.
     [:select {:value     (if-let [p (:phase entry)] (name p) "auto")
               :aria-label "Phase"
               :title     "Which half of the app this league opens in. Auto follows the season: once a week is played or the draft is done, it opens in season."
               :on-change #(let [v (.. % -target -value)]
                             (rf/dispatch [:set-phase k (when (not= v "auto") (keyword v))]))}
      [:option {:value "auto"} "Auto"]
      [:option {:value "draft"} "Draft"]
      [:option {:value "season"} "Season"]]
     [:button.plain {:disabled (not active?)
                     :on-click #(rf/dispatch [:refresh-league (select-keys entry [:provider :league-id])])}
      "Re-sync"]]))

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
      (map (fn [x]
             ^{:key x}
             [:button.provider-tab {:class (when (= x p) "on")
                                    :on-click #(do (reset! picked x) (reset! draft {}))}
              (providers/label x)])
           (providers/providers))]
     [:div.connect-form
      (map (fn [f] ^{:key (:key f)} [credential-field f draft])
           (providers/fields p))]
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
     [:button.plain {:disabled (some? bad)
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
        (map (fn [[k _ :as row]] ^{:key k} [league-row row active-key]) leagues)]
       (when-not (seq choices)
         [:p.muted "No leagues from this account yet."]))

     (when-let [unadded (seq (unadded-choices provider choices leagues))]
       [:div.league-choices
        (map (fn [{:keys [league-id name num-teams status] :as choice}]
               ^{:key league-id}
               [:button.league-choice {:on-click #(rf/dispatch [:league-choose ak choice])}
                [:span.league-choice-name name]
                [:span.muted (str/join " · " (cons "" (remove nil? [(when num-teams (str num-teams "-team"))
                                                                    status])))]
                [:span.league-choice-add "Add"]])
             unadded)])

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
         [:div.card-head
          [:h3 "Accounts"]
          (when-not open?
            [:button.link {:on-click #(reset! adding? true)} "+ Add account"])]
         (when open?
           [connect-form picked draft #(reset! adding? false)])

         (when (seq groups)
           [:div.account-groups
            (map (fn [[ak acct leagues]]
                   ^{:key (or ak "orphans")}
                   [account-group [ak acct leagues]
                    {:active-key active-key :reconnect! reconnect! :typed-id typed-id}])
                 groups)])
         (when (and (empty? groups) any?)
           [:p.muted "No leagues yet — pick one above, or paste a league ID."])
         (when status [:div.sync-status status])]))))

(defn- fixed-field
  "A value the active league sets: shown, never edited — `db/league-owned-keys`."
  [label value]
  [:label.field
   [:span label]
   [:input {:type "number" :value (str value) :disabled true :read-only true}]])

(defn- league-config []
  (let [cfg   @(rf/subscribe [:config])
        owned @(rf/subscribe [:league-owned-keys])
        rules @(rf/subscribe [:active-league-rules])
        field (fn [k label]
                (if (owned k)
                  [fixed-field label (get cfg k)]
                  [num-field label (get cfg k) #(rf/dispatch [:edit-config {k %}])]))]
    [:section.settings-card
     [:h3 "League"]
     [:div.fields
      (field :num-teams "Teams")
      (field :starting-bankroll "Budget $")]
     ;; Worded for either reason the import brought none: a snake draft has no
     ;; budget, and Sleeper's draft fetch is best-effort.
     (when (and (= :imported (:status rules)) (not (:bankroll? rules)))
       [:p.muted.field-note "Your league's import didn't include an auction budget, so this one is yours to set."])]))

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
  "Neither of Sleeper's projection horizons carries this stat, so a weight here cannot change any player's points.")

(defn- unprojected-field
  "Shows the weight — an imported league may well set it — but will not pretend
  it is editable, because nothing in the projections can respond to it."
  [label value]
  [:label.field.unprojected {:title unprojected-note}
   [:span label [:i.not-projected "not projected"]]
   [:input {:type "number" :value (str value) :disabled true :read-only true}]])

(defn- scoring-fields
  "One group's fields. The key rides on each branch's vector, not on the `cond`
  — metadata on a special form is dropped at compile time, which left every row
  keyless and reconciled by index."
  [weights read-only? stats]
  [:div.fields
   (map (fn [[stat-key label]]
          (cond
            (contains? scoring/unprojected-stats stat-key)
            ^{:key stat-key} [unprojected-field label (get weights stat-key 0)]
            read-only?
            ^{:key stat-key} [fixed-field label (get weights stat-key 0)]
            :else
            ^{:key stat-key} [weight-field label (get weights stat-key 0)
                              #(rf/dispatch [:set-scoring-weight stat-key %])]))
        stats)])

(defn- priced-here?
  "Does the league put a weight on anything in `stats`? What decides whether a
  disclosure opens on arrival: a manager whose league really does score the
  points-allowed tiers should find them open rather than go looking.

  `usable-weight` and not a bare `zero?` — a cleared input box sends NaN, which
  `JSON.stringify` writes as null, and reading either as a priced rule would
  pop a disclosure open mid-keystroke. Same hazard that function exists for."
  [weights stats]
  (boolean (some (fn [[k _]] (not (zero? (scoring/usable-weight (get weights k 0)))))
                 stats)))

(defn- disclosed-fields
  "One group's `:more` weights behind a disclosure.

  `:open` is seeded once per group rather than recomputed, because it is a
  *controlled* attribute: read from the live weights it would flip back to false
  the moment a manager cleared a rule he had just typed, closing the section
  under his cursor. Opening on arrival is a mount-time question, so `with-let`
  answers it once and the DOM owns the toggle from then on."
  [weights read-only? label stats]
  (r/with-let [open? (priced-here? weights stats)]
    [:details.scoring-more {:open open?}
     [:summary label]
     [scoring-fields weights read-only? stats]]))

(defn- custom-scoring-editor
  "The weight grid. `:stats` is always drawn; `:more` sits behind a disclosure,
  because eighty-eight fields in one grid is a worse editor than twenty-nine
  was and every rule in `:more` is zero in a league that has not imported one."
  [weights read-only?]
  [:div.scoring-groups
   (map (fn [{:keys [group stats more]}]
          ^{:key group}
          [:div.scoring-group
           [:h4 group]
           (when (seq stats) [scoring-fields weights read-only? stats])
           (when (seq more)
             [disclosed-fields weights read-only?
              (if (seq stats) "More rules" group) more])])
        db/scoring-catalog)])

(defn- chips [rules]
  ;; Chips rather than one comma-joined run: rule keys have no spaces to break
  ;; on, and a single long token pushed straight out of the card.
  (into [:div.rule-chips]
        (map (fn [rule] ^{:key rule} [:span.rule-chip rule]))
        rules))

(defn import-warning
  "What the last import could not apply, and what it applied but cannot project.

  Two states and not one, because collapsing them is what made this confusing:
  a rule with no key at all scores nothing anywhere, while a rule the model
  holds but no projection carries still scores the weeks that have happened. The
  old copy called both 'not applied' and blamed the flat stat line for it, which
  was never the reason — Sleeper states a tier as a stat, so the model always
  could hold one and simply had no key for it."
  []
  (let [{:keys [unsupported]} @(rf/subscribe [:active-league-rules])
        weights   (scoring/resolve-config @(rf/subscribe [:active-league-scoring]))
        unprojected (sort (keep (fn [[k w]]
                                  (when (and (contains? scoring/unprojected-stats k)
                                             (not (zero? (scoring/usable-weight w))))
                                    (name k)))
                                weights))]
    (when (or (seq unsupported) (seq unprojected))
      [:div.scoring-warning
       (when (seq unsupported)
         [:div
          [:b (str (count unsupported) " scoring rules are not modelled.")]
          [:p.muted "These price one stat differently depending on who earned it,
                     which one weight cannot express, so your board will differ
                     from your league where they matter:"]
          [chips unsupported]])
       (when (seq unprojected)
         [:div
          [:b (str (count unprojected) " rules have no projection.")]
          [:p.muted "Your league scores these and so does Draft Day, but nobody
                     projects them — they move your in-season numbers once the
                     games are played, and cannot move the draft board:"]
          [chips unprojected]])])))

(defn league-source
  "Where a connected league's settings came from, and whether they came at all.

  Heads every section that shows them. They are read-only, so a manager who
  cannot change a number has to be able to tell whose it is — and when the
  import failed, that the board is still on the settings it had before, which
  for a newly added league are another league's."
  []
  (let [league     @(rf/subscribe [:active-league])
        importing? @(rf/subscribe [:active-league-importing?])
        {:keys [status error unsupported]} @(rf/subscribe [:active-league-rules])
        named      [:b (or (:name league) (:league-id league))]]
    (cond
      (nil? league) nil

      importing?
      [:p.league-source.muted "Importing " named "'s settings…"]

      (= :imported status)
      [:p.league-source.muted "From " named
       (when-let [season (:season league)] (str " (" season ")"))
       ". Re-sync the league to refresh them."]

      :else
      [:div.scoring-warning
       [:b (if (= :failed status)
             (str "Couldn't import this league's settings: " error)
             "This league's settings haven't been imported yet.")]
       ;; An earlier good import leaves its `:unsupported` list behind, and the
       ;; board is still on *those* settings — this league's, only older.
       [:p.muted (if (some? unsupported)
                   "The board is still using the last imported settings."
                   "The board is still using the previous settings, which may be another league's.")]
       [:button.plain {:on-click #(rf/dispatch [:import-league (select-keys league [:provider :league-id])])}
        "Retry import"]])))

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

(defn- league-scoring
  "A connected league's scoring, as imported. Shown, never edited: the league
  sets its rules, and Re-sync is how they are refreshed."
  [cfg]
  (let [s (:scoring cfg)]
    [:section.settings-card
     [import-warning]
     [vendor-gap-warning]
     [custom-scoring-editor (if (map? s) s (scoring/resolve-config s)) true]]))

(defn- scoring-config []
  (let [cfg    @(rf/subscribe [:config])
        mode   @(rf/subscribe [:scoring-mode])
        mode-s (name mode)]
    (if (contains? @(rf/subscribe [:league-owned-keys]) :scoring)
      [league-scoring cfg]
      [:section.settings-card
       [:label.field.preset
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
       [vendor-gap-warning]
       (when (= mode :custom)
         [custom-scoring-editor (:scoring cfg) false])])))

(defn- roster-config []
  (let [cfg    @(rf/subscribe [:config])
        roster (:roster cfg)
        fixed? (contains? @(rf/subscribe [:league-owned-keys]) :roster)
        set-r  (fn [k v] (rf/dispatch [:edit-config {:roster (assoc roster k v)}]))]
    [:section.settings-card
     [:h3 "Roster"]
     [:div.fields
      ;; The key rides on each branch — see `custom-scoring-editor`.
      (map (fn [[k label]]
             (if fixed?
               ^{:key k} [fixed-field label (get roster k 0)]
               ^{:key k} [num-field label (get roster k 0) #(set-r k %)]))
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
        (map (fn [{:keys [archived-at season league picks teams config]}]
               ^{:key archived-at}
               [:li.archive-entry
                [:div.archive-head
                 [:b (or league "Draft")]
                 (when season [:span.muted (str " \u00b7 " season)])]
                [:div.muted
                 (str (count picks) " picks \u00b7 " (count teams) " teams \u00b7 $"
                      (:starting-bankroll config) " each \u00b7 "
                      (subs (str archived-at) 0 10))]])
             drafts)]
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

(def section-ledes
  "The sentence under each section's heading, where one earns its place."
  {:leagues "Connect a fantasy account to pull its leagues. Everything on the board — scoring, rosters, waivers — follows whichever league is active. Credentials are kept in this browser and sent only to read your leagues."
   :scoring "How the active league scores. A connected league's rules come from its import and are read-only."
   :roster  "A connected league's team count, roster and auction budget come from its import and are read-only."
   :draft   "Your auction budget plan, and the drafts already done."})

(defn section-body [k]
  (case k
    :leagues [connected-accounts]
    :scoring [:<> [league-source] [scoring-config]]
    :roster  [:<> [league-source] [league-config] [roster-config]]
    :draft   [:<> [budget-config] [draft-archive]]
    :data    [danger-zone]
    [connected-accounts]))

(defn settings-nav
  "The section list. Each entry carries what is waiting inside it, so an expired
  session or a dropped scoring rule is visible without opening its section."
  []
  (let [on     @(rf/subscribe [:settings-section])
        alerts @(rf/subscribe [:settings-alerts])]
    [:nav.settings-nav
     (map (fn [[k label]]
            ^{:key k}
            [:button {:class (when (= k on) "on")
                      :on-click #(rf/dispatch [:set-settings-section k])}
             label
             (let [missing [:span.nav-dot {:title "This league's settings were not imported"}]]
               (case k
                 :leagues (when (:leagues alerts)
                            [:span.nav-dot {:title "An account needs reconnecting"}])
                 ;; Missing outranks the count: it asks for a Retry, and the count
                 ;; asks for nothing the manager can do.
                 :scoring (cond
                            (:rules-missing? alerts) missing
                            (pos? (:scoring alerts))
                            [:span.nav-badge {:title "Scoring rules this board could not apply"}
                             (:scoring alerts)])
                 :roster  (when (:rules-missing? alerts) missing)
                 nil))])
          db/settings-sections)]))

(defn settings
  "Every section stays mounted and only the chosen one is shown. Unmounting the
  rest threw away whatever was typed into them — a half-pasted ESPN cookie gone
  because the manager glanced at Scoring. Hidden sections are `display: none`,
  so they are still not on the page for a card to be stretched against."
  []
  (let [on (or @(rf/subscribe [:settings-section]) :leagues)]
    [:div.settings
     [settings-nav]
     (map (fn [[k label]]
            ^{:key k}
            [:div.settings-section {:hidden (not= k on)}
             [:h2 label]
             (when-let [lede (section-ledes k)] [:p.lede lede])
             [section-body k]])
          db/settings-sections)]))
