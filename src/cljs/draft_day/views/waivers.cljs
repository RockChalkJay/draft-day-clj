(ns draft-day.views.waivers
  "The in-season board: who is free in your league, how much better he is than
  the man you would drop, and what to bid.

  Nothing here is priced in auction dollars. The draft board's Worth and Value
  divide a bankroll among a whole roster on one night; a waiver claim is a single
  seat against a budget spent down over months. See `rankings.waiver` for why
  those two numbers cannot be the same number.

  The header states the week out loud, and says so most loudly when there is no
  week — in preseason the rest-of-season board *is* the draft board, which is the
  honest answer but the one a manager is most likely to misread as live."
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [re-frame.core :as rf]
            [draft-day.db :as db]
            [draft-day.views.board :as board]
            [draft-day.views.columns :as columns]
            [draft-day.views.util :as util]))

;; ---- cells ----

(defn trend-class
  "Rising, falling, or neither. The band around 1.0 is deliberately wide: a
  receiver at 1.04 has not earned an arrow, and colouring noise is how a column
  stops being read at all."
  [t]
  (cond (nil? t) nil
        (>= t 1.25) "trend-up"
        (<= t 0.75) "trend-down"
        :else nil))

(defn format-trend [t]
  (if (number? t) (str (.toFixed t 2) "×") "–"))

(defn cell [k p]
  (case k
    :rank      [:td.num.muted (:rank p)]
    :name      [:td.player
                [:span.p-name (:player-name p)]
                (when-let [st (:sleeper/injury-status p)]
                  (when (db/serious-injury? st)
                    [:span.inj-flag {:title st} " ⚠"]))]
    :team      [:td (or (:team p) "–")]
    :position  [:td (util/pos-label p)]
    :bye       [:td.num (or (:bye p) "–")]
    :ros       [:td.num (board/format-whole (:ros-points p))]
    ;; The headline. Signed, because a free agent worse than the man you would
    ;; drop is not an add — and flattening that to zero would make the whole
    ;; tail of the pool look equally plausible.
    ;;
    ;; Rounded ONCE, and both the colour and the digits read from that. Colouring
    ;; the raw value and printing the rounded one put a green dash on the board
    ;; for an upgrade of 0.4 — `sign-class` saw a positive number while `signed`
    ;; dashed out the zero. Same rule `controls/val-cell` states: the colour and
    ;; the digits have to come from the same value.
    :upgrade   (let [n (js/Math.round (or (:upgrade p) 0))]
                 [:td.num {:class (util/sign-class n)} (util/signed n)])
    ;; A nil bid and a $0 bid are different answers and must not render the
    ;; same. nil is "this league does not bid"; $0 is a legal FAAB bid that says
    ;; he is worth the minimum.
    :bid       [:td.num (if (number? (:bid p)) (str "$" (:bid p)) "–")]
    :trend     [:td.num {:class (trend-class (:trend p))
                         :title (when (:trend p)
                                  "Recent opportunity per game against his season rate")}
                (format-trend (:trend p))]
    :gp        [:td.num.muted (or (get-in p [:nflverse/season-to-date :games]) "–")]
    :tgt       [:td.num.muted (board/format-whole
                               (get-in p [:nflverse/season-to-date :usage :targets]))]
    :car       [:td.num.muted (board/format-whole
                               (get-in p [:nflverse/season-to-date :usage :carries]))]
    :ros-vorp  [:td.num (board/format-whole (:ros-vorp p))]
    :preseason [:td.num.muted (board/format-whole (:points p))]
    :ecr       [:td.num.muted (or (:fantasypros/ecr p) "–")]
    :risk      (let [lvl (:injury-risk p)
                     txt (or (:injury/reason p) "No injury history to judge")]
                 [:td.risk {:title txt :aria-label txt}
                  (if lvl [board/risk-bar lvl] [:span.muted "–"])])
    :inj       (let [st (:sleeper/injury-status p)]
                 [:td {:class (when (db/serious-injury? st) "inj-serious")}
                  (or st "–")])
    [:td "–"]))

;; ---- header ----

(defn header-cell [col sort]
  (let [k       (:key col)
        d       (db/waiver-columns-by-key k)
        active? (= (:key sort) k)]
    [:th {:on-click #(rf/dispatch [:set-waiver-sort k])
          :title    (:tooltip d)
          :class    (when active? "sorted")}
     (:label d)
     [:span.sort-ind (cond (not active?) " ↕" (= -1 (:dir sort)) " ▼" :else " ▲")]]))

;; ---- panels ----

(defn sync-panel
  "Connect a league, and say plainly when none is connected.

  Without a synced league the board still ranks every player by rest-of-season
  value, which is useful — but it is not a *waiver* board, because it cannot know
  who is already taken. That distinction is the whole reason this panel is loud
  rather than a settings field."
  []
  ;; `nil` rather than "" so the field can tell 'never typed in' from 'cleared',
  ;; and fall back to the id the last sync came back with. It lives in a
  ;; component-local atom, which empties on every mount — so without that
  ;; fallback a manager returning next session reads persisted, month-old
  ;; rosters with the re-sync button greyed out and no record of which league
  ;; they came from.
  (let [typed (r/atom nil)]
    (fn []
      (let [synced? @(rf/subscribe [:league-synced?])
            teams   @(rf/subscribe [:sync-teams])
            mine    @(rf/subscribe [:my-roster-id])
            status  @(rf/subscribe [:waiver-status])
            known   @(rf/subscribe [:synced-league-id])
            league-id (or @typed known "")]
        [:div.sync-panel
         [:div.sync-row
          [:input {:type "text" :placeholder "Sleeper league ID"
                   :value league-id
                   :on-change #(reset! typed (.. % -target -value))}]
          [:button {:disabled (str/blank? league-id)
                    :on-click #(rf/dispatch [:sync-league {:provider "sleeper"
                                                           :league-id league-id}])}
           (if synced? "Re-sync rosters" "Sync rosters")]
          (when synced?
            [:button.secondary {:on-click #(rf/dispatch [:fetch-waivers])} "Refresh board"])]
         (when status [:div.sync-status status])
         (if synced?
           [:div.sync-team
            [:label "My team "
             [:select {:value (str mine)
                       :on-change (fn [e]
                                    (let [v (.. e -target -value)]
                                      ;; The roster id round-trips through the
                                      ;; DOM as a string; the synced league keys
                                      ;; on the number the provider sent, so it
                                      ;; has to go back as one or nothing
                                      ;; matches and the board silently reports
                                      ;; no drop and no budget.
                                      (rf/dispatch [:set-my-roster-id
                                                    (when-not (str/blank? v)
                                                      (js/parseInt v 10))])))}
              [:option {:value ""} "— pick a team —"]
              (for [t teams]
                ^{:key (:roster-id t)}
                [:option {:value (str (:roster-id t))} (:name t)])]]]
           [:div.sync-empty
            "No league connected — this is a rest-of-season ranking of "
            [:em "everyone"] ", not of who is actually free. "
            "Sync a Sleeper league to see the waiver wire."])]))))

(defn faab-panel []
  (let [{:keys [type budget left rival-max]} @(rf/subscribe [:my-faab])
        {:keys [claims-left]} @(rf/subscribe [:waiver-meta])]
    (when type
      [:div.faab-panel
       (if (= "faab" (name type))
         [:<>
          [:div.stat [:span.stat-label "Budget left"]
           [:span.stat-val.good (str "$" (or left 0) " of $" (or budget 0))]]
          [:div.stat {:title "The largest budget anyone else still holds — what it would take to be sure"}
           [:span.stat-label "Rival max"]
           [:span.stat-val (if rival-max (str "$" rival-max) "–")]]
          [:div.stat {:title "Waiver runs the season has left. Bids divide your budget across these, which is why they grow as the season shortens"}
           [:span.stat-label "Runs left"] [:span.stat-val (or claims-left "–")]]]
         [:div.stat.muted
          "This league runs waiver priority, not FAAB — there is no bid to make."])])))

(defn my-roster-panel
  "What the manager already has, beside what he could claim.

  The board is free agents only, so without this the tab never shows the roster
  a claim is measured against — and the `My team` dropdown, whose whole job is to
  identify that roster, appears to do nothing when it changes. Picking a team
  moves Upgrade, Bid and the budget, but all of those are numbers elsewhere on
  the screen; this is the part that visibly answers 'which team am I'.

  Starters above bench, because the synced league knows the real lineup and the
  draft config's slot template does not. A row the board could not value keeps
  its seat and says so rather than vanishing — see `waiver/my-roster`."
  []
  (let [roster  @(rf/subscribe [:my-waiver-roster])
        synced? @(rf/subscribe [:league-synced?])]
    [:div.roster-panel.waiver-roster
     [:div.roster-head [:h3 "My Roster"]]
     (cond
       (not synced?)
       [:p.muted "Sync a league to see your roster."]

       ;; nil, not empty: no team is picked. This is the line that was missing —
       ;; it says what the dropdown is for.
       (nil? roster)
       [:p.muted "Pick your team above to see your roster and what a claim would cost."]

       (empty? roster)
       [:p.muted "This team holds nobody yet."]

       :else
       (let [row (fn [p]
                   ^{:key (:player-id p)}
                   [:tr {:class (str (when (:drop? p) "drop-seat ")
                                     (when (:parked? p) "parked"))}
                    [:td.slot (or (:position p) "–")]
                    [:td.slot-player
                     (if (:unvalued? p)
                       [:span.muted {:title (str "No projection for id " (:player-id p))}
                        (:player-id p)]
                       (:player-name p))
                     (when (:parked? p) [:span.parked-tag {:title "IR or taxi"} " IR"])
                     (when (:drop? p) [:span.drop-tag {:title "A claim would cost this seat"} " ↓"])]
                    [:td.num (board/format-whole (:ros-points p))]])
             {starters true bench false} (group-by (comp boolean :starter?) roster)]
         [:table.roster
          [:thead [:tr [:th.slot "Pos"] [:th "Player"] [:th.num "ROS"]]]
          [:tbody
           (when (seq starters)
             [:<> [:tr.roster-group [:td {:col-span 3} "Starters"]] (map row starters)])
           (when (seq bench)
             [:<> [:tr.roster-group [:td {:col-span 3} "Bench"]] (map row bench)])]]))]))

(defn week-banner
  "Which season this board is for — in three states, not two.

  Preseason is said out loud rather than left to be inferred from an empty GP
  column: in August this board is the draft board, and a manager who reads it as
  live is reading a projection as a result. But that claim is only worth making
  when it is *known* — asserting it while the board is still loading, or after a
  failed refresh, states a fact about the season on no evidence at all, in week
  10 as readily as in August."
  []
  (let [{:keys [through-week]} @(rf/subscribe [:waiver-meta])]
    (case @(rf/subscribe [:season-phase])
      :in-season [:div.week-banner (str "Rest-of-season, through week " through-week)]
      :preseason [:div.week-banner.preseason
                  "Preseason — no games played yet, so this is the full-season projection."]
      [:div.week-banner "Loading the rest-of-season board…"])))

;; ---- the view ----

(defn waivers-view []
  (let [players @(rf/subscribe [:waiver-players])
        cols    @(rf/subscribe [:visible-waiver-columns])
        sort    @(rf/subscribe [:waiver-sort])]
    [:div.waivers-view
     [week-banner]
     [:div.waiver-panels [sync-panel] [faab-panel]]
     [:div.board-controls
      [:div.filters [board/pos-filter] [board/search-box]]]
     [:details.col-details
      [:summary "⚙ Columns"]
      [columns/waiver-column-picker]]
     [:div.waiver-body
      [:div.table-scroll
       [:table.board
        [:thead [:tr (map (fn [c] ^{:key (:key c)} [header-cell c sort]) cols)]]
        [:tbody
         (map (fn [p]
                ^{:key (:player-id p)}
                [:tr {:class (when (and (:drop-candidate p) (pos? (or (:upgrade p) 0)))
                               "upgrade")}
                 (map (fn [{k :key}] ^{:key k} [cell k p]) cols)])
              players)]]]
      [:aside.waiver-roster-col [my-roster-panel]]]
     (when-let [rostered @(rf/subscribe [:rostered-matches])]
       (when (seq rostered)
         [:div.rostered-note
          "Not free: "
          (->> rostered
               (map (fn [{:keys [player-name position team]}]
                      (str player-name " (" position ") — " team)))
               (interpose ", ")
               (into [:span]))]))
     (when-let [drop (:drop-candidate (first players))]
       [:div.drop-note
        "A claim costs a roster spot. Yours would come from "
        [:strong (:player-name drop)]
        (str " (" (:position drop) ", "
             (board/format-whole (:ros-points drop)) " rest-of-season points).")])]))
