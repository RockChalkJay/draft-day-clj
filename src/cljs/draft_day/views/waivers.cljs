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
    :upgrade   [:td.num {:class (util/sign-class (:upgrade p))}
                (util/signed (js/Math.round (or (:upgrade p) 0)))]
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
  (let [league-id (r/atom "")]
    (fn []
      (let [synced? @(rf/subscribe [:league-synced?])
            teams   @(rf/subscribe [:sync-teams])
            mine    @(rf/subscribe [:my-roster-id])
            status  @(rf/subscribe [:waiver-status])]
        [:div.sync-panel
         [:div.sync-row
          [:input {:type "text" :placeholder "Sleeper league ID"
                   :value @league-id
                   :on-change #(reset! league-id (.. % -target -value))}]
          [:button {:disabled (str/blank? @league-id)
                    :on-click #(rf/dispatch [:sync-league {:provider "sleeper"
                                                           :league-id @league-id}])}
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

(defn week-banner []
  (let [{:keys [through-week]} @(rf/subscribe [:waiver-meta])
        in-season? @(rf/subscribe [:in-season?])]
    (if in-season?
      [:div.week-banner (str "Rest-of-season, through week " through-week)]
      ;; Said out loud rather than left to be inferred from an empty GP column:
      ;; in preseason this board is the draft board, and a manager who reads it
      ;; as live is reading a projection as a result.
      [:div.week-banner.preseason
       "Preseason — no games played yet, so this is the full-season projection."])))

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
     (when-let [drop (:drop-candidate (first players))]
       [:div.drop-note
        "A claim costs a roster spot. Yours would come from "
        [:strong (:player-name drop)]
        (str " (" (:position drop) ", "
             (board/format-whole (:ros-points drop)) " rest-of-season points).")])]))
