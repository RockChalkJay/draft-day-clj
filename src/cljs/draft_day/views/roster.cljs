(ns draft-day.views.roster
  (:require [re-frame.core :as rf]
            [draft-day.db :as db]))

(defn- avail-cell
  "Open slot: pooled budget still available for its bucket. Blank when the
  bucket has no plan set; warn-colored once the pool is exhausted."
  [slot avail]
  (when-some [a (get avail (db/slot->budget-key (:pos slot)))]
    [:span.avail {:class (when (<= a 0) "over")}
     (if (neg? a) (str "−$" (- a)) (str "$" a))]))

(defn my-roster []
  (let [team      @(rf/subscribe [:my-team])
        by-id     @(rf/subscribe [:players-by-id])
        drafted   @(rf/subscribe [:drafted])
        avail     @(rf/subscribe [:budget-avail])
        uncovered @(rf/subscribe [:my-uncovered-starters])]
    [:div.roster-panel
     [:div.roster-head
      [:h3 "My Roster"]]
     [:table.roster
      [:thead
       [:tr
        [:th.slot "Slot"]
        [:th "Player"]
        [:th.slot-bye "Bye"]
        [:th.slot-budget "$"]
        [:th.slot-undo]]]
      [:tbody
       (map-indexed
        (fn [i slot]
          (let [pid (:player-id slot)
                p   (get by-id pid)]
            ^{:key i}
            [:tr
             [:td.slot (:pos slot)]
             [:td.slot-player (if p (:player-name p) [:span.muted "—"])]
             [:td.slot-bye {:class (when (contains? uncovered pid) "bye-uncovered")
                            :title (when (contains? uncovered pid)
                                     (str "No bench " (:position p) " covers bye " (:bye p)))}
              (when p (or (:bye p) "–"))]
             [:td.slot-budget
              (if p
                [:span.paid (str "$" (get-in drafted [pid :price]))]
                [avail-cell slot avail])]
             [:td.slot-undo
              (when p [:button.undo {:title "Undo" :on-click #(rf/dispatch [:undo-pick pid])} "↩"])]]))
        (:roster team))]]]))

(defn- league-sync-panel []
  (let [cfg      @(rf/subscribe [:config])
        sync     @(rf/subscribe [:league-sync])
        users    (when sync (:users sync))
        teams    (when sync (:teams sync))
        by-id    @(rf/subscribe [:players-by-id])
        league-id (:league-id cfg)]
    [:section.league-sync
     [:h3 "League sync"]
     [:p.muted "Read-only current roster data from Sleeper. This stays separate from the draft board state."]
     [:div.row
      [:input {:type "text"
               :placeholder "Sleeper League ID"
               :value league-id
               :on-change #(rf/dispatch [:apply-config {:league-id (.. % -target -value)}])}]
      [:button.primary {:on-click #(when (seq league-id)
                                     (rf/dispatch [:sync-league {:provider "sleeper" :league-id league-id}]))}
       "Sync league"]
      [:button {:on-click #(rf/dispatch [:clear-league-sync])}
       "Clear"]]
     (if sync
       (let [{:keys [league]} sync]
         [:div.sync-details
          [:h4 (str (:name league) " · " (:season league))]
          [:div.sync-grid
           [:div
            [:h5 "Managers"]
            (if (seq users)
              [:ul
               (for [{:keys [user-id display-name username]} users]
                 ^{:key user-id}
                 [:li (or display-name username user-id)])]
              [:p.muted "No managers returned."])]
           [:div
            [:h5 "Teams"]
            (if (seq teams)
              [:ul
               (for [{:keys [team-id manager roster]} teams]
                 ^{:key team-id}
                 [:li
                  [:b (str "Team " team-id)]
                  [:div (str manager)]
                  [:small (str (count roster) " current roster slots")]
                  (when (seq roster)
                    [:div.sync-roster
                     (for [player-id roster]
                       (let [p (get by-id player-id)]
                         ^{:key (str team-id "-" player-id)}
                         [:span.sync-player (or (:player-name p) player-id)]))])])]
              [:p.muted "No team data returned."])]]])
       [:p.muted "No league sync data loaded yet."])]))

(defn league-view []
  (let [teams   @(rf/subscribe [:teams])
        by-id   @(rf/subscribe [:players-by-id])
        drafted @(rf/subscribe [:drafted])
        my-id   @(rf/subscribe [:my-team-id])]
    [:div.league-panel
     [league-sync-panel]
     [:div.league-grid
      (map (fn [t]
             ^{:key (:team-id t)}
             [:div.team-card
              [:div.team-head
               (:name t)
               (when (= (:team-id t) my-id) [:span.you " (You)"])
               [:span.muted (str " · $" (:bankroll t))]]
              [:table.roster
               [:tbody
                (map-indexed
                 (fn [i slot]
                   (let [p (get by-id (:player-id slot))]
                     ^{:key i}
                     [:tr
                      [:td.slot (:pos slot)]
                      [:td (if p (:player-name p) [:span.muted "—"])]
                      [:td.num.muted (when p (str "$" (get-in drafted [(:player-id slot) :price])))]]))
                 (:roster t))]]])
           teams)]]))
