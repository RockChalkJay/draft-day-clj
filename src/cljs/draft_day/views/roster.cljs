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

(defn team-money
  "What a team can still do in the room: the dollars left, the most it can bid
  while filling every other seat at $1, and how many seats are filled. The same
  `db/max-bid` the header's Max Bid reads, so the two cannot disagree about the
  manager's own team."
  [t]
  (let [seats (count (:roster t))]
    {:left    (:bankroll t)
     :max-bid (db/max-bid t)
     :filled  (- seats (db/open-slots t))
     :seats   seats}))

(defn league-view
  "Draft night's view of the room: who has drafted whom and what each team has
  left to spend. The money leads each card, because on the night the question a
  card answers first is whether that team can still outbid you."
  []
  (let [teams   @(rf/subscribe [:teams])
        by-id   @(rf/subscribe [:players-by-id])
        drafted @(rf/subscribe [:drafted])
        my-id   @(rf/subscribe [:my-team-id])]
    [:div.league-grid
     (map (fn [t]
            (let [{:keys [left max-bid filled seats]} (team-money t)
                  mine? (= (:team-id t) my-id)]
              ^{:key (:team-id t)}
              [:div.team-card {:class (when mine? "mine")}
               [:div.team-head.split
                (:name t) (when mine? [:span.you " (You)"])
                [:span.money-left (str "$" left " left")]]
               [:div.team-sub
                [:span "Max bid " [:b (str "$" max-bid)]]
                [:span (str filled " of " seats " filled")]]
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
                  (:roster t))]]]))
          teams)]))
