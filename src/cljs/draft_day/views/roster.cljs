(ns draft-day.views.roster
  (:require [re-frame.core :as rf]))

(defn- budget-target
  "Suggested $ for an open slot: best-available Worth for that slot's position
  (FLEX = best of RB/WR/TE), $1 for bench/K/DST."
  [slot best]
  (case (:pos slot)
    "FLEX"        (apply max 0 (map #(get best % 0) ["RB" "WR" "TE"]))
    "BENCH"       nil
    ("K" "DST")   1
    (get best (:pos slot))))

(defn my-roster []
  (let [team      @(rf/subscribe [:my-team])
        by-id     @(rf/subscribe [:players-by-id])
        drafted   @(rf/subscribe [:drafted])
        best      @(rf/subscribe [:best-worth-by-pos])
        max-bid   @(rf/subscribe [:my-max-bid])
        nominated (get by-id @(rf/subscribe [:nominated-id]))]
    [:div.roster-panel
     [:div.roster-head
      [:h3 "My Roster"]
      [:div.roster-cash
       [:span "Bankroll " [:b (str "$" (:bankroll team))]]
       [:span.muted (str "· max bid $" max-bid)]]]
     [:div.nom-box
      (if nominated
        [:span (:player-name nominated) " "
         [:span.muted (str (:position nominated) " · " (:team nominated) " · worth $" (:worth nominated))]]
        [:span.muted "No player currently nominated."])]
     [:table.roster
      [:tbody
       (for [[i slot] (map-indexed vector (:roster team))]
         (let [pid (:player-id slot)
               p   (get by-id pid)]
           ^{:key i}
           [:tr
            [:td.slot (:pos slot)]
            [:td.slot-player (if p (:player-name p) [:span.muted "—"])]
            [:td.slot-budget
             (if p
               [:span.paid (str "$" (get-in drafted [pid :price]))]
               (let [t (budget-target slot best)]
                 [:span.target (if (and t (pos? t)) (str "~$" t) "$1")]))]
            [:td.slot-undo
             (when p [:button.undo {:title "Undo" :on-click #(rf/dispatch [:undo-pick pid])} "↩"])]]))]]]))

(defn league-view []
  (let [teams   @(rf/subscribe [:teams])
        by-id   @(rf/subscribe [:players-by-id])
        drafted @(rf/subscribe [:drafted])
        my-id   @(rf/subscribe [:my-team-id])]
    [:div.league-grid
     (for [t teams]
       ^{:key (:team-id t)}
       [:div.team-card
        [:div.team-head
         (:name t) (when (= (:team-id t) my-id) [:span.you " (You)"])
         [:span.muted (str " · $" (:bankroll t))]]
        [:table.roster
         [:tbody
          (for [[i slot] (map-indexed vector (:roster t))]
            (let [p (get by-id (:player-id slot))]
              ^{:key i}
              [:tr
               [:td.slot (:pos slot)]
               [:td (if p (:player-name p) [:span.muted "—"])]
               [:td.num.muted (when p (str "$" (get-in drafted [(:player-id slot) :price])))]]))]]])]))
