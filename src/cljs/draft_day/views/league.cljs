(ns draft-day.views.league
  "The season's League tab: every team in the synced league — who holds whom,
  its record and the FAAB it has left — in standings order.

  The draft half has a League tab too (`views.roster/league-view`), and it is a
  different screen: the auction room's picks and bankrolls. This one reads the
  synced rosters, through `db/team-roster`, so a player shows under the team
  the provider says holds him rather than the team that drafted him in August."
  (:require [re-frame.core :as rf]
            [draft-day.views.team :as team]
            [draft-day.views.waivers :as waivers]))

(defn player-row [p bench?]
  ^{:key (:player-id p)}
  [:tr {:class (when bench? "bench")}
   [:td.slot (or (:slot p) (:position p) "–")]
   [:td (if (:unvalued? p)
          [:span.muted {:title (str "No player for id " (:player-id p))} (:player-id p)]
          [:button.name-btn {:on-click #(waivers/open-detail! % p)
                             :title    "Player detail"}
           (if bench? (:player-name p) [:b (:player-name p)])])]
   [:td.muted (:team p)]])

(defn team-card [{:keys [team starters bench parked mine?]} faab?]
  [:div.team-card {:class (when mine? "mine")}
   [:div.team-head.split
    (:name team)
    (when mine? [:span.you " (You)"])
    (when-let [rec (team/record-label team)] [:span.rec rec])]
   [:div.team-sub
    (when faab? [:span "FAAB " [:b (str "$" (or (:faab-left team) 0))] " left"])
    (when-let [pos (:waiver-position team)] [:span (str "Waiver #" pos)])]
   [:table.roster
    [:tbody
     (map #(player-row % false) starters)
     (when (seq bench)
       [:<> [:tr.roster-group [:td {:col-span 3} "Bench"]]
        (map #(player-row % true) bench)])
     (when (seq parked)
       [:<> [:tr.roster-group [:td {:col-span 3} "IR / Taxi"]]
        (map #(player-row % true) parked)])]]])

(defn season-view []
  (let [rosters @(rf/subscribe [:league-rosters])
        ls      @(rf/subscribe [:league-sync])
        faab?   (= "faab" (some-> ls :waiver :type name))]
    (if (seq rosters)
      [:div.league-grid
       (for [r rosters]
         ^{:key (get-in r [:team :roster-id])}
         [team-card r faab?])]
      [:p.muted "Sync a league under Settings to see every team's roster."])))
