(ns draft-day.views.league
  "The season's League tab: every team in the synced league — who holds whom,
  its record and the FAAB it has left — in standings order.

  The draft half has a League tab too (`views.roster/league-view`), and it is a
  different screen: the auction room's picks and bankrolls. This one reads the
  synced rosters, through `db/team-roster`, so a player shows under the team
  the provider says holds him rather than the team that drafted him in August."
  (:require [re-frame.core :as rf]
            [draft-day.bid-history :as bid-history]
            [draft-day.db :as db]
            [draft-day.views.util :as util]
            [draft-day.views.waivers :as waivers]))

(defn player-row
  "One held player. `openable?` says whether the detail modal has a row for
  him: it reads the waiver board, which carries free agents and the manager's
  own roster but not another team's, and a name that looks clickable and does
  nothing is worse than plain text.

  The first cell is a seat for a starter and a position below the bench line.
  A starter whose seat is unknown gets a dash — his position there would read
  as the seat `db/starter-seats` declined to guess."
  [p bench? openable?]
  ^{:key (:player-id p)}
  [:tr {:class (when bench? "bench")}
   [:td.slot (if bench? (or (:position p) "–") (or (:slot p) "–"))]
   [:td (let [nm (if bench? (:player-name p) [:b (:player-name p)])]
          (cond
            (:unvalued? p)
            [:span.muted {:title (str "No player for id " (:player-id p))} (:player-id p)]

            openable?
            [:button.name-btn {:on-click #(waivers/open-detail! % p)
                               :title    "Player detail"}
             nm]

            :else nm))]
   [:td.muted (:team p)]])

(defn style-tag
  "How this team bids, as a tag on its name with the evidence as its title, or
  nil for a league with no bid history to read. A manager with none in it is
  \"No bids yet\" rather than untagged, which would read as the history
  missing."
  [profile history?]
  (when history?
    [:span.style-tag {:title (when profile (bid-history/style-line profile))}
     (get bid-history/style-labels (if profile (:style profile) :new))]))

(defn team-card [{:keys [team starters bench parked mine? profile]} faab? openable
                 & [history?]]
  [:div.team-card {:class (when mine? "mine")}
   [:div.team-head.split
    (:name team)
    (when mine? [:span.you " (You)"])
    [style-tag profile history?]
    (when-let [rec (db/record-label team)] [:span.rec rec])]
   [:div.team-sub
    ;; A balance the host did not report is a dash, not $0: a rival read as
    ;; broke is one the manager thinks cannot outbid him.
    (when faab?
      (if (number? (:faab-left team))
        [:b {:title "FAAB left"} (util/faab (:faab-left team))]
        [:span.muted {:title "The host did not report this team's FAAB"} "FAAB –"]))
    (when-let [pos (:waiver-position team)] [:span (str "Waiver #" pos)])]
   [:table.roster
    [:tbody
     (map #(player-row % false (openable (:player-id %))) starters)
     (when (seq bench)
       [:<> [:tr.roster-group [:td {:col-span 3} "Bench"]]
        (map #(player-row % true (openable (:player-id %))) bench)])
     (when (seq parked)
       [:<> [:tr.roster-group [:td {:col-span 3} "IR"]]
        (map #(player-row % true (openable (:player-id %))) parked)])]]])

(defn season-view []
  (let [synced?  @(rf/subscribe [:league-synced?])
        rosters  @(rf/subscribe [:league-rosters])
        failed   @(rf/subscribe [:universe-error])
        ls       @(rf/subscribe [:league-sync])
        openable @(rf/subscribe [:comparable-by-id])
        profiles @(rf/subscribe [:league-bid-profiles])
        faab?   (= "faab" (some-> ls :waiver :type name))]
    (cond
      (not synced?)
      [:p.muted "Sync a league under Settings to see every team's roster."]

      ;; Synced, but the universe that names the players is not here.
      (nil? rosters)
      [:p.muted (if failed
                  (str "The player list failed to load (" failed "), so there is"
                       " nobody to name on these rosters. Reload to try again.")
                  "Loading players…")]

      :else
      [:div.league-grid
       (map (fn [r]
              ^{:key (get-in r [:team :roster-id])}
              [team-card (assoc r :profile (bid-history/team-profile (:team r) profiles))
               faab? openable (some? profiles)])
            rosters)])))
