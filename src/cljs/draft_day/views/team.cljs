(ns draft-day.views.team
  "My Team: the manager's synced roster, full width, and where the season half
  of the app opens.

  It used to be a 280px panel beside the waiver board, which answered one
  question — what a claim would cost — and left the rest of the roster's week
  with nowhere to be read: who each starter plays, what he is projected for,
  whether he is hurt. The panel stays on Waivers for that one question; this is
  the rest.

  Rows are the waiver reply's full board rows for the players the manager holds
  (`:my-roster-players`), ordered and flagged by `:my-roster` — which carries the
  seat, the IR/taxi mark and the drop. Both come from the one `/api/waivers`
  reply, so opening this tab loads that board."
  (:require [re-frame.core :as rf]
            [draft-day.views.board :as board]
            [draft-day.views.waivers :as waivers]))

(def columns
  [[:slot "Slot"] [:name "Player"] [:position "Pos"] [:team "Team"] [:opp "Opp"]
   [:week "Week"] [:ros "ROS"] [:risk "Risk"] [:inj "Inj"]])

(defn roster-groups
  "The roster in its three blocks — starters, bench, IR and taxi — as
  `[label rows]`, keeping the server's order inside each.

  Each row is the full board row where the board has one, with the roster's
  own flags laid over it; a player the board cannot value keeps his row with
  only those flags, rather than vanishing from the roster he is on."
  [roster players]
  (let [by-id (into {} (map (juxt :player-id identity)) players)
        rows  (map #(merge (get by-id (:player-id %)) %) roster)]
    [["Starters"  (filterv :starter? rows)]
     ["Bench"     (filterv #(not (or (:starter? %) (:parked? %))) rows)]
     ["IR / Taxi" (filterv #(and (:parked? %) (not (:starter? %))) rows)]]))

(defn record-label
  "`2–1`, or nil when the league reports no record — a dash beside a team name
  reads as a score."
  [{:keys [wins losses]}]
  (when (and wins losses) (str wins "–" losses)))

(defn team-cell [k p week]
  (case k
    ;; A starter whose seat is not known gets a dash, never BN: he is in the
    ;; Starters block, and a league that cannot name the seat has not benched him.
    :slot [:td.muted (cond (:slot p)     (:slot p)
                           (:starter? p) "–"
                           (:parked? p)  "IR"
                           :else         "BN")]
    :name (if (:unvalued? p)
            [:td.player [:span.muted {:title (str "No projection for id " (:player-id p))}
                         (:player-id p)]]
            (cond-> (waivers/cell :name p week)
              (:drop? p) (conj [:span.drop-tag {:title "A claim would cost this seat"}
                                " ↓ drop"])))
    (waivers/cell k p week)))

(defn roster-table [groups week]
  [:div.table-scroll
   [:table.board
    [:thead [:tr (for [[k label] columns] ^{:key k} [:th label])]]
    [:tbody
     (for [[label rows] groups
           :when (seq rows)]
       ^{:key label}
       [:<>
        [:tr.group-row [:td {:col-span (count columns)} label]]
        (for [p rows]
          ^{:key (:player-id p)}
          [:tr {:class (cond (:drop? p) "drop-seat" (:parked? p) "parked")}
           (for [[k _] columns] ^{:key k} [team-cell k p week])])])]]])

(defn team-strip []
  (let [{:keys [my-team-name]} @(rf/subscribe [:account])
        team @(rf/subscribe [:my-sync-team])]
    [:div.status-strip
     [:span.team-title (or my-team-name "My Team")]
     (when-let [rec (record-label team)] [:span.strip-note [:b rec]])
     (when-let [pos (:waiver-position team)]
       [:span.strip-note {:title "Where your claims fall in the waiver order"}
        (str "Waiver priority " pos)])]))

(defn team-view []
  (let [waivers @(rf/subscribe [:waivers])
        roster  @(rf/subscribe [:my-waiver-roster])
        synced? @(rf/subscribe [:league-synced?])
        week    (:week @(rf/subscribe [:waiver-meta]))]
    [:div.team-view
     [team-strip]
     (cond
       (not synced?)  [:p.muted "Sync a league under Settings to see your roster."]
       (nil? waivers) [:p.muted "Loading your roster…"]
       (nil? roster)  [:p.muted "Pick your team under Settings to see your roster."]
       (empty? roster) [:p.muted "This team holds nobody yet."]
       :else [roster-table (roster-groups roster (:my-roster-players waivers)) week])
     (when-let [drop (some #(when (:drop? %) %) roster)]
       [:div.drop-note
        "A claim costs a roster spot. Yours would come from "
        [:strong (:player-name drop)]
        (str " (" (:position drop) ", "
             (board/format-whole (:ros-points drop)) " rest-of-season points).")])]))
