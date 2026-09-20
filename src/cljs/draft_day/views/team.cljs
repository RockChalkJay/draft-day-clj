(ns draft-day.views.team
  "My Team: the manager's synced roster, full width, and where the season half
  of the app opens.

  It used to be a 280px panel beside the waiver board, which answered one
  question — what a claim would cost — and left the rest of the roster's week
  with nowhere to be read: who each starter plays, what he is projected for,
  whether he is hurt. The panel stays on Waivers for that one question; this is
  the rest.

  The roster is `:my-roster` — the manager's own card off the League tab, so the
  two cannot disagree about whom he holds — and its rows are the waiver reply's
  full board rows, which is why opening this tab loads that board."
  (:require [re-frame.core :as rf]
            [draft-day.db :as db]
            [draft-day.views.board :as board]
            [draft-day.views.waivers :as waivers]))

(def columns
  [[:slot "Slot"] [:name "Player"] [:position "Pos"] [:team "Team"] [:opp "Opp"]
   [:week "Week"] [:ros "ROS"] [:risk "Risk"] [:inj "Inj"]])

(defn roster-groups
  "`:my-roster`'s three blocks as `[label rows]`, in its order, each row flagged
  with its block for the cells that read it: the Slot cell's dash, BN or IR, and
  the IR row's muted class."
  [{:keys [starters bench parked]}]
  [["Starters"  (mapv #(assoc % :starter? true) starters)]
   ["Bench"     bench]
   ["IR / Taxi" (mapv #(assoc % :parked? true) parked)]])

(defn team-cell [k p week]
  (case k
    ;; A starter whose seat is not known gets a dash, never BN: he is in the
    ;; Starters block, and a league that cannot name the seat has not benched him.
    :slot [:td.muted (cond (:slot p)     (:slot p)
                           (:starter? p) "–"
                           (:parked? p)  "IR"
                           :else         "BN")]
    :name (if (:unvalued? p)
            [:td.player [:span.muted {:title (str "No player for id " (:player-id p))}
                         (:player-id p)]]
            (cond-> (waivers/cell :name p week)
              (:drop? p) (conj [:span.drop-tag {:title "A claim would cost this seat"}
                                " ↓ drop"])))
    (waivers/cell k p week)))

(defn roster-table [groups week]
  [:div.table-scroll
   [:table.board
    [:thead [:tr (map (fn [[k label]] ^{:key k} [:th label]) columns)]]
    [:tbody
     (->> groups
          (filter (fn [[_ rows]] (seq rows)))
          (map (fn [[label rows]]
                 ^{:key label}
                 [:<>
                  [:tr.group-row [:td {:col-span (count columns)} label]]
                  (map (fn [p]
                         ^{:key (:player-id p)}
                         [:tr {:class (cond (:drop? p) "drop-seat" (:parked? p) "parked")}
                          (map (fn [[k _]] ^{:key k} [team-cell k p week]) columns)])
                       rows)])))]]])

(defn team-strip []
  (let [{:keys [my-team-name]} @(rf/subscribe [:account])
        team @(rf/subscribe [:my-sync-team])]
    [:div.status-strip
     [:span.team-title (or my-team-name "My Team")]
     (when-let [rec (db/record-label team)] [:span.strip-note [:b rec]])
     (when-let [pos (:waiver-position team)]
       [:span.strip-note {:title "Where your claims fall in the waiver order"}
        (str "Waiver priority " pos)])]))

(defn empty-roster?
  "Whether a `:my-roster` holds nobody — not whether there is one to read."
  [roster]
  (every? empty? (vals roster)))

(defn team-view []
  (let [waivers @(rf/subscribe [:waivers])
        roster  @(rf/subscribe [:my-roster])
        synced? @(rf/subscribe [:league-synced?])
        team    @(rf/subscribe [:my-sync-team])
        failed  @(rf/subscribe [:universe-error])
        week    (:week @(rf/subscribe [:waiver-meta]))]
    [:div.team-view
     [team-strip]
     (cond
       (not synced?)  [:p.muted "Sync a league under Settings to see your roster."]
       (nil? team)    [:p.muted "Pick your team under Settings to see your roster."]
       (nil? roster)  [:p.muted (if failed
                                  (str "The player list failed to load (" failed
                                       "). Reload to try again.")
                                  "Loading your roster…")]
       ;; The rows are the waiver reply's, so a roster without it has names and
       ;; nothing else to read against them.
       (nil? waivers) [:p.muted "Loading your roster…"]
       (empty-roster? roster) [:p.muted "This team holds nobody yet."]
       :else [roster-table (roster-groups roster) week])
     (when-let [drop (and roster (:drop-candidate waivers))]
       [:div.drop-note
        "A claim costs a roster spot. Yours would come from "
        [:strong (:player-name drop)]
        (str " (" (:position drop) ", "
             (board/format-whole (:ros-points drop)) " rest-of-season points).")])]))
