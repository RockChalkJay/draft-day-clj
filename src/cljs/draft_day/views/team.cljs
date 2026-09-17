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
            [draft-day.db :as db]
            [draft-day.views.board :as board]
            [draft-day.views.util :as util]
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

;; ---- the side cards ----
;; Three questions a manager opens this tab to ask before he opens any other:
;; who am I playing, is anything wrong with my lineup, and is anyone on the wire
;; worth a claim. Each is answered from a board already fetched — the matchup
;; reply and the waiver reply — and each links to the tab that says more.

(defn next-kickoff
  "The earliest kickoff among `starters` whose game has not started, as an ISO
  stamp, or nil. ISO stamps sort as strings."
  [starters]
  (->> starters
       (filter #(and (:kickoff/at %)
                     (contains? #{nil "STATUS_SCHEDULED"} (:kickoff/status %))))
       (map :kickoff/at)
       sort
       first))

(defn lineup-issues
  "What is wrong with the lineup as set, as sentences: a starter on bye, a
  starter carrying an injury designation, and what the best lineup by projection
  would gain. Empty when there is nothing to say."
  [team week]
  (let [starters (remove :empty? (:starters team))
        gain     (get-in team [:optimal :projected :gain])]
    (cond-> (vec
             (concat
              (for [p starters
                    :when (= "Bye" (waivers/week-matchup p week))]
                {:text (str (:player-name p) " is on bye") :tag (:slot p) :warn? true})
              (for [p starters
                    :let [st (:sleeper/injury-status p)]
                    :when st]
                {:text (str (:player-name p) " — " st) :tag (:slot p)
                 :warn? (boolean (db/serious-injury? st))})))
      (and (number? gain) (pos? gain))
      (conj {:text (str "Best lineup by projection: +" (.toFixed gain 1))
             :tag "Matchup"}))))

(defn best-claims
  "The free agents who would improve the starting lineup, best first — the
  waiver board's own order — at most `n`."
  [players n]
  (->> players
       (filter #(pos? (or (:lineup-upgrade %) 0)))
       (sort-by db/waiver-rank-key)
       (take n)
       vec))

(defn go-link [view label]
  [:button.link {:on-click #(rf/dispatch [:set-view view])} label])

(defn this-week-card []
  (let [[mine theirs] @(rf/subscribe [:my-matchup])
        m @(rf/subscribe [:matchup])]
    [:div.side-card
     [:h3 "This week"]
     (if-not mine
       [:p.muted (if m "No matchup for your team this week." "Loading this week's matchup…")]
       [:<>
        [:div.side-line [:span.muted "vs"]
         [:b (if theirs
               (str (:name theirs)
                    (when-let [rec (record-label theirs)] (str " (" rec ")")))
               "No opponent")]]
        [:div.side-line [:span.muted "Projected"]
         [:span (str (board/format-one-decimal (:projected mine))
                     (when theirs (str " – " (board/format-one-decimal (:projected theirs)))))]]
        (when-let [at (util/kickoff-label (next-kickoff (:starters mine)))]
          [:div.side-line [:span.muted "Next kickoff"] [:span at]])])
     [:div.side-line [go-link :matchup "Open matchup →"]]]))

(defn lineup-check-card []
  (let [[mine] @(rf/subscribe [:my-matchup])
        week   (:week @(rf/subscribe [:matchup]))]
    (when mine
      (let [issues (lineup-issues mine week)]
        [:div.side-card
         [:h3 "Lineup check"]
         (if (seq issues)
           (for [{:keys [text tag warn?]} issues]
             ^{:key text}
             [:div.side-line
              [:span (when warn? [:span.warn "⚠ "]) text]
              [:span.muted tag]])
           [:div.side-line [:span.muted "Nothing to fix in your lineup."]])]))))

(defn best-claims-card []
  (let [w @(rf/subscribe [:waivers])]
    (when w
      (let [claims (best-claims (:players w) 3)]
        [:div.side-card
         [:h3 "Best claims"]
         (if (seq claims)
           (for [p claims]
             ^{:key (:player-id p)}
             [:div.side-line
              [:span (:player-name p) [:span.muted (str " " (:position p))]]
              [:span.good (str "+" (js/Math.round (:lineup-upgrade p))
                               (when (number? (:bid p)) (str " · $" (:bid p))))]])
           [:div.side-line [:span.muted "Nobody on the wire improves your lineup."]])
         [:div.side-line [go-link :waivers "Open waivers →"]]]))))

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
       :else [:div.team-body
              [roster-table (roster-groups roster (:my-roster-players waivers)) week]
              [:div.side-cards
               [this-week-card]
               [lineup-check-card]
               [best-claims-card]]])
     (when-let [drop (some #(when (:drop? %) %) roster)]
       [:div.drop-note
        "A claim costs a roster spot. Yours would come from "
        [:strong (:player-name drop)]
        (str " (" (:position drop) ", "
             (board/format-whole (:ros-points drop)) " rest-of-season points).")])]))
