(ns draft-day.views.controls
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [draft-day.views.util :as util]))

(defn- nominate! [p] (rf/dispatch [:set-nominated (:player-id p)]))

(defn- top5-card [i p]
  [:div.sugg-card {:on-click #(nominate! p)}
   [:div.rank (str "#" (inc i))]
   [:div.pname (:player-name p)]
   [:div.pmeta (str (:position p) " · " (:team p))]
   [:div.pprice (util/money (:worth p))]])

(defn- pos-card [pos players]
  [:div.pos-card
   [:h5 pos]
   (map (fn [p]
          ^{:key (:player-id p)}
          [:div.pos-row {:on-click #(nominate! p)}
           [:span.pn (:player-name p)]
           [:span.pv (util/money (:worth p))]])
        players)])

(defn- need-card [{:keys [pos player]}]
  [:div.need-card {:on-click #(when player (nominate! player))}
   [:h5 (str pos " (open starter slot)")]
   (if player
     [:<>
      [:div.pname (:player-name player)]
      [:div.pprice (str "$" (:worth player))]]
     [:div.muted "—"])])

(defn suggestions []
  (let [top    @(rf/subscribe [:top-overall])
        by-pos @(rf/subscribe [:top-by-position])
        needs  @(rf/subscribe [:best-value-for-needs])]
    [:div.suggestions
     [:div.sugg-section
      [:h4 "Top 5 Overall"]
      [:div.sugg-row (map-indexed (fn [i p] ^{:key (:player-id p)} [top5-card i p]) top)]]
     [:div.sugg-section
      [:h4 "Top 3 Per Position"]
      [:div.sugg-row (map (fn [pos] ^{:key pos} [pos-card pos (get by-pos pos)])
                          ["QB" "RB" "WR" "TE"])]]
     (when (seq needs)
       [:div.sugg-section
        [:h4 "Best Value For Your Needs"]
        [:div.sugg-row (map (fn [n] ^{:key (:pos n)} 
        [need-card n]) needs)]])]))

(defn- silhouette []
  [:svg {:width 64 :height 64 :view-box "0 0 24 24" :fill "none"
         :stroke "currentColor" :stroke-width 1.4}
   [:circle {:cx 12 :cy 8 :r 4}]
   [:path {:d "M4 21c0-4 3.6-7 8-7s8 3 8 7"}]])

(defn- face
  "Silhouette underneath, headshot on top — a missing image hides itself and
  falls through to the silhouette, so no load-state bookkeeping is needed."
  [p]
  [:div.nt-face
   [silhouette]
   [:img.nt-img {:src      (util/headshot-url p)
                 :alt      ""
                 :on-error #(set! (.. % -target -style -display) "none")}]])

(defn- val-cell [label amount]
  [:div.nt-val [:span.lbl label] [:span.amt amount]])

(defn- nominate-form
  "Form-2 so the bid/team live in local reagent atoms (synchronous updates — no
  dropped keystrokes on a fast controlled input). Keyed on the player so it
  remounts fresh per nomination."
  [p]
  (let [bid  (r/atom "")
        team (r/atom @(rf/subscribe [:my-team-id]))]
    (fn [p]
      (let [teams @(rf/subscribe [:teams])]
        [:div.nom-tile
         [:div.nt-label "On the block"]
         [:div.nt-body
          [face p]
          [:div.nt-main
           [:div.nt-name (:player-name p)]
           [:div.nt-meta (str (:position p) " · " (:team p)
                              (when-let [b (:bye p)] (str " · Bye " b)))]
           [:div.nt-vals
            [val-cell "Worth" (util/money (:worth p))]
            [val-cell "Mkt" (util/money (:market p))]
            [val-cell "ESPN" (util/money-rnd (:espn/auction-value p))]
            [val-cell "FP" (util/money-rnd (:fantasypros/aav p))]]]]
         [:div.nt-actions
          [:label.nt-bid-label "Bid $"]
          [:input.bid {:type        "number"
                       :placeholder "0"
                       :value       @bid
                       :min         1
                       :on-change   #(reset! bid (.. % -target -value))}]
          [:select {:value     @team
                    :on-change #(reset! team (.. % -target -value))}
           (map (fn [t]
                  ^{:key (:team-id t)}
                  [:option {:value (:team-id t)} (str (:name t) " (" (util/money (:bankroll t)) ")")])
                teams)]
          [:button.primary
           {:disabled (or (nil? @bid) (= "" @bid))
            :on-click #(rf/dispatch [:record-pick {:player-id (:player-id p)
                                                   :price     @bid
                                                   :team-id   @team
                                                   :position  (:position p)}])}
           "Record Pick"]]]))))

(defn nominate-tile []
  (let [nominated @(rf/subscribe [:nominated-id])
        p (get @(rf/subscribe [:players-by-id]) nominated)]
    (if p
      ^{:key nominated} [nominate-form p]
      [:div.nom-tile
       [:div.nt-label "On the block"]
       [:div.nt-body
        [:div.nt-face [silhouette]]
        [:div.nt-main [:div.muted "Click a player to nominate…"]]]])))
