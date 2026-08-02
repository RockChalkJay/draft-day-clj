(ns draft-day.views.controls
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [draft-day.views.util :as util]))

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
        [:div.nt-main [:div.muted "Click a player or watch-list entry to nominate…"]]]])))
