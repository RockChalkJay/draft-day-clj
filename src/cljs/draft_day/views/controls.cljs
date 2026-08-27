(ns draft-day.views.controls
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [draft-day.db :as db]
            [draft-day.views.board :as board]
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

(defn- bye-tag
  "The nominated player's bye, colored like the board: red pulse when drafting
  would stack a starter's bye, green when it would cover an uncovered starter."
  [p]
  (when-let [b (:bye p)]
    (let [exp   @(rf/subscribe [:my-bye-exposure])
          pos   (:position p)
          clash? (db/board-bye-clash? pos b exp)
          cover? (and (not clash?) (db/covers-starter? pos b exp))]
      [:span " · Bye "
       [:span {:class (cond clash? "bye-clash" cover? "bye-cover")
               :title (cond clash? (str "You already start a " pos " on bye " b)
                            cover? (str "Covers one of your uncovered " pos " starters"))}
        b]])))

(defn- risk-tag
  "The nominated player's injury risk, as the board's bar plus the word the board
  has no room for. This is the one place a manager is about to commit money, so
  it is worth the extra characters here even though the column stays a glyph."
  [p]
  (when-let [lvl (:injury-risk p)]
    (let [txt (:injury/reason p)]
      [:span " · "
       [:span.nt-risk {:title txt :aria-label txt}
        [board/risk-bar lvl]
        [:span.nt-risk-word {:class (when (db/serious-injury? (:sleeper/injury-status p))
                                      "inj-serious")}
         (get board/risk-words lvl)]]])))

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
           [:div.nt-meta (util/pos-label p) " · " (:team p) [bye-tag p] [risk-tag p]]
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
