(ns draftday.views.controls
  (:require [reagent.core :as r]
            [re-frame.core :as rf]))

(def ^:private profiles
  [[:balanced "Balanced"] [:floor "Floor"] [:ceiling "Ceiling"] [:scarcity "Scarcity"]])

(defn profile-switcher []
  (let [active @(rf/subscribe [:profile])]
    [:div.profile-switch
     [:span.ps-label "Strategy"]
     (for [[k label] profiles]
       ^{:key k}
       [:button {:class (when (= active k) "on")
                 :on-click #(rf/dispatch [:set-profile k])}
        label])]))

(defn- chip [p]
  [:span.chip {:on-click #(rf/dispatch [:set-nominated (:player-id p)])}
   (:player-name p) [:b (str " $" (:worth p))]])

(defn suggestions []
  (let [top    @(rf/subscribe [:top-overall])
        by-pos @(rf/subscribe [:top-by-position])]
    [:div.suggestions
     [:div.sugg-block
      [:h4 "Top 5 Overall"]
      [:div.chips (for [p top] ^{:key (:player-id p)} [chip p])]]
     [:div.sugg-cols
      (for [pos ["QB" "RB" "WR" "TE"]]
        ^{:key pos}
        [:div.sugg-block
         [:h4 pos]
         [:div.chips (for [p (get by-pos pos)] ^{:key (:player-id p)} [chip p])]])]]))

(defn- nominate-form
  "Form-2 so the bid/team live in local reagent atoms (synchronous updates — no
  dropped keystrokes on a fast controlled input). Keyed on the player so it
  remounts fresh per nomination."
  [p]
  (let [bid  (r/atom "")
        team (r/atom @(rf/subscribe [:my-team-id]))]
    (fn [p]
      (let [teams @(rf/subscribe [:teams])]
        [:div.nominate-bar
         [:span.nom-name (:player-name p) " "
          [:span.muted (str (:position p) " · " (:team p))]]
         [:span.nom-hint "Worth " [:b (str "$" (:worth p))]]
         [:input.bid {:type "number" :placeholder "Bid $" :value @bid :min 1
                      :on-change #(reset! bid (.. % -target -value))}]
         [:select {:value @team :on-change #(reset! team (.. % -target -value))}
          (for [t teams]
            ^{:key (:team-id t)}
            [:option {:value (:team-id t)} (str (:name t) " ($" (:bankroll t) ")")])]
         [:button.primary
          {:disabled (or (nil? @bid) (= "" @bid))
           :on-click #(rf/dispatch [:record-pick {:player-id (:player-id p) :price @bid
                                                   :team-id @team :position (:position p)}])}
          "Record Pick"]]))))

(defn nominate-bar []
  (let [nominated @(rf/subscribe [:nominated-id])
        p         (get @(rf/subscribe [:players-by-id]) nominated)]
    (if p
      ^{:key nominated} [nominate-form p]
      [:div.nominate-bar [:span.muted "Click a player to nominate…"]])))
