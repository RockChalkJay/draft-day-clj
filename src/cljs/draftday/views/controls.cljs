(ns draftday.views.controls
  (:require [re-frame.core :as rf]))

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

(defn nominate-bar []
  (let [nominated @(rf/subscribe [:nominated-id])
        by-id     @(rf/subscribe [:players-by-id])
        bid       @(rf/subscribe [:bid])
        bid-team  @(rf/subscribe [:bid-team])
        teams     @(rf/subscribe [:teams])
        p         (get by-id nominated)]
    [:div.nominate-bar
     (if p
       [:<>
        [:span.nom-name (:player-name p) " "
         [:span.muted (str (:position p) " · " (:team p))]]
        [:span.nom-hint "Worth " [:b (str "$" (:worth p))]]
        [:input.bid {:type "number" :placeholder "Bid $" :value bid :min 1
                     :on-change #(rf/dispatch [:set-bid (.. % -target -value)])}]
        [:select {:value bid-team
                  :on-change #(rf/dispatch [:set-bid-team (.. % -target -value)])}
         (for [t teams]
           ^{:key (:team-id t)}
           [:option {:value (:team-id t)} (str (:name t) " ($" (:bankroll t) ")")])]
        [:button.primary
         {:disabled (or (nil? bid) (= "" bid))
          :on-click #(rf/dispatch [:record-pick {:player-id (:player-id p) :price bid
                                                  :team-id bid-team :position (:position p)}])}
         "Record Pick"]]
       [:span.muted "Click a player to nominate…"])]))
