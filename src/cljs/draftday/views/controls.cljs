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

(defn- nominate! [p] (rf/dispatch [:set-nominated (:player-id p)]))

(defn- top5-card [i p]
  [:div.sugg-card {:on-click #(nominate! p)}
   [:div.rank (str "#" (inc i))]
   [:div.pname (:player-name p)]
   [:div.pmeta (str (:position p) " · " (:team p))]
   [:div.pprice (str "$" (:worth p))]])

(defn- pos-card [pos players]
  [:div.pos-card
   [:h5 pos]
   (for [p players]
     ^{:key (:player-id p)}
     [:div.pos-row {:on-click #(nominate! p)}
      [:span.pn (:player-name p)]
      [:span.pv (str "$" (:worth p))]])])

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
      [:div.sugg-row (for [pos ["QB" "RB" "WR" "TE"]] ^{:key pos} [pos-card pos (get by-pos pos)])]]
     (when (seq needs)
       [:div.sugg-section
        [:h4 "Best Value For Your Needs"]
        [:div.sugg-row (for [n needs] ^{:key (:pos n)} [need-card n])]])]))

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
