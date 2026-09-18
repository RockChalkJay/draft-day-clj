(ns draft-day.views.modal
  (:require [reagent.core :as r]
            [re-frame.core :as rf]))

(defn- resize [names n]
  (vec (take n (concat names (repeat "")))))

(defn start-draft-modal
  "Form-2: local atoms for the league setup, seeded from current config/teams.
  Starting the draft rebuilds fresh teams and resets all in-progress draft state.

  A connected league's team count and budget are its own (`db/league-owned-keys`),
  so they are shown here but not edited; `:start-draft` holds the same line."
  []
  (let [cfg    @(rf/subscribe [:config])
        teams  @(rf/subscribe [:teams])
        n      (r/atom (:num-teams cfg))
        budget (r/atom (:starting-bankroll cfg))
        names  (r/atom (mapv :name teams))]
    (fn []
      (let [owned @(rf/subscribe [:league-owned-keys])
            why   "Set by your league — Re-sync it in Settings to refresh"]
        [:div.modal-overlay
         {:on-click #(when (= (.-target %) (.-currentTarget %)) (rf/dispatch [:close-modal]))}
         [:div.modal
          [:h2 "Start Draft"]
          [:p.muted "Sets up the league and clears any picks already recorded."]
          [:div.fields
           [:label.field [:span "Teams"]
            [:input {:type "number" :min 2 :max 16 :value @n
                     :disabled (contains? owned :num-teams)
                     :title (when (contains? owned :num-teams) why)
                     :on-change #(let [v (js/parseInt (.. % -target -value) 10)]
                                   (when (and (integer? v) (pos? v))
                                     (reset! n v)
                                     (swap! names resize v)))}]]
           [:label.field [:span "Budget $"]
            [:input {:type "number" :min 1 :value @budget
                     :disabled (contains? owned :starting-bankroll)
                     :title (when (contains? owned :starting-bankroll) why)
                     :on-change #(reset! budget (js/parseInt (.. % -target -value) 10))}]]]
          [:h4 "Team names " [:span.muted "(first is you)"]]
          [:div.team-names
           (map (fn [i]
                  ^{:key i}
                  [:input {:type "text"
                           :placeholder (if (zero? i) "You" (str "Team " (inc i)))
                           :value (get @names i "")
                           :on-change #(swap! names assoc i (.. % -target -value))}])
                (range @n))]
          [:div.modal-actions
           [:button {:on-click #(rf/dispatch [:close-modal])} "Cancel"]
           [:button.primary
            {:on-click #(rf/dispatch [:start-draft
                                      {:num-teams @n :starting-bankroll @budget
                                       :team-names (mapv (fn [i] (get @names i "")) (range @n))}])}
            "Start Draft (resets picks)"]]]]))))

(defn reset-cache-modal []
  [:div.modal-overlay
   {:on-click #(when (= (.-target %) (.-currentTarget %)) (rf/dispatch [:close-modal]))}
   [:div.modal
    [:h2 "Reset Player Cache"]
    [:p.muted "Clears the cached player data on the server and re-fetches live prices and rankings from Sleeper, FantasyPros and ESPN. Your draft picks, teams and settings are untouched."]
    [:div.modal-actions
     [:button {:on-click #(rf/dispatch [:close-modal])} "Cancel"]
     [:button.danger {:on-click #(rf/dispatch [:reset-cache])} "Reset Cache"]]]])
