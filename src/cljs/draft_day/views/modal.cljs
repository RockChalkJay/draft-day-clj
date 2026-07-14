(ns draft-day.views.modal
  (:require [reagent.core :as r]
            [re-frame.core :as rf]))

(defn- resize [names n]
  (vec (take n (concat names (repeat "")))))

(defn start-draft-modal
  "Form-2: local atoms for the league setup, seeded from current config/teams.
  Starting the draft rebuilds fresh teams and resets all in-progress draft state."
  []
  (let [cfg    @(rf/subscribe [:config])
        teams  @(rf/subscribe [:teams])
        n      (r/atom (:num-teams cfg))
        budget (r/atom (:starting-bankroll cfg))
        tiers  (r/atom (:num-tiers cfg))
        names  (r/atom (mapv :name teams))]
    (fn []
      [:div.modal-overlay
       {:on-click #(when (= (.-target %) (.-currentTarget %)) (rf/dispatch [:close-modal]))}
       [:div.modal
        [:h2 "Start Draft"]
        [:p.muted "Sets up the league and clears any picks already recorded."]
        [:div.fields
         [:label.field [:span "Teams"]
          [:input {:type "number" :min 2 :max 16 :value @n
                   :on-change #(let [v (js/parseInt (.. % -target -value) 10)]
                                 (when (and (integer? v) (pos? v))
                                   (reset! n v)
                                   (swap! names resize v)))}]]
         [:label.field [:span "Budget $"]
          [:input {:type "number" :min 1 :value @budget
                   :on-change #(reset! budget (js/parseInt (.. % -target -value) 10))}]]
         [:label.field [:span "Tiers"]
          [:input {:type "number" :min 1 :value @tiers
                   :on-change #(reset! tiers (js/parseInt (.. % -target -value) 10))}]]]
        [:h4 "Team names " [:span.muted "(first is you)"]]
        [:div.team-names
         (for [i (range @n)]
           ^{:key i}
           [:input {:type "text"
                    :placeholder (if (zero? i) "You" (str "Team " (inc i)))
                    :value (get @names i "")
                    :on-change #(swap! names assoc i (.. % -target -value))}])]
        [:div.modal-actions
         [:button {:on-click #(rf/dispatch [:close-modal])} "Cancel"]
         [:button.primary
          {:on-click #(rf/dispatch [:start-draft
                                    {:num-teams @n :starting-bankroll @budget :num-tiers @tiers
                                     :team-names (mapv (fn [i] (get @names i "")) (range @n))}])}
          "Start Draft (resets picks)"]]]])))
