(ns draftday.views.settings
  (:require [reagent.core :as r]
            [re-frame.core :as rf]))

(defn- num-field [label value on-change]
  [:label.field
   [:span label]
   [:input {:type "number" :value value :min 0
            :on-change #(on-change (js/parseInt (.. % -target -value) 10))}]])

(defn- sleeper-import []
  (let [league-id (r/atom "1380540443179118592")]
    (fn []
      [:section.settings-card.sleeper
       [:h3 "Import from Sleeper"]
       [:p.muted "Paste a Sleeper league ID to pull its scoring + roster settings."]
       [:div.row
        [:input {:type "text" 
                 :placeholder "League ID" 
                 :value @league-id
                 :on-change #(reset! league-id (.. % -target -value))}]
        [:button.primary {:on-click #(when (seq @league-id)
                                       (rf/dispatch [:import-sleeper @league-id]))}
         "Load from Sleeper"]]])))

(defn- league-config []
  (let [cfg @(rf/subscribe [:config])]
    [:section.settings-card
     [:h3 "League"]
     [:div.fields
      [num-field "Teams" (:num-teams cfg) #(rf/dispatch [:apply-config {:num-teams %}])]
      [num-field "Budget $" (:starting-bankroll cfg) #(rf/dispatch [:apply-config {:starting-bankroll %}])]
      [num-field "Tiers" (:num-tiers cfg) #(rf/dispatch [:apply-config {:num-tiers %}])]]]))

(defn- scoring-config []
  (let [cfg @(rf/subscribe [:config])]
    [:section.settings-card
     [:h3 "Scoring"]
     [:label.field
      [:span "Preset"]
      [:select {:value (name (:scoring cfg))
                :on-change #(rf/dispatch [:apply-config {:scoring (keyword (.. % -target -value))}])}
       [:option {:value "standard"} "Standard"]
       [:option {:value "half-ppr"} "Half PPR"]
       [:option {:value "ppr"} "PPR"]]]]))

(defn- roster-config []
  (let [cfg    @(rf/subscribe [:config])
        roster (:roster cfg)
        set-r  (fn [k v] (rf/dispatch [:apply-config {:roster (assoc roster k v)}]))]
    [:section.settings-card
     [:h3 "Roster"]
     [:div.fields
      (for [[k label] [[:qb "QB"] [:rb "RB"] [:wr "WR"] [:te "TE"]
                       [:flex "FLEX"] [:k "K"] [:dst "DST"] [:bench "Bench"]]]
        ^{:key k}
        [num-field label (get roster k 0) #(set-r k %)])]]))

(defn settings []
  [:div.settings
   [sleeper-import]
   [league-config]
   [scoring-config]
   [roster-config]])
