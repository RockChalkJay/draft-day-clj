(ns draft-day.views.settings
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [draft-day.db :as db]))

(defn- num-field [label value on-change]
  [:label.field
   [:span label]
   [:input {:type "number" :value value :min 0
            :on-change #(on-change (js/parseInt (.. % -target -value) 10))}]])

(defn- weight-field [label value on-change]
  [:label.field
   [:span label]
   [:input {:type "number" :value value :step "0.01"
            :on-change #(on-change (js/parseFloat (.. % -target -value)))}]])

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
                                       (rf/dispatch [:import-league {:provider "sleeper" :league-id @league-id}]))}
         "Load from Sleeper"]]])))

(defn- league-config []
  (let [cfg @(rf/subscribe [:config])]
    [:section.settings-card
     [:h3 "League"]
     [:div.fields
      [num-field "Teams" (:num-teams cfg) #(rf/dispatch [:apply-config {:num-teams %}])]
      [num-field "Budget $" (:starting-bankroll cfg) #(rf/dispatch [:apply-config {:starting-bankroll %}])]
      [num-field "Tiers" (:num-tiers cfg) #(rf/dispatch [:apply-config {:num-tiers %}])]]]))

(defn- custom-scoring-editor [scoring]
  [:div.scoring-groups
   (for [{:keys [group stats]} db/scoring-catalog]
     ^{:key group}
     [:div.scoring-group
      [:h4 group]
      [:div.fields
       (for [[stat-key label] stats]
         ^{:key stat-key}
         [weight-field label (get scoring stat-key 0)
          #(rf/dispatch [:set-scoring-weight stat-key %])])]])])

(defn- scoring-config []
  (let [cfg    @(rf/subscribe [:config])
        mode   @(rf/subscribe [:scoring-mode])
        mode-s (name mode)]
    [:section.settings-card
     [:h3 "Scoring"]
     [:label.field
      [:span "Preset"]
      [:select {:value mode-s
                :on-change #(let [v (.. % -target -value)]
                              (if (= v "custom")
                                (rf/dispatch [:enable-custom-scoring])
                                (rf/dispatch [:select-scoring-preset (keyword v)])))}
       [:option {:value "standard"} "Standard"]
       [:option {:value "half-ppr"} "Half PPR"]
       [:option {:value "ppr"} "PPR"]
       [:option {:value "custom"} "Custom"]]]
     (when (= mode :custom)
       [custom-scoring-editor (:scoring cfg)])]))

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
