(ns draft-day.views.settings
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [draft-day.db :as db]))

(defn- numeric-field
  "A number input that keeps what you typed until you leave it.

  Binding a controlled input straight to the parsed value cannot hold an
  intermediate `-`, `0.` or empty string — the round-trip through app-db rewrites
  the DOM mid-keystroke, and parsing those yields NaN. A NaN weight used to
  serialize as null and 400 the rankings call, blanking the board. So local state
  holds the raw text, only a successful parse is dispatched, and blur re-syncs the
  field with whatever app-db actually holds."
  [_label _value _opts]
  (let [raw (r/atom nil)]
    (fn [label value {:keys [step min-v parse on-change]}]
      [:label.field
       [:span label]
       [:input (cond-> {:type      "number"
                        :step      step
                        :value     (if (some? @raw) @raw (str value))
                        :on-change (fn [e]
                                     (let [s (.. e -target -value)
                                           v (parse s)]
                                       (reset! raw s)
                                       (when-not (js/isNaN v) (on-change v))))
                        :on-blur   (fn [_] (reset! raw nil))}
                 min-v (assoc :min min-v))]])))

(defn- num-field [label value on-change]
  [numeric-field label value
   {:step "1" :min-v 0 :parse #(js/parseInt % 10) :on-change on-change}])

(defn- weight-field [label value on-change]
  [numeric-field label value
   {:step "0.01" :parse js/parseFloat :on-change on-change}])

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
      [num-field "Budget $" (:starting-bankroll cfg) #(rf/dispatch [:apply-config {:starting-bankroll %}])]]]))

(defn- budget-config []
  (let [cfg      @(rf/subscribe [:config])
        plan     (:budget-plan cfg)
        bankroll (:starting-bankroll cfg)
        total    (reduce + (map (fn [[_ k]] (get plan k 0)) db/budget-order))
        over?    (> total bankroll)]
    [:section.settings-card
     [:h3 "Budget Plan"]
     [:p.muted (str "Split your $" bankroll " across positions. "
                    "My Roster tracks your spend against it live.")]
     [:div.fields
      (map (fn [[label k]]
             ^{:key k}
             [num-field label (get plan k 0) #(rf/dispatch [:set-position-budget k %])])
           db/budget-order)]
     [:div.budget-tally {:class (when over? "over")}
      "Allocated " [:b (str "$" total)] (str " of $" bankroll)
      (cond
        over?              (str " · $" (- total bankroll) " over budget")
        (< total bankroll) (str " · $" (- bankroll total) " unallocated"))]]))

(defn- custom-scoring-editor [scoring]
  [:div.scoring-groups
   (map (fn [{:keys [group stats]}]
          ^{:key group}
          [:div.scoring-group
           [:h4 group]
           [:div.fields
            (map (fn [[stat-key label]]
                   ^{:key stat-key}
                   [weight-field label (get scoring stat-key 0)
                    #(rf/dispatch [:set-scoring-weight stat-key %])])
                 stats)]])
        db/scoring-catalog)])

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
      (map (fn [[k label]]
             ^{:key k}
             [num-field label (get roster k 0) #(set-r k %)])
           [[:qb "QB"] [:rb "RB"] [:wr "WR"] [:te "TE"]
            [:flex "FLEX"] [:k "K"] [:dst "DST"] [:bench "Bench"]])]]))

(defn- danger-zone []
  [:section.settings-card.danger-zone
   [:h3 "Danger Zone"]
   [:p.muted "Force the server to drop its cached player data and re-fetch live prices from Sleeper, FantasyPros and ESPN. Does not affect your draft or league settings."]
   [:button.danger {:on-click #(rf/dispatch [:show-modal :reset-cache])} "Reset Player Cache"]])

(defn settings []
  [:div.settings
   [sleeper-import]
   [league-config]
   [budget-config]
   [scoring-config]
   [roster-config]
   [danger-zone]])
