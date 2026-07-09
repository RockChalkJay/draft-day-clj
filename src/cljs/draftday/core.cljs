(ns draftday.core
  "re-frame entry point: wires events/subs, mounts the app, boots data load."
  (:require [reagent.dom.client :as rdomc]
            [re-frame.core :as rf]
            [draftday.events]
            [draftday.subs]
            [draftday.views.board :as board]
            [draftday.views.controls :as controls]
            [draftday.views.roster :as roster]
            [draftday.views.columns :as columns]
            [draftday.views.settings :as settings]
            [draftday.views.modal :as modal]))

(defn- fmt-mult [x] (str "×" (.toFixed (or x 1) 2)))

(defn- header []
  (let [market  @(rf/subscribe [:market])
        my-team @(rf/subscribe [:my-team])
        max-bid @(rf/subscribe [:my-max-bid])
        view    @(rf/subscribe [:view])
        status  @(rf/subscribe [:status])
        infl    (* (or (:inflation market) 1) (or (:market-heat market) 1))]
    [:header.top
     [:div.brand "🏈 Draft Day"]
     [:nav.views
      (for [[v label] [[:board "Board"] [:league "League"] [:settings "Settings"]]]
        ^{:key v}
        [:button {:class (when (= view v) "on") :on-click #(rf/dispatch [:set-view v])} label])]
     [:div.status status]
     [:div.stats
      [:div.stat {:title "Market inflation × phase decay"}
       [:span.stat-label "Market"] [:span.stat-val (fmt-mult infl)]]
      [:div.stat {:title "Σ(price paid − value) — rising = room overpaying"}
       [:span.stat-label "Infl Idx"]
       [:span.stat-val {:class (cond (> (or (:inflation-index market) 0) 0) "warn"
                                     (< (or (:inflation-index market) 0) 0) "good")}
        (str "$" (js/Math.round (or (:inflation-index market) 0)))]]
      [:div.stat [:span.stat-label "Bankroll"] [:span.stat-val.good (str "$" (:bankroll my-team))]]
      [:div.stat [:span.stat-label "Max Bid"] [:span.stat-val.good (str "$" max-bid)]]]
     [:button.start-draft {:on-click #(rf/dispatch [:show-modal :start-draft])} "Start Draft"]]))

(defn- board-view []
  [:div.board-view
   [controls/profile-switcher]
   [controls/suggestions]
   [:div.board-layout
    [:div.board-main
     [controls/nominate-bar]
     [:details.col-details
      [:summary "⚙ Columns"]
      [columns/column-picker]]
     [board/board]]
    [:aside.board-side
     [roster/my-roster]]]])

(defn app []
  (let [view  @(rf/subscribe [:view])
        modal @(rf/subscribe [:modal])]
    [:div.app
     [header]
     [:main
      (case view
        :league   [roster/league-view]
        :settings [settings/settings]
        [board-view])]
     (when (= modal :start-draft)
       [modal/start-draft-modal])]))

(defonce root (rdomc/create-root (.getElementById js/document "app")))

(defn ^:export init []
  (rf/dispatch-sync [:boot])
  (rdomc/render root [app]))
