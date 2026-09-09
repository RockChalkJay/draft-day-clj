(ns draft-day.core
  "re-frame entry point: wires events/subs, mounts the app, boots data load."
  (:require [reagent.core :as r]
            [reagent.dom.client :as rdomc]
            [re-frame.core :as rf]
            [draft-day.db :as db]
            [draft-day.events]
            [draft-day.subs]
            [draft-day.views.board :as board]
            [draft-day.views.controls :as controls]
            [draft-day.views.roster :as roster]
            [draft-day.views.watchlist :as watchlist]
            [draft-day.views.columns :as columns]
            [draft-day.views.settings :as settings]
            [draft-day.views.waivers :as waivers]
            [draft-day.views.compare :as compare]
            [draft-day.views.modal :as modal]
            [draft-day.views.player-detail :as player-detail]))

(defn- fmt-mult [x] (str "×" (.toFixed (or x 1) 2)))

(defn league-switcher
  "Which league the whole app is about, on every tab.

  A dropdown once there is more than one, a plain label when there is one, and a
  route to Settings when there is none — because a select with a single option is
  a control that asks you to confirm the only possible answer, and one with no
  options is a control that cannot be used."
  []
  (let [leagues @(rf/subscribe [:league-list])
        active  @(rf/subscribe [:active-league-key])
        {:keys [league-name my-team-name]} @(rf/subscribe [:account])]
    [:div.league-switcher
     (cond
       (empty? leagues)
       [:button.link {:on-click #(rf/dispatch [:set-view :settings])} "Connect a league"]

       (= 1 (count leagues))
       [:span.league-label [:b (or league-name "League")]]

       :else
       [:select {:value (str active)
                 :title "Switch league — the board, the prices and the waiver wire all follow"
                 :on-change #(rf/dispatch [:set-active-league (.. % -target -value)])}
        (for [[k e] leagues]
          ^{:key k} [:option {:value k} (or (:name e) (:league-id e))])])
     (when my-team-name [:span.league-team my-team-name])]))

(defn- header []
  (let [market  @(rf/subscribe [:market])
        my-team @(rf/subscribe [:my-team])
        max-bid @(rf/subscribe [:my-max-bid])
        view    @(rf/subscribe [:view])
        status  @(rf/subscribe [:status])
        ;; the banded figure the board prices at, straight from the server —
        ;; multiplying :inflation by :market-heat here skipped the band and read
        ;; ×0.40 where the board was pricing at 0.50
        infl    (or (:market-multiplier market)
                    (* (or (:inflation market) 1) (or (:market-heat market) 1)))]
    [:header.top
     [:div.brand "🏈 Draft Day"]
     [:nav.views
      (map (fn [[v label]]
             ^{:key v}
             [:button {:class (when (= view v) "on") :on-click #(rf/dispatch [:set-view v])} label])
           [[:board "Board"] [:waivers "Waivers"] [:league "League"] [:settings "Settings"]])]
     [league-switcher]
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
     [:button.start-draft {:on-click #(rf/dispatch [:show-modal {:kind :start-draft}])} "Start Draft"]]))

(defn- board-view []
  [:div.board-view
   [:div.three-panels
    [:div.watchlist-col [watchlist/watchlist-panel]]
    [:div.tile-col [controls/nominate-tile]]
    [:aside.roster-col [roster/my-roster]]]
   [:details.col-details
    [:summary "⚙ Columns"]
    [columns/column-picker]]
   [board/board]])

(defn app []
  ;; The one document-level key handler in the app, and it is here because this
  ;; is the only component that knows what is on screen at once. The compare
  ;; tile used to own its own; a second one for the modal would have meant
  ;; Escape closing the modal and clearing the comparison behind it in one
  ;; keystroke, with the order deciding which. `:escape-pressed` holds the
  ;; precedence instead.
  (r/with-let [on-key (fn [e]
                        (when (= "Escape" (.-key e))
                          (rf/dispatch [:escape-pressed])))
               _      (.addEventListener js/document "keydown" on-key)]
    (let [view  @(rf/subscribe [:view])
          modal @(rf/subscribe [:modal])
          kind  (db/modal-kind modal)]
      [:div.app
       [header]
       [:main
        (case view
          :league   [roster/league-view]
          :settings [settings/settings]
          :waivers  [waivers/waivers-view]
          [board-view])]
       ;; Mounted here rather than inside the waivers view because it is
       ;; `position: fixed` and needs no place in that DOM — and because putting
       ;; it there would make `views.waivers` and `views.compare` require each
       ;; other, which ClojureScript will not load.
       (when (= view :waivers)
         [compare/compare-tile])
       ;; The detail modal is mounted for the same reason, one step further out:
       ;; it requires `views.compare`, which requires `views.waivers`, so the two
       ;; board surfaces reach it by dispatching `:show-modal` and never by
       ;; requiring it. Do not "tidy" this into the waivers view.
       (when (= :player-detail kind)
         [player-detail/player-detail-modal (:player-id modal)])
       (when (= :start-draft kind)
         [modal/start-draft-modal])
       (when (= :reset-cache kind)
         [modal/reset-cache-modal])])
    (finally
      (.removeEventListener js/document "keydown" on-key))))

(defonce root (rdomc/create-root (.getElementById js/document "app")))

(defn ^:export init []
  (rf/dispatch-sync [:boot])
  (rdomc/render root [app]))
