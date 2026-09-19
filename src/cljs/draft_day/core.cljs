(ns draft-day.core
  "re-frame entry point: wires events/subs, mounts the app, boots data load."
  (:require [reagent.core :as r]
            [reagent.dom.client :as rdomc]
            [re-frame.core :as rf]
            [draft-day.db :as db]
            [draft-day.events]
            [draft-day.subs]
            [draft-day.views.board :as board]
            [draft-day.views.header :as header]
            [draft-day.views.controls :as controls]
            [draft-day.views.roster :as roster]
            [draft-day.views.watchlist :as watchlist]
            [draft-day.views.columns :as columns]
            [draft-day.views.settings :as settings]
            [draft-day.views.waivers :as waivers]
            [draft-day.views.matchup :as matchup]
            [draft-day.views.compare :as compare]
            [draft-day.views.modal :as modal]
            [draft-day.views.player-detail :as player-detail]))

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
       [header/header]
       [:main
        (case view
          :league   [roster/league-view]
          :settings [settings/settings]
          :waivers  [waivers/waivers-view]
          :matchup  [matchup/matchup-view]
          ;; Not placed until the phase is known — `events/place-view`.
          nil       [:p.muted "Loading…"]
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
