(ns draft-day.views.header-test
  (:require [cljs.test :refer [deftest is use-fixtures]]
            [re-frame.core :as rf]
            [re-frame.db :as rdb]
            [draft-day.db :as db]
            [draft-day.events]
            [draft-day.subs]
            [draft-day.test-render :refer [render]]
            [draft-day.views.header :as header]))

(use-fixtures :each
  {:before (fn []
             (rf/clear-subscription-cache!)
             (reset! rdb/app-db (db/default-db)))
   :after  (fn []
             (rf/clear-subscription-cache!)
             (reset! rdb/app-db (db/default-db)))})

(deftest while-the-week-is-loading-neither-half-is-drawn
  ;; Drawing the draft half first put Start Draft in front of a manager in week
  ;; ten.
  (let [html (render header/header)]
    (is (not (re-find #"Start Draft|Board|Matchup" html)))))

(deftest once-it-is-known-its-half-is
  (swap! rdb/app-db assoc :universe {:through-week 0} :view :board)
  (is (re-find #"Start Draft" (render header/header))))

(deftest a-league-on-a-host-with-no-matchup-board-has-no-matchup-tab
  (swap! rdb/app-db assoc :universe {:through-week 3} :view :waivers
         :active-league "yahoo:1" :leagues {"yahoo:1" {:provider "yahoo" :league-id "1"}})
  (let [html (render header/header)]
    (is (re-find #"Waivers" html))
    (is (not (re-find #"Matchup" html)))))

(deftest an-espn-league-s-season-has-one
  (swap! rdb/app-db assoc :universe {:through-week 3} :view :waivers
         :active-league "espn:1" :leagues {"espn:1" {:provider "espn" :league-id "1"}})
  (is (re-find #"Matchup" (render header/header))))

(deftest a-truncated-status-can-still-be-read
  ;; The header squeezes the status first, so a failure has to survive on hover.
  (swap! rdb/app-db assoc :status "Rankings update failed: 502")
  (is (re-find #"\[:div.status \{:title \"Rankings update failed: 502\"\}"
               (render header/header))))

(deftest an-unreported-budget-is-a-dash-not-zero
  (swap! rdb/app-db assoc :universe {:through-week 3} :view :team
         :active-league "sleeper:1"
         :leagues {"sleeper:1" {:provider "sleeper" :league-id "1" :my-roster-id 1
                                :sync {:waiver {:type "faab" :budget 100}
                                       :teams [{:roster-id 1 :name "Mine"}]}}})
  (is (re-find #"–[^/]*/ \$100" (render header/header))))

(deftest the-season-row-carries-no-mode-chip-and-no-routine-status
  ;; Routine is whatever the last load wrote, not a message shape.
  (swap! rdb/app-db assoc :universe {:through-week 3} :view :waivers
         :status "642 players · cache" :universe-status "642 players · cache")
  (let [html (render header/header)]
    (is (not (re-find #"mode-tag" html)))
    (is (not (re-find #"642 players" html)))))

(deftest a-reworded-routine-status-is-still-routine
  (swap! rdb/app-db assoc :universe {:through-week 3} :view :waivers
         :status "all 642 loaded (cache)" :universe-status "all 642 loaded (cache)")
  (is (not (re-find #"all 642 loaded" (render header/header)))))

(deftest a-status-with-something-to-say-stays-on-the-season-row
  (swap! rdb/app-db assoc :universe {:through-week 3} :view :waivers
         :status "642 players · cache" :universe-status "642 players · cache")
  (rf/dispatch-sync [:recompute-failed (:recompute-seq @rdb/app-db) "502"])
  (rf/clear-subscription-cache!)
  (is (re-find #"Rankings update failed: 502" (render header/header))))

(deftest a-failed-sync-turns-the-icon-red-and-says-why
  (swap! rdb/app-db assoc :universe {:through-week 3} :view :waivers
         :active-league "sleeper:1"
         :leagues {"sleeper:1" {:provider "sleeper" :league-id "1"}})
  (rf/dispatch-sync [:league-sync-failed "sleeper:1" "502" 502])
  (rf/clear-subscription-cache!)
  (let [html (render header/sync-button)]
    (is (re-find #":class \"failed\"" html))
    (is (re-find #"Sync failed: 502" html))))

(deftest a-sync-in-flight-is-not-red
  (swap! rdb/app-db assoc :universe {:through-week 3} :view :waivers
         :active-league "sleeper:1" :sync-status "Syncing rosters…" :sync-error nil
         :leagues {"sleeper:1" {:provider "sleeper" :league-id "1"}})
  (let [html (render header/sync-button)]
    (is (not (re-find #"failed\"" html)))
    (is (re-find #"Syncing rosters" html))))
