(ns draft-day.views.league-test
  (:require [cljs.test :refer [deftest is use-fixtures]]
            [re-frame.core :as rf]
            [re-frame.db :as rdb]
            [draft-day.db :as db]
            [draft-day.events]
            [draft-day.subs]
            [draft-day.test-render :refer [render]]
            [draft-day.views.league :as league]))

(use-fixtures :each
  {:before (fn []
             (rf/clear-subscription-cache!)
             (reset! rdb/app-db (db/default-db)))
   :after  (fn []
             (rf/clear-subscription-cache!)
             (reset! rdb/app-db (db/default-db)))})

(defn- synced! []
  (swap! rdb/app-db assoc
         :players [{:player-id "00-qb" :player-name "Jalen Hurts" :position "QB" :team "PHI"
                    :ids {:sleeper "11"}}
                   {:player-id "00-wr" :player-name "Ja'Marr Chase" :position "WR" :team "CIN"
                    :ids {:sleeper "22"}}]
         :leagues {"sleeper:1"
                   {:provider "sleeper" :league-id "1" :my-roster-id 2
                    :sync {:provider "sleeper"
                           :waiver {:type "faab" :budget 100}
                           :roster-positions ["QB" "WR" "BENCH"]
                           :teams [{:roster-id 1 :name "Taco Corp" :wins 1 :losses 1
                                    :faab-left 88 :player-ids ["22"] :active-ids ["22"]
                                    :starter-ids ["0" "22"]}
                                   {:roster-id 2 :name "Gridiron Gremlins" :wins 2 :losses 0
                                    :faab-left 74 :player-ids ["11"] :active-ids ["11"]
                                    :starter-ids ["11" "0"]}]}}}
         :active-league "sleeper:1")
  (rf/clear-subscription-cache!))

(deftest every-team-is-listed-in-standings-order-with-its-money
  (synced!)
  (let [rosters @(rf/subscribe [:league-rosters])]
    (is (= ["Gridiron Gremlins" "Taco Corp"] (mapv (comp :name :team) rosters)))
    (is (:mine? (first rosters)))
    (is (= "Ja'Marr Chase" (get-in (second rosters) [:starters 0 :player-name]))
        "a provider id resolves to the player the universe knows"))
  (let [html (render league/season-view)]
    (is (re-find #"\$74" html))
    (is (re-find #"2–0" html))
    (is (re-find #"\(You\)" html))))

(deftest a-league-with-no-sync-says-where-to-get-one
  (is (re-find #"Sync a league" (render league/season-view))))
