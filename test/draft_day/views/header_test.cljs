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
  ;; ten, and the "Go to season" link beside it stored an override for nothing.
  (let [html (render header/header)]
    (is (not (re-find #"Start Draft|Go to|Board|Matchup" html)))))

(deftest once-it-is-known-its-half-is
  (swap! rdb/app-db assoc :universe {:through-week 0} :view :board)
  (let [html (render header/header)]
    (is (re-find #"Start Draft" html))
    (is (re-find #"Go to season" html))))

(deftest an-espn-league-s-season-has-no-matchup-tab
  (swap! rdb/app-db assoc :universe {:through-week 3} :view :waivers
         :active-league "espn:1" :leagues {"espn:1" {:provider "espn" :league-id "1"}})
  (let [html (render header/header)]
    (is (re-find #"Waivers" html))
    (is (not (re-find #"Matchup" html)))))
