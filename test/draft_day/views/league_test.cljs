(ns draft-day.views.league-test
  (:require [cljs.test :refer [deftest is testing use-fixtures]]
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

(deftest only-a-name-the-detail-modal-can-open-is-a-button
  (let [row (fn [p bench? openable?] (render (fn [] (league/player-row p bench? openable?))))]
    (is (re-find #"name-btn" (row {:player-id "a" :player-name "Mine"} false true)))
    (is (not (re-find #"name-btn" (row {:player-id "b" :player-name "Theirs"} false false)))
        "another team's player is not on the waiver board the modal reads")))

(deftest a-starter-with-no-known-seat-shows-a-dash-not-his-position
  (let [slot (fn [p bench?] (last (nth (league/player-row p bench? false) 2)))]
    (is (= "FLEX" (slot {:slot "FLEX" :position "WR"} false)))
    (is (= "–" (slot {:position "WR"} false)))
    (is (= "WR" (slot {:position "WR"} true)) "below the bench line the column is a position")))

(deftest a-league-with-no-sync-says-where-to-get-one
  (is (re-find #"Sync a league" (render league/season-view))))

(deftest rosters-wait-for-the-players-that-name-them
  ;; Every id resolved to nobody before the universe landed, and a card of raw
  ;; ids reads exactly like a broken crosswalk.
  (synced!)
  (swap! rdb/app-db assoc :players [])
  (rf/clear-subscription-cache!)
  (let [html (render league/season-view)]
    (is (re-find #"Loading players" html))
    (is (not (re-find #"No player for id" html))))
  (testing "and say so when it is not coming"
    (swap! rdb/app-db assoc :universe-error "502")
    (is (re-find #"failed to load \(502\)" (render league/season-view)))))

(deftest a-balance-the-host-did-not-report-is-not-zero
  (let [sub-line (fn [left] (render (fn [] (league/team-card
                                            {:team {:name "T" :faab-left left}
                                             :starters [] :bench [] :parked []}
                                            true {}))))]
    (is (re-find #"\$0" (sub-line 0)) "a spent budget is a real $0")
    (is (not (re-find #"\$" (sub-line nil))))
    (is (re-find #"did not report" (sub-line nil)))))

(deftest a-teams-bidding-style-is-a-tag-with-its-evidence
  (let [card (fn [profile history?]
               (render (fn [] (league/team-card {:team {:name "T" :faab-left 76}
                                                 :starters [] :bench [] :parked []
                                                 :profile profile}
                                                true {} history?))))
        p    {:style :zero-flyer :bids 40 :per-week 4.1 :zero-share 0.79 :max-bid 1}]
    (is (re-find #"style-tag.*\"\$0 flyer\"\]" (card p true)))
    (is (re-find #"4.1 claims a week, 79% at \$0, top bid \$1" (card p true)) "as the tag's title")
    (is (re-find #"No bids yet" (card nil true)) "a manager the history holds no bid from")
    (is (not (re-find #"style-tag" (card nil false))) "no history to read, no tag")
    (is (re-find #"\[:b \{:title \"FAAB left\"\} \"\$76\"\]" (card nil false)) "the balance, bare")))

(deftest profiles-reach-the-league-tab-with-their-style-a-keyword
  (synced!)
  (swap! rdb/app-db assoc-in [:leagues "sleeper:1" :sync :bid-history]
         {:profiles [{:manager "roster:1" :style "zero-flyer" :bids 3 :per-week 4.1
                      :zero-share 0.79 :max-bid 1}]})
  (is (= [:zero-flyer] (mapv :style @(rf/subscribe [:league-bid-profiles]))))
  (is (re-find #"\$0 flyer" (render league/season-view))))
