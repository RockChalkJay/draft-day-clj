(ns draft-day.views.team-test
  (:require [cljs.test :refer [deftest is testing use-fixtures]]
            [re-frame.core :as rf]
            [re-frame.db :as rdb]
            [draft-day.db :as db]
            [draft-day.events]
            [draft-day.subs]
            [draft-day.test-render :refer [render]]
            [draft-day.views.team :as team]))

(use-fixtures :each
  {:before (fn []
             (rf/clear-subscription-cache!)
             (reset! rdb/app-db (db/default-db)))
   :after  (fn []
             (rf/clear-subscription-cache!)
             (reset! rdb/app-db (db/default-db)))})

(def ^:private roster
  [{:player-id "qb" :player-name "Jalen Hurts" :position "QB" :starter? true :slot "QB"}
   {:player-id "wr" :player-name "DeVonta Smith" :position "WR" :starter? true :slot "FLEX"}
   {:player-id "rb" :player-name "Tyler Allgeier" :position "RB" :drop? true}
   {:player-id "ir" :player-name "Nico Collins" :position "WR" :parked? true}
   {:player-id "ghost" :unvalued? true}])

(def ^:private players
  [{:player-id "qb" :player-name "Jalen Hurts" :position "QB" :team "PHI" :week-points 21.4}
   {:player-id "wr" :player-name "DeVonta Smith" :position "WR" :team "PHI"}
   {:player-id "rb" :player-name "Tyler Allgeier" :position "RB" :team "ATL" :ros-points 71.0}
   {:player-id "ir" :player-name "Nico Collins" :position "WR" :team "HOU"}])

(deftest the-roster-comes-in-three-blocks-in-the-servers-order
  (let [[[_ starters] [_ bench] [_ parked]] (team/roster-groups roster players)]
    (is (= ["qb" "wr"] (mapv :player-id starters)))
    (is (= ["rb" "ghost"] (mapv :player-id bench))
        "a player the board cannot value stays on the roster he is on")
    (is (= ["ir"] (mapv :player-id parked)))
    (testing "the board row fills in what the roster row does not carry"
      (is (= "PHI" (:team (first starters))))
      (is (= "FLEX" (:slot (second starters))) "and the roster's own flags win"))))

(deftest a-record-is-said-only-when-the-league-reports-one
  (is (= "2–1" (team/record-label {:wins 2 :losses 1})))
  (is (nil? (team/record-label {:wins 2}))))

(deftest the-slot-cell-names-the-seat
  (is (= "FLEX" (last (team/team-cell :slot {:slot "FLEX"} 3))))
  (is (= "BN" (last (team/team-cell :slot {} 3))))
  (is (= "IR" (last (team/team-cell :slot {:parked? true} 3))))
  (is (= "–" (last (team/team-cell :slot {:starter? true} 3)))
      "a starter whose seat is unknown is not labelled bench"))

(defn- with-synced-team! [waivers]
  (swap! rdb/app-db assoc
         :leagues {"sleeper:1" {:provider "sleeper" :league-id "1" :my-roster-id 1
                                :sync {:teams [{:roster-id 1 :name "Gridiron Gremlins"
                                                :wins 2 :losses 0 :waiver-position 9}]}}}
         :active-league "sleeper:1"
         :waivers waivers)
  (rf/clear-subscription-cache!))

(deftest the-view-says-which-state-it-is-in
  (is (re-find #"Sync a league" (render team/team-view)))
  (with-synced-team! nil)
  (is (re-find #"Loading your roster" (render team/team-view)))
  (with-synced-team! {:my-roster nil})
  (is (re-find #"Pick your team" (render team/team-view)))
  (with-synced-team! {:my-roster [] :my-roster-players []})
  (is (re-find #"holds nobody" (render team/team-view))))

(deftest a-synced-roster-draws-its-blocks-its-record-and-its-drop
  (with-synced-team! {:my-roster roster :my-roster-players players})
  (let [html (render team/team-view)]
    (is (re-find #"Gridiron Gremlins" html))
    (is (re-find #"2–0" html))
    (is (re-find #"Waiver priority 9" html))
    (is (re-find #"Starters" html))
    (is (re-find #"IR / Taxi" html))
    (is (re-find #"drop-seat" html) "the seat a claim would cost is marked")
    (is (re-find #"Yours would come from" html))))

;; ---- side cards ----

(def ^:private my-side
  {:roster-id 1 :name "Gridiron Gremlins" :projected 110.2
   :starters [{:player-id "p1" :player-name "Tony Pollard" :slot "RB" :bye 3}
              {:player-id "p2" :player-name "Kyren Williams" :slot "RB"
               :sleeper/injury-status "Questionable"
               :kickoff/at "2026-09-20T17:00:00Z" :kickoff/status "STATUS_SCHEDULED"}
              {:player-id "p3" :player-name "Puka Nacua" :slot "WR"
               :kickoff/at "2026-09-18T00:15:00Z" :kickoff/status "STATUS_FINAL"}
              {:slot "FLEX" :empty? true}]
   :optimal {:projected {:gain 11.6}}})

(deftest the-lineup-check-names-byes-injuries-and-what-the-best-lineup-gains
  (let [issues (team/lineup-issues my-side 3)]
    (is (= ["Tony Pollard is on bye" "Kyren Williams — Questionable"
            "Best lineup by projection: +11.6"]
           (mapv :text issues)))
    (is (not (:warn? (second issues))) "a Questionable is not shouted like an IR")
    (is (:warn? (first issues)))
    (is (= :matchup (:link (last issues))) "the one issue fixed elsewhere links there"))
  (is (empty? (team/lineup-issues {:starters [] :optimal {:projected {:gain 0}}} 3))
      "a clean lineup has nothing to say"))

(deftest the-next-kickoff-skips-games-already-played
  (is (= "2026-09-20T17:00:00Z" (team/next-kickoff (:starters my-side)))
      "Thursday's final is behind us; Sunday's is next")
  (is (nil? (team/next-kickoff [{:kickoff/at "2026-09-18T00:15:00Z"
                                 :kickoff/status "STATUS_IN_PROGRESS"}]))))

(deftest best-claims-are-lineup-upgrades-in-the-boards-order
  (let [ps [{:player-id "bench" :upgrade 9.0 :lineup-upgrade 0.0}
            {:player-id "small" :lineup-upgrade 2.0 :upgrade 1.0}
            {:player-id "big" :lineup-upgrade 6.0 :upgrade 4.0}
            {:player-id "none"}]]
    (is (= ["big" "small"] (mapv :player-id (team/best-claims ps 3))))
    (is (= ["big"] (mapv :player-id (team/best-claims ps 1))))))

(deftest the-this-week-card-reads-the-managers-own-game
  (with-synced-team! {:my-roster roster :my-roster-players players})
  (swap! rdb/app-db assoc
         :matchup {:week 3
                   :matchups [{:matchup-id 1 :roster-ids [5 6]}
                              {:matchup-id 2 :roster-ids [2 1]}]
                   :teams [my-side
                           {:roster-id 2 :name "Waiver Wire Warriors" :wins 1 :losses 1
                            :projected 118.2}
                           {:roster-id 5 :name "Taco Corp"} {:roster-id 6 :name "Hurts So Good"}]}
         :matchup-pick 5)
  (rf/clear-subscription-cache!)
  (let [html (render team/this-week-card)]
    (is (re-find #"Waiver Wire Warriors \(1–1\)" html)
        "his own opponent, whatever game the Matchup tab has picked")
    (is (re-find #"110.2 – 118.2" html))))
