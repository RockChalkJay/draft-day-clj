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

(def ^:private players
  [{:player-id "qb" :player-name "Jalen Hurts" :position "QB" :team "PHI" :week-points 21.4
    :ids {:sleeper "1"}}
   {:player-id "wr" :player-name "DeVonta Smith" :position "WR" :team "PHI" :ids {:sleeper "2"}}
   {:player-id "rb" :player-name "Tyler Allgeier" :position "RB" :team "ATL" :ros-points 71.0
    :ids {:sleeper "3"}}
   {:player-id "ir" :player-name "Nico Collins" :position "WR" :team "HOU" :ids {:sleeper "4"}}])

(deftest the-roster-comes-in-three-blocks-flagged-for-their-cells
  (let [[[_ starters] [_ bench] [_ parked]]
        (team/roster-groups {:starters [{:player-id "qb" :slot "QB"}]
                             :bench    [{:player-id "rb"}]
                             :parked   [{:player-id "ir"}]})]
    (is (= [["qb" true "QB"]] (mapv (juxt :player-id :starter? :slot) starters)))
    (is (= [["rb" nil nil]] (mapv (juxt :player-id :starter? :parked?) bench)))
    (is (= [["ir" true]] (mapv (juxt :player-id :parked?) parked)))))

(deftest the-slot-cell-names-the-seat
  (is (= "FLEX" (last (team/team-cell :slot {:slot "FLEX"} 3))))
  (is (= "BN" (last (team/team-cell :slot {} 3))))
  (is (= "IR" (last (team/team-cell :slot {:parked? true} 3))))
  (is (= "–" (last (team/team-cell :slot {:starter? true} 3)))
      "a starter whose seat is unknown is not labelled bench"))

(defn- with-synced-team!
  ([waivers] (with-synced-team! waivers 1))
  ([waivers mine]
   (swap! rdb/app-db assoc
          :leagues {"sleeper:1" {:provider "sleeper" :league-id "1" :my-roster-id mine
                                 :sync {:provider "sleeper"
                                        :roster-positions ["QB" "FLEX" "BENCH"]
                                        :teams [{:roster-id 1 :name "Gridiron Gremlins"
                                                 :wins 2 :losses 0 :waiver-position 9
                                                 :player-ids ["1" "2" "3" "4" "99"]
                                                 :active-ids ["1" "2" "3" "99"]
                                                 :starter-ids ["1" "2"]}
                                                {:roster-id 2 :name "Empty"
                                                 :player-ids [] :active-ids []}]}}}
          :active-league "sleeper:1"
          :waivers waivers)
   (rf/clear-subscription-cache!)))

(deftest the-view-says-which-state-it-is-in
  (is (re-find #"Sync a league" (render team/team-view)))
  (with-synced-team! nil nil)
  (is (re-find #"Pick your team" (render team/team-view)))
  (with-synced-team! nil)
  (is (re-find #"Loading your roster" (render team/team-view)) "no players to name yet")
  (swap! rdb/app-db assoc :players players)
  (rf/clear-subscription-cache!)
  (is (re-find #"Loading your roster" (render team/team-view)) "nor the board's rows")
  (with-synced-team! {:my-roster-players []} 2)
  (is (re-find #"holds nobody" (render team/team-view))))

(deftest a-synced-roster-draws-its-blocks-its-record-and-its-drop
  (swap! rdb/app-db assoc :players players)
  (with-synced-team! {:my-roster-players players
                      :drop-candidate {:player-id "rb" :player-name "Tyler Allgeier"
                                       :position "RB" :ros-points 71.0}})
  (let [{:keys [starters bench parked]} @(rf/subscribe [:my-roster])]
    (is (= [["qb" "QB"] ["wr" "FLEX"]] (mapv (juxt :player-id :slot) starters)))
    (is (= ["rb" "99"] (mapv :player-id bench))
        "a player nobody can resolve stays on the roster he is on")
    (is (= ["ir"] (mapv :player-id parked))))
  (let [html (render team/team-view)]
    (is (re-find #"Gridiron Gremlins" html))
    (is (re-find #"2–0" html))
    (is (re-find #"Waiver priority 9" html))
    (is (re-find #"Starters" html))
    (is (re-find #"IR" html))
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
            "Best lineup by projection: +11.60"]
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
  (with-synced-team! {:my-roster-players players})
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
    (is (re-find #"110.20 – 118.20" html))))
