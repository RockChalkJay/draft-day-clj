(ns draft-day.sensei.fixtures
  "A small synced league and the waiver reply for it, shaped as
  `sensei.context/as-posted` leaves them: string providers, keyword keys.")

(def players
  "The universe. Ids match across spaces, so the crosswalk is the identity."
  [{:player-id "p1" :player-name "Alpha Back" :position "RB" :team "KC"
    :ids {:sleeper "p1" :espn 101}}
   {:player-id "p2" :player-name "Bravo Wide" :position "WR" :team "BUF"
    :ids {:sleeper "p2" :espn 102}}
   {:player-id "p3" :player-name "Charlie End" :position "TE" :team "DET"
    :ids {:sleeper "p3"}
    :sleeper/injury-status "IR" :sleeper/injury-notes "Placed on IR Tuesday"}
   {:player-id "f1" :player-name "Free One" :position "WR" :team "NE"
    :ids {:sleeper "f1" :espn 201}}
   {:player-id "f2" :player-name "Free Two" :position "RB" :team "NYJ"
    :ids {:sleeper "f2"}}
   {:player-id "f3" :player-name "Free Three" :position "WR" :team "MIA"
    :ids {:sleeper "f3"}}])

(def league
  {:provider         "sleeper"
   :league-id        "123"
   :roster-positions ["RB" "WR" "BENCH" "BENCH"]
   :waiver           {:type "faab" :budget 100}
   :teams            [{:roster-id 1 :name "Mine" :wins 3 :losses 2
                       :player-ids ["p1" "p2" "p3"] :active-ids ["p1" "p2"]
                       :starter-ids ["p1" "0"]}
                      {:roster-id 2 :name "Rival" :player-ids [] :active-ids []
                       :starter-ids []}]})

(def body
  {:scoring "ppr" :num-teams 2 :league league :my-roster-id 1
   :roster {:rb 1 :wr 1 :bench 2} :roster-size 4})

(def free-agents
  [{:player-id "f1" :player-name "Free One" :position "WR" :team "NE"
    :lineup-upgrade 0.0 :upgrade 12.345 :ros-points 80.0 :week-points 9.87
    :week/opponent "NYJ" :week/home? false :season-pos-rank 40
    :bid 7 :typical-bid 5 :rivals 3
    :sleeper/injury-status "Questionable" :sleeper/injury-body-part "Ankle"}
   {:player-id "f2" :player-name "Free Two" :position "RB" :team "NYJ"
    :lineup-upgrade 15.0 :upgrade 20.0 :ros-points 90.0 :trend 1.2345}
   {:player-id "f3" :player-name "Free Three" :position "WR" :team "MIA"
    :lineup-upgrade 15.0 :upgrade 30.0 :ros-points 70.0}])

(def reply
  {:players           free-agents
   :my-roster-players [{:player-id "p1" :player-name "Alpha Back" :position "RB"
                        :ros-points 150.0 :week-points 14.2}
                       {:player-id "p2" :player-name "Bravo Wide" :position "WR"
                        :ros-points 60.0}
                       {:player-id "p3" :player-name "Charlie End" :position "TE"
                        :sleeper/injury-status "IR"}]
   :drop-candidate    {:player-id "p2" :player-name "Bravo Wide" :position "WR"
                       :ros-points 60.0}
   :faab              {:type "faab" :budget 100 :left 60 :rival-max 95}
   :claims-left       nil
   :week              7
   :through-week      6})

(defn news [espn-id] {:news [{:headline (str "note " espn-id)}] :errors []})

(defn deps
  "Tool deps over the fixtures, any of `:body`, `:reply` and `:news` replaced."
  ([] (deps {}))
  ([overrides]
   (let [m (merge {:body body :reply reply :news news} overrides)]
     {:body    (delay (:body m))
      :reply   (delay (:reply m))
      :players (delay players)
      :news    (:news m)})))
