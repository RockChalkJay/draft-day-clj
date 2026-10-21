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
    :lineup-upgrade 15.0 :upgrade 30.0 :ros-points 70.0 :outlook-ppg 11.234}
   {:player-id "f4" :player-name "Free Kicker" :position "K" :team "MIN"
    :lineup-upgrade 40.0 :upgrade 40.0 :ros-points 120.0}])

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
   :drop-costs        {"p1" 150.0 "p2" 0.0}
   :faab              {:type "faab" :budget 100 :left 60 :rival-max 95}
   :claims-left       nil
   :week              7
   :through-week      6})

(defn news [espn-id] {:news [{:headline (str "note " espn-id)}] :errors []})

(def entry
  "A discovered Sleeper league, as `context/discover` leaves it: the account's
  user id and credentials ride along."
  {:key "sleeper:123" :provider :sleeper :league-id "123" :name "Main League"
   :season "2026" :num-teams 2 :user-id "u-me" :credentials {:username "me"}})

(def entry-2
  "A second league, on another host, whose user id is a SWID."
  {:key "espn:456" :provider :espn :league-id "456" :name "Work League"
   :season "2026" :num-teams 10 :user-id "{SWID-0000}"
   :credentials {:swid "SWID-0000" :espn-s2 "S2SECRET"}})

(def body-2 (assoc-in body [:league :teams 0 :name] "Work Team"))

(def reply-2 (assoc reply :week 8))

(defn deps
  "Tool deps over the fixtures, any of `:found`, `:body`, `:reply` (fns of a
  league entry) and `:news` replaced."
  ([] (deps {}))
  ([overrides]
   (let [m (merge {:found {:leagues [entry] :errors []}
                   :body  (constantly body)
                   :reply (constantly reply)
                   :news  news}
                  overrides)]
     {:accounts []
      :found    (delay (:found m))
      :body     (:body m)
      :reply    (:reply m)
      :players  (delay players)
      :news     (:news m)})))
