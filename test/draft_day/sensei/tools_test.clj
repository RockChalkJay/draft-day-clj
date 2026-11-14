(ns draft-day.sensei.tools-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [draft-day.db :as db]
            [draft-day.ingestion.season :as season]
            [draft-day.sensei.context :as context]
            [draft-day.sensei.fixtures :as fx]
            [draft-day.sensei.tools :as tools]))

(defn- call [tool-name args deps]
  (tools/run (tools/by-name tool-name) deps args))

(deftest a-row-is-compact-and-formatted
  (let [r (tools/row (first fx/free-agents))]
    (is (= "@ NYJ" (:opp r)))
    (is (= "Questionable (Ankle)" (:inj r)))
    (is (= 12.3 (:upgrade r)) "rounded to one place")
    (is (= 9.9 (:week r)))
    (is (= 40 (:pos-rank r)) "the season rank in season")
    (is (not-any? nil? (vals r)))
    (is (not (contains? r :ros-points)) "no surface shows raw rest-of-season points"))
  (is (= 1.23 (:trend (tools/row (second fx/free-agents)))) "the trend ratio keeps two places")
  (is (= [:name :player-id :position] (take 3 (keys (tools/row (first fx/free-agents)))))
      "the name leads, for the reader"))

(deftest a-row-drops-what-the-board-hides-at-his-position
  (let [line {:nflverse/season-to-date {:games 5 :stats {:rec 0.0 :pass_yd 1200.0}
                                        :usage {:targets 0.0 :carries 20.0}}}
        qb   (tools/row (merge {:player-id "q" :position "QB"} line))
        wr   (tools/row (merge {:player-id "w" :position "WR"} line))]
    (is (= 20.0 (:car qb)))
    (is (not-any? #(contains? qb %) [:tgt :rec]) "a quarterback is not targeted")
    (is (= 0.0 (:tgt wr)) "a receiver's zero is a fact")))

(deftest the-bid-model-stays-off-the-rows-while-it-is-on-hold
  (is (not-any? #(contains? (tools/row (first fx/free-agents)) %) [:bid :typical :rivals]))
  (with-redefs [db/bid-predictions? true]
    (is (= {:bid 7 :typical 5 :rivals 3}
           (select-keys (tools/row (first fx/free-agents)) [:bid :typical :rivals])))))

(deftest waiver-targets-follow-the-waivers-tab-rank
  (let [{:keys [result]} (call "waiver_targets" {} (fx/deps))]
    (is (= ["f3" "f2" "f1"] (map :player-id (:players result)))
        "lineup first, then upgrade")
    (is (= "Bravo Wide" (:drop-candidate result)))
    (is (= 7 (:week result))))
  (testing "a position and a limit"
    (is (= ["f3" "f1"] (map :player-id (:players (:result (call "waiver_targets" {:position "wr"} (fx/deps)))))))
    (is (= ["f3"] (map :player-id (:players (:result (call "waiver_targets" {:limit 1} (fx/deps))))))))
  (testing "a position the board does not have"
    (is (re-find #"position must be one of" (:error (call "waiver_targets" {:position "LB"} (fx/deps)))))))

(def ^:private board-agents
  "Free agents each on one board but the lineup one: b1 trending, b2 hot, b3 in
  a vacated role, b4 climbing the snap counts, b5 and b6 ranked by the experts
  at WR, b7 at RB. b8 has nothing but a lineup gain."
  [{:player-id "b1" :player-name "Bee One" :position "WR" :lineup-upgrade 0.0 :upgrade -5.0 :ros-points 40.0
    :trending/adds 9000}
   {:player-id "b2" :player-name "Bee Two" :position "RB" :lineup-upgrade 0.0 :upgrade -4.0 :ros-points 50.0
    :form-points 14.0 :season-ppg 8.0 :season-gp 5}
   {:player-id "b3" :player-name "Bee Three" :position "RB" :lineup-upgrade 0.0 :upgrade -6.0 :ros-points 30.0
    :role/since 4 :role/vacated-by "Starter Back" :role/vacated-status "IR"
    :snap-share 0.6 :snap-share-season 0.55}
   {:player-id "b4" :player-name "Bee Four" :position "WR" :lineup-upgrade 0.0 :upgrade -7.0 :ros-points 20.0
    :snap-share 0.7 :snap-share-season 0.4}
   {:player-id "b5" :player-name "Bee Five" :position "WR" :lineup-upgrade 0.0 :upgrade -8.0 :ros-points 10.0
    :fantasypros/wk-pos-rank "WR31"}
   {:player-id "b6" :player-name "Bee Six" :position "WR" :lineup-upgrade 0.0 :upgrade -9.0 :ros-points 9.0
    :fantasypros/wk-pos-rank "WR27"}
   {:player-id "b7" :player-name "Bee Seven" :position "RB" :lineup-upgrade 0.0 :upgrade -9.5 :ros-points 8.0
    :fantasypros/wk-pos-rank "RB40"}
   {:player-id "b8" :player-name "Bee Eight" :position "TE" :lineup-upgrade 6.0 :upgrade 6.0 :ros-points 70.0}])

(defn- board-deps [players]
  (fx/deps {:reply (constantly (assoc fx/reply :players players))}))

(defn- targets [players args]
  (:players (:result (call "waiver_targets" args (board-deps players)))))

(defn- on-of [rows id]
  (:on (first (filter #(= id (:player-id %)) rows))))

(deftest waiver-targets-surface-players-the-lineup-arithmetic-rates-zero
  (let [rows (targets board-agents {:limit 1})]
    (is (= "b8" (:player-id (first rows))) "the lineup board leads, as before")
    (is (= ["b8" "b1" "b2" "b3" "b4" "b7" "b6" "b5"] (map :player-id rows))
        "then adds, form, role (vacated role first), then the experts' best at each position in turn")
    (is (= ["lineup"] (on-of rows "b8")))
    (is (= ["adds"] (on-of rows "b1")))
    (is (= ["form"] (on-of rows "b2")))
    (is (= ["role"] (on-of rows "b3")))
    (is (= ["role"] (on-of rows "b4")) "a snap share fifteen points over his season's")
    (is (= ["experts"] (on-of rows "b6")))))

(deftest a-player-on-several-boards-is-listed-once-with-every-board
  (let [shared (assoc (nth board-agents 1) :trending/adds 100 :fantasypros/wk-pos-rank "RB12" :lineup-upgrade 9.0)
        rows   (targets [shared] {})]
    (is (= ["b2"] (map :player-id rows)))
    (is (= ["lineup" "adds" "form" "experts"] (:on (first rows))))))

(deftest the-boards-are-bounded-and-ordered
  (let [adds (map #(assoc (first board-agents) :player-id (str "a" %) :player-name (str "Add " %)
                          :trending/adds (* 100 %))
                  (range 1 8))
        rows (targets adds {:limit 1})]
    (is (= ["a7" "a6" "a5" "a4"] (map :player-id (filter #(some #{"adds"} (:on %)) rows)))
        "the four most added, most first")))

(deftest the-form-board-needs-two-games-and-a-rate-above-his-season-s
  (let [hot   {:player-id "h" :player-name "H" :position "RB" :form-points 14.0 :season-ppg 8.0 :season-gp 1}
        flat  (assoc hot :player-id "f" :season-gp 6 :form-points 7.0)
        ok    (assoc hot :player-id "o" :season-gp 6)]
    (is (= ["o"] (map :player-id (tools/form-board [hot flat ok])))
        "one game is not form, and below his average is not hot")))

(deftest a-position-narrows-every-board-and-widens-the-experts
  (let [wrs   (map #(assoc (first board-agents) :player-id (str "w" %) :player-name (str "W " %)
                           :fantasypros/wk-pos-rank (str "WR" (+ 20 %)))
                   (range 1 7))
        rows  (targets (concat wrs board-agents) {:position "wr" :limit 1})]
    (is (every? #(= "WR" (:position %)) rows))
    (is (= 4 (count (filter #(some #{"experts"} (:on %)) rows)))
        "four at the one position named, where the unfiltered list takes two at each")))

(deftest my-team-splits-the-roster-as-my-team-draws-it
  (let [{:keys [result]} (call "my_team" {} (fx/deps))]
    (is (= [["p1" "RB"]] (map (juxt :player-id :slot) (:starters result)))
        "the empty seat is dropped, the seat label kept")
    (is (= ["p2"] (map :player-id (:bench result))))
    (is (true? (:drop-candidate (first (:bench result)))))
    (is (= ["p3"] (map :player-id (:injured-reserve result))))
    (is (= {:type "faab" :budget 100 :left 60} (:faab result)) "no rival max: that is bidding")
    (is (= {:wins 3 :losses 2} (:record result)))
    (is (= "Mine" (:team result)))))

(deftest team-roster-reads-a-rival-and-never-the-owner
  (let [{:keys [result]} (call "team_roster" {:team "rival"} (fx/deps))]
    (is (= "Rival" (:team result)))
    (is (= ["r1"] (map :player-id (:starters result))))
    (is (nil? (:mine result)))
    (is (not (re-find #"u-rival" (pr-str result)))))
  (is (= "Mine" (:team (:result (call "team_roster" {:team "1"} (fx/deps))))) "by roster id")
  (is (true? (:mine (:result (call "team_roster" {} (fx/deps))))) "no team is mine")
  (is (re-find #"No team matches .*Choices: 1 Mine; 2 Rival"
               (:error (call "team_roster" {:team "nobody"} (fx/deps))))))

(deftest league-teams-is-standings-with-mine-marked
  (let [{:keys [result]} (call "league_teams" {} (fx/deps))]
    (is (= ["Rival" "Mine"] (map :team (:teams result))))
    (is (= [nil true] (map :mine (:teams result))))
    (is (not (re-find #"u-rival" (pr-str result))))))

(deftest matchup-pairs-me-with-my-opponent
  (let [{:keys [result]} (call "matchup" {} (fx/deps))]
    (is (= 7 (:week result)))
    (is (= "Mine" (get-in result [:mine :team])))
    (is (= "Rival" (get-in result [:opponent :team])))
    (is (= 16.1 (get-in result [:opponent :projected])))
    (is (= 14.2 (get-in result [:mine :starters 0 :proj]))))
  (testing "a bye"
    (let [bye (assoc fx/matchup :matchups [])
          {:keys [result]} (call "matchup" {} (fx/deps {:matchup (constantly bye)}))]
      (is (re-find #"bye" (:note result))))))

(deftest player-news-finds-a-player-by-name-or-id
  (let [{:keys [result]} (call "player_news" {:player "alpha back"} (fx/deps))]
    (is (= "p1" (get-in result [:player :player-id])))
    (is (= [{:headline "note 101"}] (:news result))))
  (is (= "p1" (get-in (call "player_news" {:player "p1"} (fx/deps)) [:result :player :player-id])))
  (testing "no ESPN id is a reason, not an error"
    (let [{:keys [result]} (call "player_news" {:player "Charlie End"} (fx/deps))]
      (is (re-find #"no ESPN id" (:reason result)))
      (is (= "Placed on IR Tuesday" (:injury-notes result)))))
  (testing "a feed that failed says so"
    (is (= ["timeout"] (get-in (call "player_news" {:player "p1"}
                                     (fx/deps {:news (fn [_] {:news [] :errors [{:message "timeout"}]})}))
                               [:result :errors])))))

(deftest player-news-asks-again-when-a-name-is-ambiguous-or-unknown
  (let [twins (fn [] (conj fx/players {:player-id "p9" :player-name "Alpha Back" :position "LB"}))]
    (is (re-find #"more than one player.*p1.*p9"
                 (:error (tools/run (tools/by-name "player_news")
                                    (assoc (fx/deps) :players (delay (twins)))
                                    {:player "Alpha Back"})))))
  (is (re-find #"no player matches" (:error (call "player_news" {:player "Nobody"} (fx/deps))))))

(deftest a-failed-context-is-the-models-to-read
  (is (= {:error "sync failed: 502"}
         (call "my_team" {} (fx/deps {:body  (constantly {:error "sync failed: 502"})
                                      :reply (constantly {:error "sync failed: 502"})})))))

(deftest the-board-is-read-only-when-a-tool-needs-it
  (let [boards (atom 0)
        server {:waivers-reply (fn [_] (swap! boards inc) fx/reply)
                :players       (constantly fx/players)
                :news          (constantly {:news [] :errors []})
                :env           (constantly {})}
        found  {:leagues [fx/entry fx/entry-2] :errors []}]
    (with-redefs [context/leagues        (constantly found)
                  context/league-context #(if (= (:key %) (:key fx/entry)) fx/body fx/body-2)]
      (let [deps (tools/deps server)]
        (call "player_news" {:player "p1"} deps)
        (call "leagues" {} deps)
        (is (zero? @boards) "neither news nor the league list needs a board")
        (call "my_team" {:league "sleeper:123"} deps)
        (call "waiver_targets" {:league "Main League"} deps)
        (is (= 1 @boards) "one board per league per request, however many tools read it")
        (call "waiver_targets" {:league "espn:456"} deps)
        (is (= 2 @boards) "another league is another board")
        (call "waiver_targets" {:position "LB" :league "espn:456"} deps)
        (is (= 2 @boards))))
    (with-redefs [context/leagues        (constantly {:leagues [fx/entry] :errors []})
                  context/league-context (constantly {:error "no league"})]
      (let [deps (tools/deps server)]
        (is (= {:error "no league"} (call "waiver_targets" {} deps)))
        (is (= 2 @boards) "no board is priced for a league that did not load")))))

(deftest a-bad-position-costs-no-board
  (let [deps (fx/deps {:reply (fn [_] (throw (ex-info "priced a board" {})))})]
    (is (re-find #"position must be one of" (:error (call "waiver_targets" {:position "LB"} deps))))))

;; ---- more than one league ----

(def ^:private two {:leagues [fx/entry fx/entry-2] :errors []})

(defn- two-leagues
  ([] (two-leagues two))
  ([found]
   (fx/deps {:found found
             :body  #(if (= (:key %) (:key fx/entry)) fx/body fx/body-2)
             :reply #(if (= (:key %) (:key fx/entry)) fx/reply fx/reply-2)})))

(deftest leagues-states-each-league-and-never-the-account
  (let [{:keys [result]} (call "leagues" {} (two-leagues (assoc two :errors ["ESPN league lookup failed: 502"])))]
    (is (= [{:league "sleeper:123" :name "Main League" :provider "Sleeper" :season "2026" :teams 2}
            {:league "espn:456" :name "Work League" :provider "ESPN" :season "2026" :teams 10}]
           (:leagues result)))
    (is (= ["ESPN league lookup failed: 502"] (:errors result)) "an account that could not be listed says so")
    (is (not-any? #(re-find % (pr-str result)) [#"SWID" #"S2SECRET" #"u-me" #"credentials" #"user-id"])
        "an ESPN user id is the SWID, half the cookie pair")))

(deftest no-leagues-says-how-to-get-some
  (is (re-find #"DRAFTDAY_SENSEI_SLEEPER_USER"
               (:error (call "leagues" {} (fx/deps {:found {:leagues [] :errors []}})))))
  (is (re-find #"ESPN league lookup failed: 401"
               (:error (call "leagues" {} (fx/deps {:found {:leagues [] :errors ["ESPN league lookup failed: 401"]}}))))))

(deftest a-league-tool-names-its-league-when-there-are-several
  (testing "none named"
    (doseq [t ["my_team" "waiver_targets"]]
      (is (re-find #"sleeper:123 \(Main League\); espn:456 \(Work League\)"
                   (:error (call t {} (two-leagues)))) t)))
  (testing "by key, or by name in any case"
    (is (= {:key "espn:456" :name "Work League"}
           (get-in (call "my_team" {:league "espn:456"} (two-leagues)) [:result :league])))
    (is (= {:key "espn:456" :name "Work League"}
           (get-in (call "waiver_targets" {:league "work LEAGUE"} (two-leagues)) [:result :league]))))
  (testing "each answers from its own board"
    (is (= 7 (get-in (call "waiver_targets" {:league "sleeper:123"} (two-leagues)) [:result :week])))
    (is (= 8 (get-in (call "waiver_targets" {:league "espn:456"} (two-leagues)) [:result :week])))
    (is (= "Mine" (get-in (call "my_team" {:league "sleeper:123"} (two-leagues)) [:result :team])))
    (is (= "Work Team" (get-in (call "my_team" {:league "espn:456"} (two-leagues)) [:result :team]))))
  (testing "one that is not there"
    (is (re-find #"No league matches \"nope\"" (:error (call "my_team" {:league "nope"} (two-leagues)))))))

(deftest the-only-league-needs-no-name-unless-another-could-not-be-listed
  (is (= "sleeper:123" (get-in (call "my_team" {} (fx/deps)) [:result :league :key])))
  (let [lonely (fx/deps {:found {:leagues [fx/entry] :errors ["ESPN league lookup failed: 502"]}})]
    (is (re-find #"Not listed: ESPN league lookup failed: 502" (:error (call "my_team" {} lonely)))
        "the only league there is, is not known to be the only league")
    (is (= "sleeper:123" (get-in (call "my_team" {:league "sleeper:123"} lonely) [:result :league :key]))
        "named, it answers")))

(deftest every-tool-has-a-schema-and-a-handler
  (doseq [t tools/tools]
    (is (= "object" (get-in t [:input-schema :type])) (:name t))
    (is (fn? (:handler t)) (:name t))
    (is (seq (:description t)) (:name t))))

(deftest player-news-keeps-the-newest-few
  (let [feed (fn [_] {:news (mapv #(hash-map :headline (str "n" %)) (range 10)) :errors []})
        news (fn [args] (get-in (call "player_news" args (fx/deps {:news feed})) [:result :news]))]
    (is (= 5 (count (news {:player "p1"}))))
    (is (= ["n0" "n1"] (map :headline (news {:player "p1" :limit 2}))) "newest first, as the feed sorts")))

(deftest waiver-targets-are-claims-not-streams
  (let [ids #(map :player-id (:players (:result (call "waiver_targets" %1 (fx/deps)))))]
    (is (= ["f3" "f2" "f1"] (ids {}))
        "the kicker leads the board on lineup 40 and still stays off the default list")
    (is (= ["f4"] (ids {:position "K"})) "named, he comes back")))

(deftest a-row-states-the-outlook-and-what-a-drop-costs
  (let [r (tools/row (assoc (nth fx/free-agents 2) :drop-cost 0.0))]
    (is (= 11.2 (:outlook r)))
    (is (= 0.0 (:drop-cost r)) "a free drop is a fact, not a gap"))
  (is (not (contains? (tools/row (first fx/free-agents)) :outlook))))

(deftest my-team-states-what-each-player-costs-to-drop
  (let [{:keys [result]} (call "my_team" {} (fx/deps))]
    (is (= 150.0 (:drop-cost (first (:starters result)))))
    (is (= 0.0 (:drop-cost (first (:bench result)))))))

(deftest evaluate-claim-prices-an-add-against-the-drop-candidate
  (let [{:keys [result]} (call "evaluate_claim" {:add "Free Three"} (fx/deps))]
    (is (= "Free Three" (get-in result [:add :name])))
    (is (= "Bravo Wide" (get-in result [:drop :name])) "the board's drop candidate by default")
    (is (= 10.0 (:lineup result)) "a 70 WR for a 60 WR in the WR seat")
    (is (= 10.0 (:upgrade result)))
    (is (= 0.0 (:lineup-this-week result)) "neither has a projection this week")
    (is (not-any? #(contains? result %) [:starts-this-week :displaces])
        "a free agent with no projection does not play")))

(deftest evaluate-claim-can-compare-a-different-drop
  (let [{:keys [result]} (call "evaluate_claim" {:add "f1" :drop "Alpha Back"} (fx/deps))]
    (is (= "Alpha Back" (get-in result [:drop :name])))
    (is (= -130.0 (:lineup result)) "a starter leaves and no one fills the RB seat")
    (is (= -70.0 (:upgrade result)))
    (is (= -4.3 (:lineup-this-week result)) "his 14.2 this week goes, and the claim's 9.9 fills the WR seat")
    (is (= 9.9 (get-in result [:add :week])) "the add's own projection is its own field")
    (is (= "WR" (:starts-this-week result)))
    (is (not (contains? result :displaces)) "the starter he replaces is the drop, not a displacement")))

(deftest evaluate-claim-names-whom-the-add-pushes-out-this-week
  (let [reply (-> fx/reply
                  (assoc :drop-candidate nil)
                  (update :my-roster-players
                          (fn [ps] (mapv #(cond-> % (= "p2" (:player-id %)) (assoc :week-points 5.0)) ps))))
        {:keys [result]} (call "evaluate_claim" {:add "f1"} (fx/deps {:reply (constantly reply)}))]
    (is (not (contains? result :drop)) "an open seat gives nobody up")
    (is (= 4.9 (:lineup-this-week result)) "9.87 in the WR seat over his 5.0")
    (is (= "WR" (:starts-this-week result)))
    (is (= "Bravo Wide" (get-in result [:displaces :name])))))

(deftest evaluate-claim-needs-the-managers-team
  (let [gone (assoc-in fx/body [:league :teams] [(get-in fx/body [:league :teams 1])])]
    (is (re-find #"team is not in this league's sync"
                 (:error (call "evaluate_claim" {:add "f1"} (fx/deps {:body (constantly gone)})))))))

(deftest evaluate-claim-asks-again-rather-than-guessing
  (is (re-find #"no free agent matches" (:error (call "evaluate_claim" {:add "Nobody"} (fx/deps)))))
  (is (re-find #"no player on the active roster matches"
               (:error (call "evaluate_claim" {:add "f1" :drop "Charlie End"} (fx/deps))))
      "a player on IR holds no active seat"))

(deftest a-blank-drop-is-no-drop-named
  (let [{:keys [result]} (call "evaluate_claim" {:add "f3" :drop "  "} (fx/deps))]
    (is (= "Bravo Wide" (get-in result [:drop :name])) "the board's drop candidate")))

(deftest a-blank-name-matches-nobody
  (is (empty? (tools/find-players [{:player-id "x" :player-name nil}] ""))
      "an unvalued row has no name to equal an empty query")
  (is (empty? (tools/find-players [{:player-id "x" :player-name nil}] "Jr."))
      "nor a query that normalizes to nothing"))

(deftest a-drop-named-by-a-suffix-alone-is-refused
  (let [body (-> fx/body
                 (update-in [:league :teams 0 :player-ids] conj "ghost")
                 (update-in [:league :teams 0 :active-ids] conj "ghost"))]
    (is (re-find #"no player on the active roster matches"
                 (:error (call "evaluate_claim" {:add "f1" :drop "Jr."}
                               (fx/deps {:body (constantly body)}))))
        "an unvalued player on the roster is not the drop")))

(deftest waiver-targets-say-which-positions-they-cover
  (is (= ["QB" "RB" "WR" "TE"] (:positions (:result (call "waiver_targets" {} (fx/deps))))))
  (is (= ["K"] (:positions (:result (call "waiver_targets" {:position "K"} (fx/deps)))))))

(deftest a-row-states-the-experts-rest-of-season-rank
  (let [p (assoc (first fx/free-agents) :fantasypros/ros-pos-rank "WR31"
                 :fantasypros/ros-rank-min 28 :fantasypros/ros-rank-max 40)]
    (is (= "WR31 (28-40)" (:ros-ecr (tools/row p)))))
  (is (= "WR31" (:ros-ecr (tools/row (assoc (first fx/free-agents) :fantasypros/ros-pos-rank "WR31")))))
  (is (not (contains? (tools/row (first fx/free-agents)) :ros-ecr)) "unranked: no key"))

(deftest a-row-states-the-experts-rank-for-this-week
  (let [p (assoc (first fx/free-agents) :fantasypros/wk-pos-rank "RB29" :fantasypros/wk-rank-min 28
                 :fantasypros/wk-rank-max 33 :fantasypros/wk-grade "C")]
    (is (= "RB29 (28-33) C" (:wk-ecr (tools/row p)))))
  (is (= "RB29" (:wk-ecr (tools/row (assoc (first fx/free-agents) :fantasypros/wk-pos-rank "RB29")))))
  (is (not (contains? (tools/row (first fx/free-agents)) :wk-ecr)) "unranked: no key")
  (is (str/includes? (tools/legend) "- wk-ecr:")))

(deftest a-row-states-snaps-and-a-taken-over-role
  (let [r (tools/row (assoc (first fx/free-agents) :snap-share 0.6527 :snap-share-season 0.31
                            :role/since 3 :role/vacated-by "De'Von Achane" :role/vacated-status "IR"))]
    (is (= 65 (:snap r)))
    (is (= 31 (:snap-season r)))
    (is (= "since wk 3, De'Von Achane IR" (:role r))))
  (is (not-any? #(contains? (tools/row (first fx/free-agents)) %) [:snap :snap-season :role])))

(deftest a-row-states-red-zone-counts-as-whole-numbers
  (let [r (tools/row (assoc (first fx/free-agents) :rz-rush 14.0 :rz-tgt 1.0))]
    (is (= 14 (:rz-rush r)))
    (is (= 1 (:rz-tgt r)))
    (is (not (contains? r :rz-pass)) "none recorded: no key")))

;; ---- defense vs position ----

(def ^:private allowed
  {"NYG" {"WR" {:rank 8 :of 32 :per-game 31.24 :recent-per-game 29.6 :games 7 :points 218.7}
          "RB" {:rank 30 :of 32 :per-game 14.0 :recent-per-game 12.0 :games 7 :points 98.0}}
   "NYJ" {"WR" {:rank 20 :of 32 :per-game 28.8 :recent-per-game 33.1 :games 8 :points 230.4}}})

(defn- dvp-deps []
  (fx/deps {:reply (constantly (assoc fx/reply :defense-vs-position allowed))}))

(deftest a-row-states-what-his-opponent-allows-his-position
  (is (= "WR 8th most (31.2/g)"
         (:opp-vs-pos (tools/row (assoc (first fx/free-agents)
                                        :opp-vs-pos (assoc (get-in allowed ["NYG" "WR"]) :team "NYG"))))))
  (is (= [:opp :opp-vs-pos :inj]
         (->> (keys (tools/row (assoc (first fx/free-agents) :opp-vs-pos {:rank 1 :of 32 :per-game 1.0})))
              (filter #{:opp :opp-vs-pos :inj})))
      "beside the opponent it is about")
  (is (not (contains? (tools/row (first fx/free-agents)) :opp-vs-pos)) "no line: no key")
  (is (str/includes? (tools/legend) "- opp-vs-pos:")))

(deftest defense-vs-position-compares-the-named-teams
  (let [{:keys [result]} (call "defense_vs_position" {:teams ["Giants" "nyj"] :position "WR"} (dvp-deps))]
    (is (= ["NYG" "NYJ"] (map :team (:teams result))))
    (is (= {:team "NYG" :name "New York Giants" :position "WR" :rank "8th most"
            :per-game 31.2 :recent 29.6 :games 7}
           (first (:teams result))))
    (is (= "this league's own" (:scoring result)))
    (is (= 6 (:through-week result)))))

(deftest defense-vs-position-lists-every-team-at-a-position-most-first
  (let [{:keys [result]} (call "defense_vs_position" {:position "WR"} (dvp-deps))]
    (is (= ["8th most" "13th fewest"] (map :rank (:teams result))))))

(deftest defense-vs-position-reads-every-position-of-a-named-team
  (let [{:keys [result]} (call "defense_vs_position" {:teams "New York Giants"} (dvp-deps))]
    (is (= ["RB" "WR"] (map :position (:teams result))) "in the board's position order")
    (is (= "3rd fewest" (:rank (first (:teams result)))))))

(deftest defense-vs-position-asks-again-rather-than-guessing-a-team
  (let [deps (fx/deps {:reply (fn [_] (throw (ex-info "priced a board" {})))})]
    (is (re-find #"More than one team matches \"new york\": NYG New York Giants, NYJ New York Jets"
                 (:error (call "defense_vs_position" {:teams ["New York"]} deps))))
    (is (re-find #"No team matches" (:error (call "defense_vs_position" {:teams ["Gotham"]} deps))))
    (is (re-find #"name a position, teams, or both" (:error (call "defense_vs_position" {} deps))))))

(deftest defense-vs-position-before-a-game-says-so
  (is (re-find #"no game has been played"
               (:error (call "defense_vs_position" {:position "WR"}
                             (fx/deps {:reply (constantly (assoc fx/reply :through-week 0))})))))
  (is (re-find #"game logs are unavailable"
               (:error (call "defense_vs_position" {:position "WR"} (fx/deps))))
      "weeks were played but no log arrived: not the same as no games"))

;; ---- league bids ----

(def ^:private history
  "This season's auctions and last season's, as `transactions/cached-history`
  reads them: ids in Sleeper's space, owner ids on every bid."
  {:seasons [{:season "2026" :league-id "123" :budget 100
              :auctions [{:week 3 :player-id "f1"
                          :bids [{:roster-id 2 :owner-id "u-rival" :amount 12 :won? true}
                                 {:roster-id 1 :owner-id "u-me" :amount 9 :won? false}]}
                         {:week 5 :player-id "f2"
                          :bids [{:roster-id 1 :owner-id "u-me" :amount 30 :won? true}]}
                         {:week 5 :player-id "f3"
                          :bids [{:roster-id 2 :owner-id "u-rival" :amount 4 :won? true}
                                 {:roster-id 1 :owner-id "u-me" :amount 3 :won? false}
                                 {:roster-id 1 :owner-id "u-me" :amount 1 :won? false}]}
                         {:week 6 :player-id "gone"
                          :bids [{:roster-id 2 :owner-id "u-rival" :amount 2 :won? true}]}]}
             {:season "2025" :league-id "99" :budget 100
              :auctions [{:week 2 :player-id "f2"
                          :bids [{:roster-id 2 :owner-id "u-rival" :amount 50 :won? true}]}]}]})

(defn- bids-deps []
  (fx/deps {:bid-history (fn [provider league-id season]
                           (when (= ["sleeper" "123"] [provider league-id]) history))}))

(deftest league-bids-states-what-each-auction-went-for
  (let [{:keys [result]} (call "league_bids" {} (bids-deps))
        [gone f2 f3 f1] (:bids result)]
    (is (= "2026" (:season result)))
    (is (= 100 (:budget result)))
    (is (= 4 (:auctions result)))
    (is (= [6 5 5 3] (map :week (:bids result))) "newest first, the bigger bid first within a week")
    (is (= {:week 3 :name "Free One" :position "WR" :team "NE" :winner "Rival"
            :won 12 :won-pct 12.0 :second 9 :bidders 2}
           f1))
    (is (= {:week 5 :name "Free Two" :position "RB" :team "NYJ" :winner "Mine"
            :won 30 :won-pct 30.0 :bidders 1}
           f2)
        "an uncontested claim has no second bid")
    (is (= 3 (:second f3)) "the runner-up is the best losing bid")
    (is (= {:week 6 :name "gone" :unvalued true :winner "Rival" :won 2 :won-pct 2.0 :bidders 1} gone)
        "a player the universe does not hold keeps his id")
    (is (not (str/includes? (pr-str result) "u-rival")) "an owner id is never stated")))

(deftest league-bids-summarize-each-position-and-last-season
  (let [{:keys [result]} (call "league_bids" {} (bids-deps))]
    (is (= [{:position "WR" :auctions 2 :median-pct 12.0 :p90-pct 12.0 :top-pct 12.0
             :contested 2 :contested-median-pct 12.0}
            {:position "RB" :auctions 1 :median-pct 30.0 :p90-pct 30.0 :top-pct 30.0
             :contested 0}]
           (:by-position result))
        "most auctions first; an unvalued player has no position to count under")
    (is (= 1 (:unplaced result)) "the player the universe lost is counted, not dropped silently")
    (is (= {:by-position [{:position "RB" :auctions 1 :median-pct 50.0 :p90-pct 50.0 :top-pct 50.0 :contested 0}]}
           (:last-season result)))))

(deftest league-bids-narrow-by-position-and-weeks
  (is (= ["Free One" "Free Three"]
         (sort (map :name (:bids (:result (call "league_bids" {:position "WR"} (bids-deps))))))))
  (is (= [6 5 5] (map :week (:bids (:result (call "league_bids" {:weeks 2} (bids-deps))))))
      "weeks 5 and 6, the last two of a season whose latest auctions ran in week 6"))

(deftest league-bids-say-why-there-are-none
  (is (re-find #"no bid history is cached"
               (:error (call "league_bids" {} (fx/deps)))))
  (let [espn (fx/deps {:found {:leagues [fx/entry-2] :errors []}
                       :body  (fn [_] (throw (ex-info "read the league" {})))})]
    (is (re-find #"ESPN does not publish past waiver bids" (:error (call "league_bids" {} espn)))
        "answered before the league is imported")))

(deftest league-bids-quantiles-are-nearest-rank
  (is (= 3 (tools/quantile [5 1 3 4 2] 0.5)))
  (is (= 5 (tools/quantile [5 1 3 4 2] 0.9)))
  (is (nil? (tools/quantile [] 0.5))))

;; ---- accurate experts ----

(def ^:private boards
  "Three finished seasons' top tens, cut short. Thorman's name is spelled two
  ways, as the vendor spells it."
  {2025 [{:id 317 :rank 1 :name "Justin Boone" :outlet "Yahoo" :site "sports.yahoo.com" :pos-ranks {"RB" 5 "WR" 1}}
         {:id 534 :rank 2 :name "Patrick Thorman" :outlet "Establish the Run" :site "establishtherun.com" :pos-ranks {"RB" 2 "WR" 2}}
         {:id 125 :rank 5 :name "Jeff Ratcliffe" :outlet "FTN" :site "www.ftnfantasy.com" :pos-ranks {"RB" 16 "WR" 12}}]
   2024 [{:id 120 :rank 3 :name "Sean Koerner" :outlet "The Action Network" :site "www.actionnetwork.com" :pos-ranks {"RB" 1 "WR" 9}}
         {:id 534 :rank 4 :name "Pat Thorman" :outlet "Establish the Run" :site "establishtherun.com" :pos-ranks {"RB" 3 "WR" 4}}
         {:id 125 :rank 5 :name "Jeff Ratcliffe" :outlet "FTN" :site "www.ftnfantasy.com" :pos-ranks {"RB" 2 "WR" 20}}]
   2023 [{:id 120 :rank 1 :name "Sean Koerner" :outlet "The Action Network" :site "www.actionnetwork.com" :pos-ranks {"RB" 1 "WR" 1}}
         {:id 534 :rank 4 :name "Pat Thorman" :outlet "Establish the Run" :site "establishtherun.com" :pos-ranks {"RB" 6 "WR" 3}}]})

(defn- accuracy-deps [boards]
  (fx/deps {:accuracy (fn [s] (some->> (get boards s) (hash-map :season s :experts)))}))

(defn- experts-call [args boards]
  (with-redefs [season/current (constantly 2026)]
    (call "accurate_experts" args (accuracy-deps boards))))

(deftest accurate-experts-are-those-who-repeat-in-the-top-ten
  (let [{:keys [result]} (experts-call {} boards)
        [thorman :as listed] (:experts result)]
    (is (= ["Patrick Thorman" "Sean Koerner" "Jeff Ratcliffe"] (map :name listed))
        "most finishes first, then best average rank")
    (is (= {:name "Patrick Thorman" :outlet "Establish the Run" :site "establishtherun.com"
            :top-ten 3 :ranks {"2025" 2 "2024" 4 "2023" 4} :avg-rank 3.3}
           thorman)
        "joined by id across spellings, named as the newest season names him")
    (is (= [2025 2024 2023] (:seasons result)) "the three seasons before this one")
    (is (= [{:name "Justin Boone" :outlet "Yahoo" :site "sports.yahoo.com" :rank 1}] (:last-season result))
        "one top-ten season is shown beside them, not among them")))

(deftest accurate-experts-at-a-position-are-ordered-by-it
  (let [listed (:experts (:result (experts-call {:position "rb"} boards)))]
    (is (= ["Sean Koerner" "Patrick Thorman" "Jeff Ratcliffe"] (map :name listed)))
    (is (= {"2024" 1 "2023" 1} (:pos-ranks (first listed))))
    (is (= 1.0 (:pos-avg (first listed))))))

(deftest accurate-experts-say-which-season-they-could-not-read
  (let [{:keys [result]} (experts-call {} (dissoc boards 2023))]
    (is (= [2023] (:unread result)))
    (is (= ["Patrick Thorman" "Jeff Ratcliffe"] (map :name (:experts result)))
        "a season unread is a finish nobody can count"))
  (is (re-find #"could not be read" (:error (experts-call {} {})))))
