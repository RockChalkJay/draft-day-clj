(ns draft-day.sensei.tools-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.db :as db]
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
