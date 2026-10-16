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
         (call "my_team" {} (fx/deps {:body {:error "sync failed: 502"}
                                      :reply {:error "sync failed: 502"}})))))

(deftest the-board-is-read-only-when-a-tool-needs-it
  (let [boards (atom 0)
        server {:waivers-reply (fn [_] (swap! boards inc) fx/reply)
                :players       (constantly fx/players)
                :news          (constantly {:news [] :errors []})}]
    (with-redefs [context/league-context (constantly fx/body)]
      (let [deps (tools/deps server nil)]
        (call "player_news" {:player "p1"} deps)
        (is (zero? @boards) "news needs no board")
        (call "my_team" {} deps)
        (call "waiver_targets" {} deps)
        (is (= 1 @boards) "one board per request, however many tools read it")))
    (with-redefs [context/league-context (constantly {:error "no league"})]
      (let [deps (tools/deps server nil)]
        (is (= {:error "no league"} (call "waiver_targets" {} deps)))
        (is (= 1 @boards) "no board is priced for a league that did not load")))))

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
