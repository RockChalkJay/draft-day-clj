(ns draft-day.replay.sleeper-test
  "The replay harness's first tests. No network: every fixture is a hand-built
  Sleeper response shape."
  (:require [clojure.test :refer [deftest is testing]]
            [org.httpkit.client :as http]
            [draft-day.replay.sleeper :as s]))

;; ---- the fetch layer --------------------------------------------------------
;; Retries are exercised with a 1ms backoff; the real defaults would make these
;; tests take half a minute.

(def ^:private fast {:retries 3 :backoff-ms 1})

(defn- stub
  "Stand in for http-kit's client, answering `responses` in order (the last one
  repeats). Returns [calls-atom stub-fn]."
  [responses]
  (let [calls (atom 0)
        rs    (atom responses)]
    [calls (fn [_url _opts]
             (swap! calls inc)
             (let [r (first @rs)]
               (when (next @rs) (swap! rs rest))
               (delay r)))]))

(deftest a-200-with-a-null-body-is-not-found-not-data
  ;; Sleeper answers 200 with a literal JSON `null` for ids it does not know.
  ;; Parsed naively that is nil, which flows on looking exactly like an empty
  ;; league — the trap `ingestion/league_import/sleeper.clj` already calls out.
  (let [[calls f] (stub [{:status 200 :body "null"}])]
    (with-redefs [http/get f]
      (let [r (s/fetch "/league/nope" fast)]
        (is (false? (:ok? r)))
        (is (= :not-found (:reason r)))
        (is (= 1 @calls) "a definitive answer is not retried")))))

(deftest a-404-is-definitive
  (let [[calls f] (stub [{:status 404 :body ""}])]
    (with-redefs [http/get f]
      (is (= :not-found (:reason (s/fetch "/draft/gone" fast))))
      (is (= 1 @calls) "absence is not retried"))))

(deftest a-persistent-429-surfaces-as-throttled
  ;; The failure this whole layer exists for. Before, every non-200 came back as
  ;; nil and read as "no auction here", so a rate-limited crawl reported a data
  ;; drought instead of asking to slow down.
  (let [[calls f] (stub [{:status 429 :body ""}])]
    (with-redefs [http/get f]
      (let [r (s/fetch "/user/123/leagues/nfl/2024" fast)]
        (is (false? (:ok? r)))
        (is (= :throttled (:reason r)) "distinguishable from an empty result")
        (is (= 3 @calls) "and retried before giving up")))))

(deftest a-429-that-clears-succeeds
  (let [[calls f] (stub [{:status 429 :body ""}
                         {:status 200 :body "[{\"league_id\":\"lg1\"}]"}])]
    (with-redefs [http/get f]
      (let [r (s/fetch "/user/123/leagues/nfl/2024" fast)]
        (is (:ok? r))
        (is (= "lg1" (:league_id (first (:body r)))))
        (is (= 2 @calls))))))

(deftest a-good-body-parses
  (let [[_ f] (stub [{:status 200 :body "{\"draft_id\":\"d1\",\"type\":\"auction\"}"}])]
    (with-redefs [http/get f]
      (let [r (s/fetch "/draft/d1" fast)]
        (is (:ok? r))
        (is (= "d1" (get-in r [:body :draft_id])))
        (is (= {:draft_id "d1" :type "auction"} (s/body r)))))))

(deftest body-collapses-every-failure-to-nil
  (is (nil? (s/body {:ok? false :reason :throttled})))
  (is (nil? (s/body {:ok? false :reason :not-found})))
  (is (= [1 2] (s/body {:ok? true :body [1 2]}))))

;; ---- fixtures ---------------------------------------------------------------

(defn- pick
  ([n roster amt] (pick n roster amt "RB"))
  ([n roster amt pos]
   {:player_id (str "p" n)
    :pick_no   n
    :roster_id roster
    :metadata  (cond-> {:position pos}
                 amt (assoc :amount (str amt)))}))

(defn- picks
  "n picks spread over `teams` rosters, every one priced at `amt`."
  [n teams amt]
  (mapv #(pick % (inc (mod % teams)) amt) (range n)))

(defn- auction
  ([] (auction {}))
  ([overrides]
   (merge {:draft_id "d1" :season "2024" :type "auction" :status "complete"
           :league_id "lg1" :settings {:teams 12 :budget 200}}
          overrides)))

(defn- league
  ([] (league 1.0))
  ([rec] {:league_id "lg1" :scoring_settings {:rec rec}}))

;; ---- the gate ---------------------------------------------------------------

(deftest a-plain-clean-auction-is-accepted
  (let [d (s/auction-decision (auction) (picks 24 12 15) (league))]
    (is (:ok? d))
    (is (= :accepted (:reason d)))
    (is (= {:num-teams 12 :budget 200 :scoring-rec 1.0 :picks 24} (:meta d)))))

(deftest the-shapes-the-old-gate-refused-are-accepted-now
  ;; The whole point of widening. `replay/core` configures the engine from each
  ;; league's own settings via `league_import`, so none of this needed to be
  ;; uniform — and requiring it is what held the corpus at two drafts. The
  ;; $250 half-PPR case is the one already sitting in the cache, which could
  ;; never have passed the gate that was supposed to have collected it.
  (testing "a $250 budget"
    (is (:ok? (s/auction-decision (auction {:settings {:teams 12 :budget 250}})
                                  (picks 24 12 15) (league)))))
  (testing "half-PPR"
    (is (:ok? (s/auction-decision (auction) (picks 24 12 15) (league 0.5)))))
  (testing "standard scoring"
    (is (:ok? (s/auction-decision (auction) (picks 24 12 15) (league 0.0)))))
  (testing "ten teams"
    (is (:ok? (s/auction-decision (auction {:settings {:teams 10 :budget 100}})
                                  (picks 20 10 8) (league)))))
  (testing "an eight-team league"
    (is (:ok? (s/auction-decision (auction {:settings {:teams 8 :budget 300}})
                                  (picks 16 8 20) (league)))))
  (testing "a team that left money on the table"
    ;; the old gate required every roster to spend >=85% of budget, which calls a
    ;; legitimate way to lose an auction a corrupt record
    (let [thrifty (conj (picks 23 12 15) (pick 99 12 1))]
      (is (:ok? (s/auction-decision (auction) thrifty (league)))))))

(deftest rejection-reasons
  (testing "not an auction"
    (is (= :not-auction (:reason (s/auction-decision (auction {:type "snake"})
                                                     (picks 24 12 15) (league))))))
  (testing "still drafting"
    (is (= :incomplete (:reason (s/auction-decision (auction {:status "drafting"})
                                                    (picks 24 12 15) (league))))))
  (testing "no picks recorded"
    (is (= :no-picks (:reason (s/auction-decision (auction) [] (league))))))
  (testing "most picks carry no price"
    (let [half (into (picks 12 12 15) (mapv #(pick (+ 100 %) 1 nil) (range 12)))]
      (is (= :few-amounts (:reason (s/auction-decision (auction) half (league)))))))
  (testing "a roster never appears"
    (is (= :missing-teams (:reason (s/auction-decision (auction) (picks 24 11 15) (league))))))
  (testing "back-filled at a dollar"
    (let [dollars (into (picks 6 12 15) (mapv #(pick (+ 200 %) (inc (mod % 12)) 1) (range 18)))]
      (is (= :dollar-defaulted (:reason (s/auction-decision (auction) dollars (league)))))))
  (testing "no draft at all"
    (is (= :no-draft (:reason (s/auction-decision nil [] (league))))))
  (testing "a season whose projections are contaminated"
    ;; The season sweep stops at 2021, but `league-chain` walks previous_league_id
    ;; several hops back — so a 2021 league reaches its 2019 predecessor, whose
    ;; drafts would be scored against projections that already knew the outcome.
    (doseq [yr ["2020" "2019" "2018"]]
      (is (= :season-contaminated
             (:reason (s/auction-decision (auction {:season yr}) (picks 24 12 15) (league))))
          (str yr " is before the vintage boundary"))))
  (testing "too small to be a market"
    ;; The first widened crawl accepted a four-team $1000 league — a mock. Four
    ;; bidders do not price like twelve, so its prices would be noise in a corpus
    ;; meant to calibrate against real rooms. Rejected by reason, not silently.
    (is (= :too-small (:reason (s/auction-decision (auction {:settings {:teams 4 :budget 1000}})
                                                   (picks 12 4 80) (league 1.5)))))))

(deftest the-cheap-half-of-the-gate-rules-out-without-fetching-picks
  ;; `draft-shape` exists so the widened crawl does not spend a picks request on
  ;; every snake draft in the wild just to learn it is a snake draft.
  (is (= :not-auction (s/draft-shape (auction {:type "snake"}))))
  (is (= :incomplete  (s/draft-shape (auction {:status "drafting"}))))
  (is (= :no-draft    (s/draft-shape nil)))
  (is (= :too-small   (s/draft-shape (auction {:settings {:teams 4 :budget 1000}}))))
  (is (= :season-contaminated (s/draft-shape (auction {:season "2019"}))))
  (is (nil? (s/draft-shape (auction))) "a completed auction is still a candidate")
  (is (nil? (s/draft-shape (auction {:season "2021"}))) "2021 is the boundary, inclusive"))

(deftest a-league-is-visited-once-however-many-ways-reach-it
  ;; A user's own league list carries both last season's node and this season's,
  ;; and this season's chains back to last season's — so without deduping, the
  ;; older league is walked once per season it appears in and every draft it ran
  ;; is probed twice. That inflated the crawl histogram (five acceptances for
  ;; three drafts) and spent a picks fetch on each duplicate.
  (let [nodes {"b" {:league_id "b" :previous_league_id nil}}]
    (with-redefs [s/league (fn [id] {:ok? true :body (get nodes id)})]
      (let [this-season {:league_id "a" :previous_league_id "b"}
            last-season {:league_id "b" :previous_league_id nil}
            hist        (s/league-histories [this-season last-season] 5)]
        (is (= ["a" "b"] (mapv :league_id hist))
            "b is reached by both the chain and the season sweep, but collected once")))))

;; ---- normalization ----------------------------------------------------------

(deftest normalize-keeps-only-priced-picks-in-nomination-order
  (let [raw [(pick 3 1 30) (pick 1 2 nil) (pick 2 3 12)]
        nd  (s/normalize-draft (auction) raw (league 0.5))]
    (is (= [2 3] (mapv :pick-no (:picks nd))) "unpriced picks dropped, rest sorted")
    (is (= [12.0 30.0] (mapv :price (:picks nd))))
    (is (= "d1" (:draft-id nd)))
    (is (= 12 (:num-teams nd)))
    (is (= 200 (:budget nd)))
    (is (= 0.5 (:scoring-rec nd)) "scoring recorded so the corpus can be sliced")))

(deftest normalize-defaults-a-missing-budget
  (is (= 200 (:budget (s/normalize-draft (auction {:settings {:teams 12}}) [] nil)))))

;; ---- the previous_league_id walk --------------------------------------------

(deftest league-chain-walks-back-through-seasons
  ;; A keeper league keeps its history as a linked list, one node per season.
  ;; Reading only the newest node is what threw away every auction a league ran
  ;; before this year.
  (let [nodes {"b" {:league_id "b" :previous_league_id "c"}
               "c" {:league_id "c" :previous_league_id nil}}]
    (with-redefs [s/league (fn [id] {:ok? true :body (get nodes id)})]
      (let [chain (s/league-chain {:league_id "a" :previous_league_id "b"} 5)]
        (is (= ["a" "b" "c"] (mapv :league_id chain)))))))

(deftest league-chain-is-bounded-and-cycle-safe
  (testing "depth caps a very long history"
    (with-redefs [s/league (fn [id] {:ok? true :body {:league_id id
                                                      :previous_league_id (str id "x")}})]
      (is (= 3 (count (s/league-chain {:league_id "a" :previous_league_id "b"} 3))))))

  (testing "a cycle terminates rather than running away"
    (let [nodes {"b" {:league_id "b" :previous_league_id "a"}}]
      (with-redefs [s/league (fn [id] {:ok? true :body (get nodes id)})]
        (is (= ["a" "b"] (mapv :league_id (s/league-chain {:league_id "a"
                                                           :previous_league_id "b"} 10)))))))

  (testing "a league that cannot be read ends the chain"
    (with-redefs [s/league (fn [_] {:ok? false :reason :not-found})]
      (is (= ["a"] (mapv :league_id (s/league-chain {:league_id "a"
                                                     :previous_league_id "b"} 10)))))))
