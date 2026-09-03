(ns draft-day.api.routes-test
  (:require [clojure.test :refer [deftest is]]
            [jsonista.core :as json]
            [draft-day.api.routes :as routes]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.ingestion.league-import :as league-import]
            [draft-day.ingestion.league-sync :as league-sync]
            [draft-day.scoring :as scoring]))

(def ^:private mapper (json/object-mapper {:decode-key-fn keyword}))
(defn- parse [resp] (json/read-value (:body resp) mapper))
(defn- input-stream [s] (java.io.ByteArrayInputStream. (.getBytes ^String s "UTF-8")))

(def ^:private fixture
  {:players (-> (vec (for [i (range 40)]
                       {:player-id (str "rb" i) :player-name (str "RB" i) :position "RB"
                        :stats {:rush_yd (- 2000 (* i 40)) :rush_td (- 12 (* i 0.2))
                                :rec 40 :rec_yd 300 :rec_td 2}}))
                ;; market sources on the top two RBs; rest have none
                (assoc-in [0 :espn/auction-value] 40.0)   ; + FP below -> consensus
                (assoc-in [0 :fantasypros/aav] 60.0)
                (assoc-in [1 :espn/auction-value] 30.0))  ; ESPN only
   :schema-version 1
   :season 2026
   :fetched-at "2026-08-09T12:00:00Z"
   :validation {:n 40 :kept 40 :dropped-blank-id 0 :dropped-duplicate 0}
   :source "sample"})

(deftest players-endpoint-returns-universe
  (routes/reset-universe!)
  (with-redefs [pipeline/load-universe (fn [& _] fixture)]
    (let [resp (routes/players-handler {:query-params {}})
          b    (parse resp)]
      (is (= 200 (:status resp)))
      (is (= 40 (:count b)))
      (is (= "sample" (:source b))))))

(deftest players-endpoint-does-not-ship-the-per-format-bundle
  ;; There is no league on this endpoint to pick a format for, and the client
  ;; reads the flat vendor columns off /api/rankings — so shipping all three
  ;; formats of every vendor column just triples the biggest payload we serve.
  (routes/reset-universe!)
  (let [bundled (update fixture :players
                        (fn [ps] (mapv #(assoc % :vendor/by-format
                                               {:ppr {:sleeper/adp 1.0}
                                                :half-ppr {:sleeper/adp 2.0}
                                                :standard {:sleeper/adp 3.0}})
                                       ps)))]
    (with-redefs [pipeline/load-universe (fn [& _] bundled)]
      (let [b (parse (routes/players-handler {:query-params {}}))]
        (is (= 40 (:count b)) "every row is still there")
        (is (every? #(nil? (:vendor/by-format %)) (:players b)))
        (is (not (re-find #"by-format" (:body (routes/players-handler {:query-params {}})))))))))

(deftest cache-reset-endpoint-clears-memory-and-disk
  (routes/reset-universe!)
  (let [calls (atom 0) deleted (atom nil)]
    (with-redefs [pipeline/load-universe (fn [& _] (swap! calls inc) fixture)
                  pipeline/delete-cache! (fn [path] (reset! deleted path))]
      (routes/players-handler {:query-params {}})   ; seed the in-memory cache
      (is (= 1 @calls))
      (let [resp (routes/cache-reset-handler {})
            b    (parse resp)]
        (is (= 200 (:status resp)))
        (is (= "ok" (:status b)))
        (is (= pipeline/default-cache-path @deleted)))
      (routes/players-handler {:query-params {}})   ; proves the atom was cleared
      (is (= 2 @calls)))))

(deftest players-endpoint-reports-universe-provenance
  (routes/reset-universe!)
  (with-redefs [pipeline/load-universe (fn [& _] fixture)]
    (let [b (parse (routes/players-handler {:query-params {}}))]
      (is (= {:schema-version 1
              :season 2026
              :fetched-at "2026-08-09T12:00:00Z"
              :source "sample"
              :validation {:n 40 :kept 40 :dropped-blank-id 0
                           :dropped-duplicate 0}}
             (:universe b)))
      (is (nil? (get-in b [:universe :players]))
          "the rows are not duplicated into the provenance block"))))

(deftest rankings-endpoint-values-the-board
  (routes/reset-universe!)
  (with-redefs [pipeline/load-universe (fn [& _] fixture)]
    (let [roster (into ["RB" "RB" "FLEX"] (repeat 3 "BENCH"))
          ls     {:teams (vec (for [i (range 12)]
                                {:team-id (str "t" i) :bankroll 200
                                 :roster (mapv (fn [p] {:pos p :player-id nil}) roster)}))
                  :drafted-player-ids [] :starting-bankroll 200 :picks []}
          req    {:body (input-stream
                         (json/write-value-as-string
                          {:num-teams 12 :scoring "ppr" :league-state ls}))}
          resp   (routes/rankings-handler req)
          b      (parse resp)]
      (is (= 200 (:status resp)))
      (is (contains? b :inflation))
      (is (some #(pos? (:worth %)) (:players b)))
      ;; market normalized to the 12x$200 = $2400 pool; rb0 = mean(40*1.2, 60*1.0) = 54,
      ;; rb1 = 30*1.2 = 36; source-less players get nil market + nil edge
      (let [by-id (into {} (map (juxt :player-id identity)) (:players b))]
        (is (= 54 (:market (by-id "rb0"))))
        (is (= 36 (:market (by-id "rb1"))))
        (is (= (- (:worth (by-id "rb0")) 54) (:edge (by-id "rb0"))))
        (is (nil? (:market (by-id "rb5"))))
        (is (nil? (:edge (by-id "rb5"))))))))

(deftest league-import-endpoint-success
  (with-redefs [league-import/import-league
                (fn [_] {:ok true :config {:scoring {:rec 1.0} :roster {:qb 1} :num-teams 10}})]
    (let [req  {:body (input-stream (json/write-value-as-string {:provider "sleeper" :league-id "123"}))}
          resp (routes/league-import-handler req)
          b    (parse resp)]
      (is (= 200 (:status resp)))
      (is (= 10 (:num-teams b))))))

(deftest league-import-endpoint-failure
  (with-redefs [league-import/import-league
                (fn [_] {:ok false :status 404 :error "Sleeper league not found"})]
    (let [req  {:body (input-stream (json/write-value-as-string {:provider "sleeper" :league-id "999999"}))}
          resp (routes/league-import-handler req)
          b    (parse resp)]
      (is (= 404 (:status resp)))
      (is (= "Sleeper league not found" (:error b))))))

(deftest league-import-endpoint-blank-league-id
  (let [req  {:body (input-stream (json/write-value-as-string {:provider "sleeper" :league-id ""}))}
        resp (routes/league-import-handler req)]
    (is (= 400 (:status resp)))))

;; ---- input-validation / hardening regressions ----

(deftest resolve-scoring-bounds-custom-map
  ;; a custom {stat weight} map is trimmed to known stat keys, so an oversized
  ;; client map can't amplify per-player scoring; presets/garbage fall back sanely
  (is (= {:rec 1.0 :pass_td 4.0}
         (routes/resolve-scoring {:rec 1.0 :pass_td 4.0 :bogus 99 :evil 1000})))
  (is (= (:ppr scoring/presets) (routes/resolve-scoring "ppr")))
  (is (= (:ppr scoring/presets) (routes/resolve-scoring nil))))

(deftest rankings-endpoint-rejects-malformed-league-state
  (routes/reset-universe!)
  (with-redefs [pipeline/load-universe (fn [& _] fixture)]
    (let [req  {:body (input-stream
                       (json/write-value-as-string
                        {:num-teams 12 :scoring "ppr" :league-state "not-a-map"}))}
          resp (routes/rankings-handler req)]
      (is (= 400 (:status resp))))))

(deftest league-import-endpoint-rejects-nonnumeric-league-id
  (let [req  {:body (input-stream
                     (json/write-value-as-string
                      {:provider "sleeper" :league-id "1/../evil"}))}
        resp (routes/league-import-handler req)
        b    (parse resp)]
    (is (= 400 (:status resp)))
    (is (= "league-id must be numeric" (:error b)))))

;; ---- scoring reaches the board, and malformed configs do not blank it ----

(defn- rankings [scoring]
  (routes/rankings-handler
   {:body (input-stream
           (json/write-value-as-string
            {:num-teams 12 :scoring scoring
             :league-state {:teams (vec (repeat 12 {:bankroll 200.0
                                                    :roster (vec (repeat 15 {:pos "BENCH"}))}))
                            :drafted-player-ids [] :starting-bankroll 200 :picks []}}))}))

(defn- worth-of [s] (into {} (map (juxt :player-id :worth)) (:players (parse (rankings s)))))

(deftest the-same-league-state-under-two-scorings-values-differently
  (routes/reset-universe!)
  ;; Reception volume has to *vary* for reception scoring to move dollars — see
  ;; the test below for why the shared fixture cannot show this.
  (let [u (assoc fixture :players
                 (vec (for [i (range 40)]
                        {:player-id (str "rb" i) :player-name (str "RB" i) :position "RB"
                         :stats {:rush_yd (- 1500 (* i 30)) :rush_td (- 12 (* i 0.2))
                                 :rec (* i 3) :rec_yd (* i 25) :rec_td 1}})))]
    (with-redefs [pipeline/load-universe (fn [& _] u)]
      (is (not= (worth-of "standard") (worth-of "ppr"))))))

(deftest a-scoring-change-flat-across-the-pool-does-not-move-dollars
  ;; Not a bug — the invariant behind it. Value is a share of a fixed money pool
  ;; split by VORP, and VORP is points minus replacement's points. Give all 40
  ;; fixture RBs the same 40 catches and PPR lifts every one of them by 40,
  ;; replacement included, so every share is exactly where it was. Worth changes
  ;; when a scoring rule changes players *relative to each other*, which is what
  ;; makes the previous test's varying fixture necessary rather than incidental.
  (routes/reset-universe!)
  (with-redefs [pipeline/load-universe (fn [& _] fixture)]
    (is (= (worth-of "standard") (worth-of "ppr")))
    (is (not= (into {} (map (juxt :player-id :points)) (:players (parse (rankings "standard"))))
              (into {} (map (juxt :player-id :points)) (:players (parse (rankings "ppr")))))
        "the projections themselves still move")))

(deftest a-null-weight-does-not-take-the-board-down
  ;; Clearing a box in the custom scoring editor sends NaN, which JSON.stringify
  ;; writes as null. That used to throw on (zero? nil), return a 400, and — since
  ;; the frontend read any JSON body as success — blank the whole board.
  (routes/reset-universe!)
  (with-redefs [pipeline/load-universe (fn [& _] fixture)]
    (let [resp (rankings (assoc (:ppr scoring/presets) :rec nil))
          b    (parse resp)]
      (is (= 200 (:status resp)))
      (is (= 40 (count (:players b))))
      (is (= (into {} (map (juxt :player-id :worth)) (:players (parse (rankings "standard"))))
             (into {} (map (juxt :player-id :worth)) (:players b)))
          "an unusable weight costs that one stat, which is exactly standard scoring here"))))

(deftest a-config-that-cannot-score-anything-is-rejected
  ;; It used to return 200 and a board where every player was worth $0, which
  ;; reads as a valuation rather than as a broken config.
  (routes/reset-universe!)
  (with-redefs [pipeline/load-universe (fn [& _] fixture)]
    (doseq [s [{}
               {:rec 0.0 :rec_yd 0.0}
               {:rec nil}
               ;; What Settings actually produces when a manager zeroes every
               ;; weight: the four stats nothing is projected for are rendered
               ;; disabled, so they keep the preset's numbers. They score nobody.
               (select-keys (:ppr scoring/presets) scoring/unprojected-stats)]]
      (let [resp (rankings s)]
        (is (= 400 (:status resp)) (pr-str s))
        (is (re-find #"non-zero" (:error (parse resp))))))))

(deftest a-legacy-num-tiers-key-is-simply-ignored
  ;; Persisted configs written before the tier count became automatic still send
  ;; it; an unknown key must not fail the request.
  (routes/reset-universe!)
  (with-redefs [pipeline/load-universe (fn [& _] fixture)]
    (let [resp (routes/rankings-handler
                {:body (input-stream
                        (json/write-value-as-string
                         {:num-teams 12 :num-tiers 5 :scoring "ppr"
                          :league-state {:teams (vec (repeat 12 {:bankroll 200.0 :roster []}))
                                         :drafted-player-ids [] :starting-bankroll 200 :picks []}}))})]
      (is (= 200 (:status resp))))))

(deftest the-board-carries-both-tier-scales
  ;; The board picks a scale from its position filter with no round trip, so the
  ;; response has to carry both — and :tier has to keep meaning the positional
  ;; one, since every consumer that predates the overall scale reads it.
  (routes/reset-universe!)
  (with-redefs [pipeline/load-universe (fn [& _] fixture)]
    (let [players (:players (parse (rankings "ppr")))]
      (is (seq players))
      (is (every? #(and (get-in % [:tiers :position]) (get-in % [:tiers :overall]))
                  players)
          "every player is tiered on both scales — there is no untiered bucket")
      (is (every? #(= (:tier %) (get-in % [:tiers :position])) players)))))

(deftest the-board-carries-the-format-matching-the-league
  (routes/reset-universe!)
  (let [u (update fixture :players
                  (fn [ps] (assoc-in (vec ps) [0 :vendor/by-format]
                                     {:ppr {:sleeper/adp 1.0} :standard {:sleeper/adp 9.0}})))
        adp-of (fn [s] (->> (:players (parse (rankings s)))
                            (filter #(= "rb0" (:player-id %))) first :sleeper/adp))]
    (with-redefs [pipeline/load-universe (fn [& _] u)]
      (is (= 1.0 (adp-of "ppr")))
      (is (= 9.0 (adp-of "standard")))
      (is (not-any? :vendor/by-format (:players (parse (rankings "ppr"))))
          "the three-format bundle never ships to the client"))))

(deftest rankings-endpoint-does-not-ship-the-season-history
  ;; /api/players carries the history once; this endpoint carries the whole board
  ;; and is re-POSTed after every pick, so three seasons of stat lines per player
  ;; would ride the hottest path in the app for data the engine never reads.
  (routes/reset-universe!)
  (let [with-history (update fixture :players
                             (fn [ps] (mapv #(assoc % :nflverse/history
                                                    [{:season 2024 :stats {:rush_yd 1200.0 :rush_td 9.0}}
                                                     {:season 2025 :stats {:rush_yd 1400.0 :rush_td 11.0}}])
                                            ps)))]
    (with-redefs [pipeline/load-universe (fn [& _] with-history)]
      (let [ls   {:teams (vec (for [i (range 12)]
                                {:team-id (str "t" i) :bankroll 200
                                 :roster (mapv (fn [p] {:pos p :player-id nil})
                                               (into ["RB" "RB" "FLEX"] (repeat 3 "BENCH")))}))
                  :drafted-player-ids [] :starting-bankroll 200 :picks []}
            req  #(hash-map :body (input-stream
                                   (json/write-value-as-string
                                    {:num-teams 12 :scoring "ppr" :league-state ls})))
            resp (routes/rankings-handler (req))
            b    (parse resp)]
        (is (= 200 (:status resp)))
        (is (= 40 (count (:players b))) "every row is still there")
        (is (every? #(nil? (:nflverse/history %)) (:players b)))
        (is (not (re-find #"history" (:body (routes/rankings-handler (req))))))
        (is (some #(pos? (:worth %)) (:players b))
            "and the board is still valued")))))

(deftest without-history-leaves-every-other-column-alone
  ;; The strip is a dissoc of one key, not a select-keys — a player that never
  ;; had a history key must come through untouched.
  (is (= [{:player-id "rb0" :worth 40} {:player-id "rb1" :worth 30}]
         (routes/without-history
          [{:player-id "rb0" :worth 40
            :nflverse/history [{:season 2025 :stats {:rush_yd 1.0}}]}
           {:player-id "rb1" :worth 30}]))))

;; ---- waivers ----

(def ^:private in-season
  "The same fixture, mid-season: week 8, with a realized line on the top two
  backs and none on anybody else."
  (-> fixture
      (assoc :through-week 8)
      (assoc-in [:players 0 :bye] 6)
      (assoc-in [:players 0 :nflverse/season-to-date]
                {:games 7 :stats {:rush_yd 900.0 :rush_td 8.0} :usage {:carries 150.0}})
      (assoc-in [:players 1 :nflverse/season-to-date]
                {:games 7 :stats {:rush_yd 200.0} :usage {:carries 60.0}})
      ;; One player with a durability history, so the Risk assertion tests that
      ;; the stage ran rather than that the fixture is empty — `risk-for`
      ;; correctly returns nil for a player it has no evidence about.
      (assoc-in [:players 0 :sleeper/years-exp] 3)
      (assoc-in [:players 0 :nflverse/games-seasons] {2023 17 2024 17 2025 17})
      (assoc-in [:players 0 :nflverse/games-by-season] {2023 17 2024 10 2025 17})))

(def ^:private synced
  {:teams [{:roster-id 1 :name "Mine"   :player-ids ["rb2" "rb3"]
            :active-ids ["rb2" "rb3"] :faab-left 60}
           {:roster-id 2 :name "Rivals" :player-ids ["rb4"]
            :active-ids ["rb4"] :faab-left 95}]
   :waiver {:type "faab" :budget 100}
   :roster-size 2
   :playoff-week-start 15})

(defn- waivers [body]
  (routes/reset-universe!)
  (with-redefs [pipeline/load-universe (fn [& _] in-season)]
    (routes/waivers-handler {:body (input-stream (json/write-value-as-string body))})))

(deftest waivers-endpoint-ranks-only-the-free-agents
  (let [b   (parse (waivers {:scoring "ppr" :num-teams 12 :league synced
                             :my-roster-id 1 :roster-size 2}))
        ids (set (map :player-id (:players b)))]
    (is (not-any? ids ["rb2" "rb3" "rb4"]) "rostered players are not offers")
    (is (contains? ids "rb0"))
    (is (= {:rb2 "Mine" :rb3 "Mine" :rb4 "Rivals"} (:rostered b))
        "who holds whom, without re-sending their rows")))

(deftest waivers-endpoint-runs-the-columns-it-ships
  ;; Three columns come from `static-rankings`, not from `rankings.ros`, and the
  ;; handler does not call it: without them Risk renders a dash for every row
  ;; with the tooltip "No injury history to judge", Pre is blank, and
  ;; `util/pos-label` shows "RB" where the header promises "RB7" — three shipped
  ;; columns permanently dead with nothing failing to say so.
  (let [b (parse (waivers {:scoring "ppr" :num-teams 12 :league synced
                           :my-roster-id 1}))
        p (first (:players b))]
    (is (number? (:points p)) "the Pre column, and the line ROS is correcting")
    (is (number? (:pos-rank p)) "the ordinal in the Pos cell")
    (is (every? #(number? (:pos-rank %)) (:players b))
        "and every row, or sorting by Pos falls back to arbitrary order")
    (is (number? (:injury-risk p)) "the Risk column, for a player with a history")
    (is (seq (:injury/reason p)) "and the text that is the whole of that cell")
    (is (not-any? #(contains? % :injury-risk) (rest (:players b)))
        "still nothing invented for the players it has no evidence about")))

(deftest waivers-endpoint-does-not-ship-the-projections-working-state
  ;; Same argument as `without-history`, on the same re-POSTed-every-refresh
  ;; path: `:ros/stats` is a whole stat map per player and no client reads it.
  (let [b (parse (waivers {:scoring "ppr" :num-teams 12 :league synced :my-roster-id 1}))]
    (is (every? #(number? (:ros-points %)) (:players b)) "the score survives")
    (is (not-any? #(contains? % :ros/stats) (:players b)))
    (is (not-any? #(contains? % :ros/games-remaining) (:players b)))
    (is (not-any? #(contains? % :ros/games-played) (:players b)))))

(deftest waivers-endpoint-prices-in-rest-of-season-points-not-auction-dollars
  ;; The draft board's money prices a whole roster out of a bankroll on draft
  ;; night; a claim is one seat against a budget spent over months.
  (let [b (parse (waivers {:scoring "ppr" :num-teams 12 :league synced
                           :my-roster-id 1 :roster-size 2}))
        p (first (:players b))]
    (is (every? #(contains? p %) [:ros-points :ros-vorp :upgrade :bid]))
    (is (not-any? #(contains? p %) [:worth :value :bargain :market :edge]))))

(deftest waivers-endpoint-reports-the-week-it-priced-for
  (let [b (parse (waivers {:scoring "ppr" :num-teams 12 :league synced :my-roster-id 1}))]
    (is (= 8 (:through-week b)))
    (is (= 17 (:season-games b)))
    (is (= 6 (:claims-left b)) "week 8 of a league whose playoffs start in 15")))

(deftest waivers-endpoint-carries-the-budget-and-what-a-rival-could-outbid-with
  (let [b (parse (waivers {:scoring "ppr" :num-teams 12 :league synced
                           :my-roster-id 1 :roster-size 2}))]
    (is (= 60 (get-in b [:faab :left])))
    (is (= 95 (get-in b [:faab :rival-max])))
    (is (= "faab" (get-in b [:faab :type])))
    ;; The type crosses the wire as a string, so this is the assertion that
    ;; catches a bid rule which only ever matched the keyword.
    (is (some #(pos? (:bid %)) (:players b)) "real money on real players")
    (is (<= (reduce + 0 (map :bid (take (:claims-left b)
                                        (sort-by #(- (:upgrade %)) (:players b)))))
            (+ 60 (:claims-left b)))
        "and the claims still available do not overspend the budget")))

(deftest waivers-endpoint-works-before-a-league-is-synced
  ;; Not an error — a manager who has not connected a league yet still gets a
  ;; rest-of-season ranking.
  (let [b (parse (waivers {:scoring "ppr" :num-teams 12}))]
    (is (= 40 (count (:players b))))
    (is (every? #(nil? (:bid %)) (:players b)))
    (is (empty? (:rostered b)))))

(deftest waivers-endpoint-refuses-a-board-that-scores-nothing
  ;; Same guard and the same reason as the rankings board: a waiver board where
  ;; nobody is an upgrade over anybody is a lie, not a board.
  (let [resp (waivers {:scoring {:rec 0 :rush_yd 0} :num-teams 12})]
    (is (= 400 (:status resp)))
    (is (re-find #"non-zero weight" (:error (parse resp))))))

(deftest waivers-endpoint-is-the-preseason-board-in-preseason
  ;; through-week 0 and no realized line anywhere: rest-of-season is the whole
  ;; season, so this is the draft board asked a different question.
  (routes/reset-universe!)
  (with-redefs [pipeline/load-universe (fn [& _] (assoc fixture :through-week 0))]
    (let [b (parse (routes/waivers-handler
                    {:body (input-stream (json/write-value-as-string
                                          {:scoring "ppr" :num-teams 12}))}))]
      (is (= 0 (:through-week b)))
      (is (= 40 (count (:players b))))
      (is (every? #(pos? (:ros-points %)) (:players b))
          "a full season of projection is still ahead of everyone"))))

(deftest waivers-endpoint-reports-a-bad-body-rather-than-throwing
  (is (= 400 (:status (routes/waivers-handler {:body (input-stream "{not json")})))))

;; ---- league sync endpoint ----

(deftest league-sync-endpoint-validates-the-id-the-same-way-import-does
  ;; One rule, shared: a second copy is how one endpoint ends up accepting what
  ;; the other rejects.
  (doseq [h [routes/league-sync-handler routes/league-import-handler]]
    (is (= 400 (:status (h {:body (input-stream (json/write-value-as-string
                                                 {:provider "sleeper" :league-id ""}))}))))
    (is (= 400 (:status (h {:body (input-stream (json/write-value-as-string
                                                 {:provider "sleeper" :league-id "abc"}))}))))))

(deftest league-sync-endpoint-returns-the-normalized-league
  (with-redefs [league-sync/sync-league (fn [_] {:ok true :league synced})]
    (let [resp (routes/league-sync-handler
                {:body (input-stream (json/write-value-as-string
                                      {:provider "sleeper" :league-id "123"}))})]
      (is (= 200 (:status resp)))
      (is (= 2 (count (:teams (parse resp))))))))

(deftest league-sync-endpoint-passes-a-failures-status-through
  (with-redefs [league-sync/sync-league (fn [_] {:ok false :status 404 :error "not found"})]
    (let [resp (routes/league-sync-handler
                {:body (input-stream (json/write-value-as-string
                                      {:provider "sleeper" :league-id "999"}))})]
      (is (= 404 (:status resp)))
      (is (= "not found" (:error (parse resp)))))))

;; ---- connecting an account ----

(deftest league-user-endpoint-returns-the-account-and-its-leagues
  (with-redefs [league-sync/find-leagues
                (fn [_] {:ok true
                         :user {:user-id "u1" :display-name "jay" :avatar "a"}
                         :leagues [{:league-id "L1" :name "RaiderNation" :num-teams 12}]})]
    (let [resp (routes/league-user-handler {:query-params {"username" "rockchalkjay"}})
          b    (parse resp)]
      (is (= 200 (:status resp)))
      (is (= "u1" (get-in b [:user :user-id])))
      (is (= 1 (count (:leagues b)))))))

(deftest league-user-endpoint-passes-a-404-through
  (with-redefs [league-sync/find-leagues
                (fn [_] {:ok false :status 404 :error "Sleeper user not found"})]
    (let [resp (routes/league-user-handler {:query-params {"username" "nope"}})]
      (is (= 404 (:status resp)))
      (is (= "Sleeper user not found" (:error (parse resp)))))))

(deftest a-username-is-guarded-separately-from-a-league-id
  ;; `league-id-error` is `#"\d+"` — the path-traversal defence for an id that
  ;; goes into a URL path segment. A username occupies the same position and is
  ;; not numeric, so borrowing that rule would reject every real name while
  ;; having no rule at all would pass a slash straight through.
  (is (nil? (routes/username-error "rockchalkjay")))
  (is (nil? (routes/username-error "a_1")))
  ;; On evidence, not taste: `the-commish` is a real Sleeper account, so a
  ;; letters-and-digits rule would refuse a legitimate name.
  (is (nil? (routes/username-error "the-commish")) "hyphens are real handles")
  (is (nil? (routes/username-error "has.dot")))
  (is (some? (routes/username-error "")))
  (is (some? (routes/username-error "   ")))
  ;; What is dangerous is the path, not the punctuation.
  (is (some? (routes/username-error "1/../evil")) "a slash injects a segment")
  (is (some? (routes/username-error "a..b")) "and `..` climbs one")
  (is (some? (routes/username-error "..")))
  (is (some? (routes/username-error ".hidden")) "the segment cannot start with a dot")
  (is (some? (routes/username-error "has space")))
  (is (some? (routes/username-error (apply str (repeat 33 "a")))) "bounded"))

(deftest league-user-endpoint-refuses-a-bad-username-before-any-fetch
  (let [called (atom false)]
    (with-redefs [league-sync/find-leagues (fn [_] (reset! called true) {:ok true})]
      (let [resp (routes/league-user-handler {:query-params {"username" "1/../evil"}})]
        (is (= 400 (:status resp)))
        (is (false? @called) "nothing reaches the provider")))))
