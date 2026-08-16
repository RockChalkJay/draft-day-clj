(ns draft-day.api.routes-test
  (:require [clojure.test :refer [deftest is]]
            [jsonista.core :as json]
            [draft-day.api.routes :as routes]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.ingestion.league-import :as league-import]
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

(deftest the-board-ships-every-tier-strategy-at-both-scales
  ;; The client switches technique with no round trip, which only works if the
  ;; response already carries all of them.
  (routes/reset-universe!)
  (with-redefs [pipeline/load-universe (fn [& _] fixture)]
    (let [players (:players (parse (rankings "ppr")))]
      (is (seq players))
      (doseq [p players]
        (is (contains? (:tiers p) :cliffs) (:player-id p))
        (is (contains? (:tiers p) :ecr) (:player-id p))
        (is (= (:tier p) (get-in p [:tiers :cliffs :position]))
            "the flat :tier stays the default strategy's positional tier"))
      (is (some #(get-in % [:tiers :cliffs :overall]) players)
          "the overall scale is populated, not merely present"))))
