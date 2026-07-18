(ns draft-day.api.routes-test
  (:require [clojure.test :refer [deftest is]]
            [jsonista.core :as json]
            [draft-day.api.routes :as routes]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.ingestion.league-import :as league-import]))

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
   :source "sample"})

(deftest players-endpoint-returns-universe
  (routes/reset-universe!)
  (with-redefs [pipeline/load-universe (fn [& _] fixture)]
    (let [resp (routes/players-handler {:query-params {}})
          b    (parse resp)]
      (is (= 200 (:status resp)))
      (is (= 40 (:count b)))
      (is (= "sample" (:source b))))))

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
                          {:num-teams 12 :scoring "ppr" :profile "balanced" :league-state ls}))}
          resp   (routes/rankings-handler req)
          b      (parse resp)]
      (is (= 200 (:status resp)))
      (is (contains? b :inflation))
      (is (= "balanced" (:profile b)))
      (is (some #(pos? (:worth %)) (:players b)))
      ;; cross-lens worths attached for divergence badges
      (is (every? #(and (contains? % :worth-floor) (contains? % :worth-ceiling)) (:players b)))
      ;; market normalized to the 12x$200 = $2400 pool; rb0 = mean(40*1.2, 60*1.0) = 54,
      ;; rb1 = 30*1.2 = 36; source-less players get nil market + nil edge
      (let [by-id (into {} (map (juxt :player-id identity)) (:players b))]
        (is (= 54 (:market (by-id "rb0"))))
        (is (= 36 (:market (by-id "rb1"))))
        (is (= (- (:worth (by-id "rb0")) 54) (:edge (by-id "rb0"))))
        (is (nil? (:market (by-id "rb5"))))
        (is (nil? (:edge (by-id "rb5"))))))))

(deftest scoring-presets-endpoint-returns-presets-and-stat-keys
  (let [resp (routes/scoring-presets-handler {})
        b    (parse resp)]
    (is (= 200 (:status resp)))
    (is (= #{:standard :half-ppr :ppr} (set (keys (:presets b)))))
    (is (seq (:stat-keys b)))))

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
    (let [req  {:body (input-stream (json/write-value-as-string {:provider "sleeper" :league-id "bogus"}))}
          resp (routes/league-import-handler req)
          b    (parse resp)]
      (is (= 404 (:status resp)))
      (is (= "Sleeper league not found" (:error b))))))

(deftest league-import-endpoint-blank-league-id
  (let [req  {:body (input-stream (json/write-value-as-string {:provider "sleeper" :league-id ""}))}
        resp (routes/league-import-handler req)]
    (is (= 400 (:status resp)))))
