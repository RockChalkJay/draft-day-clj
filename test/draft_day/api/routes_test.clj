(ns draft-day.api.routes-test
  (:require [clojure.test :refer [deftest is]]
            [jsonista.core :as json]
            [draft-day.api.routes :as routes]
            [draft-day.ingestion.pipeline :as pipeline]))

(def ^:private mapper (json/object-mapper {:decode-key-fn keyword}))
(defn- parse [resp] (json/read-value (:body resp) mapper))
(defn- input-stream [s] (java.io.ByteArrayInputStream. (.getBytes ^String s "UTF-8")))

(def ^:private fixture
  {:players (vec (for [i (range 40)]
                   {:player-id (str "rb" i) :player-name (str "RB" i) :position "RB"
                    :stats {:rush_yd (- 2000 (* i 40)) :rush_td (- 12 (* i 0.2))
                            :rec 40 :rec_yd 300 :rec_td 2}}))
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
      (is (every? #(and (contains? % :worth-floor) (contains? % :worth-ceiling)) (:players b))))))
