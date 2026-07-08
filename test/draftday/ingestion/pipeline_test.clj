(ns draftday.ingestion.pipeline-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [draftday.ingestion.pipeline :as pipeline]
            [draftday.ingestion.sleeper :as sleeper]))

(defn- tmp [name] (str (System/getProperty "java.io.tmpdir") "/dd-" name ".transit"))

(deftest transit-roundtrip-and-freshness
  (let [path (tmp "roundtrip")
        data [{:player-id "a" :position "RB" :stats {:rush_yd 100.0} :sleeper/adp 1.4}]]
    (pipeline/write-transit! path data)
    (is (= data (pipeline/read-transit path)))       ; namespaced keys survive
    (is (pipeline/cache-fresh? path 24))
    (.delete (io/file path))
    (is (not (pipeline/cache-fresh? path 24)))))     ; missing file -> not fresh

(deftest bundled-sample-loads
  (let [s (pipeline/load-sample)]
    (is (seq s))
    (is (every? :player-id s))))

(deftest resolution-chain
  (let [path    (tmp "chain")
        fixture [{:player-id "x" :position "RB" :stats {} :sleeper/pts-ppr 100.0}]]
    (.delete (io/file path))
    ;; live success writes the cache
    (with-redefs [sleeper/fetch-universe (fn [& _] fixture)]
      (let [r (pipeline/load-universe {:refresh true :cache-path path})]
        (is (= "live" (:source r)))
        (is (= fixture (:players r)))))
    ;; fetch down -> stale cache preferred over sample
    (with-redefs [sleeper/fetch-universe (fn [& _] (throw (ex-info "down" {})))]
      (let [r (pipeline/load-universe {:refresh true :cache-path path})]
        (is (= "cache" (:source r)))
        (is (= fixture (:players r)))))
    ;; fetch down + no cache -> bundled sample
    (.delete (io/file path))
    (with-redefs [sleeper/fetch-universe (fn [& _] (throw (ex-info "down" {})))]
      (let [r (pipeline/load-universe {:refresh true :cache-path path})]
        (is (= "sample" (:source r)))
        (is (seq (:players r)))))))
