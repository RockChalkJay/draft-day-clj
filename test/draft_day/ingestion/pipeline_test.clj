(ns draft-day.ingestion.pipeline-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.ingestion.sleeper :as sleeper]))

(defn- tmp [name] (str (System/getProperty "java.io.tmpdir") "/dd-" name ".transit"))

(defn universe-fixture
  "A universe big enough to clear validate's systemic-failure floor, so the
  chain tests exercise the real gate rather than a disabled one."
  [n]
  (mapv (fn [i] {:player-id (str "p" i) :player-name (str "Player " i)
                 :position "RB" :stats {} :sleeper/pts-ppr 100.0})
        (range n)))

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
  ;; `offline?` reads DRAFTDAY_OFFLINE at call time, so without this the whole
  ;; chain short-circuits to the sample in any shell that exports it — which
  ;; CLAUDE.md recommends for dev.
  (with-redefs [pipeline/offline? (constantly false)
                ;; The chain is about which source wins, not about enrichment;
                ;; stubbing it keeps the test off the network entirely.
                pipeline/enrich-universe (fn [_season universe] universe)]
    (let [path    (tmp "chain")
          fixture (universe-fixture 120)]
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
          (is (seq (:players r))))))))

(deftest offline-short-circuits-to-the-sample
  (with-redefs [pipeline/offline? (constantly true)
                sleeper/fetch-universe (fn [& _]
                                         (throw (AssertionError. "no network offline")))]
    (let [r (pipeline/load-universe {:refresh true :cache-path (tmp "unused")})]
      (is (= "sample" (:source r)))
      (is (seq (:players r))))))
