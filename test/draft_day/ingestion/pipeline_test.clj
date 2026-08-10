(ns draft-day.ingestion.pipeline-test
  (:require [clojure.test :refer [deftest is testing]]
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

(deftest universe-is-stamped-with-its-provenance
  (with-redefs [pipeline/offline? (constantly false)
                pipeline/enrich-universe (fn [_ u] u)
                pipeline/now-iso (constantly "2026-08-09T12:00:00Z")
                sleeper/fetch-universe (fn [& _] (universe-fixture 120))]
    (let [path (tmp "stamp")]
      (.delete (io/file path))
      (let [r (pipeline/load-universe {:refresh true :cache-path path
                                       :season 2026})]
        (is (= pipeline/schema-version (:schema-version r)))
        (is (= 2026 (:season r)))
        (is (= "2026-08-09T12:00:00Z" (:fetched-at r)))
        (is (= "live" (:source r)))
        (is (= 120 (get-in r [:validation :kept]))))

      (testing "a cached read reports when it was *fetched*, not read"
        (with-redefs [pipeline/now-iso (constantly "2027-01-01T00:00:00Z")]
          (let [r (pipeline/load-universe {:cache-path path})]
            (is (= "cache" (:source r)))
            (is (= "2026-08-09T12:00:00Z" (:fetched-at r))
                "provenance travels with the data, not the read")
            (is (= 2026 (:season r)))))))))

(deftest cached-universe-rejects-a-foreign-schema
  (let [path (tmp "schema")]
    (testing "a bare pre-versioning vector is schema 0, so it is not reused"
      (pipeline/write-transit! path (universe-fixture 120))
      (is (= {:schema-version 0 :players (universe-fixture 120)}
             (pipeline/cached->universe (pipeline/read-transit path))))
      (is (nil? (pipeline/cached-universe path))))

    (testing "a future schema is refused rather than read as missing columns"
      (pipeline/write-transit! path {:schema-version 999
                                     :players (universe-fixture 120)})
      (is (nil? (pipeline/cached-universe path))))

    (testing "a cache that validates down to nothing is not served"
      (pipeline/write-transit! path {:schema-version pipeline/schema-version
                                     :players [{:player-id nil}]})
      (is (nil? (pipeline/cached-universe path))))

    (.delete (io/file path))
    (is (nil? (pipeline/cached-universe path)))))

(deftest a-stale-cache-still-beats-the-sample
  (with-redefs [pipeline/offline? (constantly false)
                pipeline/enrich-universe (fn [_ u] u)]
    (let [path    (tmp "stale")
          fixture (universe-fixture 120)]
      (.delete (io/file path))
      (with-redefs [sleeper/fetch-universe (fn [& _] fixture)]
        (pipeline/load-universe {:refresh true :cache-path path :season 2025}))
      (with-redefs [sleeper/fetch-universe (fn [& _] (throw (ex-info "down" {})))]
        (let [r (pipeline/load-universe {:refresh true :cache-path path})]
          (is (= "cache" (:source r)))
          (is (= 2025 (:season r))
              "the stale season is visible, so the board can say it is old"))))))

(deftest offline-short-circuits-to-the-sample
  (with-redefs [pipeline/offline? (constantly true)
                sleeper/fetch-universe (fn [& _]
                                         (throw (AssertionError. "no network offline")))]
    (let [r (pipeline/load-universe {:refresh true :cache-path (tmp "unused")})]
      (is (= "sample" (:source r)))
      (is (seq (:players r))))))
