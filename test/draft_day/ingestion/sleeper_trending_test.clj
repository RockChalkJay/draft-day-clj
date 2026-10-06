(ns draft-day.ingestion.sleeper-trending-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [draft-day.ingestion.nflverse-weekly :as nflverse-weekly]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.ingestion.sleeper-trending :as trending]
            [draft-day.json :refer [mapper]]
            [jsonista.core :as json]))

(use-fixtures :each
  (fn [t] (with-redefs [trending/current-season        (constantly 2026)
                        trending/current-through-week (constantly 4)]
            (t))))

(defn- snapshot-files [dir]
  (filter #(.isFile ^java.io.File %) (file-seq (io/file dir))))

(defn- temp-dir []
  (str (java.nio.file.Files/createTempDirectory "trending" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- temp-path []
  (let [f (java.io.File/createTempFile "trending" ".transit")]
    (.delete f)
    (.deleteOnExit f)
    (str f)))

(def ^:private raw [{:player_id "4034" :count 605907} {:player_id "MIN" :count 152600}])

(deftest the-list-is-keyed-by-sleeper-id-a-defense-by-its-team
  (is (= {"4034" 605907 "MIN" 152600} (trending/normalize raw)))
  (is (= {"7" 3} (trending/normalize [{:player_id 7 :count 3} {:player_id nil :count 9}]))
      "a numeric id is a string, and a row with no id is dropped")
  (is (= {"7" 3} (trending/normalize [{:player_id 7 :count 3} {:player_id "8" :count 0}]))
      "a zero count would collapse the log scale heat is read on"))

(deftest an-empty-answer-is-a-failure-not-a-fresh-list
  (let [path (temp-path)
        dir  (temp-dir)]
    (with-redefs [pipeline/offline? (constantly false)]
      (with-redefs [trending/fetch-raw (constantly raw)]
        (trending/load-adds {:path path :dir dir}))
      (.setLastModified (io/file path) 0)
      (doseq [body [nil [] {:error "down"}]]
        (with-redefs [trending/fetch-raw (constantly body)]
          (pipeline/reset-cache-failures!)
          (is (= 605907 (get-in (trending/load-adds {:path path :dir dir}) [:adds "4034"]))
              (str (pr-str body) " keeps the good list"))))
      (is (= 1 (count (snapshot-files dir))) "and records no empty snapshot"))))

(deftest a-failure-is-not-retried-on-every-request
  (let [path  (temp-path)
        calls (atom 0)]
    (pipeline/reset-cache-failures!)
    (with-redefs [pipeline/offline?  (constantly false)
                  trending/fetch-raw (fn [_] (swap! calls inc) (throw (ex-info "timeout" {})))]
      (trending/load-adds {:path path :dir (temp-dir)})
      (trending/load-adds {:path path :dir (temp-dir)})
      (is (= 1 @calls) "every Sleeper caller shares its permits"))
    (with-redefs [pipeline/offline?        (constantly false)
                  trending/fetch-raw       (constantly raw)
                  pipeline/failure-backoff-ms 0]
      (is (some? (trending/load-adds {:path path :dir (temp-dir)})) "and is retried once it expires"))))

(deftest a-cache-that-will-not-write-does-not-cost-the-board-its-list
  (with-redefs [trending/fetch-raw (constantly raw)
                pipeline/offline?  (constantly false)]
    (let [blocked (java.io.File/createTempFile "not-a-dir" "")]
      (.deleteOnExit blocked)
      (is (= 605907 (get-in (trending/load-adds {:path (str blocked "/cache.transit") :dir (temp-dir)})
                            [:adds "4034"]))))))

(deftest a-replayed-week-gets-no-list-from-today
  (with-redefs [pipeline/offline?             (constantly false)
                nflverse-weekly/as-of-week (constantly 5)
                trending/fetch-raw            (fn [_] (throw (AssertionError. "must not fetch")))]
    (is (nil? (trending/load-adds {:path (temp-path) :dir (temp-dir)})))))

(deftest a-fresh-cache-is-served-without-asking-sleeper
  (let [path  (temp-path)
        calls (atom 0)]
    (with-redefs [trending/fetch-raw (fn [_] (swap! calls inc) raw)
                  pipeline/offline?  (constantly false)]
      (let [dir (temp-dir)]
        (is (= {"4034" 605907 "MIN" 152600} (:adds (trending/load-adds {:path path :dir dir}))))
        (is (= 48 (:lookback-hours (trending/load-adds {:path path :dir dir}))))
        (is (= 1 @calls) "the second read is the cache")
        (is (= 1 (count (snapshot-files dir))) "and only the fetch kept a snapshot")))))

(deftest every-live-fetch-keeps-a-snapshot-of-the-list
  (let [dir (temp-dir)]
    (with-redefs [trending/fetch-raw (constantly raw)
                  pipeline/offline?  (constantly false)]
      (let [env  (trending/load-adds {:path (temp-path) :dir dir})
            path (trending/snapshot-path dir 2026 4 (:fetched-at env))
            snap (json/read-value (slurp path) mapper)]
        (is (= (:adds env) (trending/adds-of snap 48)) "the list as it was, under the time it was fetched")
        (is (= 2026 (:season snap)))
        (is (= 4 (:through_week snap)))
        (is (= [["add" 48]] (map (juxt :type :lookback_hours) (:lists snap)))
            "the board records the one list it reads")))))

(deftest fetch-list-ranks-by-count
  (with-redefs [trending/fetch-raw (constantly [{:player_id "b" :count 5} {:player_id "a" :count 5}
                                                {:player_id "c" :count 9} {:player_id "z" :count 0}])]
    (is (= {:type "add" :lookback_hours 48
            :players [{:rank 1 :player_id "c" :count 9}
                      {:rank 2 :player_id "a" :count 5}
                      {:rank 3 :player_id "b" :count 5}]}
           (trending/fetch-list {})))
    (is (= "drop" (:type (trending/fetch-list {:type "drop" :lookback-hours 24}))))
    (is (= 24 (:lookback_hours (trending/fetch-list {:type "drop" :lookback-hours 24}))))))

(deftest fetch-list-treats-an-empty-answer-as-a-failure
  (doseq [body [nil [] [{:player_id "a" :count 0}]]]
    (with-redefs [trending/fetch-raw (constantly body)]
      (is (thrown? clojure.lang.ExceptionInfo (trending/fetch-list {}))))))

(deftest adds-of-reads-the-adds-over-one-window
  (let [snap {:lists [{:type "add" :lookback_hours 24 :players [{:player_id "1" :count 3}]}
                      {:type "add" :lookback_hours 48 :players [{:player_id "2" :count 7}]}
                      {:type "drop" :lookback_hours 48 :players [{:player_id "9" :count 1}]}]}]
    (is (= {"2" 7} (trending/adds-of snap 48)))
    (is (= {"1" 3} (trending/adds-of snap 24)))
    (is (= {} (trending/adds-of snap 72)))))

(deftest write-snapshot-names-the-week-and-time
  (let [dir (temp-dir)
        snap (trending/snapshot "2026-10-07T07:00:22.114Z" 2026 4 100 [])
        path (trending/write-snapshot! dir snap)]
    (is (= (str dir "/2026/week-04/2026-10-07T07-00-22Z.json") path))
    (is (= snap (update (json/read-value (slurp path) mapper) :lists vec))
        "plain JSON that reads back as written")
    (is (= (str dir "/2026/week-unknown/x.json")
           (trending/snapshot-path dir 2026 nil "x")))
    (is (= (str dir "/unknown-season/week-00/x.json")
           (trending/snapshot-path dir nil 0 "x")))))

(deftest a-snapshot-that-will-not-write-does-not-cost-the-board-its-list
  (with-redefs [trending/fetch-raw (constantly raw)
                pipeline/offline?  (constantly false)]
    (let [blocked (java.io.File/createTempFile "not-a-dir" "")]
      (.deleteOnExit blocked)
      (is (= 605907 (get-in (trending/load-adds {:path (temp-path) :dir (str blocked)})
                            [:adds "4034"]))))))

(deftest a-failed-fetch-serves-the-last-list-with-its-own-age
  (pipeline/reset-cache-failures!)
  (let [path (temp-path)]
    (with-redefs [pipeline/offline? (constantly false)]
      (with-redefs [trending/fetch-raw (constantly raw)]
        (trending/load-adds {:path path :dir (temp-dir)}))
      (.setLastModified (io/file path) 0)
      (with-redefs [trending/fetch-raw (fn [_] (throw (ex-info "Sleeper trending non-200" {:status 503})))]
        (let [env (trending/load-adds {:path path :dir (temp-dir)})]
          (is (= 605907 (get-in env [:adds "4034"])) "stale beats nothing")
          (is (string? (:fetched-at env)) "and says when it is from")))
      (testing "with nothing cached there is nothing to serve"
        (pipeline/reset-cache-failures!)
        (with-redefs [trending/fetch-raw (fn [_] (throw (ex-info "down" {})))]
          (is (nil? (trending/load-adds {:path (temp-path) :dir (temp-dir)}))))))))

(deftest offline-there-is-no-list
  (with-redefs [pipeline/offline?  (constantly true)
                trending/fetch-raw (fn [_] (throw (AssertionError. "offline must not fetch")))]
    (is (nil? (trending/load-adds {:path (temp-path)})))))

(deftest adds-join-by-sleeper-id-and-a-defense-by-its-abbreviation
  (let [[rb dst off] (pipeline/assoc-trending
                      [{:player-id "00-gsis" :ids {:sleeper "4034"}}
                       {:player-id "MIN" :position "DST"}
                       {:player-id "00-other" :ids {:sleeper "9999"}}]
                      {"4034" 605907 "MIN" 152600})]
    (is (= 605907 (:trending/adds rb)))
    (is (= 152600 (:trending/adds dst)))
    (is (not (contains? off :trending/adds)) "off the list says nothing about his adds")))
