(ns draft-day.ingestion.sleeper-trending-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.ingestion.sleeper-trending :as trending]))

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
      "a numeric id is a string, and a row with no id is dropped"))

(deftest a-fresh-cache-is-served-without-asking-sleeper
  (let [path  (temp-path)
        calls (atom 0)]
    (with-redefs [trending/fetch-raw (fn [] (swap! calls inc) raw)
                  pipeline/offline?  (constantly false)]
      (let [dir (temp-dir)]
        (is (= {"4034" 605907 "MIN" 152600} (:adds (trending/load-adds {:path path :dir dir}))))
        (is (= 48 (:lookback-hours (trending/load-adds {:path path :dir dir}))))
        (is (= 1 @calls) "the second read is the cache")
        (is (= 1 (count (.list (io/file dir)))) "and only the fetch kept a snapshot")))))

(deftest every-live-fetch-keeps-a-snapshot-of-the-list
  (let [dir (temp-dir)]
    (with-redefs [trending/fetch-raw (constantly raw)
                  pipeline/offline?  (constantly false)]
      (let [env  (trending/load-adds {:path (temp-path) :dir dir})
            snap (pipeline/read-transit (trending/snapshot-path dir (:fetched-at env)))]
        (is (= (:adds env) (:adds snap)) "the list as it was, under the time it was fetched")))))

(deftest a-snapshot-that-will-not-write-does-not-cost-the-board-its-list
  (with-redefs [trending/fetch-raw (constantly raw)
                pipeline/offline?  (constantly false)]
    (let [blocked (java.io.File/createTempFile "not-a-dir" "")]
      (.deleteOnExit blocked)
      (is (= 605907 (get-in (trending/load-adds {:path (temp-path) :dir (str blocked)})
                            [:adds "4034"]))))))

(deftest a-failed-fetch-serves-the-last-list-with-its-own-age
  (let [path (temp-path)]
    (with-redefs [pipeline/offline? (constantly false)]
      (with-redefs [trending/fetch-raw (constantly raw)]
        (trending/load-adds {:path path :dir (temp-dir)}))
      (.setLastModified (io/file path) 0)
      (with-redefs [trending/fetch-raw (fn [] (throw (ex-info "Sleeper trending non-200" {:status 503})))]
        (let [env (trending/load-adds {:path path :dir (temp-dir)})]
          (is (= 605907 (get-in env [:adds "4034"])) "stale beats nothing")
          (is (string? (:fetched-at env)) "and says when it is from")))
      (testing "with nothing cached there is nothing to serve"
        (with-redefs [trending/fetch-raw (fn [] (throw (ex-info "down" {})))]
          (is (nil? (trending/load-adds {:path (temp-path) :dir (temp-dir)}))))))))

(deftest offline-there-is-no-list
  (with-redefs [pipeline/offline?  (constantly true)
                trending/fetch-raw (fn [] (throw (AssertionError. "offline must not fetch")))]
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
