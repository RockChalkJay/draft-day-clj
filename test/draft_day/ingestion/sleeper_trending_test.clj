(ns draft-day.ingestion.sleeper-trending-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.ingestion.sleeper-trending :as trending]))

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
      (is (= {"4034" 605907 "MIN" 152600} (:adds (trending/load-adds {:path path}))))
      (is (= 48 (:lookback-hours (trending/load-adds {:path path}))))
      (is (= 1 @calls) "the second read is the cache"))))

(deftest a-failed-fetch-serves-the-last-list-with-its-own-age
  (let [path (temp-path)]
    (with-redefs [pipeline/offline? (constantly false)]
      (with-redefs [trending/fetch-raw (constantly raw)]
        (trending/load-adds {:path path}))
      (.setLastModified (io/file path) 0)
      (with-redefs [trending/fetch-raw (fn [] (throw (ex-info "Sleeper trending non-200" {:status 503})))]
        (let [env (trending/load-adds {:path path})]
          (is (= 605907 (get-in env [:adds "4034"])) "stale beats nothing")
          (is (string? (:fetched-at env)) "and says when it is from")))
      (testing "with nothing cached there is nothing to serve"
        (with-redefs [trending/fetch-raw (fn [] (throw (ex-info "down" {})))]
          (is (nil? (trending/load-adds {:path (temp-path)}))))))))

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
