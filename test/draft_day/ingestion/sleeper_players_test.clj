(ns draft-day.ingestion.sleeper-players-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [draft-day.ingestion.nflverse-weekly :as nflverse-weekly]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.ingestion.sleeper-players :as sp]))

(defn- temp-path []
  (let [f (java.io.File/createTempFile "injuries" ".transit")]
    (.delete f)
    (.deleteOnExit f)
    (str f)))

(def ^:private raw
  {"7"  {:player_id "7" :injury_status "Questionable" :injury_body_part "Hip" :injury_notes "Soreness" :news_updated 1790812230509}
   "8"  {:player_id "8" :injury_status "" :injury_body_part "Knee"}
   "9"  {:player_id "9" :injury_status nil}
   "10" {:player_id "10" :injury_status "IR"}})

(deftest only-a-player-with-a-designation-is-kept
  (is (= {"7"  {:status "Questionable" :body-part "Hip" :notes "Soreness" :updated-at 1790812230509}
          "10" {:status "IR"}}
         (sp/normalize raw))
      "a blank status is none, and absent fields are omitted rather than nil"))

(deftest a-failed-fetch-serves-the-last-list-and-an-empty-answer-is-a-failure
  (let [path (temp-path)]
    (with-redefs [pipeline/offline? (constantly false)]
      (with-redefs [sp/fetch-raw (constantly raw)]
        (sp/load-injuries {:path path}))
      (.setLastModified (io/file path) 0)
      (doseq [body [{} nil {:error "down"}]]
        (with-redefs [sp/fetch-raw (constantly body)]
          (reset! @#'sp/failed-at {})
          (is (= "Questionable" (get-in (sp/load-injuries {:path path}) [:injuries "7" :status]))
              (str (pr-str body) " keeps the good list")))))))

(deftest a-fresh-cache-is-served-without-asking-sleeper
  (let [path  (temp-path)
        calls (atom 0)]
    (with-redefs [sp/fetch-raw      (fn [] (swap! calls inc) raw)
                  pipeline/offline? (constantly false)]
      (sp/load-injuries {:path path})
      (is (= #{"7" "10"} (set (keys (:injuries (sp/load-injuries {:path path}))))))
      (is (= 1 @calls) "the second read is the cache"))))

(deftest offline-and-a-replayed-week-get-no-list
  (with-redefs [sp/fetch-raw (fn [] (throw (AssertionError. "must not fetch")))]
    (with-redefs [pipeline/offline? (constantly true)]
      (is (nil? (sp/load-injuries {:path (temp-path)}))))
    (with-redefs [pipeline/offline?            (constantly false)
                  nflverse-weekly/as-of-week (constantly 5)]
      (is (nil? (sp/load-injuries {:path (temp-path)}))))))

(deftest the-list-overrides-the-projections-feed-for-every-player
  (let [players [{:player-id "7"  :sleeper/injury-status nil}
                 {:player-id "20" :sleeper/injury-status "Questionable" :sleeper/injury-notes "stale"}
                 {:player-id "x" :ids {:sleeper "10"} :sleeper/injury-status nil}]
        out     (sp/assoc-injuries players (sp/normalize raw))]
    (is (= "Questionable" (:sleeper/injury-status (first out))))
    (is (= ["Hip" "Soreness"] ((juxt :sleeper/injury-body-part :sleeper/injury-notes) (first out))))
    (is (nil? (:sleeper/injury-status (second out))) "a player the list does not name is healthy")
    (is (not (contains? (second out) :sleeper/injury-notes)) "and his stale detail goes with the status")
    (is (= "IR" (:sleeper/injury-status (nth out 2))) "joined on the Sleeper id when the crosswalk has one")
    (is (= players (sp/assoc-injuries players nil)) "no list leaves everyone as the feed said")))

(deftest only-a-live-or-cached-universe-is-joined
  (let [env {:source "sample" :players [{:player-id "7"}]}]
    (with-redefs [sp/load-injuries (fn [] (throw (AssertionError. "must not load")))]
      (is (= env (sp/with-injuries env))))))
