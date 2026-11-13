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
          (pipeline/reset-cache-failures!)
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

(def ^:private listing
  {"12491" {:player_id "12491" :first_name "Corey" :last_name "Kiner" :position "RB" :team "NE"}
   "8"     {:player_id "8" :first_name "Cut" :last_name "Kicker" :position "K" :team nil}
   "9"     {:player_id "9" :first_name "Travis" :last_name "Hunter" :position "DB"
            :fantasy_positions ["DB" "WR"] :team "JAX"}
   "10"    {:player_id "10" :first_name "Some" :last_name "Linebacker" :position "LB"
            :fantasy_positions ["LB"] :team "DAL"}
   "NE"    {:player_id "NE" :first_name "New England" :last_name "" :position "DEF" :team "NE"}})

(deftest the-directory-names-every-skill-player-by-sleeper-id
  (is (= {"12491" {:name "Corey Kiner" :position "RB" :team "NE"}
          "8"     {:name "Cut Kicker" :position "K"}
          "9"     {:name "Travis Hunter" :position "WR" :team "JAX"}}
         (sp/directory listing))
      "a two-way player is filed where he scores, a teamless one has no :team, and
       neither an IDP player nor a team defense is listed"))

(deftest the-directory-rides-the-same-cache-as-the-designations
  (let [path (temp-path)]
    (with-redefs [sp/fetch-raw      (constantly listing)
                  pipeline/offline? (constantly false)
                  sp/default-cache-path path]
      (is (= "Corey Kiner" (:name (get (sp/load-directory) "12491"))))
      (is (= {} (:injuries (sp/load-injuries)))
          "nobody in this listing carries a designation"))
    (with-redefs [pipeline/offline? (constantly true)]
      (is (nil? (sp/load-directory)) "offline there is none, which is not an empty one"))))

(def ^:private env {:source "cache" :players [{:player-id "00-1" :ids {:sleeper "100"}}]})

(defn- with-week
  "`f` in season, week 8, with Sleeper projecting `lines` and naming `directory`."
  [lines directory f]
  (with-redefs [pipeline/offline?            (constantly false)
                nflverse-weekly/as-of-week (constantly nil)
                pipeline/load-weekly       (fn [& _] {:lines lines})
                sp/load-directory          (constantly directory)]
    (f)))

(def ^:private kiner {"99999999" {:name "Corey Kiner" :position "RB" :team "NE"}})

(deftest a-player-sleeper-projects-for-the-week-is-added-in-season
  (with-week {"100" {} "99999999" {}} kiner
    #(is (= ["00-1" "99999999"]
            (mapv :player-id (:players (sp/with-unseasoned env 2026 (constantly 8))))))))

(deftest nothing-is-added-before-week-two-offline-or-replaying
  (with-week {"99999999" {}} kiner
    (fn []
      (is (= env (sp/with-unseasoned env 2026 (constantly 1))) "the August board is drawn")
      (is (= env (sp/with-unseasoned env 2026 (constantly nil))) "no week from Sleeper")
      (let [sample (assoc env :source "sample")]
        (is (= sample (sp/with-unseasoned sample 2026 (constantly 8)))))
      (with-redefs [pipeline/offline? (constantly true)]
        (is (= env (sp/with-unseasoned env 2026 (constantly 8)))))
      (with-redefs [nflverse-weekly/as-of-week (constantly 5)]
        (is (= env (sp/with-unseasoned env 2026 (constantly 8))) "today's line would leak the future")))))

(deftest the-universe-is-left-as-it-was-when-a-source-is-missing
  (with-week {} kiner
    #(is (= env (sp/with-unseasoned env 2026 (constantly 8))) "no weekly line"))
  (with-week {"99999999" {}} nil
    #(is (= env (sp/with-unseasoned env 2026 (constantly 8))) "no directory")))

(deftest an-unchanged-input-gives-back-the-same-universe-object
  (with-week {"99999999" {}} kiner
    (fn []
      (is (identical? (sp/with-unseasoned env 2026 (constantly 8))
                      (sp/with-unseasoned env 2026 (constantly 8)))
          "so `pipeline/with-realized`'s identity memo keeps working"))))

(deftest a-missing-source-is-logged-once-until-the-reason-changes
  (let [warned (atom [])]
    (reset! @#'sp/unseasoned-skip nil)
    (with-redefs [clojure.tools.logging/log* (fn [_ level _ msg] (when (= :warn level) (swap! warned conj msg)))]
      (with-week {} kiner
        (fn []
          (sp/with-unseasoned env 2026 (constantly 8))
          (sp/with-unseasoned env 2026 (constantly 8))
          (is (= 1 (count @warned)) "the second request says nothing new")))
      (with-week {"99999999" {}} nil
        (fn []
          (sp/with-unseasoned env 2026 (constantly 8))
          (is (= 2 (count @warned)) "a different reason is news")))
      (with-week {"99999999" {}} kiner
        (fn []
          (sp/with-unseasoned env 2026 (constantly 8))))
      (with-week {} kiner
        (fn []
          (sp/with-unseasoned env 2026 (constantly 8))
          (is (= 3 (count @warned)) "and so is the same one after it recovered"))))))
