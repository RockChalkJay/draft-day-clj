(ns draft-day.tools.projections-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [draft-day.ingestion.espn-schedule :as espn-schedule]
            [draft-day.ingestion.matchups :as matchups]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.ingestion.sleeper :as sleeper]
            [draft-day.ingestion.sleeper-players :as sleeper-players]
            [draft-day.ingestion.sleeper-trending :as trending]
            [draft-day.tools.projections :as projections]
            [jsonista.core :as json]))

(defn temp-dir []
  (str "target/projections-test-" (System/nanoTime)))

(defn delete-tree! [dir]
  (doseq [f (reverse (file-seq (io/file dir)))] (.delete ^java.io.File f)))

(defmacro with-dir [[sym] & body]
  `(let [~sym (temp-dir)]
     (try ~@body (finally (delete-tree! ~sym)))))

(defn files-under [dir]
  (->> (file-seq (io/file dir))
       (filter #(.isFile ^java.io.File %))
       (map #(.getPath ^java.io.File %))
       sort))

(defn read-json [path]
  (json/read-value (slurp path)))

(defn quietly
  "`(f)`'s result, with its output and error text swallowed."
  [f]
  (binding [*err* (java.io.StringWriter.)]
    (let [result (atom nil)]
      (with-out-str (reset! result (f)))
      @result)))

(def lines {"1" {:stats {:pts_ppr 12.5} :opponent "SEA" :home? true :updated-at 1789444813166}})
(def kickoffs {"SEA" {:kickoff "2026-10-11T17:00Z"}})
(def injuries {"1" {:status "Questionable" :body-part "Knee"}})

(defmacro with-stubs
  "Stub every fetch a run makes; `overrides` replace the defaults."
  [overrides & body]
  `(with-redefs [trending/current-season         (constantly 2026)
                 trending/current-through-week   (constantly 4)
                 matchups/current-week           (fn [& _#] 5)
                 sleeper/fetch-weekly            (fn [& _#] lines)
                 espn-schedule/fetch             (fn [& _#] kickoffs)
                 sleeper-players/live            (fn [] {:injuries injuries})]
     (with-redefs ~overrides ~@body)))

(deftest parse-args-reads-the-flags
  (is (= {:dir projections/default-dir :gap-hours projections/default-gap-hours}
         (projections/parse-args [])))
  (is (= 5 (:week (projections/parse-args ["--week" "5"]))))
  (is (= "x" (:dir (projections/parse-args ["--dir" "x"]))))
  (is (true? (:report? (projections/parse-args ["--report"]))))
  (is (= 12 (:gap-hours (projections/parse-args ["--gap-hours" "12"])))))

(deftest a-run-writes-the-line-kickoffs-and-injuries-under-the-target-week
  (with-dir [dir]
    (with-stubs []
      (is (zero? (quietly #(projections/run {:dir dir}))))
      (let [[path :as paths] (files-under dir)]
        (is (= 1 (count paths)))
        (is (re-matches #".*/2026/week-05/\d{4}-\d{2}-\d{2}T\d{2}-\d{2}-\d{2}Z\.json" path))
        (let [doc (read-json path)]
          (is (= 1 (get doc "schema_version")))
          (is (= 5 (get doc "target_week")))
          (is (= 4 (get doc "through_week")))
          (is (= 12.5 (get-in doc ["lines" "1" "stats" "pts_ppr"])))
          (is (= "2026-10-11T17:00Z" (get-in doc ["kickoffs" "SEA" "kickoff"])))
          (is (= "Questionable" (get-in doc ["injuries" "1" "status"]))))))))

(deftest --week-overrides-the-week-sleeper-names
  (with-dir [dir]
    (with-stubs []
      (quietly #(projections/run {:dir dir :week 7}))
      (is (re-find #"/2026/week-07/" (first (files-under dir)))))))

(deftest the-week-falls-back-to-the-one-after-those-played
  (with-dir [dir]
    (with-stubs [matchups/current-week (fn [& _] (throw (ex-info "down" {})))]
      (quietly #(projections/run {:dir dir}))
      (is (re-find #"/2026/week-05/" (first (files-under dir)))))))

(deftest a-failed-injury-list-still-lands-the-line
  (with-dir [dir]
    (with-stubs [sleeper-players/live (fn [] (throw (ex-info "boom" {})))]
      (is (zero? (quietly #(projections/run {:dir dir}))))
      (let [doc (read-json (first (files-under dir)))]
        (is (nil? (get doc "injuries")))
        (is (seq (get doc "lines")))))))

(deftest a-missing-schedule-leaves-kickoffs-empty
  (with-dir [dir]
    (with-stubs [espn-schedule/fetch (fn [& _] nil)]
      (quietly #(projections/run {:dir dir}))
      (is (= {} (get (read-json (first (files-under dir))) "kickoffs"))))))

(deftest a-failed-or-empty-line-writes-nothing
  (with-dir [dir]
    (testing "a line Sleeper answers empty"
      (with-stubs [sleeper/fetch-weekly (fn [& _] {})]
        (is (= 1 (quietly #(projections/run {:dir dir}))))))
    (testing "a line that errors"
      (with-stubs [sleeper/fetch-weekly (fn [& _] (throw (ex-info "boom" {})))]
        (is (= 1 (quietly #(projections/run {:dir dir}))))))
    (testing "no week known"
      (with-stubs [matchups/current-week (fn [& _] nil)
                   trending/current-through-week (constantly nil)]
        (is (= 1 (quietly #(projections/run {:dir dir}))))))
    (is (empty? (files-under dir)))))

(deftest each-run-adds-a-file
  (with-dir [dir]
    (let [times (atom ["2026-10-07T07:00:22Z" "2026-10-07T07:15:22Z"])]
      (with-stubs [pipeline/now-iso (fn [] (ffirst (swap-vals! times rest)))]
        (quietly #(projections/run {:dir dir}))
        (quietly #(projections/run {:dir dir}))
        (is (= [(str dir "/2026/week-05/2026-10-07T07-00-22Z.json")
                (str dir "/2026/week-05/2026-10-07T07-15-22Z.json")]
               (files-under dir)))))))

(defn touch! [dir season week file]
  (let [f (io/file dir season week file)]
    (io/make-parents f)
    (spit f "{}")))

(deftest the-report-counts-files-and-finds-the-longest-gap
  (with-dir [dir]
    (touch! dir "2026" "week-05" "2026-10-07T08-00-00Z.json")
    (touch! dir "2026" "week-05" "2026-10-07T10-30-00Z.json")
    (touch! dir "2026" "week-05" "2026-10-08T10-30-00Z.json")
    (touch! dir "2026" "week-06" "2026-10-14T08-00-00Z.json")
    (let [[w5 w6] (projections/week-summaries dir)]
      (is (= 3 (:count w5)))
      (is (= 24.0 (:longest-gap-hours w5)))
      (is (= 1 (:count w6)))
      (is (nil? (:longest-gap-hours w6))))
    (testing "a gap over the limit is flagged, one under it is not"
      (let [[w5] (projections/week-summaries dir)]
        (is (re-find #"GAP$" (projections/report-line 12 w5)))
        (is (not (re-find #"GAP" (projections/report-line 36 w5))))))))

(deftest the-report-of-an-empty-directory-says-so
  (with-dir [dir]
    (is (re-find #"no snapshots" (with-out-str (projections/report {:dir dir :gap-hours 36}))))))
