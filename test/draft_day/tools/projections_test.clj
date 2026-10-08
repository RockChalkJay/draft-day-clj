(ns draft-day.tools.projections-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [draft-day.ingestion.espn-schedule :as espn-schedule]
            [draft-day.ingestion.matchups.sleeper :as sleeper-state]
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

(def entries
  [{:player_id "1" :team "DAL" :opponent "SEA" :game_id "g1" :updated_at 1789444813166
    :stats {:pts_ppr 12.5 :pass_yd 250.0 :adp_dd_ppr 40.0}
    :player {:injury_status "Out"}}
   {:player_id "2" :team "DAL" :stats {:gp 1.0}}])
(def kickoffs {"SEA" {:kickoff "2026-10-11T17:00Z"}})
(def injuries {"1" {:status "Questionable" :body-part "Knee"}})
(def state {:season "2026" :week 5 :display_week 5 :season_type "regular"})

(defmacro with-stubs
  "Stub every fetch a run makes; `overrides` replace the defaults."
  [overrides & body]
  `(with-redefs [trending/current-season         (constantly 2026)
                 sleeper-state/nfl-state         (fn [] state)
                 pipeline/load-realized          (fn [& _#] {:weekly {:through-week 4}})
                 sleeper/fetch-weekly-entries    (fn [& _#] entries)
                 espn-schedule/fetch             (fn [& _#] kickoffs)
                 sleeper-players/live            (fn [] {:injuries injuries})]
     (with-redefs ~overrides ~@body)))

(deftest parse-args-reads-the-flags
  (is (= {:dir projections/default-dir :gap-hours projections/default-gap-hours
          :min-files projections/default-min-files}
         (projections/parse-args [])))
  (is (= 5 (:week (projections/parse-args ["--week" "5"]))))
  (is (= "x" (:dir (projections/parse-args ["--dir" "x"]))))
  (is (true? (:report (projections/parse-args ["--report"]))))
  (is (= 12 (:gap-hours (projections/parse-args ["--gap-hours" "12"]))))
  (is (= 2 (:min-files (projections/parse-args ["--min-files" "2"]))))
  (is (true? (:help (projections/parse-args ["--help"])))))

(deftest a-malformed-or-unknown-flag-throws-rather-than-falling-back
  (doseq [args [["--week" "five"] ["--week" "5.0"] ["--week" "0"] ["--week" "-3"]
                ["--gap-hours" "abc"] ["--gap-hours" "1.5"] ["--min-files" "x"] ["--week"]
                ["--wek" "5"] ["stray"]]]
    (is (thrown? clojure.lang.ExceptionInfo (projections/parse-args args))
        (str "should reject " args))))

(deftest a-run-writes-the-raw-entries-kickoffs-and-injuries-under-the-target-week
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
          (is (= ["1"] (keys (get doc "lines")))
              "only players with a projected score, keyed by Sleeper id")
          (is (= {"pts_ppr" 12.5 "pass_yd" 250.0 "adp_dd_ppr" 40.0}
                 (get-in doc ["lines" "1" "stats"]))
              "Sleeper's own total and stats outside our scoring vocabulary survive")
          (is (not (contains? (get-in doc ["lines" "1"]) "player"))
              "the player record, with its stale injury fields, is dropped")
          (is (= "2026-10-11T17:00Z" (get-in doc ["kickoffs" "SEA" "kickoff"])))
          (is (= "Questionable" (get-in doc ["injuries" "1" "status"]))))))))

(deftest the-season-is-the-one-sleeper-names
  (with-dir [dir]
    (with-stubs [sleeper-state/nfl-state (fn [] (assoc state :season "2026" :week 18))
                 trending/current-season (constantly 2027)]
      (quietly #(projections/run {:dir dir}))
      (is (re-find #"/2026/week-18/" (first (files-under dir)))
          "a January week keeps the season it belongs to, not the calendar year's"))))

(deftest --week-overrides-the-week-sleeper-names
  (with-dir [dir]
    (with-stubs []
      (quietly #(projections/run {:dir dir :week 7}))
      (is (re-find #"/2026/week-07/" (first (files-under dir)))))))

(deftest the-stamp-is-taken-before-the-fetches
  (with-dir [dir]
    (let [order (atom [])]
      (with-stubs [pipeline/now-iso            (fn [] (swap! order conj :stamp) "2026-10-07T07:00:22Z")
                   sleeper/fetch-weekly-entries (fn [& _] (swap! order conj :line) entries)
                   sleeper-players/live         (fn [] (swap! order conj :injuries) {:injuries injuries})]
        (quietly #(projections/run {:dir dir}))
        (is (= [:stamp :line :injuries] @order))))))

(deftest a-failed-injury-list-still-lands-the-line
  (with-dir [dir]
    (with-stubs [sleeper-players/live (fn [] (throw (ex-info "boom" {})))]
      (is (zero? (quietly #(projections/run {:dir dir}))))
      (let [doc (read-json (first (files-under dir)))]
        (is (nil? (get doc "injuries")))
        (is (seq (get doc "lines")))))))

(deftest a-missing-schedule-leaves-kickoffs-empty-and-says-so
  (with-dir [dir]
    (with-stubs [espn-schedule/fetch (fn [& _] nil)]
      (let [err (java.io.StringWriter.)]
        (binding [*err* err]
          (with-out-str (projections/run {:dir dir})))
        (is (re-find #"kickoffs failed" (str err))))
      (is (= {} (get (read-json (first (files-under dir))) "kickoffs"))))))

(deftest a-failed-or-empty-line-writes-nothing
  (with-dir [dir]
    (testing "a line Sleeper answers empty"
      (with-stubs [sleeper/fetch-weekly-entries (fn [& _] [])]
        (is (= 1 (quietly #(projections/run {:dir dir}))))))
    (testing "a line that errors"
      (with-stubs [sleeper/fetch-weekly-entries (fn [& _] (throw (ex-info "boom" {})))]
        (is (= 1 (quietly #(projections/run {:dir dir}))))))
    (testing "Sleeper does not name the week, however many weeks nflverse has finished"
      (with-stubs [sleeper-state/nfl-state (fn [] (throw (ex-info "down" {})))]
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

(def now (java.time.Instant/parse "2026-10-15T08:00:00Z"))

(defn reported
  "What `report` prints for `dir` at `now` under `opts`."
  [dir opts]
  (with-out-str (projections/report (merge {:dir dir :gap-hours 72 :min-files 3 :now now} opts))))

(deftest the-report-counts-files-and-finds-the-longest-gap
  (with-dir [dir]
    (touch! dir "2026" "week-05" "2026-10-07T08-00-00Z.json")
    (touch! dir "2026" "week-05" "2026-10-07T10-30-00Z.json")
    (touch! dir "2026" "week-05" "2026-10-08T10-30-00Z.json")
    (touch! dir "2026" "week-06" "2026-10-14T08-00-00Z.json")
    (let [[w5 w6] (projections/week-summaries dir)]
      (is (= 3 (:count w5)))
      (is (= 5 (:week w5)))
      (is (= "2026" (:season w5)))
      (is (= 24.0 (:longest-gap-hours w5)))
      (is (= 1 (:count w6)))
      (is (nil? (:longest-gap-hours w6))))))

(deftest the-report-flags-a-gap-only-over-the-limit
  (with-dir [dir]
    (touch! dir "2026" "week-05" "2026-10-07T08-00-00Z.json")
    (touch! dir "2026" "week-05" "2026-10-08T08-00-00Z.json")
    (touch! dir "2026" "week-05" "2026-10-09T08-00-00Z.json")
    (is (re-find #"GAP" (reported dir {:gap-hours 12})))
    (is (not (re-find #"GAP" (reported dir {:gap-hours 36}))))))

(deftest the-report-flags-a-finished-week-with-too-few-files-but-not-the-newest
  (with-dir [dir]
    (touch! dir "2026" "week-05" "2026-10-07T08-00-00Z.json")
    (touch! dir "2026" "week-06" "2026-10-14T08-00-00Z.json")
    (let [[w5 w6] (str/split-lines (reported dir {}))]
      (is (re-find #"FEW" w5) "week 5 is finished with one file of three")
      (is (not (re-find #"FEW" w6)) "week 6 is still filling"))))

(deftest the-report-names-a-week-with-no-files
  (with-dir [dir]
    (touch! dir "2026" "week-05" "2026-10-07T08-00-00Z.json")
    (touch! dir "2026" "week-07" "2026-10-21T08-00-00Z.json")
    (is (re-find #"2026/week-06  MISSING" (reported dir {:now (java.time.Instant/parse "2026-10-21T09:00:00Z")})))))

(deftest the-report-flags-a-stale-newest-snapshot
  (with-dir [dir]
    (touch! dir "2026" "week-06" "2026-10-14T08-00-00Z.json")
    (is (re-find #"newest snapshot .* 24\.0h ago$" (reported dir {:now (java.time.Instant/parse "2026-10-15T08:00:00Z")})))
    (is (re-find #"STALE" (reported dir {:now (java.time.Instant/parse "2026-10-20T08:00:00Z")})))))

(deftest the-report-of-an-empty-directory-says-so
  (with-dir [dir]
    (is (re-find #"no snapshots" (reported dir {})))))
