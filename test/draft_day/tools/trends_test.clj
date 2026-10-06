(ns draft-day.tools.trends-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.ingestion.sleeper-trending :as trending]
            [draft-day.tools.trends :as trends]
            [jsonista.core :as json]))

(defn temp-dir []
  (str "target/trends-test-" (System/nanoTime)))

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

(def adds  [{:player_id "1" :count 50} {:player_id "2" :count 90} {:player_id "3" :count 0}])
(def drops [{:player_id "9" :count 7}])

(defn stub-fetch [{:keys [type]}]
  (if (= "drop" type) drops adds))

(deftest parse-args-reads-the-flags
  (is (= trends/defaults (trends/parse-args [])))
  (is (= ["drop"] (:types (trends/parse-args ["--types" "drop"]))))
  (is (= [24 48] (:lookbacks (trends/parse-args ["--lookbacks" "24,48"]))))
  (is (= 25 (:limit (trends/parse-args ["--limit" "25"]))))
  (is (= "x" (:dir (trends/parse-args ["--dir" "x"])))))

(deftest snapshot-path-names-the-season-week-and-time
  (is (= "d/2026/week-04/2026-10-07T07-00-22Z.json"
         (trending/snapshot-path "d" 2026 4 "2026-10-07T07:00:22.114Z")))
  (is (= "d/2026/week-00/2026-10-07T07-00-22Z.json"
         (trending/snapshot-path "d" 2026 0 "2026-10-07T07:00:22Z")))
  (is (= "d/2026/week-unknown/2026-10-07T07-00-22Z.json"
         (trending/snapshot-path "d" 2026 nil "2026-10-07T07:00:22Z"))))

(deftest a-run-writes-one-json-file-in-the-week-folder
  (with-dir [dir]
    (with-redefs [trending/fetch-raw        stub-fetch
                  trending/current-season        (constantly 2026)
                  trending/current-through-week (constantly 4)
                  trends/names-index        (constantly {"2" {:name "Ann Lee" :pos "RB" :team "SEA"}})]
      (is (zero? (quietly #(trends/run ["--dir" dir]))))
      (let [[path :as paths] (files-under dir)]
        (is (= 1 (count paths)))
        (is (re-matches #".*/2026/week-04/\d{4}-\d{2}-\d{2}T\d{2}-\d{2}-\d{2}Z\.json" path))
        (let [doc (read-json path)]
          (is (= 1 (get doc "schema_version")))
          (is (= 2026 (get doc "season")))
          (is (= 4 (get doc "through_week")))
          (is (= 100 (get doc "limit")))
          (is (string? (get doc "fetched_at")))
          (is (= [["add" 48] ["drop" 48]]
                 (map (juxt #(get % "type") #(get % "lookback_hours")) (get doc "lists"))))
          (testing "ranked by count, zero counts dropped, names joined where known"
            (is (= [{"rank" 1 "player_id" "2" "count" 90 "name" "Ann Lee" "pos" "RB" "team" "SEA"}
                    {"rank" 2 "player_id" "1" "count" 50 "name" nil "pos" nil "team" nil}]
                   (get-in doc ["lists" 0 "players"]))))
          (is (= ["9"] (map #(get % "player_id") (get-in doc ["lists" 1 "players"])))))))))

(deftest a-run-can-take-several-lookbacks-and-one-type
  (with-dir [dir]
    (with-redefs [trending/fetch-raw        stub-fetch
                  trending/current-season        (constantly 2026)
                  trending/current-through-week (constantly 4)
                  trends/names-index        (constantly {})]
      (quietly #(trends/run ["--dir" dir "--types" "add" "--lookbacks" "24,48"]))
      (is (= [["add" 24] ["add" 48]]
             (map (juxt #(get % "type") #(get % "lookback_hours"))
                  (get (read-json (first (files-under dir))) "lists")))))))

(deftest an-unknown-week-lands-in-its-own-folder
  (with-dir [dir]
    (with-redefs [trending/fetch-raw        stub-fetch
                  trending/current-season        (constantly 2026)
                  trending/current-through-week (constantly nil)
                  trends/names-index        (constantly {})]
      (quietly #(trends/run ["--dir" dir]))
      (is (re-find #"/2026/week-unknown/" (first (files-under dir))))
      (is (nil? (get (read-json (first (files-under dir))) "through_week"))))))

(deftest a-failed-or-empty-list-writes-nothing
  (with-dir [dir]
    (with-redefs [trending/current-season        (constantly 2026)
                  trending/current-through-week (constantly 4)
                  trends/names-index        (constantly {})]
      (testing "a list Sleeper answers empty"
        (with-redefs [trending/fetch-raw (fn [{:keys [type]}] (if (= "drop" type) [] adds))]
          (is (= 1 (quietly #(trends/run ["--dir" dir]))))))
      (testing "a list that errors"
        (with-redefs [trending/fetch-raw (fn [_] (throw (ex-info "boom" {})))]
          (is (= 1 (quietly #(trends/run ["--dir" dir]))))))
      (is (empty? (files-under dir))))))

(deftest each-run-adds-a-file
  (with-dir [dir]
    (let [times (atom ["2026-10-07T07:00:22Z" "2026-10-07T07:15:22Z"])]
      (with-redefs [trending/fetch-raw          stub-fetch
                    trending/current-season        (constantly 2026)
                  trending/current-through-week (constantly 4)
                    trends/names-index          (constantly {})
                    pipeline/now-iso            (fn [] (ffirst (swap-vals! times rest)))]
        (quietly #(trends/run ["--dir" dir]))
        (quietly #(trends/run ["--dir" dir]))
        (is (= [(str dir "/2026/week-04/2026-10-07T07-00-22Z.json")
                (str dir "/2026/week-04/2026-10-07T07-15-22Z.json")]
               (files-under dir)))))))
