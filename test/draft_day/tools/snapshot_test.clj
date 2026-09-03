(ns draft-day.tools.snapshot-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.tools.snapshot :as snapshot]))

(def ^:private complete
  {:sources (into {} (map (fn [l] [l {:ok? true :rows 1 :matched 1}]))
                  pipeline/enrichment-source-labels)
   :players [{:player-id "1"}]
   :season 2026})

(deftest parse-args-reads-season-and-partial-flag
  (is (= {:season nil :allow-partial false} (snapshot/parse-args [])))
  (is (= {:season 2024 :allow-partial false}
         (snapshot/parse-args ["--season" "2024"])))
  (is (= {:season 2024 :allow-partial true}
         (snapshot/parse-args ["--season" "2024" "--allow-partial"])))
  (testing "flag order does not matter"
    (is (= {:season 2024 :allow-partial true}
           (snapshot/parse-args ["--allow-partial" "--season" "2024"])))))

(deftest missing-sources-names-what-a-capture-lacks
  (testing "a complete capture is missing nothing"
    (is (empty? (snapshot/missing-sources complete))))

  (testing "an unavailable source is missing"
    (is (= [:espn]
           (snapshot/missing-sources
            (assoc-in complete [:sources :espn] {:ok? false})))))

  (testing "a source absent from the stamp entirely is missing too"
    (is (= [:espn]
           (snapshot/missing-sources
            (update complete :sources dissoc :espn)))))

  (testing "a capture with no stamp at all is missing everything"
    (is (= pipeline/enrichment-source-labels
           (snapshot/missing-sources {})))))

(deftest summarize-reports-every-known-source
  (let [out (snapshot/summarize (assoc complete :captured-at "2026-08-09T00:00:00Z"))]
    (doseq [label pipeline/enrichment-source-labels]
      (is (re-find (re-pattern (str label)) out)
          (str label " should appear in the summary")))
    (is (re-find #"season 2026" out))))

(deftest capture-stamps-what-it-got
  (with-redefs [pipeline/fetch-enriched-universe
                (fn [_] {:players [{:player-id "1"}]
                         :sources {:espn {:ok? true :rows 1 :matched 1}}})
                pipeline/now-iso (constantly "2026-08-09T00:00:00Z")]
    (let [snap (snapshot/capture 2026)]
      (is (= pipeline/schema-version (:schema-version snap)))
      (is (= 2026 (:season snap)))
      (is (= "2026-08-09T00:00:00Z" (:captured-at snap)))
      (is (= [{:player-id "1"}] (:players snap)))
      (is (= {:espn {:ok? true :rows 1 :matched 1}} (:sources snap)))
      (is (= 0 (:through-week snap))
          "a capture with no week is preseason, not an unstamped one")))

  (testing "a capture taken mid-season says which week it is"
    ;; Without the stamp `sample-universe` reads a week-9 fixture back as
    ;; preseason, and every rest-of-season projection on an offline board
    ;; silently prorates over a full year.
    (with-redefs [pipeline/fetch-enriched-universe
                  (fn [_] {:players [{:player-id "1"}] :sources {} :through-week 9})
                  pipeline/now-iso (constantly "2026-11-04T00:00:00Z")]
      (is (= 9 (:through-week (snapshot/capture 2026)))))))
