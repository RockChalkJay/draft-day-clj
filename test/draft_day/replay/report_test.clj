(ns draft-day.replay.report-test
  "The report entry point's pure parts. Everything else here reads or writes
  `data/replay_cache/`, so it is left to the crawl itself."
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.replay.report :as report]))

(deftest crawl-bounds-come-off-the-command-line
  ;; All three were REPL-only: `run` destructured two of them and `-main` parsed
  ;; neither, while --max-drafts-per-user — which decides how much any one
  ;; community may contribute, and so whether the corpus can be balanced at all —
  ;; was not plumbed through `run` in the first place. A sweep that runs for hours
  ;; is the thing most wanted from a shell with a bound on it.
  (is (= {:max-users 50 :max-drafts 10 :max-drafts-per-user 3}
         (report/parse-bounds ["--max-users=50" "--max-drafts=10"
                               "--max-drafts-per-user=3"])))

  (testing "the arguments that are not bounds are left alone"
    ;; --rebuild/--fresh and bare draft ids arrive through the same argv
    (is (= {} (report/parse-bounds ["--rebuild" "--fresh" "1234567890"]))))

  (testing "a flag with no value is not a bound"
    (is (= {} (report/parse-bounds ["--max-users" "--max-users=x"])))))
