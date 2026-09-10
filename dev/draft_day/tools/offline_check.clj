(ns draft-day.tools.offline-check
  "Prove `lein test` reaches no third-party service.

  Runs the whole suite with every HTTP entry point redefined to throw, and
  reports how many tests tried to call one. A unit suite that depends on
  somebody else's uptime is slow, impolite and eventually flaky.

  IT EXISTS BECAUSE THE SYMPTOM IS INVISIBLE. `pipeline/best-effort` catches
  everything a fetch throws and returns nil, so a test that hammers FantasyPros
  passes identically whether the scrape succeeded, failed, or was never made —
  the only trace is a `WARN ... unavailable` that reads exactly like a
  deliberately stubbed source. Reading test output cannot find these; this can.
  It found twenty-four on the run that prompted it, twelve of them scrapes of a
  vendor that answers a 429 to exactly this kind of traffic.

  Grepping for `http/get` cannot replace it either: what matters is not which
  namespaces *can* fetch but which ones a test actually reaches, through however
  many layers of `best-effort` and `parallel/all`.

  TWO CLIENTS COVER EVERY REACHABLE FETCH. http-kit is Sleeper, FantasyPros,
  ESPN's scoreboard and both league providers; `nflverse/http-get-string` is the
  JDK client the two nflverse namespaces share. ESPN's 37MB auction endpoint has
  a private client of its own and is reached only through `espn/fetch`, which
  every test that gets near it already stubs.

  Usage:
    lein run -m draft-day.tools.offline-check

  It runs the same tests `lein test` does — everything except `^:integration`,
  mirroring the `:default` selector in project.clj, since the vendor contracts
  are *supposed* to call out.

  Exits non-zero if any test made a call, so it can gate a release. It is not
  itself a test: it has to run the suite, and a test that runs the suite is a
  recursion. Run it when adding a test that touches ingestion."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as t]
            [draft-day.ingestion.nflverse :as nflverse]
            [org.httpkit.client :as http]))

(def ^:private calls (atom 0))

(defn- boom [& _]
  (swap! calls inc)
  (throw (ex-info "network call from a unit test" {})))

(defn test-namespaces
  "Every `*-test` namespace under `test/`, derived from the filenames rather
  than from a registry — a new one is covered the day it is written."
  []
  (->> (file-seq (io/file "test"))
       (map #(.getPath %))
       (filter #(re-find #"_test\.clj$" %))
       (map #(-> % (str/replace #"^test/" "") (str/replace #"\.clj$" "")
                 (str/replace "_" "-") (str/replace "/" ".") symbol))
       sort))

(defn unit-vars
  "The test vars `lein test` would run: every deftest except `^:integration`.

  This restates `:test-selectors :default` in project.clj, which Leiningen owns
  and does not expose to a running JVM. If the two ever disagree this tool
  checks a different set than `lein test` runs and still says OFFLINE."
  [nss]
  (->> nss
       (mapcat #(vals (ns-interns %)))
       (filter #(:test (meta %)))
       (remove #(:integration (meta %)))))

(defn -main [& _]
  (let [nss (test-namespaces)]
    ;; Reset, so a second run in a REPL does not report the first run's calls.
    (reset! calls 0)
    (run! require nss)
    ;; Two clients cover every reachable fetch — see the ns docstring.
    (with-redefs [http/get                 boom
                  nflverse/http-get-string boom]
      (let [vars (unit-vars nss)
            {:keys [fail error]}
            (binding [t/*report-counters* (ref t/*initial-report-counters*)]
              (t/test-vars vars)
              @t/*report-counters*)
            n @calls]
        (println)
        ;; The counts are printed because a run that silently tested nothing
        ;; would otherwise look exactly like a clean one.
        (println (format "ran %d tests, %d failures, %d errors"
                         (count vars) fail error))
        (println (if (zero? n)
                   "OFFLINE: no test reached a third-party service."
                   (str "NOT OFFLINE: " n " network call(s) from the suite.")))
        (shutdown-agents)
        (System/exit (if (and (zero? n) (zero? fail) (zero? error)) 0 1))))))
