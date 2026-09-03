(ns draft-day.tools.snapshot
  "Regenerate `resources/sample_players.edn`, the committed offline universe.

  That file is what `DRAFTDAY_OFFLINE=1` serves and what the fallout chain falls
  back to when the network is gone, so tests and offline dev see whatever it
  contains. Being a hand-captured fixture, it goes stale silently: the committed
  copy predates the FantasyPros AAV and sleepers joins, so the board's
  default-visible FP$ column is universally blank offline — and nothing says the
  column is structurally absent rather than merely unmatched.

  Usage:
    lein run -m draft-day.tools.snapshot                 ; current season
    lein run -m draft-day.tools.snapshot -- --season 2026
    lein run -m draft-day.tools.snapshot -- --allow-partial

  A partial capture is refused by default. Writing a sample that is quietly
  missing a source is precisely the failure this tool exists to prevent, so
  degrading to one has to be a decision somebody typed."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.ingestion.sleeper :as sleeper]))

(def sample-path
  "Written in place; it is a committed artifact, so the diff is the review."
  "resources/sample_players.edn")

(defn capture
  "Fetch a live, validated, enriched universe and stamp it. The stamp is what
  lets `sample-universe` report the fixture's real provenance instead of
  pretending it is current."
  [season]
  (let [{:keys [players sources through-week]} (pipeline/fetch-enriched-universe season)]
    {:schema-version pipeline/schema-version
     :season         season
     :captured-at    (pipeline/now-iso)
     ;; Stamped like the season is: a sample captured in week 9 has to say so,
     ;; or `sample-universe` reads it back as preseason and every rest-of-season
     ;; projection in an offline board silently prorates over a full year.
     :through-week   (or through-week 0)
     :sources        sources
     :players        players}))

(defn missing-sources
  "Labels that `enrich-universe` knows about but this capture did not get."
  [{:keys [sources]}]
  (remove #(:ok? (get sources %)) pipeline/enrichment-source-labels))

(defn summarize
  "Human-readable capture summary, one line per source."
  [{:keys [season captured-at players sources] :as snap}]
  (str/join
   "\n"
   (concat [(format "season %s, captured %s, %d players" season captured-at
                    (count players))]
           (for [label pipeline/enrichment-source-labels
                 :let [{:keys [ok? rows matched]} (get sources label)]]
             (format "  %-24s %s" label
                     (if ok?
                       (format "%s rows, %s matched" rows (or matched "n/a"))
                       "UNAVAILABLE")))
           (when-let [m (seq (missing-sources snap))]
             [(str "\nmissing: " (str/join ", " m))]))))

(defn write-sample!
  "Pretty-print the snapshot so the committed diff stays reviewable."
  [path snap]
  (io/make-parents path)
  (with-open [w (io/writer path)]
    (binding [*out* w, *print-length* nil, *print-level* nil]
      (pp/pprint snap))))

(defn parse-args
  "`--season <year>` and `--allow-partial`; anything else is ignored."
  [args]
  {:season        (some (fn [[a b]] (when (= a "--season") (Long/parseLong b)))
                        (partition 2 1 args))
   :allow-partial (boolean (some #{"--allow-partial"} args))})

(defn -main [& args]
  (let [{:keys [season allow-partial]} (parse-args args)
        season (or season (sleeper/current-season))]
    (println "capturing season" season "…")
    (let [snap    (capture season)
          missing (missing-sources snap)]
      (println (summarize snap))
      (if (and (seq missing) (not allow-partial))
        (do (println "\nRefusing to write a partial sample: the committed"
                     "fixture would silently lose those columns.")
            (println "Re-run when they are reachable, or pass --allow-partial.")
            (System/exit 1))
        (do (write-sample! sample-path snap)
            (println "\nwrote" sample-path)
            (System/exit 0))))))
