(ns draft-day.tools.refresh-player-ids
  "Regenerate `resources/player_ids.edn`, the pinned Sleeper->GSIS crosswalk the
  app derives `:player-id` from.

  Deliberately out of band. The snapshot being committed is what makes id
  derivation pure, offline and reproducible, and what keeps the data alive if
  DynastyProcess ever disappears — so refreshing it is a reviewed event, not
  something ingestion does behind your back.

  Usage:
    lein run -m draft-day.tools.refresh-player-ids
    lein run -m draft-day.tools.refresh-player-ids -- --allow-changes

  THE DIFF GUARD IS THE POINT. A refresh may add players and may fill in a GSIS
  id that was previously absent, because both leave existing ids alone. What it
  must never do silently is *change* a mapping that already resolved: every
  saved draft on disk is keyed by the id that mapping produced, so changing one
  orphans real data. Those are reported and refuse to write without
  --allow-changes."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [draft-day.benchmark.fetch :as fetch]
            [draft-day.ingestion.player-ids :as player-ids]))

(def snapshot-path "resources/player_ids.edn")

(defn fetch-rows
  "Network: the current DynastyProcess crosswalk as CSV rows."
  []
  (if-let [body (fetch/http-get-string player-ids/playerids-url)]
    (fetch/parse-csv body)
    (throw (ex-info "could not fetch DynastyProcess crosswalk"
                    {:url player-ids/playerids-url}))))

(defn diff-crosswalks
  "Compare old and new {sleeper gsis} maps.

  `:changed` is the only dangerous class: a Sleeper id that resolved to one GSIS
  id and now resolves to another. `:filled` (absent -> present) is safe, since
  nothing was keyed on the old value."
  [old new]
  (let [old-rows (player-ids/snapshot-crosswalk old)
        new-rows (player-ids/snapshot-crosswalk new)]
    {:changed (into {} (keep (fn [[sid gsis]]
                               (when-let [was (get old-rows sid)]
                                 (when (not= was gsis)
                                   [sid {:was was :now gsis}]))))
                    new-rows)
     :filled  (count (remove (fn [[sid _]] (contains? old-rows sid)) new-rows))
     :dropped (into [] (remove #(contains? new-rows %)) (keys old-rows))}))

(defn build
  "Pure: CSV rows -> the snapshot envelope."
  [rows generated-at]
  (let [projected (player-ids/rows->snapshot-rows rows)]
    {:schema-version player-ids/snapshot-schema-version
     :generated-at   generated-at
     :source         player-ids/playerids-url
     :n              (count projected)
     :rows           projected}))

(defn write-snapshot!
  "One row per line: the file is committed, so the diff has to be readable."
  [path {:keys [rows] :as snap}]
  (io/make-parents path)
  (with-open [w (io/writer path)]
    (binding [*out* w, *print-length* nil, *print-level* nil]
      (println "{:schema-version" (:schema-version snap))
      (println " :generated-at" (pr-str (:generated-at snap)))
      (println " :source" (pr-str (:source snap)))
      (println " :n" (:n snap))
      (println " :rows")
      (println " [")
      ;; sorted so a refresh diffs as changed *values*, not reshuffled keys
      (doseq [r rows] (println " " (pr-str (into (sorted-map) r))))
      (println " ]}"))))

(defn -main [& args]
  (let [allow-changes (boolean (some #{"--allow-changes"} args))
        old  (player-ids/read-snapshot player-ids/snapshot-resource)
        rows (fetch-rows)
        snap (build rows (str (java.time.LocalDate/now)))
        {:keys [changed filled dropped]} (diff-crosswalks (:rows old)
                                                          (:rows snap))]
    (println (format "%d CSV rows -> %d with a sleeper id" (count rows) (:n snap)))
    (println (format "gsis mappings: %d total, %d newly filled, %d dropped"
                     (count (player-ids/snapshot-crosswalk (:rows snap)))
                     filled (count dropped)))
    (when (seq changed)
      (println (format "\n%d CHANGED mapping(s) — these would orphan saved drafts:"
                       (count changed)))
      (doseq [[sid {:keys [was now]}] (take 20 changed)]
        (println (format "  sleeper %s: %s -> %s" sid was now))))
    (if (and (seq changed) (not allow-changes))
      (do (println "\nRefusing to write. Every saved draft is keyed by the id a"
                   "mapping produced,")
          (println "so changing one silently orphans real data. Review the list"
                   "above, then")
          (println "re-run with --allow-changes if the new mappings are correct.")
          (System/exit 1))
      (do (write-snapshot! snapshot-path snap)
          (println "\nwrote" snapshot-path
                   (format "(%.1f MB)"
                           (/ (.length (io/file snapshot-path)) 1048576.0)))
          (System/exit 0)))))
