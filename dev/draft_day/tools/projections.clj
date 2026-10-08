(ns draft-day.tools.projections
  "Save the week's Sleeper projection line, its kickoffs and the injury list as
  JSON, for scoring the weekly board against what happened.

  Sleeper revises a past week's line after the games (its `updated_at` lands
  after the week), so the line the board showed on Sunday morning cannot be
  fetched later. Each run is one live fetch, bypassing the TTL caches, and one
  file; when to run it is up to whoever runs it.

    lein run -m draft-day.tools.projections [--week N] [--dir data/projections]
    lein run -m draft-day.tools.projections --report [--dir data/trends] [--gap-hours 36]

  The file is `<dir>/<season>/week-NN/<UTC time>.json`, NN being the week the
  line is *for* (the week being played), where `tools.trends` files by weeks
  played. Both are in the document as `target_week` and `through_week`. The
  line is the one `/api/waivers` serves, in Sleeper's player ids, and the
  injury list is the complete designation list as of the run. If the line comes
  back empty nothing is written; a failed injury list leaves `injuries` null
  and the line still lands.

  `--report` lists the snapshots under a directory per week, with the longest
  gap between two of them, and works on `data/trends` as well.

  Run from cron in Denver time, Wednesday 08:00 (after Sleeper's overnight
  waiver run), Thursday 16:30 (before Thursday night's kickoff) and Sunday 09:30
  (when the early window's inactives land).

  Exit codes: 0 written (or reported), 1 the line failed."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [draft-day.ingestion.espn-schedule :as espn-schedule]
            [draft-day.ingestion.matchups :as matchups]
            [draft-day.ingestion.matchups.sleeper]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.ingestion.sleeper :as sleeper]
            [draft-day.ingestion.sleeper-players :as sleeper-players]
            [draft-day.ingestion.sleeper-trending :as trending]
            [jsonista.core :as json])
  (:import (java.time Duration Instant)))

(def schema-version 1)

(def default-dir "data/projections")

(def default-gap-hours 36)

(defn parse-args
  "The options `args` give over the defaults. A flag it does not know is ignored,
  as in `tools.trends`."
  [args]
  (let [value (fn [flag] (some (fn [[a b]] (when (= a flag) b)) (partition 2 1 args)))]
    (cond-> {:dir default-dir :gap-hours default-gap-hours}
      (some #{"--report"} args) (assoc :report? true)
      (value "--week")          (assoc :week (parse-long (value "--week")))
      (value "--dir")           (assoc :dir (value "--dir"))
      (value "--gap-hours")     (assoc :gap-hours (parse-long (value "--gap-hours"))))))

(defn target-week
  "The week to file under: `week` if given, else the one Sleeper says is being
  played, else the one after the weeks nflverse has finished. nil if none is
  known."
  [week]
  (or week
      (pipeline/best-effort (matchups/current-week :sleeper {}))
      (some-> (trending/current-through-week) inc)))

(defn snapshot
  "The document a snapshot file holds."
  [fetched-at season target through lines kickoffs injuries]
  {:schema_version schema-version
   :fetched_at     fetched-at
   :season         season
   :target_week    target
   :through_week   through
   :lines          lines
   :kickoffs       kickoffs
   :injuries       injuries})

(defn write-snapshot!
  "Save `snap` under its season and target week; returns the path."
  [dir snap]
  (let [path (trending/snapshot-path dir (:season snap) (:target_week snap) (:fetched_at snap))]
    (io/make-parents path)
    (spit path (json/write-value-as-string snap trending/pretty-mapper))
    path))

(defn fetch-injuries
  "The designations `{sleeper-id {...}}`, or nil, with the reason on stderr."
  []
  (try (:injuries (sleeper-players/live))
       (catch Exception e
         (binding [*out* *err*] (println "injuries failed:" (ex-message e)))
         nil)))

(defn run
  "Fetch, save and report; returns the exit code."
  [{:keys [dir week]}]
  (try
    (let [season (trending/current-season)
          target (or (target-week week) (throw (ex-info "no week known; pass --week" {})))
          lines  (sleeper/fetch-weekly season target)]
      (when (empty? lines)
        (throw (ex-info (str "week " target " line empty") {})))
      (let [snap (snapshot (pipeline/now-iso) season target (trending/current-through-week)
                           lines
                           (or (espn-schedule/fetch season target) {})
                           (fetch-injuries))]
        (println (format "wrote %s (week %d: %d players, %d teams with kickoffs, %s injuries)"
                         (write-snapshot! dir snap) target (count lines) (count (:kickoffs snap))
                         (if-let [inj (:injuries snap)] (count inj) "no"))))
      0)
    (catch Exception e
      (binding [*out* *err*] (println "failed:" (ex-message e)))
      1)))

(defn file-instant
  "When a snapshot file was taken, read off its name: `2026-10-07T07-00-22Z.json`."
  [^java.io.File f]
  (let [[_ date h m s] (re-matches #"(\d{4}-\d{2}-\d{2})T(\d{2})-(\d{2})-(\d{2})Z\.json" (.getName f))]
    (when date (Instant/parse (str date "T" h ":" m ":" s "Z")))))

(defn week-summaries
  "One map per week folder under `dir`'s seasons, in order: `:path`, `:count`,
  `:first`, `:last` and `:longest-gap-hours` (nil for fewer than two files)."
  [dir]
  (->> (file-seq (io/file dir))
       (filter #(.isDirectory ^java.io.File %))
       (filter #(re-matches #"week-.+" (.getName ^java.io.File %)))
       (sort-by #(.getPath ^java.io.File %))
       (map (fn [^java.io.File d]
              (let [times (sort (keep file-instant (.listFiles d)))]
                {:path              (.getPath d)
                 :count             (count times)
                 :first             (first times)
                 :last              (last times)
                 :longest-gap-hours (when (next times)
                                      (->> (map vector times (rest times))
                                           (map (fn [[a b]] (.toMinutes (Duration/between a b))))
                                           (reduce max)
                                           (#(/ % 60.0))))})))))

(defn report-line
  "`data/projections/2026/week-05  3 files  2026-10-08T14:00:03Z to ...  longest gap 52.0h  GAP`"
  [gap-hours {:keys [path count first last longest-gap-hours]}]
  (format "%s  %d file%s  %s to %s  longest gap %s%s"
          path count (if (= 1 count) "" "s") first last
          (if longest-gap-hours (format "%.1fh" longest-gap-hours) "-")
          (if (and longest-gap-hours (> longest-gap-hours gap-hours)) "  GAP" "")))

(defn report
  "Print a line per week folder under `dir`; returns the exit code."
  [{:keys [dir gap-hours]}]
  (let [weeks (week-summaries dir)]
    (if (empty? weeks)
      (println "no snapshots under" dir)
      (run! (comp println (partial report-line gap-hours)) weeks))
    0))

(defn -main [& args]
  (let [opts (parse-args args)]
    (System/exit (if (:report? opts) (report opts) (run opts)))))
