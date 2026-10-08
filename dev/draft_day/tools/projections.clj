(ns draft-day.tools.projections
  "Save the week's Sleeper projection entries, its kickoffs and the injury list
  as JSON, for scoring the weekly board against what happened.

  Sleeper revises a past week's line after the games (its `updated_at` lands
  after the week), so the line the board showed on Sunday morning cannot be
  fetched later. Each run is one live fetch and one file; when to run it is up
  to whoever runs it.

    lein run -m draft-day.tools.projections [--week N] [--dir data/projections]
    lein run -m draft-day.tools.projections --report [--dir data/trends]
                                 [--gap-hours 72] [--min-files 3]

  The file is `<dir>/<season>/week-NN/<UTC time>.json`, NN being the week the
  line is *for* (the week being played), where `tools.trends` files by weeks
  played. Both are in the document as `target_week` and `through_week`. The
  season and week come from one Sleeper state reply, so a January playoff week
  keeps the season it belongs to. `lines` holds Sleeper's own entries, with
  every stat and its point totals, for the players it projects; `injuries` is
  the complete designation list as of the run. If the line comes back empty
  nothing is written; a failed injury or kickoff fetch is reported on stderr and
  leaves its key null or empty while the line still lands. `through_week` is
  read through the realized cache, as `tools.trends` reads it, and is null when
  that is out of reach.

  `--report` lists the snapshots under a directory per week with the longest gap
  between two of them, and flags a gap over `--gap-hours`, a finished week with
  fewer than `--min-files` files, a week missing between the first and the last,
  and a newest snapshot older than `--gap-hours`. It works on `data/trends` as
  well, where a count of 24 or more a day is the norm.

  Run from cron in Denver time, Wednesday 08:00 (after Sleeper's overnight
  waiver run), Thursday 16:30 (before Thursday night's kickoff) and Sunday 10:15
  (after the early window's inactives, 90 minutes before its 11:00 kickoff).

  Exit codes: 0 written (or reported), 1 the line failed or a flag was bad."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [draft-day.ingestion.espn-schedule :as espn-schedule]
            [draft-day.ingestion.matchups.sleeper :as sleeper-state]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.ingestion.sleeper :as sleeper]
            [draft-day.ingestion.sleeper-players :as sleeper-players]
            [draft-day.ingestion.sleeper-trending :as trending]
            [jsonista.core :as json])
  (:import (java.time Duration Instant)))

(def schema-version 1)

(def default-dir "data/projections")

(def default-gap-hours 72)

(def default-min-files 3)

(defn flag-value
  "The text after `flag` in `args`, nil if the flag is absent. A flag with
  nothing after it throws."
  [args flag]
  (when-let [i (first (keep-indexed (fn [i a] (when (= a flag) i)) args))]
    (or (nth args (inc i) nil)
        (throw (ex-info (str flag " needs a value") {})))))

(defn whole-number
  "`text` as a positive integer; throws naming `flag` otherwise."
  [flag text]
  (let [n (parse-long text)]
    (if (and n (pos? n))
      n
      (throw (ex-info (str flag " needs a positive whole number, got " text) {})))))

(defn parse-args
  "The options `args` give over the defaults. A flag it does not know is ignored,
  as in `tools.trends`; a number that is not one throws."
  [args]
  (let [value   (partial flag-value args)
        numeric (fn [opts k flag]
                  (if-let [text (value flag)] (assoc opts k (whole-number flag text)) opts))]
    (-> {:dir default-dir :gap-hours default-gap-hours :min-files default-min-files}
        (cond-> (some #{"--report"} args) (assoc :report? true)
                (value "--dir")           (assoc :dir (value "--dir")))
        (numeric :week "--week")
        (numeric :gap-hours "--gap-hours")
        (numeric :min-files "--min-files"))))

(defn season-and-week
  "`{:season :week}` off one Sleeper state reply: `week` if given, else the week
  being played, nil when Sleeper does not say. The season is the state's own,
  else the calendar year's. The week is never derived from the weeks nflverse
  has finished, which names next week once Thursday's game is in."
  [week]
  (let [state (pipeline/best-effort (sleeper-state/nfl-state))]
    {:season (or (some-> state :season str parse-long) (trending/current-season))
     :week   (or week (sleeper-state/state-week state))}))

(defn lines-of
  "`{sleeper-id entry}` for the raw `entries` Sleeper projects a score for. The
  entry keeps its stat map whole, with the point totals, and drops the player
  record riding along on it, whose injury fields are the stale ones."
  [entries]
  (into {}
        (keep (fn [{:keys [player_id stats] :as entry}]
                (when (and player_id (:pts_ppr stats))
                  [player_id (select-keys entry [:stats :team :opponent :game_id :status
                                                 :company :updated_at :last_modified])])))
        entries))

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

(defn warn!
  "`msg` on stderr."
  [msg]
  (binding [*out* *err*] (println msg)))

(defn fetch-injuries
  "The designations `{sleeper-id {...}}`, or nil, with the reason on stderr."
  []
  (try (:injuries (sleeper-players/live))
       (catch Exception e
         (warn! (str "injuries failed: " (ex-message e)))
         nil)))

(defn fetch-kickoffs
  "The week's kickoffs by team, or `{}`, with a note on stderr when ESPN gave
  none."
  [season week]
  (or (espn-schedule/fetch season week)
      (do (warn! "kickoffs failed: ESPN scoreboard gave no teams") {})))

(defn run
  "Fetch, save and report; returns the exit code."
  [{:keys [dir week]}]
  (try
    (let [fetched-at (pipeline/now-iso)
          {:keys [season] target :week} (season-and-week week)]
      (when-not target
        (throw (ex-info "no week known; pass --week" {})))
      (let [lines (lines-of (sleeper/fetch-weekly-entries season target))]
        (when (empty? lines)
          (throw (ex-info (str "week " target " line empty") {})))
        (let [snap (snapshot fetched-at season target
                             (pipeline/best-effort
                              (some-> season pipeline/load-realized :weekly :through-week))
                             lines
                             (fetch-kickoffs season target)
                             (fetch-injuries))]
          (println (format "wrote %s (week %d: %d players, %d teams with kickoffs, %s injuries)"
                           (trending/write-snapshot! dir snap target) target (count lines)
                           (count (:kickoffs snap))
                           (if-let [inj (:injuries snap)] (count inj) "no")))))
      0)
    (catch Exception e
      (warn! (str "failed: " (ex-message e)))
      1)))

(defn file-instant
  "When a snapshot file was taken, read off its name: `2026-10-07T07-00-22Z.json`."
  [^java.io.File f]
  (let [[_ date h m s] (re-matches #"(\d{4}-\d{2}-\d{2})T(\d{2})-(\d{2})-(\d{2})Z\.json" (.getName f))]
    (when date (Instant/parse (str date "T" h ":" m ":" s "Z")))))

(defn week-summaries
  "One map per week folder under `dir`, in order: `:season` and `:week` (nil for
  `week-unknown`) off the path, `:path`, `:count`, `:first`, `:last` and
  `:longest-gap-hours` (nil for fewer than two files)."
  [dir]
  (->> (file-seq (io/file dir))
       (filter #(.isDirectory ^java.io.File %))
       (filter #(re-matches #"week-.+" (.getName ^java.io.File %)))
       (sort-by #(.getPath ^java.io.File %))
       (map (fn [^java.io.File d]
              (let [times (sort (keep file-instant (.listFiles d)))]
                {:path              (.getPath d)
                 :season            (.getName (.getParentFile d))
                 :week              (some-> (re-matches #"week-(\d+)" (.getName d)) second parse-long)
                 :count             (count times)
                 :first             (first times)
                 :last              (last times)
                 :longest-gap-hours (when (next times)
                                      (->> (map vector times (rest times))
                                           (map (fn [[a b]] (.toMinutes (Duration/between a b))))
                                           (reduce max)
                                           (#(/ % 60.0))))})))))

(defn flags
  "The problems with one week: `GAP` for a gap over `gap-hours`, `FEW` for fewer
  than `min-files` files. The newest folder is still filling, so it is never
  `FEW`."
  [{:keys [gap-hours min-files]} newest? {n :count :keys [longest-gap-hours]}]
  (cond-> []
    (and longest-gap-hours (> longest-gap-hours gap-hours)) (conj "GAP")
    (and (not newest?) (< n min-files))                     (conj "FEW")))

(defn missing-weeks
  "`[season week]` for each week absent between a season's first and last."
  [summaries]
  (mapcat (fn [[season ws]]
            (let [present (set (keep :week ws))]
              (when (seq present)
                (map (fn [w] [season w])
                     (remove present (range (apply min present) (apply max present)))))))
          (group-by :season summaries)))

(defn report-line
  "`data/projections/2026/week-05  3 files  2026-10-08T14:00:03Z to ...  longest gap 52.0h  GAP`"
  [flagged {:keys [path longest-gap-hours] n :count start :first end :last}]
  (str/join "  " (concat [path
                          (format "%d file%s" n (if (= 1 n) "" "s"))
                          (str start " to " end)
                          (str "longest gap " (if longest-gap-hours (format "%.1fh" longest-gap-hours) "-"))]
                         flagged)))

(defn report
  "Print a line per week folder under `dir`, then each missing week and the age
  of the newest snapshot; returns the exit code. `now` is the clock, for tests."
  [{:keys [dir gap-hours now] :as opts :or {now (Instant/now)}}]
  (let [weeks  (week-summaries dir)
        newest (last (sort-by :last (filter :last weeks)))]
    (if (empty? weeks)
      (println "no snapshots under" dir)
      (do
        (run! (fn [w] (println (report-line (flags opts (identical? w newest) w) w))) weeks)
        (run! (fn [[season w]] (println (format "%s/week-%02d  MISSING" season w))) (missing-weeks weeks))
        (when newest
          (let [age (/ (.toMinutes (Duration/between (:last newest) now)) 60.0)]
            (println (format "newest snapshot %s, %.1fh ago%s" (:last newest) age
                             (if (> age gap-hours) "  STALE" "")))))))
    0))

(defn -main [& args]
  (System/exit
   (try (let [opts (parse-args args)]
          (if (:report? opts) (report opts) (run opts)))
        (catch clojure.lang.ExceptionInfo e
          (warn! (str "failed: " (ex-message e)))
          1))))
