(ns draft-day.ingestion.nflverse
  "Enrichment: last season's realized usage — targets, receptions, target share.

  Free, keyless CSV releases on GitHub, keyed by GSIS id, which makes this the
  one enrichment source in the app that joins *exactly* rather than by name.
  Every universe player already carries a GSIS id (`player-ids/attach-ids`), so
  there is no reason to fall back to a name key here.

  These are facts about a season that has already happened, not projections, so
  they are deliberately NOT scoped per scoring format — unlike FantasyPros ECR
  or auction values, a target is a target in every league.

  Two traps, both of which have bitten this codebase before:

  RATE VS COUNT. `target_share` is already a **season rate** (Ja'Marr Chase 2025:
  0.304). `targets` and `receptions` are raw **counts**. Dividing the share by
  games — the obvious-looking move sitting next to the count columns — turns a
  rate into rate-per-game and silently inflates the usage of anyone who missed
  time. Counts are carried as season totals, which is how a manager quotes them
  (\"he saw 185 targets\"); `:nflverse/prior-games` rides along so a partial
  season is legible rather than looking like a decline.

  BLANK IS NOT ZERO. nflverse writes both \"\" and \"NA\" for missing, and a
  rookie has no row here at all. Every field stays nil when absent so the board
  renders a dash. Zero-filling — what the benchmark harness does, correctly, for
  modeling — would print a confident 0 for a player this source simply has no
  opinion about.

  (`draft-day.benchmark.sources.nflverse` covers the same feed for the research
  harness. It lives on the :dev source path and is not on the app's classpath,
  so the discipline above is restated here rather than shared.)"
  (:require [clojure.data.csv :as csv]
            [clojure.string :as str])
  (:import [java.net.http HttpClient HttpClient$Redirect HttpRequest
            HttpResponse$BodyHandlers]
           [java.net URI]))

(def ^:private base
  "https://github.com/nflverse/nflverse-data/releases/download/stats_player")

(defn season-url [season] (str base "/stats_player_reg_" season ".csv"))

(defn parse-csv
  "CSV text -> vector of maps keyed by header string. Values stay strings;
  coercion is this namespace's business, since \"\" and \"NA\" both mean missing."
  [text]
  (let [[header & rows] (csv/read-csv text)]
    (mapv #(zipmap header %) rows)))

(def fantasy-positions
  "The positions worth joining. nflverse publishes every player who took a snap —
  about 2000 rows a season, two thirds of them linemen, punters and defensive
  backs the fantasy universe has no row for. Keeping them would drag the join's
  hit rate to ~30% and trip `pipeline/log-enrichment!`'s per-position warning for
  a dozen positions, turning a healthy join into a wall of noise. There are no
  DST rows here — team defenses are a fantasy construct, not an nflverse player."
  #{"QB" "RB" "WR" "TE" "K"})

(defn num-or-nil
  "Parse a cell as a double, or nil when the source has no value for it."
  [s]
  (when-not (or (nil? s) (= "" s) (= "NA" s))
    (try (Double/parseDouble s) (catch Exception _ nil))))

(defn row->usage
  "Pure: one nflverse row -> the prior-season usage columns, or nil for a row
  with no GSIS id to join on.

  Counts stay season totals and the share stays untouched — see the ns docstring
  on RATE VS COUNT. Fields the source left blank stay absent rather than zero."
  [season row]
  (when-let [gsis (and (fantasy-positions (get row "position"))
                       (not-empty (str/trim (str (get row "player_id")))))]
    [gsis
     (into {:nflverse/prior-season season}
           (remove (comp nil? val))
           {:nflverse/prior-games        (num-or-nil (get row "games"))
            ;; counts — season totals, do NOT divide by games
            :nflverse/prior-targets      (num-or-nil (get row "targets"))
            :nflverse/prior-receptions   (num-or-nil (get row "receptions"))
            ;; already a season rate — do NOT divide by games
            :nflverse/prior-target-share (num-or-nil (get row "target_share"))})]))

(defn row-positions
  "Pure: {gsis-id position} over the rows, so the join's per-position hit-rate
  report means something. The GSIS key carries no position of its own, unlike
  the name keys `merge/key-position` was written for."
  [rows]
  (into {} (keep (fn [r]
                   (when-let [gsis (and (fantasy-positions (get r "position"))
                                        (not-empty (str/trim (str (get r "player_id")))))]
                     [gsis (get r "position")])))
        rows))

(defn enrichment
  "Pure: nflverse rows -> {gsis-id {columns}} for the given season."
  [season rows]
  (into {} (keep #(row->usage season %)) rows))

(defn- http-get-string
  "The JDK client rather than http-kit, and following redirects explicitly:
  nflverse release URLs 302 to object storage, and the JDK client does not
  follow redirects by default."
  [url]
  (let [client (-> (HttpClient/newBuilder)
                   (.followRedirects HttpClient$Redirect/NORMAL)
                   (.build))
        resp   (.send client
                      (.build (HttpRequest/newBuilder (URI/create url)))
                      (HttpResponse$BodyHandlers/ofString))]
    (when (= 200 (.statusCode resp)) (.body resp))))

(defn fetch
  "Network: prior-season usage for `season` -> {gsis-id {columns}}, plus the
  position index the join reports with. nil on failure.

  Returns `{:by-key ... :positions ...}` rather than a bare map because the
  caller needs both halves and reparsing the CSV to get the second would be
  silly."
  [season]
  (when-let [body (http-get-string (season-url season))]
    (let [rows (parse-csv body)]
      {:by-key    (enrichment season rows)
       :positions (row-positions rows)})))
