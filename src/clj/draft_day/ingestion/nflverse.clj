(ns draft-day.ingestion.nflverse
  "Enrichment: what a player actually did last season, and how much of the last
  few seasons he was available for.

  Free, keyless CSV releases on GitHub, keyed by GSIS id, which makes this the
  one enrichment source in the app that joins *exactly* rather than by name.
  Every universe player already carries a GSIS id (`player-ids/attach-ids`), so
  there is no reason to fall back to a name key here.

  These are facts about seasons that have already happened, not projections, so
  they are deliberately NOT scoped per scoring format — unlike FantasyPros ECR
  or auction values, a target is a target in every league.

  One fetch, two questions. The usage columns come from the newest season alone;
  the availability columns need the whole window, and re-downloading the newest
  season for the second question would be silly. Hence one task and one
  `:sources` label (`:nflverse/player-stats`) rather than two.

  Three traps, all of which have bitten this codebase or its author before:

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

  A MISSING SEASON IS NOT A MISSED SEASON. `:nflverse/games-by-season` holds only
  the seasons a player actually has a row in, and `:nflverse/games-seasons` maps
  every season that was *fetched* to how many games that season held. Consumers
  need both halves: a gap means \"he played none\" only if that season was fetched
  at all — a season the network lost would otherwise read, board-wide, as
  everybody missing seventeen games — and the season lengths ride along so a
  consumer can size a gap without knowing the NFL calendar. Deciding what a gap
  *means* is still `rankings.injury`'s job, not this namespace's.

  Why games played and not the weekly injury report: nflverse publishes one
  (`injuries/injuries_{season}.csv`, with Out/Doubtful/Questionable per week) and
  it looks like the better source until you check it. A player who lands on
  season-ending IR drops off the report entirely — Malik Nabers played 4 games in
  2025 and has *zero* \"Out\" weeks in that file. Counting designations would
  therefore rate the worst injury of the season as iron-man durable. Games played
  cannot miss that, at the cost of not knowing *why* a game was missed.

  (`draft-day.benchmark.sources.nflverse` covers the same feed for the research
  harness. It lives on the :dev source path and is not on the app's classpath,
  so the discipline above is restated here rather than shared.)"
  (:require [clojure.data.csv :as csv]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [draft-day.ingestion.parallel :as parallel])
  (:import [java.net.http HttpClient HttpClient$Redirect HttpRequest
            HttpResponse$BodyHandlers]
           [java.net URI]))

(def ^:private base
  "https://github.com/nflverse/nflverse-data/releases/download/stats_player")

(defn season-url [season] (str base "/stats_player_reg_" season ".csv"))

(def availability-lookback
  "How many completed seasons the availability window reaches back over.

  Three, unweighted. Durability is a multi-season property, so one season makes a
  single fluke read as chronic; much beyond three and a player's early career
  outweighs the shape he is in now. Weighting the recent seasons harder is the
  obvious next move and is deliberately not made here — the weights would be
  invented rather than measured, and `dev/draft_day/benchmark/` is where a
  formula earns its numbers before it ships."
  3)

(defn games-in-season
  "How many regular-season games a team played in `season`. 17 since the 2021
  expansion, 16 before it — the app only ever asks about recent seasons, but the
  benchmark harness reaches back past the boundary and a hardcoded 17 would hand
  it a free missed game for every player in every older season."
  [season]
  (if (>= season 2021) 17 16))

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

(defn- gsis-id
  "The row's GSIS id, or nil when it has none or is not a position we join."
  [row]
  (when (fantasy-positions (get row "position"))
    (not-empty (str/trim (str (get row "player_id"))))))

(defn row->usage
  "Pure: one nflverse row -> the prior-season usage columns, or nil for a row
  with no GSIS id to join on.

  Counts stay season totals and the share stays untouched — see the ns docstring
  on RATE VS COUNT. Fields the source left blank stay absent rather than zero."
  [season row]
  (when-let [gsis (gsis-id row)]
    [gsis
     (into {:nflverse/prior-season season}
           (remove (comp nil? val))
           {:nflverse/prior-games        (num-or-nil (get row "games"))
            ;; counts — season totals, do NOT divide by games
            :nflverse/prior-targets      (num-or-nil (get row "targets"))
            :nflverse/prior-receptions   (num-or-nil (get row "receptions"))
            ;; already a season rate — do NOT divide by games
            :nflverse/prior-target-share (num-or-nil (get row "target_share"))})]))

(defn row->games
  "Pure: one nflverse row -> [gsis games-played], or nil when there is nothing to
  join on or no games figure at all.

  Clamped to the season's length: the 2025 file carries an 18 for at least one
  player, and an unclamped count would hand him a *negative* missed-game total
  that averages away somebody else's real absence."
  [season row]
  (when-let [gsis (gsis-id row)]
    (when-let [g (num-or-nil (get row "games"))]
      [gsis (min g (double (games-in-season season)))])))

(defn row-positions
  "Pure: {gsis-id position} over the rows, so the join's per-position hit-rate
  report means something. The GSIS key carries no position of its own, unlike
  the name keys `merge/key-position` was written for."
  [rows]
  (into {} (keep (fn [r] (when-let [gsis (gsis-id r)] [gsis (get r "position")])))
        rows))

(defn enrichment
  "Pure: nflverse rows -> {gsis-id {columns}} for the given season."
  [season rows]
  (into {} (keep #(row->usage season %)) rows))

(defn season-games
  "Pure: nflverse rows -> {gsis-id games-played} for the given season."
  [season rows]
  (into {} (keep #(row->games season %)) rows))

(defn availability
  "Pure: {season rows} -> {gsis-id {:nflverse/games-by-season {season games}
                                    :nflverse/games-seasons  {season length}}}.

  `:nflverse/games-seasons` is the same map on every player and is carried per
  player anyway, exactly as `:nflverse/prior-season` is: it has to survive the
  left-join, the Transit cache and the committed sample, and a player row is the
  only thing that does all three. Mapping each fetched season to its length —
  rather than just listing the seasons — is what lets a consumer size a season it
  has no row for without importing this namespace's calendar.

  A player with no row in any fetched season is absent from the result entirely —
  he gets no columns rather than a window full of zeroes."
  [rows-by-season]
  (let [lengths (into (sorted-map)
                      (map (fn [s] [s (games-in-season s)]))
                      (keys rows-by-season))]
    (reduce (fn [acc season]
              (reduce-kv (fn [acc gsis g]
                           (-> acc
                               (assoc-in [gsis :nflverse/games-by-season season] g)
                               (assoc-in [gsis :nflverse/games-seasons] lengths)))
                         acc
                         (season-games season (get rows-by-season season))))
            {}
            (sort (keys rows-by-season)))))

(defn http-get-string
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

(def required-columns
  "Columns a response has to carry before it counts as this file at all.

  The check exists because the failure that matters here is not a 404 — it is a
  200 with the wrong body. A release URL that redirects to an error page, an
  object store that answers with XML, a truncated download: each parses without
  throwing, and each yields rows that carry no GSIS id, which reaches
  `availability` as a season that was fetched and in which nobody played."
  #{"player_id" "position" "games"})

(defn fetch-season-rows
  "Network: one season's parsed rows, or nil when the release did not yield any.

  Every failure shape collapses to that one nil, because `fetch` distinguishes
  only fetched from not-fetched, and here a half-answer is worse than no answer:
  a season recorded as fetched but empty is one `availability` charges every
  player in the league seventeen missed games for.

  So two things are deliberately caught rather than allowed through. A throw —
  connection reset, read timeout, malformed CSV — stays local instead of
  escaping into `parallel/all`, where it would nil the *whole* source and take
  the sibling seasons and the usage columns down with it; a lost season is
  supposed to narrow the window, not close it. And a 200 whose body is empty or
  is not this file (see `required-columns`) is reported as the miss it is."
  [season]
  (try
    (let [rows (some-> (http-get-string (season-url season)) parse-csv)]
      (when (and (seq rows)
                 (every? (set (keys (first rows))) required-columns))
        rows))
    (catch Exception e
      ;; Type and message, not the throwable: the stack trace of a read timeout
      ;; is JDK frames all the way down and says nothing the message does not.
      (log/warn "nflverse: season" season "unavailable:"
                (.getSimpleName (class e)) (ex-message e))
      nil)))

(defn fetch
  "Network: prior-season usage for `season` plus games played across the
  `availability-lookback` seasons ending there -> {gsis-id {columns}}, with the
  position index the join reports with. nil when `season` itself is unreachable.

  The seasons go out together — they are independent GETs of ~880KB each against
  one host, and awaiting them in turn would triple the cold-load stall this
  source already costs.

  An older season that fails to fetch is dropped from the window rather than
  faked: `availability` records the seasons it actually got, so the scale
  narrows its denominator instead of charging every player in the league
  seventeen missed games. The newest season failing is different — it carries the
  usage columns too — so that returns nil and the whole source reports
  unavailable, which is the honest report."
  [season]
  (let [window (range (inc (- season availability-lookback)) (inc season))
        rows   (->> (into {} (map (fn [s] [s #(fetch-season-rows s)])) window)
                    parallel/all
                    (into {} (remove (comp nil? val))))]
    (when-let [newest (get rows season)]
      {:by-key    (merge-with merge
                              (enrichment season newest)
                              (availability rows))
       :positions (row-positions newest)})))
