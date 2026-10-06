(ns draft-day.ingestion.sleeper-trending
  "Which players Sleeper's managers are adding right now: the adds over the last
  `lookback-hours` across every Sleeper league, keyless, for the waiver board.

  It stands in for expert consensus. A player the whole site is adding is one a
  league's rivals are about to bid on, which no projection says until his role
  has already shown up in the box score. `rankings.faab` reads it as heat on a
  rival's interest.

  Sleeper publishes the hundred most-added players and no more, keyed by its own
  player id, and a team defense by its abbreviation. A player off the list has
  no key at all rather than zero adds, since the list does not say he had none.

  Cached apart from the universe and the weekly line on its own TTL
  (`DRAFTDAY_TRENDING_TTL_HOURS`, default 1): it moves by the hour, and sharing
  either file's freshness would either refetch the universe hourly or serve
  Tuesday's adds on Sunday. A fetch that fails, or answers with an empty list,
  serves the last cached list with its own `:fetched-at` and is not retried for
  `pipeline/failure-backoff-ms`, since every Sleeper caller shares
  `sleeper-http`'s permits. Offline there is none, and neither is there while
  `DRAFTDAY_AS_OF_WEEK` replays a past week: today's list is news that week
  never had.

  Every live fetch also keeps a snapshot under `default-dir`, as JSON in a
  `<season>/week-NN` folder named for when it was taken. Sleeper keeps no past lists, so
  these are the only record a backtest could ever measure `faab/heat-weight`
  against. They accumulate as the board is used, and the `trends` harness
  (`dev/draft_day/trends.clj`) writes the same files on whatever schedule it is
  run, with drops and other windows too."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [draft-day.ingestion.nflverse-weekly :as nflverse-weekly]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.ingestion.season :as season]
            [draft-day.ingestion.sleeper-http :as sleeper-http]
            [draft-day.json :refer [mapper]]
            [jsonista.core :as json]))

(def lookback-hours 48)

(def schema-version 1)

(def default-cache-path (str "data/trending_adds.v" schema-version ".transit"))

(def snapshot-schema-version 1)

(def default-dir "data/trends")

(def default-limit 100)

(defn trending-url
  "The list's URL: `:type` \"add\" or \"drop\", `:lookback-hours`, `:limit`. The
  board reads the defaults, the 48-hour adds."
  ([] (trending-url {}))
  ([{:keys [type limit] :as opts}]
   (str "https://api.sleeper.app/v1/players/nfl/trending/" (or type "add")
        "?lookback_hours=" (or (:lookback-hours opts) lookback-hours)
        "&limit=" (or limit default-limit))))

(defn ttl-hours []
  (Double/parseDouble (or (System/getenv "DRAFTDAY_TRENDING_TTL_HOURS") "1")))

(defn fetch-raw
  "Network: Sleeper's trending list, `[{:player_id :count}]`, for the options
  `trending-url` takes."
  ([] (fetch-raw {}))
  ([opts]
   (let [{:keys [status body error]} (sleeper-http/get! (trending-url opts) {:timeout 15000})]
     (cond
       error          (throw (ex-info "Sleeper trending fetch failed" {:error error}))
       (= 200 status) (json/read-value body mapper)
       :else          (throw (ex-info "Sleeper trending non-200" {:status status}))))))

(defn normalize
  "`{sleeper-id adds}`, ids as strings, and only positive counts: heat is read
  on a log scale."
  [raw]
  (into {}
        (keep (fn [{:keys [player_id count]}]
                (when (and player_id (number? count) (pos? count))
                  [(str player_id) (long count)])))
        raw))

(defn fetch-list
  "Network: one list, `{:type :lookback_hours :players [{:rank :player_id :count}]}`,
  ranked by count. `opts` are the ones `trending-url` takes. An empty answer
  throws, since Sleeper always has somebody trending and caching nobody would
  read as a fresh answer."
  [opts]
  (let [type   (or (:type opts) "add")
        hours  (or (:lookback-hours opts) lookback-hours)
        counts (normalize (fetch-raw (assoc opts :type type :lookback-hours hours)))]
    (when (empty? counts)
      (throw (ex-info (str "Sleeper trending " type " list empty") {})))
    {:type           type
     :lookback_hours hours
     :players        (->> (sort-by (fn [[id n]] [(- n) id]) counts)
                          (map-indexed (fn [i [id n]] {:rank (inc i) :player_id id :count n}))
                          vec)}))

(defn counts-of
  "`{sleeper-id count}` for one list."
  [{:keys [players]}]
  (into {} (map (juxt :player_id :count)) players))

(defn adds-of
  "`{sleeper-id count}` for a snapshot's adds over `hours`; empty when it has none."
  [snap hours]
  (->> (:lists snap)
       (filter #(and (= "add" (:type %)) (= hours (:lookback_hours %))))
       first
       counts-of))

(defn current-season
  "The season the week is read for and the snapshot filed under: the calendar
  year, as `season/resolve-season` defaults it, so a January playoff week lands
  under the new year."
  []
  (season/resolve-season nil))

(defn current-through-week
  "Weeks played, from the realized cache the app itself reads; nil when nflverse
  is out of reach."
  []
  (pipeline/best-effort
   (some-> (current-season) pipeline/load-realized :weekly :through-week)))

(defn snapshot
  "The document a snapshot file holds. Players carry `:name`, `:pos` and `:team`
  only when whoever wrote it had a universe to name them from."
  [iso season week limit lists]
  {:schema_version snapshot-schema-version
   :fetched_at     iso
   :season         season
   :through_week   week
   :limit          limit
   :lists          lists})

(defn week-dir
  "`week-04`, or `week-unknown`."
  [week]
  (if week (format "week-%02d" week) "week-unknown"))

(defn snapshot-path
  "`<dir>/2026/week-04/2026-10-07T07-00-22Z.json`: the season, the weeks played,
  and the time to the second, colons out of the name for filesystems that
  refuse them. A season that is not known is `unknown-season`."
  [dir season week iso]
  (str dir "/" (or season "unknown-season") "/" (week-dir week) "/"
       (str/replace (str/replace iso #"\.\d+Z$" "Z") ":" "-") ".json"))

(def pretty-mapper (json/object-mapper {:pretty true}))

(defn write-snapshot!
  "Save `snap` where `snapshot-path` puts it; returns the path."
  [dir snap]
  (let [path (snapshot-path dir (:season snap) (:through_week snap) (:fetched_at snap))]
    (io/make-parents path)
    (spit path (json/write-value-as-string snap pretty-mapper))
    path))

(defn live
  "The envelope for a fresh list, keeping a snapshot of it. A snapshot that fails
  to write costs the record one list, never the board the list it just
  fetched."
  [dir]
  (let [lst (fetch-list {})
        iso (pipeline/now-iso)]
    (pipeline/best-effort
     (write-snapshot! dir (snapshot iso (current-season) (current-through-week) default-limit [lst])))
    {:lookback-hours lookback-hours
     :fetched-at     iso
     :adds           (counts-of lst)}))

(defn load-adds
  "`{:fetched-at :lookback-hours :adds {sleeper-id adds}}`, fresh from the cache,
  else live, else whatever is cached however old; nil offline, replaying a past
  week, or with nothing to serve."
  ([] (load-adds {}))
  ([{:keys [path dir] :or {path default-cache-path dir default-dir}}]
   (when-not (or (pipeline/offline?) (nflverse-weekly/as-of-week))
     (pipeline/load-ttl-cache {:path           path
                               :ttl-hours      (ttl-hours)
                               :schema-version schema-version
                               :fetch          #(live dir)
                               :label          "trending adds"}))))
