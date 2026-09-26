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
  Tuesday's adds on Sunday. A fetch that fails serves the last cached list with
  its own `:fetched-at`, and offline there is none.

  Every live fetch also keeps a copy under `snapshot-dir`, named for when it was
  taken. Sleeper keeps no past lists, so these are the only record a backtest
  could ever measure `faab/heat-weight` against, and they accumulate as the
  board is used rather than on anyone's schedule."
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.ingestion.sleeper-http :as sleeper-http]
            [draft-day.json :refer [mapper]]
            [jsonista.core :as json]))

(def lookback-hours 48)

(def schema-version 1)

(def default-cache-path (str "data/trending_adds.v" schema-version ".transit"))

(def snapshot-dir "data/faab_cache/trending")

(defn trending-url []
  (str "https://api.sleeper.app/v1/players/nfl/trending/add?lookback_hours="
       lookback-hours "&limit=100"))

(defn ttl-hours []
  (Double/parseDouble (or (System/getenv "DRAFTDAY_TRENDING_TTL_HOURS") "1")))

(defn fetch-raw
  "Network: Sleeper's trending list, `[{:player_id :count}]`."
  []
  (let [{:keys [status body error]} (sleeper-http/get! (trending-url) {:timeout 15000})]
    (cond
      error          (throw (ex-info "Sleeper trending fetch failed" {:error error}))
      (= 200 status) (json/read-value body mapper)
      :else          (throw (ex-info "Sleeper trending non-200" {:status status})))))

(defn normalize
  "`{sleeper-id adds}`, ids as strings."
  [raw]
  (into {}
        (keep (fn [{:keys [player_id count]}]
                (when (and player_id (number? count))
                  [(str player_id) (long count)])))
        raw))

(defn snapshot-path
  "Where a list fetched at `iso` is kept, colons out of the name for
  filesystems that refuse them."
  [dir iso]
  (str dir "/adds-" (str/replace iso ":" "-") ".transit"))

(defn live!
  "Fetch, normalize and cache the list, keeping a snapshot of it. A snapshot
  that will not write costs the record one list, never the board its list."
  [path dir]
  (let [env {:schema-version schema-version
             :lookback-hours lookback-hours
             :fetched-at     (pipeline/now-iso)
             :adds           (normalize (fetch-raw))}]
    (pipeline/write-transit! path env)
    (pipeline/best-effort (pipeline/write-transit! (snapshot-path dir (:fetched-at env)) env))
    env))

(defn read-cached [path]
  (try
    (let [env (pipeline/read-transit path)]
      (when (= schema-version (:schema-version env)) env))
    (catch Exception e
      (log/warn e "trending cache unreadable; refetching")
      nil)))

(defn load-adds
  "`{:fetched-at :lookback-hours :adds {sleeper-id adds}}`, fresh from the cache,
  else live, else whatever is cached however old; nil offline or with nothing
  to serve."
  ([] (load-adds {}))
  ([{:keys [path dir] :or {path default-cache-path dir snapshot-dir}}]
   (when-not (pipeline/offline?)
     (let [cached (read-cached path)]
       (if (and cached (pipeline/cache-fresh? path (ttl-hours)))
         cached
         (try (live! path dir)
              (catch Exception e
                (log/warn e "trending adds fetch failed:" (ex-message e))
                cached)))))))
