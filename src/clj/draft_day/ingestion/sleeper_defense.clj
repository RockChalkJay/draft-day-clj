(ns draft-day.ingestion.sleeper-defense
  "Team defenses' completed seasons, from Sleeper's season totals.

  nflverse publishes no DST row (`nflverse/fantasy-positions`), so a defense has
  no history from the source every other position's comes from. Sleeper keeps a
  season total per team defense, keyed by the team abbreviation that is also the
  defense's player id in the universe, so the join is exact.

  The columns are shaped like `nflverse`'s on purpose — `:nflverse/history`,
  `:nflverse/games-by-season`, `:nflverse/games-seasons` — because
  `draft-day.stat-lines` reads one vocabulary for every position, and a second
  key set for defenses would be a second table renderer. The key names promise a
  source this namespace does not use; the values are the same facts.

  Sleeper omits a stat a defense never accrued, so a season that was played
  fills every stat key it lacks with 0: a defense that blocked no kicks blocked
  none, and the table shows a 0 where a player the file says nothing about gets
  a dash."
  (:require [clojure.tools.logging :as log]
            [jsonista.core :as json]
            [draft-day.ingestion.nflverse :as nflverse]
            [draft-day.ingestion.parallel :as parallel]
            [draft-day.ingestion.sleeper-http :as sleeper-http]
            [draft-day.json :refer [mapper]]))

(def stat-keys
  "The totals a defense's season row is read from."
  [:sack :int :blk_kick :ff :fum_rec :yds_allow :pts_allow])

(defn season-url [season]
  (str "https://api.sleeper.app/stats/nfl/" season
       "?season_type=regular&position[]=DEF"))

(defn fetch-season
  "Network: raw season-total entries for `season`, or nil on any failure. One
  lost season narrows the window, like `nflverse/fetch-season-rows`."
  [season]
  (try
    (let [{:keys [status body error]} (sleeper-http/get! (season-url season)
                                                          {:timeout 30000})]
      (when (and (not error) (= 200 status))
        (json/read-value body mapper)))
    (catch Exception e
      (log/warn "sleeper-defense: season" season "unavailable:"
                (.getSimpleName (class e)) (ex-message e))
      nil)))

(defn entry->line
  "Pure: one raw entry -> [id {stat-key total} games], or nil without a team id
  or a played game."
  [{:keys [player_id stats]}]
  (let [gp (:gp stats)]
    (when (and player_id (number? gp) (pos? gp))
      [(str player_id)
       (into {} (map (fn [k] [k (double (or (get stats k) 0.0))])) stat-keys)
       (double gp)])))

(defn history
  "Pure: {season entries} -> {team-id {:nflverse/history [...] ...}}, oldest
  season first, in the shape `nflverse/history` and `nflverse/availability`
  produce between them."
  [entries-by-season]
  (let [lengths (into (sorted-map)
                      (map (fn [s] [s (nflverse/games-in-season s)]))
                      (keys entries-by-season))]
    (reduce
     (fn [acc season]
       (reduce (fn [acc entry]
                 (if-let [[id stats gp] (entry->line entry)]
                   (-> acc
                       (update-in [id :nflverse/history] (fnil conj [])
                                  {:season season :stats stats})
                       (assoc-in [id :nflverse/games-by-season season]
                                 (min gp (double (nflverse/games-in-season season))))
                       (assoc-in [id :nflverse/games-seasons] lengths))
                   acc))
               acc
               (get entries-by-season season)))
     {}
     (sort (keys entries-by-season)))))

(defn fetch
  "Network: the `nflverse/availability-lookback` seasons ending at `season` ->
  `{:by-key ...}`, or nil when none arrived."
  [season]
  (let [window  (range (inc (- season nflverse/availability-lookback)) (inc season))
        entries (->> (into {} (map (fn [s] [s #(fetch-season s)])) window)
                     parallel/all
                     (into {} (remove (comp empty? val))))]
    (when (seq entries)
      (let [by-key (history entries)]
        {:by-key    by-key
         :positions (zipmap (keys by-key) (repeat "DST"))}))))
