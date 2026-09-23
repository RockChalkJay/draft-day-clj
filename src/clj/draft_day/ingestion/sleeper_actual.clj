(ns draft-day.ingestion.sleeper-actual
  "Fetch Sleeper's realized weekly stats in the scoring vocabulary used by the
  league. Weekly rows are accumulated because Sleeper supplies team defenses
  and counts some stats differently from nflverse; a missing week is treated as
  no appearance, while an entry with `gp` and empty stats is a scoreless game."
  (:require [clojure.tools.logging :as log]
            [jsonista.core :as json]
            [draft-day.ingestion.sleeper-http :as sleeper-http]
            [draft-day.ingestion.parallel :as parallel]
            [draft-day.json :refer [mapper]]
            [draft-day.scoring :as scoring]))

(def ^:private base "https://api.sleeper.app")

(def ^:private positions
  "Positions to request; an unfiltered response includes unrelated players."
  ["QB" "RB" "WR" "TE" "K" "DEF"])

(def recent-window
  "Number of recent played weeks included in `:realized/recent`."
  3)

(defn- week-url [season week]
  (str base "/stats/nfl/" season "/" week "?season_type=regular"
       (apply str (map #(str "&position[]=" %) positions))))

(defn fetch-week
  "Network: raw stat entries for one week of a season (throws on failure)."
  [season week]
  (let [{:keys [status body error]} (sleeper-http/get! (week-url season week)
                                               {:timeout 30000})]
    (cond
      error          (throw (ex-info "Sleeper stats fetch failed"
                                     {:week week :error error}))
      (= 200 status) (json/read-value body mapper)
      :else          (throw (ex-info "Sleeper stats non-200"
                                     {:week week :status status})))))

(defn scored-stats
  "Return scored Sleeper stats as doubles, omitting zeros from sparse rows."
  [stats]
  (into {} (keep (fn [k]
                   (when-let [v (get stats k)]
                     (when-not (zero? v) [k (double v)]))))
        scoring/stat-keys))

(defn entry->row
  "Convert a raw entry to a played-game row, using positive `gp` as the gate."
  [{:keys [player_id stats week opponent]}]
  (when (and player_id stats week (pos? (or (:gp stats) 0)))
    {:id       player_id
     :week     (long week)
     :opponent opponent
     :stats    (scored-stats stats)}))

(defn- totals
  "Sum stats and count played rows for one player."
  [rows]
  {:games (count rows)
   :stats (reduce (fn [acc {:keys [stats]}] (merge-with + acc stats)) {} rows)})

(defn game-log
  "A player's rows as ordered per-week entries, oldest first."
  [rows]
  (mapv (fn [{:keys [week opponent stats]}]
          (cond-> {:week week :stats stats}
            opponent (assoc :opponent opponent)))
        (sort-by :week rows)))

(defn accumulate
  "Group played rows by Sleeper id and produce totals, recent totals, and a log."
  [rows]
  (let [latest (reduce max 0 (map :week rows))
        floor  (- latest (dec recent-window))]
    (reduce-kv
     (fn [acc id player-rows]
       (let [recent (filterv #(>= (:week %) floor) player-rows)]
         (assoc acc id
                (cond-> {:realized/season-to-date (totals player-rows)
                         :realized/game-log       (game-log player-rows)}
                  (seq recent) (assoc :realized/recent (totals recent))))))
     {}
     (group-by :id rows))))

(defn fetch
  "Fetch played weeks, returning Sleeper-keyed realized columns or nil if none
  arrive. Fired together; `sleeper-http` holds them to the host's limit."
  [season through-week]
  (when (and (number? through-week) (pos? through-week))
    (let [weeks   (range 1 (inc (long through-week)))
          fetch-w (fn [w]
                    #(try (fetch-week season w)
                          (catch Exception e
                            (log/warn e "sleeper-actual: week" w "failed"))))
          results (vals (parallel/all (zipmap weeks (map fetch-w weeks))))
          rows    (into [] (comp cat (keep entry->row)) results)]
      (when (seq rows)
        {:by-key       (accumulate rows)
         :through-week (reduce max 0 (map :week rows))}))))
