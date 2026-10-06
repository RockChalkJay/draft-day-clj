(ns draft-day.trends
  "Fetch Sleeper's trending add and drop lists and save them as JSON, for
  designing and testing the bid model.

  Sleeper keeps no past lists, so a saved one is the only record of what the
  site's managers were adding and dropping at that moment. Each run is one
  fetch and one file; when to run it is up to whoever runs it.

    lein run -m draft-day.trends [--types add,drop] [--lookbacks 48]
                                 [--limit 100] [--dir data/trends]

  The file is `<dir>/<season>/week-NN/<UTC time>.json`, NN being the weeks
  played according to nflverse (`week-unknown` when that cannot be read). It is the
  snapshot `ingestion.sleeper-trending` writes whenever the board fetches the
  list, with drops, other windows and player names too. If any list fails or
  comes back empty nothing is written, so a file is always complete.

  Exit codes: 0 written, 1 a list failed, 2 bad usage."
  (:require [clojure.string :as str]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.ingestion.sleeper-trending :as trending]))

(def defaults
  {:types     ["add" "drop"]
   :lookbacks [trending/lookback-hours]
   :limit     trending/default-limit
   :dir       trending/default-dir})

(def usage
  (str "usage: lein run -m draft-day.trends [--types add,drop] [--lookbacks 48]\n"
       "                                    [--limit 100] [--dir data/trends]"))

(defn bad [msg] (throw (ex-info msg {::usage true})))

(defn pos-int [s]
  (let [n (parse-long (str s))]
    (if (and n (pos? n)) n (bad (str "expected a positive whole number, got " (pr-str s))))))

(defn int-list [s] (mapv pos-int (str/split (str s) #",")))

(defn type-list [s]
  (let [ts (str/split (str s) #",")]
    (if (every? #{"add" "drop"} ts)
      (vec (distinct ts))
      (bad (str "--types takes add and/or drop, got " (pr-str s))))))

(def flags
  {"--types"     [:types type-list]
   "--lookbacks" [:lookbacks int-list]
   "--limit"     [:limit pos-int]
   "--dir"       [:dir str]})

(defn parse-args
  "The options `args` give over `defaults`, or `{:error msg}`. An unknown flag is
  an error, since a typo that is silently ignored fetches something else."
  [args]
  (try
    (loop [args args, opts defaults]
      (if-let [[flag & more] (seq args)]
        (let [[k parse] (or (get flags flag) (bad (str "unknown argument " (pr-str flag))))]
          (when (empty? more) (bad (str flag " needs a value")))
          (recur (rest more) (assoc opts k (parse (first more)))))
        opts))
    (catch clojure.lang.ExceptionInfo e
      (if (::usage (ex-data e)) {:error (ex-message e)} (throw e)))))

(defn names-index
  "`{sleeper-id {:name :pos :team}}` off the cached player universe; empty when
  there is none."
  []
  (->> (:players (pipeline/best-effort (pipeline/cached-universe pipeline/default-cache-path)))
       (keep (fn [p]
               (when-let [id (pipeline/sleeper-id p)]
                 [id {:name (:player-name p) :pos (:position p) :team (:team p)}])))
       (into {})))

(defn with-names
  "`lst` with each player's name, position and team, null where `names` has none."
  [names lst]
  (update lst :players
          (fn [ps] (mapv #(merge % {:name nil :pos nil :team nil} (get names (:player_id %))) ps))))

(defn run
  "Fetch, save and report; returns the exit code."
  [args]
  (let [{:keys [error types lookbacks limit dir]} (parse-args args)]
    (if error
      (do (binding [*out* *err*] (println error) (println usage)) 2)
      (try
        (let [names (names-index)
              lists (mapv (fn [[t lb]]
                            (with-names names (trending/fetch-list {:type t :lookback-hours lb :limit limit})))
                          (mapcat (fn [t] (map (fn [lb] [t lb]) lookbacks)) types))
              path  (trending/write-snapshot!
                     dir (trending/snapshot (pipeline/now-iso) (trending/current-season)
                                        (trending/current-through-week) limit lists))]
          (println (format "wrote %s (%s)" path
                           (str/join ", " (map #(format "%s %dh: %d players"
                                                        (:type %) (:lookback_hours %) (count (:players %)))
                                               lists))))
          0)
        (catch Exception e
          (binding [*out* *err*] (println "failed:" (ex-message e)))
          1)))))

(defn -main [& args]
  (System/exit (run args)))
