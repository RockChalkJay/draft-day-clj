(ns draft-day.tools.trending-snapshot
  "Record Sleeper's trending adds as of now, so `rankings.faab/heat-weight` can
  one day be measured rather than chosen.

  Sleeper keeps no past trending lists, so a backtest cannot rebuild the heat a
  past waiver run saw — the only record is one taken at the time. Run it each
  Tuesday before waivers process, when the list says what the week's claims
  will chase:

    lein run -m draft-day.tools.trending-snapshot

  Each run writes one file under `dir`, named for the instant it was taken, and
  never overwrites one."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.ingestion.sleeper-trending :as trending]))

(def dir "data/faab_cache/trending")

(defn snapshot-path
  "Where a snapshot taken at `iso` goes, colons out of the name for filesystems
  that refuse them."
  [iso]
  (str dir "/adds-" (str/replace iso ":" "-") ".transit"))

(defn capture []
  {:taken-at       (pipeline/now-iso)
   :lookback-hours trending/lookback-hours
   :adds           (trending/normalize (trending/fetch-raw))})

(defn -main [& _]
  (let [snap (capture)
        path (snapshot-path (:taken-at snap))]
    (when (.exists (io/file path))
      (throw (ex-info "snapshot already exists" {:path path})))
    (pipeline/write-transit! path snap)
    (println "wrote" (count (:adds snap)) "players to" path)
    (shutdown-agents)))
