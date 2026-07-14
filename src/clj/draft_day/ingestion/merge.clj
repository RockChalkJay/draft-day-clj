(ns draft-day.ingestion.merge
  "The universe-vs-enrichment invariant in code: only the universe defines rows;
  enrichment sources left-join columns by canonical key. A universe row with no
  match keeps its columns unchanged; enrichment rows with no universe match are
  dropped."
  (:require [draft-day.ingestion.match :as match]))

(defn left-join
  "Attach enrichment columns onto each universe player by (name, position) key."
  [universe enrichment-by-key]
  (mapv (fn [p]
          (let [k (match/key-for (:player-name p) (:position p))]
            (if-let [ext (get enrichment-by-key k)]
              (merge p ext)
              p)))
        universe))
