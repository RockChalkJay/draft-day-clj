(ns draft-day.ingestion.merge
  "The universe-vs-enrichment invariant in code: only the universe defines rows;
  enrichment sources left-join columns by canonical key. A universe row with no
  match keeps its columns unchanged; enrichment rows with no universe match are
  dropped.

  Those drops used to be entirely silent, which is how a source can match zero
  players at a position for months without anyone noticing. `left-join-report`
  returns the join's hit rate alongside the rows."
  (:require [clojure.string :as str]
            [draft-day.ingestion.match :as match]))

(def unmatched-sample-limit
  "How many homeless enrichment keys to keep, for eyeballing a broken join."
  25)

(defn key-position
  "The position encoded in a match key: \"tjhockenson_te\" -> \"TE\". Names are
  stripped of non-alphanumerics by `match/key-for`, so the only underscore is
  the separator."
  [k]
  (some-> k (str/split #"_") last str/upper-case))

(defn rate
  "n/d as a double, 0.0 when there is nothing to divide."
  [n d]
  (if (pos? d) (/ (double n) d) 0.0))

(defn left-join-report
  "Like `left-join`, but also reports what the join actually accomplished:

    {:rows             <enrichment rows the source published>
     :matched          <universe players that gained columns>
     :hit-rate         <matched / rows — did the join work?>
     :coverage         <matched / universe — how much of the board has this?>
     :by-position      {\"DST\" {:n 32 :rows 32 :matched 0} ...}
     :unmatched-sample [<enrichment keys with no universe home> ...]}

  The two rates answer different questions and conflating them misleads. A
  deliberately partial source (FantasyPros publishes auction values for ~150
  players) has low coverage by design but should have a near-perfect hit rate;
  a *broken* join has a low hit rate at a position the source clearly covers.
  So `:by-position` carries both the universe count and the published-row
  count, which is what makes `{\"DST\" {:n 32 :rows 32 :matched 0}}` legible as
  a structural break rather than a thin source."
  [universe enrichment-by-key]
  (let [keyed (mapv (juxt identity
                          #(match/key-for (:player-name %) (:position %)))
                    universe)
        hit?  (fn [k] (contains? enrichment-by-key k))
        universe-by-pos (frequencies (keep (comp :position first) keyed))
        matched-by-pos  (frequencies (keep (fn [[p k]]
                                             (when (hit? k) (:position p)))
                                           keyed))
        rows-by-pos     (frequencies (keep key-position
                                           (keys enrichment-by-key)))
        by-position
        (into {}
              (map (fn [pos] [pos {:n       (get universe-by-pos pos 0)
                                   :rows    (get rows-by-pos pos 0)
                                   :matched (get matched-by-pos pos 0)}]))
              (into (set (keys universe-by-pos)) (keys rows-by-pos)))
        matched (reduce + 0 (vals matched-by-pos))]
    {:players (mapv (fn [[p k]]
                      (if-let [ext (get enrichment-by-key k)] (merge p ext) p))
                    keyed)
     :report
     {:rows        (count enrichment-by-key)
      :matched     matched
      :hit-rate    (rate matched (count enrichment-by-key))
      :coverage    (rate matched (count universe))
      :by-position by-position
      :unmatched-sample
      (into [] (comp (remove (into #{} (comp (map second) (filter hit?)) keyed))
                     (take unmatched-sample-limit))
            (keys enrichment-by-key))}}))

(defn left-join
  "Attach enrichment columns onto each universe player by (name, position) key."
  [universe enrichment-by-key]
  (:players (left-join-report universe enrichment-by-key)))
