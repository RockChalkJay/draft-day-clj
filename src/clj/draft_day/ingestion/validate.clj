(ns draft-day.ingestion.validate
  "Player-id integrity at the ingestion boundary.

  `:player-id` is the entity key for the whole app — the rankings engine indexes
  on it, the wire contract carries it, and every saved draft in a manager's
  browser is keyed by it — but nothing upstream guarantees it is present or
  unique. Two concrete failures this catches:

  1. A blank id collapses every affected player onto one key downstream
     (`subs/players-by-id`, `inflation-index/par-values`), so they shadow each
     other and the board silently loses rows.
  2. A duplicate id does the same for one pair. This is not theoretical —
     `sleeper/projections-url` requests six `position[]=` params, so a
     multi-position player can legitimately come back twice.

  The checks are pure and never throw: they return a report and let the call
  site set policy, because the right response differs by branch. A live fetch
  that looks structurally broken should abort so `load-universe` falls back to
  the last good cache; a cache or sample read has nowhere better to go and can
  only drop the bad rows and say so."
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]))

(def sample-limit
  "How many offending names/ids to keep in a report, for logging."
  10)

(def default-thresholds
  "What separates 'one junk row' from 'the upstream feed changed shape'."
  {:min-kept 100 :max-dropped-rate 0.01})

(defn valid-id?
  "True when `id` is present and not blank once stringified."
  [id]
  (boolean (and (some? id) (not (str/blank? (str id))))))

(defn validate-universe
  "Pure: `players` -> `{:players kept :report {...}}`.

  Rows with a blank `:player-id` are dropped. Among rows sharing an id the first
  wins, because a later row silently shadowing an earlier one is precisely the
  failure being guarded against — and the first occurrence is the one every
  earlier ingestion run would have kept."
  [players]
  (let [{:keys [kept blank dupes]}
        (reduce (fn [acc p]
                  (let [id (:player-id p)]
                    (cond
                      (not (valid-id? id))     (update acc :blank conj p)
                      (contains? (:seen acc) id) (update acc :dupes conj p)
                      :else (-> acc
                                (update :seen conj id)
                                (update :kept conj p)))))
                {:kept [] :seen #{} :blank [] :dupes []}
                players)]
    {:players kept
     :report
     {:n                 (count players)
      :kept              (count kept)
      :dropped-blank-id  (count blank)
      :dropped-duplicate (count dupes)
      :blank-id-names    (into [] (comp (map :player-name) (take sample-limit))
                               blank)
      :duplicate-ids     (into [] (comp (map :player-id) (distinct)
                                        (take sample-limit))
                               dupes)}}))

(defn dropped-rate
  "Share of input rows that did not survive validation; 0 for an empty input."
  [{:keys [n kept]}]
  (if (pos? n) (/ (double (- n kept)) n) 0.0))

(defn systemic-failure?
  "True when a report reads as an upstream shape change rather than a bad row:
  too few players survived, or too large a share was dropped. An empty universe
  trips this too, which is the point — `{:players []}` renders as an empty board
  rather than an error, so it has to be caught here."
  ([report] (systemic-failure? report default-thresholds))
  ([report {:keys [min-kept max-dropped-rate]}]
   (boolean (or (< (:kept report) min-kept)
                (> (dropped-rate report) max-dropped-rate)))))

(defn log-report!
  "Warn about anything dropped, naming the offenders so the cause is findable."
  [label {:keys [n kept dropped-blank-id dropped-duplicate
                 blank-id-names duplicate-ids]}]
  (when (pos? dropped-blank-id)
    (log/warn (format "%s: dropped %d player(s) with a blank :player-id %s"
                      label dropped-blank-id (pr-str blank-id-names))))
  (when (pos? dropped-duplicate)
    (log/warn (format "%s: dropped %d duplicate :player-id row(s) %s"
                      label dropped-duplicate (pr-str duplicate-ids))))
  (log/info (format "%s: %d/%d players kept" label kept n)))
