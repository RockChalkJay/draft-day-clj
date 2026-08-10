(ns draft-day.replay.report
  "Entry point: load (or crawl) a clean auction corpus, replay every draft through
  the real engine, and print how well Worth predicts realized prices vs the raw
  Value and MKT baselines.

    lein with-profile +dev run -m draft-day.replay.report           ; use cached corpus
    lein with-profile +dev run -m draft-day.replay.report --rebuild ; recrawl first"
  (:require [draft-day.replay.sleeper :as sleeper]
            [draft-day.replay.core :as core]
            [draft-day.replay.metrics :as metrics]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.pprint :as pp]))

(def ^:private corpus-file "resources/corpus.edn")
;; seed the crawl from the user's own account; the BFS fans out over leaguemates.
(def ^:private seed-uid "993960010998722560")   ; rockchalkjay

(defn- slurp-edn [path] (when (.exists (io/file path)) (edn/read-string (slurp path))))
(defn- spit-edn  [path data] (io/make-parents path) (spit path (with-out-str (pp/pprint data))))

(defn load-normalized
  "Fetch+normalize a draft (cached to disk)."
  [did]
  (let [path (str "data/replay_cache/draft-" did ".edn")]
    (or (slurp-edn path)
        (let [d (sleeper/draft did) picks (sleeper/draft-picks did)]
          (when (and d picks)
            (doto (sleeper/normalize-draft d picks) (->> (spit-edn path))))))))

(defn build-corpus!
  "Crawl for clean auctions; persist accepted draft-ids to corpus.edn."
  [opts]
  (let [{:keys [accepted visited examined]}
        (sleeper/crawl [seed-uid]
                       (assoc opts :progress!
                              (fn [{:keys [visited accepted leagues]}]
                                (println (format "  visited=%d accepted=%d (+%d leagues)"
                                                 visited accepted leagues)))))]
    (println (format "crawl done: visited=%d examined=%d accepted=%d"
                     visited examined (count accepted)))
    (spit-edn corpus-file (vec accepted))
    (vec accepted)))

(defn- fmt [{:keys [n mae rmse bias spearman]}]
  (format "n=%-5d  MAE=$%-6.2f  RMSE=$%-6.2f  bias=$%-7.2f  rho=%+.3f"
          n mae rmse bias spearman))

(defn report [rows]
  (println (format "\n=== Replay report: %d picks across %d draft(s) ==="
                   (count rows) (count (distinct (map :draft-id rows)))))
  (println "\n-- Predictor vs actual price (K/DST excluded) --")
  (doseq [k [:worth :value :market]]
    (println (format "  %-7s %s" (name k) (fmt (metrics/metric rows k)))))
  (println "\n-- Worth by draft phase (late bias<0 => over-deflation) --")
  (doseq [[ph m] (metrics/by-phase rows :worth)]
    (println (format "  %-6s %s" (name ph) (fmt m))))
  (println "\n-- Worth by position --")
  (doseq [[pos m] (metrics/by-position rows :worth)]
    (println (format "  %-4s %s" pos (fmt m)))))

(defn score-ids
  "Replay + score an explicit collection of draft-ids (bypasses the crawl). The
  practical entry point when you already know the auction draft_ids you want —
  the blind crawl rarely reaches clean public auctions."
  [ids]
  (doall
   (mapcat (fn [did]
             (when-let [nd (load-normalized did)]
               (let [rs (core/score-draft nd)]
                 (println (format "  scored %s (%s): %d/%d picks"
                                  did (:season nd) (count rs) (count (:picks nd))))
                 rs)))
           ids)))

(defn run
  "Score `ids` if given, else the cached corpus, else crawl one. Prints the report."
  [{:keys [ids rebuild max-drafts max-users] :or {max-drafts 40 max-users 500}}]
  (let [ids (or (seq ids)
                (and (not rebuild) (slurp-edn corpus-file))
                (build-corpus! {:max-drafts max-drafts :max-users max-users}))]
    (println (format "scoring %d draft(s)" (count ids)))
    (doto (score-ids ids) report)))

(defn -main
  "Args: bare numeric draft-ids score those directly; --rebuild recrawls the
  corpus; no args uses the cached corpus."
  [& args]
  (let [ids (filter #(re-matches #"\d+" %) args)]
    (run {:ids ids :rebuild (boolean (some #{"--rebuild"} args))}))
  (shutdown-agents))
