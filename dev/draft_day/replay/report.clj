(ns draft-day.replay.report
  "Entry point: load (or crawl) an auction corpus, replay every draft through the
  real engine, and print how well Worth predicts realized prices vs the raw Value
  and MKT baselines.

    lein run -m draft-day.replay.report               ; use cached corpus
    lein run -m draft-day.replay.report --rebuild     ; recrawl, resuming state
    lein run -m draft-day.replay.report --fresh       ; recrawl from scratch
    lein run -m draft-day.replay.report 12345 67890   ; score these draft ids"
  (:require [draft-day.replay.sleeper :as sleeper]
            [draft-day.replay.core :as core]
            [draft-day.replay.metrics :as metrics]
            [draft-day.ingestion.pipeline :as pipeline]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.pprint :as pp]))

(def ^:private corpus-file
  "The accepted draft ids.

  Lives beside the other crawl artifacts under the gitignored `data/replay_cache/`
  rather than in `resources/`, for three reasons: it is derived data a crawl can
  rebuild, the checkpoint rewrites it every twenty users so committing it would
  churn the tree constantly, and it is a list of other people's league drafts —
  public through Sleeper's API, but not ours to publish in a repo."
  "data/replay_cache/corpus.edn")

(def ^:private state-file
  "Everything the crawl learned, so a run that was capped or interrupted resumes
  instead of restarting.

  The `v1` token is the same discipline as `benchmark/fetch.clj/cache-path` and
  `ingestion/pipeline`'s schema version: this file holds a *derived* judgement,
  not raw responses, so when the gate changes the old decisions are wrong rather
  than merely stale. Bump the token and they are ignored."
  "data/replay_cache/crawl-state-v1.transit")

;; seed the crawl from the user's own account; the BFS fans out over leaguemates.
(def ^:private seed-uid "993960010998722560")   ; rockchalkjay

(defn- slurp-edn [path] (when (.exists (io/file path)) (edn/read-string (slurp path))))
(defn- spit-edn  [path data] (io/make-parents path) (spit path (with-out-str (pp/pprint data))))

(defn load-state []
  (or (pipeline/read-transit state-file) {}))

(defn save-state! [state]
  (io/make-parents state-file)
  (pipeline/write-transit! state-file state))

(defn load-normalized
  "Fetch+normalize a draft (cached to disk).

  Returns nil only when the draft genuinely could not be read — `sleeper/fetch`
  now distinguishes a 404 from a throttle, so a rate-limited run no longer
  silently records a draft as empty."
  [did]
  (let [path (str "data/replay_cache/draft-" did ".edn")]
    (or (slurp-edn path)
        (let [d     (sleeper/body (sleeper/draft did))
              picks (sleeper/body (sleeper/draft-picks did))
              lg    (some-> (:league_id d) sleeper/league sleeper/body)]
          (when (and d picks)
            (doto (sleeper/normalize-draft d picks lg) (->> (spit-edn path))))))))

(defn print-histogram
  "Why the crawl rejected what it rejected.

  This is the diagnostic the harness was missing. A crawl that reports only its
  acceptances cannot tell a gate that is too narrow from a population that is too
  thin, which is how the corpus sat at two drafts while the gate quietly required
  exactly 12 teams, exactly $200 and exactly full PPR. If `:not-auction` dominates
  here, discovery really is the constraint and another provider is the next
  question; if anything else does, the gate still is."
  [reasons]
  (println "\n-- Crawl decisions --")
  (doseq [[reason n] (sort-by (comp - val) reasons)]
    (println (format "  %-18s %d" (name reason) n)))
  (when (pos? (get reasons :throttled 0))
    (println "  ^ throttled: Sleeper asked us to slow down; those drafts were NOT")
    (println "    examined and are not evidence of absence. Re-run to pick them up.")))

(defn print-corpus-shape
  "The spread of league shapes in the accepted corpus. If these are all 12-team
  $200 PPR, the gate did not actually widen and the run proved nothing."
  [accepted]
  (let [metas (vals accepted)
        tally (fn [k] (->> metas (map k) frequencies (sort-by key) vec))]
    (println "\n-- Accepted corpus shape --")
    (println (format "  drafts     %d" (count metas)))
    (println (format "  teams      %s" (pr-str (tally :num-teams))))
    (println (format "  budgets    %s" (pr-str (tally :budget))))
    (println (format "  rec weight %s" (pr-str (tally :scoring-rec))))))

(defn build-corpus!
  "Crawl for auctions; persist accepted draft-ids and the resumable crawl state."
  [{:keys [fresh] :as opts}]
  (let [prior (if fresh {} (load-state))
        st    (sleeper/crawl [seed-uid]
                             (assoc opts
                                    :state prior
                                    :progress!
                                    (fn [{:keys [visited accepted candidates]}]
                                      (println (format "  visited=%d accepted=%d (+%d leagues)"
                                                       visited accepted candidates)))
                                    ;; a multi-hour crawl will be interrupted; save
                                    ;; as we go so the next run resumes rather than
                                    ;; re-fetching everything
                                    :checkpoint!
                                    (fn [snap]
                                      (save-state! snap)
                                      (spit-edn corpus-file (vec (keys (:accepted snap))))
                                      (println (format "  ...checkpointed at %d users, %d drafts"
                                                       (:visited snap) (count (:accepted snap)))))))
        {:keys [accepted reasons visited examined]} st]
    (println (format "\ncrawl done: visited=%d examined=%d accepted=%d"
                     visited examined (count accepted)))
    (print-histogram reasons)
    (print-corpus-shape accepted)
    (save-state! st)
    (spit-edn corpus-file (vec (keys accepted)))
    (vec (keys accepted))))

(defn- fmt [{:keys [n mae rmse bias spearman]}]
  (format "n=%-5d  MAE=$%-6.2f  RMSE=$%-6.2f  bias=$%-7.2f  rho=%+.3f"
          n mae rmse bias spearman))

(defn report [rows]
  (println (format "\n=== Replay report: %d picks across %d draft(s) ==="
                   (count rows) (count (distinct (map :draft-id rows)))))
  (println "\n-- Predictor vs actual price (K/DST excluded) --")
  (doseq [k [:worth :value :market]]
    (println (format "  %-7s %s" (name k) (fmt (metrics/metric rows k)))))
  (println "\n-- Worth by draft phase (bias>0 => Worth above the price paid) --")
  (doseq [[ph m] (metrics/by-phase rows :worth)]
    (println (format "  %-6s %s" (name ph) (fmt m))))
  (println "\n-- Worth by position --")
  (doseq [[pos m] (metrics/by-position rows :worth)]
    (println (format "  %-4s %s" pos (fmt m)))))

(defn score-ids
  "Replay + score an explicit collection of draft-ids (bypasses the crawl)."
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
  [{:keys [ids rebuild fresh max-drafts max-users]}]
  (let [ids (or (seq ids)
                (and (not rebuild) (not fresh) (slurp-edn corpus-file))
                (build-corpus! (cond-> {:fresh fresh}
                                 max-drafts (assoc :max-drafts max-drafts)
                                 max-users  (assoc :max-users max-users))))]
    (println (format "scoring %d draft(s)" (count ids)))
    (doto (score-ids ids) report)))

(defn -main
  "Args: bare numeric draft-ids score those directly; --rebuild recrawls
  (resuming saved state); --fresh recrawls from scratch; no args uses the cached
  corpus."
  [& args]
  (let [ids (filter #(re-matches #"\d+" %) args)]
    (run {:ids     ids
          :rebuild (boolean (some #{"--rebuild"} args))
          :fresh   (boolean (some #{"--fresh"} args))}))
  (shutdown-agents))
