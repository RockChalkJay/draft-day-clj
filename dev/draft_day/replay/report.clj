(ns draft-day.replay.report
  "Entry point: load (or crawl) an auction corpus, replay every draft through the
  real engine, and print how well Worth predicts realized prices vs the raw Value
  and MKT baselines.

    lein run -m draft-day.replay.report               ; use cached corpus
    lein run -m draft-day.replay.report --rebuild     ; recrawl, resuming state
    lein run -m draft-day.replay.report --fresh       ; recrawl from scratch
    lein run -m draft-day.replay.report 12345 67890   ; score these draft ids

  A crawl takes its bounds from `--k=v` flags, all optional:

    --max-users=N            stop after visiting N users
    --max-drafts=N           stop once N drafts are accepted
    --max-drafts-per-user=N  cap what any one community contributes"
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
  rebuild, the checkpoint rewrites it every few users so committing it would
  churn the tree constantly, and it is a list of other people's league drafts —
  public through Sleeper's API, but not ours to publish in a repo."
  "data/replay_cache/corpus.edn")

(def ^:private state-file
  "Everything the crawl learned, so a run that was capped or interrupted resumes
  instead of restarting.

  The version token is the same discipline as `benchmark/fetch.clj/cache-path`
  and `ingestion/pipeline`'s schema version: this file holds a *derived*
  judgement, not raw responses, so when the gate changes the old decisions are
  wrong rather than merely stale. Bump the token and they are ignored.

  v2 earned its bump the hard way. The accepted metas gained a league type, and
  a v1 state resumed under v2 code reports every one of its drafts as having no
  type — which `wanted-superflex?` reads as an all-standard corpus and answers by
  steering toward superflex. On a corpus that was already 89% superflex that is
  precisely backwards: the balancing machinery would have driven the imbalance it
  exists to correct."
  "data/replay_cache/crawl-state-v2.transit")

;; seed the crawl from the user's own account; the BFS fans out over leaguemates.
(def ^:private seed-uid "993960010998722560")   ; rockchalkjay

(defn known-users
  "Every Sleeper user id any crawl has ever turned up, across all state versions.

  Deliberately *not* versioned, unlike the state file that carries it. A version
  token guards derived judgements — an accepted draft's metadata means something
  different after the gate changes, so reading it forward is a bug. A user id is
  an address, not a judgement: it means exactly what it meant before. Bumping the
  state version and thereby discarding 2,123 discovered users cost a crawl its
  whole seed pool and sent it back to walking out from one account."
  []
  (->> (file-seq (io/file "data/replay_cache"))
       (filter #(re-matches #"crawl-state-v\d+\.transit" (.getName %)))
       ;; Guarded the way `load-state` is. These are *older* state versions by
       ;; definition, written before `spit-atomically!` existed, so they are the
       ;; files most likely to be half-written — and `read-transit` throws on a
       ;; truncated one, which would take down every run at its seed step. One
       ;; damaged file should cost its own user list, not the crawl.
       (keep #(try (pipeline/read-transit (.getPath %)) (catch Exception _ nil)))
       (mapcat (fn [st] (concat (:frontier st) (:seen-users st))))
       distinct
       vec))

(defn seed-uids
  "Where a cold crawl starts.

  A single seed is how the corpus ended up 89% superflex — not because the seed
  account is superflex (it is not), but because one seed means one walk, and a
  walk that chases auctions falls into whichever community it first reaches
  and then never leaves.

  Seeds are spread evenly across the known-user pool rather than taken from its
  head, because that pool is in breadth-first order: its first `n` entries are
  all neighbours of each other, which is the same single neighbourhood in a
  different disguise. `state` is the crawl being resumed, whose already-visited
  users are skipped."
  [state n]
  (let [seen  (set (:seen-users state))
        pool  (remove seen (known-users))
        step  (max 1 (quot (count pool) (max 1 n)))
        picks (take n (take-nth step pool))]
    (vec (distinct (cons seed-uid picks)))))

(defn- slurp-edn
  "Read an EDN file, or nil. A half-written file reads as nil rather than
  throwing — see `save-state!` for why one can exist."
  [path]
  (when (.exists (io/file path))
    (try (edn/read-string (slurp path)) (catch Exception _ nil))))

(defn- spit-atomically!
  "Write via a temp file and rename.

  Every one of these files exists to survive an interruption, so the one moment
  they must not be destroyed by an interruption is the write itself. A partial
  transit or EDN file is not merely stale: `read-transit` throws on it, so a
  crawl killed mid-checkpoint would take down every subsequent run at startup and
  lose hours of accepted corpus. Rename is atomic; a truncated temp file is
  ignorable."
  [path write!]
  (io/make-parents path)
  (let [tmp (str path ".tmp")]
    (write! tmp)
    (.renameTo (io/file tmp) (io/file path))))

(defn- spit-edn [path data]
  (spit-atomically! path #(spit % (with-out-str (pp/pprint data)))))

(defn load-state []
  (or (try (pipeline/read-transit state-file) (catch Exception _ nil)) {}))

(defn save-state! [state]
  (spit-atomically! state-file #(pipeline/write-transit! % state)))

(defn load-normalized
  "Fetch+normalize a draft (cached to disk).

  Returns nil only when the draft genuinely could not be read — `sleeper/fetch`
  now distinguishes a 404 from a throttle, so a rate-limited run no longer
  silently records a draft as empty."
  [did]
  ;; The `v2` token is not decoration. These files hold a *derived* map, and v2
  ;; added the league type — without a new path, every draft cached under v1 would
  ;; read back with `:superflex?` absent, which is indistinguishable from
  ;; `false`. Every one of the 59 superflex drafts already on disk would have
  ;; silently reclassified as standard, and the split this whole exercise turns on
  ;; would have been quietly inverted. Same lesson `benchmark/fetch.clj/cache-path`
  ;; records from the last time a derived cache gained a field.
  (let [path (str "data/replay_cache/draft-v2-" did ".edn")]
    (or (slurp-edn path)
        (let [dresp (sleeper/draft did)
              presp (sleeper/draft-picks did)
              d     (sleeper/body dresp)
              picks (sleeper/body presp)
              lresp (some-> (:league_id d) sleeper/league)
              ;; All three, not just the league. The league fetch is the one
              ;; that would poison the cache — a throttle there classifies a
              ;; superflex draft as standard and keeps that answer forever,
              ;; filing it in the block the report calls trustworthy — but a
              ;; throttled draft or picks fetch read through `body` is nil, and
              ;; nil drops the draft from the run in silence. The report's pick
              ;; and draft counts then shrink to match, which is the same
              ;; rate-limit-as-data-drought this harness exists to stop
              ;; believing. Name which one, and cache nothing either way.
              bad   (some (fn [[label resp]]
                            (when (sleeper/unreadable? resp) [label (:reason resp)]))
                          [["draft" dresp] ["picks" presp] ["league" lresp]])]
          (cond
            bad (do (println (format "  skipped %s: %s unreadable (%s) — not cached"
                                     did (first bad) (name (second bad))))
                    nil)

            (not (and d picks)) nil

            :else
            (doto (sleeper/normalize-draft d picks (sleeper/body lresp))
              (->> (spit-edn path))))))))

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
  "The spread of league shapes in the accepted corpus.

  League type leads because it is the split that decides whether the corpus can
  answer anything. The first widened crawl came back 89% superflex, and the two
  types disagree in opposite directions about the phase bias the decay constant
  would be fitted to — so a pooled number is a statement about whichever type
  happens to dominate. If the superflex share here is not near half, no aggregate
  below it should be read as general."
  [accepted]
  (let [metas (vals accepted)
        n     (count metas)
        sf    (count (filter :superflex? metas))
        tally (fn [k] (->> metas (map k) frequencies (sort-by key) vec))]
    (println "\n-- Accepted corpus shape --")
    (println (format "  drafts         %d" n))
    (println (format "  superflex      %d (%.0f%%)   standard 1-QB %d (%.0f%%)"
                     sf (if (pos? n) (* 100.0 (/ sf n)) 0.0)
                     (- n sf) (if (pos? n) (* 100.0 (/ (- n sf) n)) 0.0)))
    (println (format "  teams          %s" (pr-str (tally :num-teams))))
    (println (format "  budgets        %s" (pr-str (tally :budget))))
    (println (format "  rec weight     %s" (pr-str (tally :scoring-rec))))))

(defn build-corpus!
  "Crawl for auctions; persist accepted draft-ids and the resumable crawl state.

  `--fresh` clears the *decisions* but deliberately keeps the address book: every
  draft is re-judged, while the users previous crawls discovered are still used
  as seeds. Forgetting who exists would throw away the only thing that lets a
  cold crawl start anywhere other than one account."
  [{:keys [fresh] :as opts}]
  (let [prior (if fresh {} (load-state))
        seeds (seed-uids prior 24)]
    (if (and (seq (:seen-users prior))
             (empty? (remove (set (:seen-users prior)) seeds))
             (empty? (:frontier prior)))
      ;; Every seed already visited and nothing queued. `crawl` would skip
      ;; straight past them and hand back the prior state, and the summary below
      ;; would then report the previous run's totals as though this run had
      ;; produced them. Saying so and stopping is the point; saying so and
      ;; carrying on would print the very thing the message warns about.
      (do (println "nothing left to crawl: the frontier is drained and every"
                   "known user has been visited.\nUse --fresh to re-judge"
                   "them, or widen the graph with new seeds.")
          (vec (keys (:accepted prior))))
      (let [st (sleeper/crawl seeds
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
        (vec (keys accepted))))))

(defn- fmt [{:keys [n mae rmse bias spearman]}]
  (format "n=%-5d  MAE=$%-6.2f  RMSE=$%-6.2f  bias=$%-7.2f  rho=%+.3f"
          n mae rmse bias spearman))

(defn- print-block [label rows]
  (println (format "\n--- %s: %d picks across %d draft(s) ---"
                   label (count rows) (count (distinct (map :draft-id rows)))))
  (println "  predictor vs actual price (K/DST excluded)")
  (doseq [k [:worth :value :market]]
    (println (format "    %-7s %s" (name k) (fmt (metrics/metric rows k)))))
  (println "  by draft phase (bias>0 => Worth above the price paid)")
  (doseq [[ph m] (metrics/by-phase rows :worth)]
    (println (format "    %-6s %s" (name ph) (fmt m))))
  (println "  by position")
  (doseq [[pos m] (metrics/by-position rows :worth)]
    (println (format "    %-4s %s" pos (fmt m)))))

(defn report
  "Print the replay metrics, split by league type before pooling.

  The split leads because pooling hid the only thing that mattered. On a corpus
  that was 89% superflex, the pooled table read MAE $5.61 and a late-phase bias
  near zero — while the standard-league minority read MAE $11.02 and a mid-phase
  bias of +$5.17, the opposite sign. A single average over two populations that
  disagree is a statement about the bigger one wearing the authority of the whole.

  The pooled table still prints, underneath, because it is the right number when
  the two blocks agree — and printing it last makes it obvious when they do not."
  [rows]
  (println (format "\n=== Replay report: %d picks across %d draft(s) ==="
                   (count rows) (count (distinct (map :draft-id rows)))))
  (let [{sf true st false} (group-by #(boolean (:superflex? %)) rows)]
    (when (seq st) (print-block "standard 1-QB" st))
    (when (seq sf) (print-block "superflex" sf))
    (when (and (seq st) (seq sf))
      (println "\n  ^ superflex valuations are contaminated until SUPER_FLEX stops")
      (println "    importing as a bench slot; the standard block is the trustworthy one.")
      ;; only worth printing when there are two populations to pool; with one it
      ;; is the block above under a second heading
      (print-block "ALL LEAGUES POOLED" rows))))

(defn score-ids
  "Replay + score an explicit collection of draft-ids (bypasses the crawl).

  Each row is stamped with its league type so `report` can split on it — the
  engine's rows describe a pick, and which kind of room it was bought in is the
  one thing about the draft the rows cannot see for themselves."
  [ids]
  (doall
   (mapcat (fn [did]
             (when-let [nd (load-normalized did)]
               (let [rs (mapv #(assoc % :superflex? (boolean (:superflex? nd)))
                              (core/score-draft nd))]
                 (println (format "  scored %s (%s, %s): %d/%d picks"
                                  did (:season nd)
                                  (if (:superflex? nd) "superflex" "1-QB")
                                  (count rs) (count (:picks nd))))
                 rs)))
           ids)))

(defn run
  "Score `ids` if given, else the cached corpus, else crawl one. Prints the report."
  [{:keys [ids rebuild fresh max-drafts max-users max-drafts-per-user]}]
  (let [ids (or (seq ids)
                (and (not rebuild) (not fresh) (seq (slurp-edn corpus-file)))
                (build-corpus! (cond-> {:fresh fresh}
                                 max-drafts (assoc :max-drafts max-drafts)
                                 max-users  (assoc :max-users max-users)
                                 max-drafts-per-user
                                 (assoc :max-drafts-per-user max-drafts-per-user))))]
    (println (format "scoring %d draft(s)" (count ids)))
    (doto (score-ids ids) report)))

(defn parse-bounds
  "`--k=v` crawl bounds from the command line, as `{:k long}`.

  They were reachable only from a REPL: `run` accepted two of them and `-main`
  parsed neither, while `--max-drafts-per-user` — the lever deciding how much any
  one community contributes, and so the one that governs whether the corpus can
  be balanced at all — was not plumbed through `run` in the first place. A sweep
  that runs for hours is precisely the thing wanted from a shell with a bound on
  it. Unknown flags are ignored rather than rejected, since `--rebuild` and
  `--fresh` come through the same argv."
  [args]
  (into {} (keep (fn [a]
                   (when-let [[_ k v] (re-matches #"--([a-z-]+)=(\d+)" a)]
                     [(keyword k) (parse-long v)])))
        args))

(defn -main
  "Args: bare numeric draft-ids score those directly; --rebuild recrawls
  (resuming saved state); --fresh recrawls from scratch; --max-users=N,
  --max-drafts=N and --max-drafts-per-user=N bound a crawl; no args uses the
  cached corpus."
  [& args]
  (let [ids (filter #(re-matches #"\d+" %) args)]
    (run (merge {:ids     ids
                 :rebuild (boolean (some #{"--rebuild"} args))
                 :fresh   (boolean (some #{"--fresh"} args))}
                (parse-bounds args))))
  (shutdown-agents))
