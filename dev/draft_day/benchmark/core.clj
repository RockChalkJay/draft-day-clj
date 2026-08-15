(ns draft-day.benchmark.core
  "Run a ranking model over historical seasons and score it.

  The model is invoked through `rankings.model/score-board` — the same seam the
  live board uses — so a model that wins here is shipped by changing a keyword,
  not by porting code out of the harness."
  (:require [clojure.set]
            [draft-day.benchmark.metrics :as metrics]
            [draft-day.benchmark.sources.nflverse :as nflverse]
            [draft-day.benchmark.truth :as truth]
            [draft-day.benchmark.vintage :as vintage]
            [draft-day.rankings.model :as model]
            [draft-day.rankings.model.blend]  ; register :points+adp / :points+adp+fade
            [draft-day.scoring :as scoring]))

(def default-slices
  "Players kept per position, regardless of how many the source published.

  Without this the pool swings with whatever the ADP source happened to list —
  137 players in 2022 against 202 in 2025 — so 'top-12 of 50 RBs' in one season
  is compared against 'top-12 of 65' in another as though they were the same
  task. Sizes are roughly three deep per starting slot in a 12-team league: the
  players a manager actually chooses between."
  {"QB" 24 "RB" 48 "WR" 60 "TE" 24})

(def default-opts
  {:scoring           (:ppr scoring/presets)
   :pool-size         200
   :adp-source        :ffc
   :projection-source :sleeper
   :truth-key         :actual/points
   :slices            default-slices})

(defn pool-rank-key
  "How a season's pool was ordered at draft time — ADP where it exists, else ECR.
  Both are 'lower is better'. Returns nil when neither is present, in which case
  the slice cannot be applied honestly and the pool is left alone."
  [players]
  (cond
    (some :adp players) :adp
    (some :ecr players) :ecr
    :else nil))

(defn apply-position-slice
  "Keep the top N per position by draft-time order, so every season and every
  source poses the same problem."
  [players slices]
  (if-not slices
    players
    (if-let [k (pool-rank-key players)]
      (into []
            (mapcat (fn [[pos grp]]
                      (let [n (get slices pos)]
                        (if-not n
                          grp
                          (take n (sort-by #(double (or (k %) Double/MAX_VALUE)) grp))))))
            (group-by :position players))
      players)))

(defn common-pool
  "Restrict two runs to the players BOTH scored in each season, and recompute
  their metrics on that intersection.

  Required before any cross-source claim: the Sleeper path pools on FFC ADP, the
  fp-archive path on FFC intersected with FantasyPros, the ECR path on ECR's own
  top-N. Comparing their hit rates untouched compares different universes and
  calls the difference a model result."
  [results-a results-b truth-key]
  (let [keys-of (fn [rs] (into {} (map (juxt :season #(set (map :gsis-id (:players %)))))
                               (remove :skipped? rs)))
        ka (keys-of results-a)
        kb (keys-of results-b)
        keep-common
        (fn [rs]
          (mapv (fn [r]
                  (if (:skipped? r)
                    r
                    (let [common (clojure.set/intersection (get ka (:season r) #{})
                                                           (get kb (:season r) #{}))
                          ps     (filterv #(contains? common (:gsis-id %)) (:players r))]
                      (assoc r :players ps :n (count ps)
                             :metrics (metrics/season-metrics ps {:truth-key truth-key})))))
                rs))]
    [(keep-common results-a) (keep-common results-b)]))

(defn apply-availability-filter
  "Optionally restrict the pool to players active in Week 1.

  This answers a real and separate question — 'conditional on the player being
  available, did the model rank him correctly?' — by removing the injury luck
  that dominates season totals.

  It is NOT a fairer default, and the reason matters: Week 1 participation is
  known only after the fact. Filtering on it hands back every camp injury the
  model failed to anticipate, so every model scores better and the models that
  score *most* better are the ones that ignored injury risk. A drafter cannot
  filter their roster this way in August. Report it beside the unfiltered number,
  never instead of it."
  [players season require-week1?]
  (if-not require-week1?
    players
    (let [active (nflverse/week1-participants season)]
      (if (empty? active)
        players                                   ; weekly data unavailable — do not silently empty the pool
        (filterv #(contains? active (:gsis-id %)) players)))))

(defn run-season
  "Score one model on one season. Returns
  {:season :model :skipped? :reason :metrics {...} :overall :n}.

  A season whose projections fail the vintage gate is skipped rather than scored:
  a number produced from contaminated inputs is worse than no number, because it
  looks like evidence."
  [season model opts]
  (let [{:keys [scoring pool-size adp-source projection-source weights truth-key
                require-week1? slices]}
        (merge default-opts opts)
        needs           (model/requires model)
        needs-projections? (contains? needs :projections)
        needs-ecr?         (contains? needs :ecr)
        fp-archive?        (= projection-source :fp-archive)
        fftoday?           (= projection-source :fftoday)
        ;; Gate BEFORE assembling, and only when the model actually reads
        ;; projections. The gate reads one cheap Sleeper payload, whereas
        ;; assembly pulls a season of nflverse outcomes, the prior season's usage
        ;; and an ADP table — so 1999-2025 would otherwise download twenty-odd
        ;; seasons purely to discard them. And a consensus/usage model has no
        ;; reason to be blocked by a projection it never looks at.
        ;; The Sleeper vintage gate applies only to Sleeper projections. The
        ;; FantasyPros archive has its own gate — capture date before kickoff —
        ;; which lives in its assembly because it is per position, not per season.
        gate (cond
               needs-ecr?               {:season season :pass? true
                                         :reason "ecr gates on capture date"}
               (not needs-projections?) {:season season :pass? true
                                         :reason "model needs no projections"}
               fp-archive?              {:season season :pass? true
                                         :reason "fp-archive gates on capture date"}
               fftoday?                 {:season season :pass? true
                                         :reason "fftoday gates behaviourally after assembly"}
               :else                    (vintage/gate season))]
    (if-not (:pass? gate)
      {:season season :model model :skipped? true :reason (:reason gate) :gate gate}
      (let [{players :players gate :gate}
            (cond
              needs-ecr?               (vintage/assemble-from-ecr season {:pool-size pool-size})
              (not needs-projections?) (vintage/assemble-from-adp season {:pool-size pool-size})
              fp-archive?              (vintage/assemble-from-fp-archive season {:pool-size pool-size})
              fftoday?                 (vintage/assemble-from-fftoday season {:pool-size pool-size})
              :else                    (vintage/assemble season {:pool-size  pool-size
                                                                 :adp-source adp-source}))]
        ;; The assembly's own gate matters for fp-archive, where usability is
        ;; decided per position (did every skill page have a pre-kickoff
        ;; capture?) and so cannot be judged before the fetch.
        (if (or (empty? players) (false? (:pass? gate)))
          {:season season :model model :skipped? true
           :reason (if (empty? players)
                     "no draftable players resolved"
                     (:reason gate))
           :gate gate}
          (let [pool   (-> players
                           (apply-position-slice slices)
                           (apply-availability-filter season require-week1?))
                scored (model/score-board model {:scoring scoring :weights weights} pool)
                with-t (truth/with-realized scored scoring)]
            {:season  season
             :model   model
             :n       (count with-t)
             :gate    gate
             ;; The scored board is carried out, not just its summary. Paired
             ;; comparison needs player-level rows: collapsing a season to one
             ;; rho leaves n=5, which cannot resolve the 0.01-0.05 differences
             ;; models actually differ by.
             :players with-t
             :metrics (metrics/season-metrics with-t {:truth-key truth-key})
             :overall (metrics/overall-spearman with-t truth-key)}))))))

(defn run
  "Score one model across seasons. Returns a vector of per-season results."
  [seasons model opts]
  (mapv #(run-season % model opts) seasons))

(defn scored-seasons
  "Only the seasons that actually produced metrics."
  [results]
  (remove :skipped? results))

(defn aggregate
  "Pool per-season metrics per position: summed hits/top-n/busts and the *worst*
  season's rho.

  Worst rather than mean is deliberate. Picking weights on the mean is how you
  end up shipping a model that is excellent in four seasons and catastrophic in
  the fifth, and a draft is a single season, not an average of five."
  [results]
  (let [rows (mapcat (fn [r] (map (fn [[pos m]] (assoc m :position pos)) (:metrics r)))
                     (scored-seasons results))]
    (into (sorted-map)
          (map (fn [[pos ms]]
                 ;; Hit-rate metrics are absent for seasons whose pool was too
                 ;; thin to support a top-N (see metrics/position-metrics), so
                 ;; they are summed over their own subset. Rho exists for every
                 ;; season, which is why :seasons and :hit-seasons can differ —
                 ;; and reporting one count for both would overstate the hit rate.
                 (let [hit-ms (filter :hits ms)]
                   [pos {:seasons     (count ms)
                         :hit-seasons (count hit-ms)
                         :hits        (reduce + 0 (map :hits hit-ms))
                         :possible    (reduce + 0 (map :top-n hit-ms))
                         :busts       (reduce + 0 (map :busts hit-ms))
                         :worst-rho   (apply min (map :spearman ms))
                         :mean-rho    (metrics/mean (map :spearman ms))}])))
          (group-by :position rows))))
