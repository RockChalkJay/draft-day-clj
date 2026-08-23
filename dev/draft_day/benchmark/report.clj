(ns draft-day.benchmark.report
  "CLI for the rankings benchmark.

    lein run -m draft-day.benchmark.report
    lein run -m draft-day.benchmark.report --seasons 2021-2025 --models points,points+adp
    lein run -m draft-day.benchmark.report --compare points points+adp
    lein run -m draft-day.benchmark.report --source-report
    lein run -m draft-day.benchmark.report --seasons 2021-2025 --scoring half-ppr --truth ppg"
  (:require [clojure.set]
            [clojure.string :as str]
            [draft-day.benchmark.core :as core]
            [draft-day.benchmark.fetch :as fetch]
            [draft-day.benchmark.metrics :as metrics]
            [draft-day.benchmark.simulate :as simulate]
            [draft-day.benchmark.auction :as auction]
            [draft-day.benchmark.sources.fantasypros-archive :as fp-archive]
            [draft-day.benchmark.sources.fantasypros-ecr :as fp-ecr]
            [draft-day.benchmark.sources.ffcalculator :as ffc]
            [draft-day.benchmark.sources.fftoday :as fftoday]
            [draft-day.benchmark.sources.ids :as ids]
            [draft-day.benchmark.sources.nflverse :as nflverse]
            [draft-day.benchmark.vintage :as vintage]
            [draft-day.rankings.model :as model]
            [draft-day.rankings.model.blend]
            [draft-day.scoring :as scoring]))

(def default-seasons [2021 2022 2023 2024 2025])

(defn parse-seasons
  "\"2021-2025\" or \"2021,2023\" -> [2021 ...]."
  [s]
  (if (str/includes? s "-")
    (let [[a b] (map #(Integer/parseInt (str/trim %)) (str/split s #"-"))]
      (vec (range a (inc b))))
    (mapv #(Integer/parseInt (str/trim %)) (str/split s #","))))

(defn usage-error [msg] (throw (ex-info msg {::usage true})))

(defn flag-value
  "The value following `flag`, rejecting a missing one or another flag. Without
  this, `--models --seasons 2021-2025` silently parses the model name as
  \"--seasons\" and fails much later with an unrelated error."
  [flag more]
  (let [v (first more)]
    (when (or (nil? v) (str/starts-with? (str v) "--"))
      (usage-error (str flag " needs a value (got "
                        (if v (str "the flag " v) "nothing")
                        "). Try --help.")))
    v))

(defn parse-models
  "Model keywords, validated against the registry so a typo fails immediately
  with the valid set rather than deep inside the engine."
  [s]
  (let [ks    (mapv (comp keyword str/trim) (str/split s #","))
        known (set (model/registered))
        bad   (remove known ks)]
    (when (seq bad)
      (usage-error
       (str "unknown model(s): " (str/join ", " (map name bad))
            "\n  models:  " (str/join ", " (map name (model/registered)))
            (when (some #{:ppr :half-ppr :standard} bad)
              (str "\n\n  Note: ppr/half-ppr/standard are SCORING presets, not models."
                   "\n  You probably want:  --models points --scoring "
                   (name (first (filter #{:ppr :half-ppr :standard} bad))))))))
    ks))

(defn parse-scoring [s]
  (let [k (keyword (str/trim s))]
    (when-not (contains? scoring/presets k)
      (usage-error (str "unknown scoring preset: " (name k)
                        "\n  presets: " (str/join ", " (map name (keys scoring/presets))))))
    k))

(defn parse-args [args]
  (loop [[a & more] args opts {}]
    (cond
      (nil? a) opts
      (#{"--help" "-h"} a)    (assoc opts :help? true)
      (= a "--seasons")       (recur (rest more) (assoc opts :seasons (parse-seasons (flag-value a more))))
      (= a "--models")        (recur (rest more) (assoc opts :models (parse-models (flag-value a more))))
      (= a "--scoring")       (recur (rest more) (assoc opts :scoring-name (parse-scoring (flag-value a more))))
      (= a "--truth")         (recur (rest more) (assoc opts :truth (keyword (flag-value a more))))
      (= a "--pool")          (recur (rest more) (assoc opts :pool-size (Integer/parseInt (flag-value a more))))
      (= a "--adp-source")    (recur (rest more) (assoc opts :adp-source (keyword (flag-value a more))))
      (= a "--projection-source") (recur (rest more) (assoc opts :projection-source (keyword (flag-value a more))))
      (= a "--source-report") (recur more (assoc opts :source-report? true))
      (= a "--refresh")       (recur more (assoc opts :refresh? true))
      (= a "--played-week1")  (recur more (assoc opts :require-week1? true))
      (= a "--power-report")  (recur more (assoc opts :power-report? true))
      (= a "--common-pool")   (recur more (assoc opts :common-pool? true))
      (= a "--simulate")      (recur more (assoc opts :simulate? true))
      (= a "--vorp")          (recur more (assoc opts :vorp? true))
      (= a "--auction")       (recur more (assoc opts :auction? true))
      (= a "--no-slice")      (recur more (assoc opts :no-slice? true))
      (= a "--force-totals")  (recur more (assoc opts :force-totals? true))
      (= a "--compare")       (let [ms (parse-models (str/join "," (take 2 more)))]
                                (when (not= 2 (count ms))
                                  (usage-error "--compare needs two models, e.g. --compare points points+adp"))
                                (recur (drop 2 more) (assoc opts :compare ms)))
      (str/starts-with? a "--") (usage-error (str "unknown flag: " a "\n  Try --help."))
      :else (recur more opts))))

(defn print-help []
  (println "
  Rankings benchmark — score a ranking model against realized outcomes.

    lein run -m draft-day.benchmark.report [flags]

  Flags
    --models  M[,M]     models to score      (default: points)
    --seasons 2021-2025 range or 2021,2023   (default: 2021-2025)
    --compare A B       two models side by side
    --source-report     per-source depth, join rates, vintage gate
    --power-report      what the corpus can resolve, before running a sweep
    --common-pool       restrict a comparison to players BOTH models scored
    --simulate          draft a team off each board and score it (the metric that
                        matches the decision: what roster do you end up with)
    --vorp              simulate one model twice — raw points board vs VORP
                        board — and bootstrap the difference. Answers whether
                        the replacement-level correction is worth anything,
                        which nothing in this harness could previously run.
    --auction           draft the season by AUCTION instead of snake: the model
                        seat bids up to its Worth, the field bids a price curve
                        measured from real auctions and disagrees about it by as
                        much as real rooms do. The only mode that exercises
                        value, inflation and phase decay — a snake pick is a
                        choice of player, and none of those three can change it.
    --no-slice          keep the raw pool instead of a fixed per-position slice
    --force-totals      allow a season-total comparison that spans 2021
    --refresh           bypass the disk cache and re-fetch from source
    --played-week1      score only players active in Week 1. Isolates ranking
                        skill from injury luck, but Week 1 activity is known only
                        AFTER the fact, so it flatters every model and most
                        flatters those that ignored injury risk. Diagnostic, not
                        a default.
    --scoring  PRESET   ppr | half-ppr | standard   (default: ppr)
    --truth    KIND     points | ppg                (default: points)
    --pool     N        ADP cutoff for draftable    (default: 200)
    --adp-source SRC    ffc | sleeper               (default: ffc)
    --projection-source S  sleeper | fftoday | fp-archive   (default: sleeper)
                        fftoday = 2008-2025, live, deepest
                        sleeper = RotoWire, 2021+; fp-archive = FantasyPros
                        consensus from the Internet Archive, 2015+ (pre-kickoff
                        captures only)

  Models are not scoring presets. Models decide the ORDER; --scoring decides how
  points are counted (and is applied to the realized outcomes too).")
  (println (str "    models:  " (str/join ", " (map name (model/registered)))))
  (println (str "    presets: " (str/join ", " (map name (keys scoring/presets)))))
  (println "
  Season coverage is limited by the INPUTS, not by outcomes. nflverse outcomes
  reach 1999, but Sleeper's projections are only frozen-preseason from 2021, so a
  projection model asked for earlier seasons will skip them. See --source-report."))

(def season-length-change
  "The NFL went from 16 to 17 games in 2021. Season TOTALS are about 6% larger
  from then on, so a window spanning the boundary compares inflated seasons
  against deflated ones and reads the difference as model skill. Per-game truth
  is unaffected."
  2021)

(defn check-era-guard
  "Refuse a totals-based comparison that straddles the 16/17-game boundary."
  [seasons truth-key force?]
  (when (and (= truth-key :actual/points)
             (not force?)
             (some #(< % season-length-change) seasons)
             (some #(>= % season-length-change) seasons))
    (usage-error
     (str "this window spans the 2021 season-length change (16 -> 17 games), and\n"
          "  season TOTALS are ~6% larger after it — the comparison would partly measure\n"
          "  the calendar.\n\n"
          "  Either:  --truth ppg        (per-game; unaffected by season length)\n"
          "  or:      --seasons 2010-2020  /  --seasons 2021-2025\n"
          "  or:      --force-totals     (proceed anyway, knowing the bias)"))))

(defn fmt [n d] (format (str "%." d "f") (double n)))

;; ---- rendering ----

(defn compress-seasons
  "[1999 2000 2001 2005] -> \"1999-2001, 2005\". Asking for the full nflverse
  outcome range means 20+ skipped seasons; one line per season buries the
  seasons that actually scored."
  [seasons]
  (->> (sort seasons)
       (reduce (fn [runs s]
                 (if (and (seq runs) (= s (inc (peek (peek runs)))))
                   (conj (pop runs) (conj (peek runs) s))
                   (conj runs [s])))
               [])
       (map (fn [run] (if (= 1 (count run))
                        (str (first run))
                        (str (first run) "-" (last run)))))
       (str/join ", ")))

(defn print-skips [results]
  (when-let [skipped (seq (filter :skipped? results))]
    (println)
    (doseq [[reason rs] (group-by :reason skipped)]
      (println (format "  skipped %d season(s) — %s" (count rs) reason))
      (println (format "    %s" (compress-seasons (map :season rs)))))
    (println "    (season coverage is limited by the inputs, not the outcomes —")
    (println "     Sleeper projections are frozen-preseason only from 2021; see --source-report)")))

(defn print-season-table [results truth-key]
  (let [scored (remove :skipped? results)]
    (if (empty? scored)
      (println "  no seasons scored.")
      (do
        (println (format "  %-6s %-4s %5s %7s %8s %7s %7s"
                         "season" "pos" "n" "hits" "median" "busts" "rho"))
        (doseq [{:keys [season metrics]} scored]
          (doseq [[pos m] metrics]
            ;; A thin pool keeps its rho but has no meaningful hit rate; show a
            ;; dash rather than a number the pool cannot support.
            (println (format "  %-6d %-4s %5d %7s %8s %7s %7s"
                             season pos (:n m)
                             (if (:hits m) (str (:hits m) "/" (:top-n m)) "-")
                             (if (:median-finish m) (fmt (:median-finish m) 1) "-")
                             (if (:busts m) (str (:busts m)) "-")
                             (fmt (:spearman m) 3)))))
        (println (format "   (truth = %s)" (name truth-key))))))
  (print-skips results))

(defn print-aggregate [agg]
  (println (format "  %-4s %8s %8s %7s %10s %9s" "pos" "seasons" "hits" "busts" "worst-rho" "mean-rho"))
  (doseq [[pos a] agg]
    (println (format "  %-4s %8d %4d/%-3d %7d %10s %9s%s"
                     pos (:seasons a) (:hits a 0) (:possible a 0) (:busts a 0)
                     (fmt (:worst-rho a) 3) (fmt (:mean-rho a) 3)
                     (if (and (:hit-seasons a) (not= (:hit-seasons a) (:seasons a)))
                       (format "   (hit rate from %d of %d seasons)" (:hit-seasons a) (:seasons a))
                       "")))))

(def exclusive-modes
  "Flags that each take over the whole run, and the flag text to name them by.

  Checked centrally because a check that lives inside one mode's own handler is
  dead code whenever another mode dispatches first: `--auction` refused to
  combine with `--vorp`, `-main` tested `:vorp?` one line earlier, and the pair
  silently ran the VORP report. Every new mode would otherwise have to name
  every older one, in both directions, with the `cond` order deciding which half
  of each pair never runs."
  {:source-report? "--source-report" :power-report? "--power-report"
   :vorp? "--vorp" :auction? "--auction"})

(defn check-one-mode
  "Refuse a run that asks for two whole modes at once."
  [opts]
  (let [named (keep (fn [[k flag]] (when (get opts k) flag)) exclusive-modes)]
    (when (next named)
      (usage-error (str "these flags are each a whole mode and cannot combine: "
                        (str/join ", " named)
                        ".\n  Run them one at a time.")))))

(defn run-model [model {:keys [seasons scoring-name truth pool-size adp-source projection-source
                              require-week1? no-slice? force-totals?]}]
  (let [seasons   (or seasons default-seasons)
        scoring*  (get scoring/presets (or scoring-name :ppr) (:ppr scoring/presets))
        truth-key (if (= truth :ppg) :actual/ppg :actual/points)
        _         (check-era-guard seasons truth-key force-totals?)
        results   (core/run seasons model {:scoring    scoring*
                                           :truth-key  truth-key
                                           :pool-size  (or pool-size 200)
                                           :adp-source (or adp-source :ffc)
                                           :projection-source (or projection-source :sleeper)
                                           :require-week1? (boolean require-week1?)
                                           :slices (when-not no-slice? core/default-slices)})]
    {:results results :truth-key truth-key :agg (core/aggregate results)}))

(defn report [opts]
  (let [models (or (:models opts) [:points])]
    (doseq [m models]
      (println)
      (println (str "=== model " m " ==="))
      (let [{:keys [results agg truth-key]} (run-model m opts)]
        (print-season-table results truth-key)
        (println)
        (println "  pooled:")
        (print-aggregate agg)))))

(defn scored-season-set [{:keys [results]}]
  (set (map :season (remove :skipped? results))))

(defn ci-str [{:keys [lo hi]}]
  (if (and lo hi) (format "[%+.3f,%+.3f]" lo hi) "[  n/a  ]"))

(defn verdict [ci]
  (cond (nil? (:lo ci))          "1 season — no interval"
        (metrics/spans-zero? ci) "indistinguishable"
        (pos? (:point ci))       "A better"
        :else                    "B better"))

(defn print-paired
  "The comparison that actually carries information.

  Two models are scored on the same seasons, so pooling their results discards
  the pairing and inherits the whole season-to-season swing. Both paired views
  are shown: season-level rho (few observations, directly interpretable) and
  player-season rank error (thousands of observations, far more power). Both are
  bootstrapped by season block, because players inside a season are correlated."
  [a b ra rb]
  (let [rho-d  (metrics/season-rho-diffs (:results ra) (:results rb))
        err-d  (metrics/player-rank-error-diffs (:results ra) (:results rb) (:truth-key ra))
        stat   (metrics/mean-of :diff)]
    (println)
    (println (format "  PAIRED per-season rho   (%s minus %s; positive favours %s)" a b a))
    (println (format "    %-5s %5s %10s %20s   %s" "pos" "n" "mean" "95% CI (block)" "verdict"))
    (doseq [[pos rows] rho-d]
      (let [ci (metrics/block-bootstrap-ci rows stat)]
        (println (format "    %-5s %5d %+10.3f %20s   %s"
                         pos (count rows) (:point ci) (ci-str ci) (verdict ci)))))
    (println)
    (println (format "  PAIRED per-player rank error   (%s minus %s; NEGATIVE favours %s)" a b a))
    (println (format "    %-5s %8s %7s %10s %20s   %s" "pos" "players" "seasons" "mean" "95% CI (block)" "verdict"))
    (doseq [[pos rows] err-d]
      (let [ci (metrics/block-bootstrap-ci rows stat)]
        (println (format "    %-5s %8d %7d %+10.3f %20s   %s"
                         pos (:n-rows ci) (:n-blocks ci) (:point ci) (ci-str ci)
                         (cond (nil? (:lo ci))          "no interval"
                               (metrics/spans-zero? ci) "indistinguishable"
                               (neg? (:point ci))       (str a " better")
                               :else                    (str b " better"))))))
    (println)
    (println "  Intervals are season-block bootstraps: whole seasons are resampled, because")
    (println "  players within a season share a common shock and are not independent.")))

(defn- print-sim-table
  "Two simulated boards side by side, per season, with a block-bootstrapped CI on
  the difference. Shared by the model-vs-model and board-vs-board comparisons —
  the arithmetic is identical, only what is being varied differs."
  [label-a label-b sa sb]
  (let [idx (fn [rows] (into {} (map (juxt :season identity)) rows))
        ia (idx sa) ib (idx sb)
        common (sort (filter (set (keys ib)) (keys ia)))
        rows (mapv (fn [s] {:season s :diff (- (:edge (ia s)) (:edge (ib s)))}) common)
        ci   (metrics/block-bootstrap-ci rows (metrics/mean-of :diff))]
    (println)
    (println "  DRAFT SIMULATION — realized points of the team each board drafted")
    (println (format "    %-8s %12s %12s %12s" "season"
                     (str label-a " edge") (str label-b " edge") "difference"))
    (doseq [s common]
      (println (format "    %-8d %12s %12s %12s"
                       s (fmt (:edge (ia s)) 1) (fmt (:edge (ib s)) 1)
                       (fmt (- (:edge (ia s)) (:edge (ib s))) 1))))
    (println (format "    %-8s %12s %12s %12s   %s"
                     "MEAN" (fmt (metrics/mean (map :edge sa)) 1)
                     (fmt (metrics/mean (map :edge sb)) 1)
                     (fmt (:point ci) 1)
                     (if (metrics/spans-zero? ci) "indistinguishable"
                         (if (pos? (:point ci)) (str label-a " better") (str label-b " better")))))
    (println (format "    95%% CI on the difference: %s" (ci-str ci)))
    (println "    Season-long approximation: no waivers, trades or weekly start/sit.")))

(defn- ordinal-key-for
  "The native ordering key for a model that does not rank on projections. nil for
  a projection model, which ranks on the :points it produced."
  [m]
  (let [needs (model/requires m)]
    (when-not (contains? needs :projections)
      (if (contains? needs :ecr) :ecr :adp))))

(defn print-simulation
  "Draft-simulation results: what team each board actually built.

  Twelve teams snake-draft; one seat uses the model, eleven use consensus ADP,
  repeated with the model in every seat so draft position cancels. 'edge' is the
  model roster's realized best-lineup points minus the field's average."
  [a b ra rb truth-key]
  (let [cfg (fn [m] (assoc simulate/default-config :ordinal-key (ordinal-key-for m)))]
    (print-sim-table (str a) (str b)
                     (simulate/run (:results ra) truth-key (cfg a))
                     (simulate/run (:results rb) truth-key (cfg b)))))

(defn vorp-report
  "Is a VORP board better than a raw-points board?

  The question `simulate/vorp-board` was written to answer and that nothing could
  ask: `simulate-season` reads a `:vorp?` config key, no caller ever set it, and
  so every `--simulate` run this harness has ever produced scored the raw-points
  board. `simulate.clj` justifies VORP's existence with a ~180-point figure that
  is stated as motivation rather than measured output, and a claim of
  '+205/season' survives in project notes without appearing anywhere in the repo.
  This is the run that settles it.

  One model, simulated twice against the same field and the same seasons — the
  only thing that varies is how the model seat orders its board — so the
  difference is the replacement-level correction and nothing else."
  [opts]
  ;; This is a mode, not a modifier, so it cannot honour the flags that belong to
  ;; `--compare`. Saying so beats discarding them: `--simulate` in particular is
  ;; the one a reader is most likely to add, since every other simulation here
  ;; requires it and this flag's help text describes simulating.
  (when-let [ignored (seq (keep (fn [[k flag]] (when (get opts k) flag))
                                [[:compare "--compare"]
                                 [:simulate? "--simulate"]
                                 [:common-pool? "--common-pool"]]))]
    (usage-error (str "--vorp is its own mode and cannot combine with "
                      (str/join ", " ignored)
                      ".\n  It already simulates, and it compares one model's two boards"
                      " rather than two models.")))
  (let [models (or (:models opts) [:points])
        m      (first models)]
    (when (next models)
      (usage-error (str "--vorp takes one model, got " (str/join ", " (map name models))
                        ".\n  It compares that model's VORP board against its own points board.")))
    (when (ordinal-key-for m)
      ;; :adp and :ecr carry no real points, only a synthesized descending scale
      ;; (see `simulate.clj`'s note on ordinal models). Replacement level computed
      ;; from that is arithmetic on a rank, and the comparison would measure
      ;; nothing.
      (usage-error (str "--vorp needs a projection model; " m " ranks on "
                        (name (ordinal-key-for m)) " and has no points to take a "
                        "replacement level from.
  Try --vorp --models points.")))
    (let [{:keys [results truth-key]} (run-model m opts)
          ;; Derived, not restated. `replacement-levels` would otherwise fall
          ;; back to its own default roster, which happens to match the lineup
          ;; simulated here — until someone changes `:starters` to run a 2QB
          ;; simulation and the VORP board silently keeps pricing replacement as
          ;; though one quarterback started.
          base (assoc simulate/default-config
                      :replacement-config (simulate/replacement-config
                                           simulate/default-config))]
      (println)
      (println (format "=== %s: VORP board vs raw-points board ===" m))
      (print-sim-table "VORP" "points"
                       (simulate/run results truth-key (assoc base :vorp? true))
                       (simulate/run results truth-key base)))))

(defn- print-auction-table
  "One season per row: what the Worth-bidding seat's roster scored against the
  eleven seats that just paid the going rate.

  Not `print-sim-table`, which varies one thing across two runs. Here the
  comparison is inside a single run — model seat against its own field — so the
  season's number is the edge itself, and the interval is over that."
  [label rows]
  (let [ci (metrics/block-bootstrap-ci rows (metrics/mean-of :edge))]
    (println)
    (println "  AUCTION SIMULATION — realized points of the team Worth bought")
    (println (format "    %-8s %12s %12s %12s %14s" "season" "model" "field mean" "edge"
                     "top buy m/f"))
    (doseq [{:keys [season model-points field-mean edge model-top-buy field-top-buy]}
            (sort-by :season rows)]
      (println (format "    %-8d %12s %12s %12s %14s"
                       season (fmt model-points 1) (fmt field-mean 1) (fmt edge 1)
                       (format "$%.0f/$%.0f" (double model-top-buy) (double field-top-buy)))))
    (println (format "    %-8s %12s %12s %12s %14s   %s"
                     "MEAN" (fmt (metrics/mean (map :model-points rows)) 1)
                     (fmt (metrics/mean (map :field-mean rows)) 1)
                     (fmt (:point ci) 1)
                     (format "$%.0f/$%.0f"
                             (metrics/mean (map :model-top-buy rows))
                             (metrics/mean (map :field-top-buy rows)))
                     (cond (metrics/spans-zero? ci) "indistinguishable from the market"
                           (pos? (:point ci))       (str label " better")
                           :else                    (str label " worse"))))
    (println (format "    95%% CI on the edge: %s" (ci-str ci)))
    (println "    'top buy' is the priciest single player the model seat bought against what")
    (println "    the average field seat paid for its own — which way the seat missed, not")
    (println "    just that it did.")
    (println "    Season-long approximation: no waivers, trades or weekly start/sit.")))

(defn auction-report
  "Does bidding Worth beat paying the going rate?

  The snake simulator cannot ask this. A snake pick is a choice of player, so
  value, inflation and phase decay — three of the five live steps — never touch
  its result; `--simulate` and `--vorp` between them measure board order and
  nothing else. An auction is a choice of how much, which is what those steps
  compute, so this is the first run in the harness that can falsify the dollars.

  Eleven seats bid a curve measured from the collected corpus of real auctions
  (`replay.price-curve`), disagreeing with each other by as much as real rooms
  disagree; the twelfth bids up to the Worth the live engine computes against
  the state of the room, recomputed before every nomination. Repeated with the
  model in every seat, so nomination luck cancels.

  The disagreement is not decoration. A field that names one number to the
  dollar makes the marginal bidder sit exactly at the mean, so any seat a dollar
  off it wins everything or nothing, and the run scores being different rather
  than being wrong — see `auction/jitter`."
  [opts]
  (when-let [ignored (seq (keep (fn [[k flag]] (when (get opts k) flag))
                                [[:compare "--compare"]
                                 [:simulate? "--simulate"]
                                 [:common-pool? "--common-pool"]]))]
    (usage-error (str "--auction is its own mode and cannot combine with "
                      (str/join ", " ignored)
                      ".\n  It already simulates, and it scores one model's seat"
                      " against the market rather than two models against each other.")))
  (let [models (or (:models opts) [:points])
        m      (first models)]
    (when (next models)
      (usage-error (str "--auction takes one model, got " (str/join ", " (map name models))
                        ".\n  It scores that model's Worth against the market.")))
    (when (ordinal-key-for m)
      (usage-error (str "--auction cannot run " (name m) ": it ranks on "
                        (name (ordinal-key-for m)) " and has no points for the valuation"
                        " chain to price.\n  Try --auction --models points.")))
    (let [{:keys [results truth-key]} (run-model m opts)
          rows (auction/run results truth-key auction/default-config)]
      (println)
      (println (format "=== %s: bidding Worth against the market ===" m))
      (if (empty? rows)
        (println "  no seasons survived the vintage gate — nothing to auction.")
        (print-auction-table "Worth" rows)))))

(defn power-report
  "What the current corpus can and cannot resolve, BEFORE running a sweep."
  [[a b] opts]
  (let [ra (run-model (or a :points) opts)
        rb (run-model (or b :points+adp) opts)
        d  (metrics/season-rho-diffs (:results ra) (:results rb))
        s  (metrics/power-summary d 0.05)]
    (println)
    (println (format "=== statistical power: %s vs %s ===" (or a :points) (or b :points+adp)))
    (println)
    (println "  Season-level rho collapses ~60 players into one number per season, so n is")
    (println "  the season count. 'resolvable' is the smallest true difference detectable at")
    (println "  80% power; observed differences between models run 0.01-0.05.")
    (println)
    (println (format "  %-5s %5s %10s %13s %15s" "pos" "n" "paired SD" "resolvable" "seasons for 0.05"))
    (doseq [[pos p] s]
      (println (format "  %-5s %5d %10s %13s %15s"
                       pos (:n p)
                       (if (:sd p) (fmt (:sd p) 3) "-")
                       (if (:mdd p) (fmt (:mdd p) 3) "-")
                       (or (:seasons-for-target p) "-"))))
    (println)
    (println "  Where 'seasons for 0.05' exceeds the seasons available, that position cannot")
    (println "  be settled by season-level rho at any corpus size — use the per-player paired")
    (println "  view or the draft simulation instead.")))

(defn compare-models [[a b] opts]
  (let [ra0 (run-model a opts)
        rb0 (run-model b opts)
        [ra rb] (if (:common-pool? opts)
                  (let [[x y] (core/common-pool (:results ra0) (:results rb0) (:truth-key ra0))]
                    [(assoc ra0 :results x :agg (core/aggregate x))
                     (assoc rb0 :results y :agg (core/aggregate y))])
                  [ra0 rb0])
        sa (scored-season-set ra)
        sb (scored-season-set rb)]
    (println)
    (println (format "=== %s vs %s ===" a b))
    ;; Models declare different input needs, so they can legitimately score
    ;; different windows (:adp reaches 2010, projection models only 2021). Hit
    ;; counts over different windows are NOT comparable, and worst-rho over 16
    ;; seasons is inherently harsher than over 5. Say so loudly rather than
    ;; letting the table imply a head-to-head that did not happen.
    (when (not= sa sb)
      (println)
      (println "  ** WINDOWS DIFFER — these columns are not a head-to-head. **")
      (println (format "     %s scored %d season(s): %s" a (count sa) (compress-seasons sa)))
      (println (format "     %s scored %d season(s): %s" b (count sb) (compress-seasons sb)))
      (println (format "     For a fair comparison restrict to the overlap:"))
      (println (format "       --seasons %s" (compress-seasons (clojure.set/intersection sa sb))))
      (println))
    ;; `ssn` is the seasons each model actually scored AT THAT POSITION. It can
    ;; differ even when the season sets match, because a position group thinner
    ;; than 2N is dropped as uninformative and the two models can draw different
    ;; pools. Without this column, 30/48 next to 25/36 reads as a head-to-head.
    (println (format "  %-4s %13s %5s %15s %5s %13s"
                     "pos" (str a " hits") "ssn" (str b " hits") "ssn" "worst-rho"))
    (doseq [pos (sort (distinct (concat (keys (:agg ra)) (keys (:agg rb)))))]
      (let [x (get (:agg ra) pos) y (get (:agg rb) pos)]
        (println (format "  %-4s %8d/%-4d %5d %10d/%-4d %5d  %6s -> %-6s"
                         pos
                         (:hits x 0) (:possible x 0) (:seasons x 0)
                         (:hits y 0) (:possible y 0) (:seasons y 0)
                         (fmt (:worst-rho x 0) 3) (fmt (:worst-rho y 0) 3)))))
    (print-paired a b ra rb)
    (when (:simulate? opts) (print-simulation a b ra rb (:truth-key ra)))))

(defn source-report [{:keys [seasons]}]
  (let [seasons (or seasons (range 2015 2026))]
    (println)
    (println "=== source report ===")
    (println)
    (println "  nflverse — realized outcomes (the answer key)")
    (println (format "    %-8s %-10s" "season" "rows"))
    (doseq [s seasons]
      (println (format "    %-8d %-10d" s (count (nflverse/outcomes s)))))
    (println)
    (println "  sleeper — vintage projections; gate is behavioural (see benchmark.vintage)")
    (println "    'entries' is what Sleeper returned; 'w/stats' is how many carried any")
    (println "    projection at all. entries>0 with w/stats=0 is a genuine absence, not a")
    (println "    failed fetch — a failed fetch reports FETCH FAILED instead.")
    (println (format "    %-8s %-9s %-9s %-7s %-12s %-6s %s"
                     "season" "entries" "w/stats" "scored" "gp-flatness" "pass?" "reason"))
    (doseq [s seasons]
      (let [g (vintage/gate s)]
        (println (format "    %-8d %-9d %-9d %-7d %-12s %-6s %s"
                         s (:raw-entries g 0) (:with-stats g 0) (:n g 0)
                         (fmt (:gp-flatness g 0) 3) (str (:pass? g)) (:reason g)))))
    (println)
    (println "  ffcalculator — vintage ADP; provenance is explicit in the payload")
    (println (format "    %-8s %-12s %-12s %-8s" "season" "start" "end" "drafts"))
    (doseq [s seasons]
      (let [w (ffc/draft-window s)]
        (println (format "    %-8d %-12s %-12s %-8s"
                         s (str (:start_date w "-")) (str (:end_date w "-"))
                         (str (:total_drafts w "-"))))))
    (println)
    (println)
    (println "  fp-archive — FantasyPros draft projections via the Internet Archive")
    (println "    Only pre-kickoff captures are used. The capture DATE is the quality")
    (println "    signal: an August capture has seen free agency, the draft and camp;")
    (println "    an April one has seen none of them and will drag that position down.")
    (println (format "    %-8s %-7s %-10s %s" "season" "rows" "complete?" "capture date per position"))
    (doseq [c (fp-archive/coverage seasons)]
      (println (format "    %-8d %-7d %-10s %s"
                       (:season c) (:n c) (str (:complete? c))
                       (pr-str (into (sorted-map)
                                     (map (fn [[k v]] [k (subs (str v) 0 8)]))
                                     (:captures c))))))
    (println)
    (println "  fftoday — preseason projections, live site, no archive (deepest source)")
    (println "    No capture date is published, so the vintage check is behavioural: a")
    (println "    preseason board still contains players who went on to miss the season.")
    (println "    The 2018 RB board ranks Le'Veon Bell third; he held out all year.")
    (println (format "    %-8s %-7s %s" "season" "rows" "by position"))
    (doseq [c (fftoday/coverage (filter #(<= 2008 % 2025) seasons))]
      (println (format "    %-8d %-7d %s" (:season c) (:n c) (pr-str (:by-position c)))))
    (println)
    (println "  fp-ecr — FantasyPros expert consensus rankings via the Internet Archive")
    (println "    Reaches 2011 — the cheatsheet IS an ordered draftable universe, so unlike")
    (println "    the projection sources it needs no ADP list. 2021+ captures are")
    (println "    JavaScript-rendered and yield nothing. Ranks cannot be re-scored under")
    (println "    custom league rules, so :ecr is scoring-agnostic.")
    (println (format "    %-8s %-10s %-7s %s" "season" "capture" "rows" "by position"))
    (doseq [c (fp-ecr/coverage seasons)]
      (println (format "    %-8d %-10s %-7d %s"
                       (:season c) (str (:capture c "-")) (:n c) (pr-str (:by-position c)))))
    (println)
    (println "  id bridge — sleeper -> gsis")
    (let [x (ids/crosswalk) n (ids/name-crosswalk)]
      (println (format "    dynastyprocess sleeper->gsis: %d   name->gsis: %d" (count x) (count n))))))

(defn -main [& args]
  (try
    (let [opts (parse-args args)]
      (when-not (:help? opts) (check-one-mode opts))
      (binding [fetch/*refresh* (boolean (:refresh? opts))]
       (cond
        (:help? opts)          (print-help)
        (:source-report? opts) (source-report opts)
        (:power-report? opts)  (power-report (or (:compare opts) [nil nil]) opts)
        (:vorp? opts)          (vorp-report opts)
        (:auction? opts)       (auction-report opts)
        (:compare opts)        (compare-models (:compare opts) opts)
        :else                  (do (report opts)
                                   (println)
                                   (println (str "  models: " (str/join ", " (map name (model/registered)))
                                                 "   (--help for flags)"))))))
    (catch clojure.lang.ExceptionInfo e
      ;; A usage mistake should read as a usage mistake, not a stack trace.
      (if (::usage (ex-data e))
        (do (println) (println (str "  " (ex-message e))) (println))
        (throw e)))
    (finally (shutdown-agents))))
