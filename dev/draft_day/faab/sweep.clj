(ns draft-day.faab.sweep
  "Score every CHOSEN constant the bid model reads, one at a time, against real
  waiver auctions: `draft-day.faab.replay` run over the frozen league set
  (`draft-day.faab.leagues`) with the constant moved and everything else as
  shipped.

  Each value is scored against the shipped one on the same auctions, so every
  difference is paired. Three things are scored, lower being better for the
  first two:

  - Who bids: log loss of each team's chance of bidding on each free agent
    that week, against whether he did.
  - Win chance: log loss of each real bid's P(win | its amount), against
    whether it won.
  - Value bid: over every auction's winner, what the value bid would have
    saved him against what he paid, a lost claim costed at `lost-claim`.

  An interval is computed twice, once resampling leagues — a league's managers
  are the same all season — and once resampling season-weeks — every league in
  a season reads the same week's news — and the wider is reported, so neither
  dependence can make a difference look surer than it is.

  Constants are moved by `with-redefs`, which holds for every thread at once,
  so every configuration runs over one chunk of `chunk-size` leagues, in
  parallel, before the next chunk starts. Each chunk's sums are written under
  `run-dir` as it finishes, so a run of many hours resumes where it stopped
  (`--fresh` starts over, which a change to the code needs), and the rival
  needs cached across a chunk's configurations are dropped with it, so memory
  does not grow with the league count.

  Not swept: `faab/heat-weight`, since the replay has no past trending lists;
  `faab/sure-win` and `faab/threat-floor`, which define what is displayed
  rather than estimate anything; the bidding-style thresholds, which are
  display only; and `faab/claim-weights`, which `draft-day.faab.interest`
  fits. `faab/cluster-spread` is swept here rather than fit there: the spread
  bidder counts ask for is not the one that prices a claim best. The Sleeper-wide backbone is refit without
  every league in the half being replayed and the season each continues
  (`half-prior`).

    lein run -m draft-day.faab.sweep                          ; fit half, every setting
    lein run -m draft-day.faab.sweep -- --sample 200 --only pseudo-bids
    lein run -m draft-day.faab.sweep -- --half score --set pseudo-bids=16"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [draft-day.benchmark.metrics :as bm]
            [draft-day.bid-history :as bid-history]
            [draft-day.faab.leagues :as leagues]
            [draft-day.faab.replay :as replay]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.rankings.faab :as faab]
            [draft-day.rankings.ros :as ros]
            [draft-day.rankings.waiver :as waiver]))

(def every-kind
  "A number as a constant keyed by league kind: the same for each."
  (fn [v] {:redraft v :keeper v :dynasty v}))

(def settings
  "Each swept constant, the values tried, the shipped one among them, and `:as`
  when a value is a number turned into what the constant holds. A
  `half-life-weeks` of 1000 is no decay at all."
  [{:name "previous-season-weight" :var #'bid-history/previous-season-weight :values [0.0 0.25 0.5 0.75 1.0]}
   {:name "half-life-weeks"        :var #'bid-history/half-life-weeks        :values [1.0 2.0 4.0 8.0 1000.0]}
   {:name "pseudo-weeks"           :var #'bid-history/pseudo-weeks           :values [0.5 1.0 2.0 4.0 8.0]}
   {:name "pseudo-bids"            :var #'bid-history/pseudo-bids            :values [1.0 2.0 4.0 8.0 16.0 32.0]}
   {:name "pseudo-league-bids"     :var #'bid-history/pseudo-league-bids     :values [3.0 10.0 30.0 100.0 300.0]}
   {:name "cap-quantile"           :var #'faab/cap-quantile                  :values [0.5 0.75 0.9 0.95 0.99]}
   {:name "stash-share"            :var #'waiver/stash-share                 :values [0.0 0.05 0.15 0.3 0.5]}
   {:name "PRIOR-GAMES"            :var #'ros/PRIOR-GAMES                    :values [2.0 4.0 6.0 10.0 17.0]}
   {:name "cluster-spread"         :var #'faab/cluster-spread                :values [0.5 0.75 1.0 1.43 2.0]
    :as every-kind}])

(defn value-of
  "What setting `s` holds at the swept number `v`."
  [s v]
  ((or (:as s) identity) v))

(defn shipped?
  "Is `v` the value setting `s` ships with?"
  [s v]
  (let [x (value-of s v) cur @(:var s)]
    (if (number? x) (== x cur) (= x cur))))

(def lost-claim
  "What a claim the value bid would have lost costs, in dollars: the replay's
  figure, so the two report the same gain."
  10.0)

(def eps
  "How close to certain a chance is taken to be before its log loss is scored."
  1e-4)

(def chunk-size
  "Leagues replayed together under one configuration; see the ns docstring."
  40)

(defn log-loss
  "The log loss of chance `p` for an outcome `y` of 1 or 0."
  [p y]
  (let [p (min (- 1.0 eps) (max eps (double p)))]
    (- (Math/log (if (pos? y) p (- 1.0 p))))))

(defn value-gain
  "What the value bid did for one auction's winner: what it saved against his
  real bid when it would still have won, less `lost-claim` when it would not.
  A tie is half of each, as the replay scores it."
  [{:keys [value amount top]}]
  (let [o (replay/outcome value top)]
    (- (* o (- amount (or value 0))) (* (- 1.0 o) lost-claim))))

(defn who-sums
  "One who-bids row as sums."
  [{:keys [p bid?]}]
  (let [y (if bid? 1.0 0.0)]
    {:who-n 1 :who-ll (log-loss p y) :who-p (double p) :who-y y}))

(defn bid-sums
  "One scored bid as sums. A winner the value bid priced adds to the gain."
  [{:keys [won? p-win p-none top top-range value walk-away] :as r}]
  (let [contested? (boolean (and top top-range))
        priced?    (boolean (and won? value walk-away))]
    {:win-n     1
     :win-ll    (log-loss p-win (if won? 1.0 0.0))
     :none-p    (double p-none)
     :none-y    (if top 0.0 1.0)
     :contest-n (if contested? 1 0)
     :p50       (if (and contested? (<= top (first top-range))) 1.0 0.0)
     :p90       (if (and contested? (<= top (second top-range))) 1.0 0.0)
     :gain-n    (if priced? 1 0)
     :gain      (if priced? (value-gain r) 0.0)
     :kept      (if priced? (replay/outcome value top) 0.0)}))

(defn add-sums [a b] (merge-with + a b))

(defn league-sums
  "One league's replay as sums in the two ways it is resampled: `:by-league`,
  one entry, and `:by-week`, keyed `[season week]`."
  [{:keys [league-id stratum]} {:keys [who bids]}]
  (let [season (:season stratum)
        rows   (concat (map (juxt :week who-sums) who) (map (juxt :week bid-sums) bids))]
    {:by-league {league-id (reduce add-sums {} (map second rows))}
     :by-week   (reduce (fn [acc [week s]] (update acc [season week] add-sums s)) {} rows)}))

(defn merge-sums [a b]
  {:by-league (merge-with add-sums (:by-league a) (:by-league b))
   :by-week   (merge-with add-sums (:by-week a) (:by-week b))})

(def needs-cache
  "`{key gains}`: one rival's lineup gain from every free agent, which no swept
  constant moves but through the board. Asking it again for every bidder in
  every run is half a replay's time."
  (atom {}))

(defn cached-rival-needs
  "`waiver/rival-needs` through `needs-cache`, keyed per team by what it reads:
  the free agents and their points, the team's own players and theirs, the
  seats. The points are the key rather than the week, since two runs a week
  apart can hold the same rosters on a different board."
  [original]
  (fn [teams my-roster-id xwalk by-id seats slots fas]
    (let [ids (mapv :player-id fas)
          run [(hash ids) (hash (mapv :ros-points fas)) seats (hash slots)]]
      (->> teams
           (remove #(= (:roster-id %) my-roster-id))
           (mapv (fn [team]
                   (let [held  (waiver/held-ids team xwalk :active-ids)
                         k     [run held (mapv #(get-in by-id [% :ros-points]) held)]
                         gains (or (get @needs-cache k)
                                   (let [[t] (original [team] nil xwalk by-id seats slots fas)
                                         g  (double-array (map #(double (get (:needs t) % 0.0)) ids))]
                                     (swap! needs-cache assoc k g)
                                     g))]
                     (assoc (select-keys team [:roster-id :owner-id :name :faab-left :waiver-position])
                            :needs (zipmap ids gains)))))))))

(defn backbone-without
  "`replay/backbone-without`, cached under a hash of the ids rather than the
  ids themselves, which for a set's worth of leagues is past the longest name
  a file may have."
  [league-ids]
  (let [ids (vec (sort league-ids))]
    (replay/cached! (format "%s/backbone-without-%d-%08x.transit"
                            replay/cache-dir (count ids) (hash ids))
                    #(with-redefs [replay/cached! (fn [_ f] (f))]
                       (replay/backbone-without ids)))))

(defn half-prior
  "The backbone refit without every league in the frozen set's `half` and each
  one's previous season, as `replay/prior-vars`: no league is scored against a
  prior it helped measure, and the other half stays in it. Leaving out the
  whole set would leave out most of the corpus."
  [half]
  (let [rows (leagues/pick (leagues/load-set) half nil)]
    (replay/prior-vars (backbone-without (distinct (mapcat (juxt :league-id :previous) rows))))))

(defn replay-league
  "One league's replay as sums, or nil when it cannot be replayed — logged, so
  a league Sleeper served in a shape the replay cannot read costs one league
  and not the run."
  [row]
  (try
    (league-sums row (replay/replay (:league-id row) {:league-prior? true}))
    (catch Exception e
      (binding [*out* *err*]
        (println "  skipped" (:league-id row) (ex-message e)))
      nil)))

(defn run-dir
  "Where a run's chunks are kept: named for its leagues and configurations."
  [rows configs]
  (format "data/faab_cache/sweep/%08x"
          (hash [(mapv :league-id rows) (mapv (fn [[label o]] [label (update-keys o str)]) configs)])))

(defn run-configs
  "`{label sums}` for every `[label overrides]` in `configs` over `rows`, under
  the backbone `prior`: a chunk at a time, every configuration over a chunk
  before the next (see the ns docstring), each chunk read back from `dir`
  when a previous run finished it."
  [prior rows configs dir]
  (with-redefs [waiver/rival-needs (cached-rival-needs waiver/rival-needs)]
    (let [chunks (vec (partition-all chunk-size rows))]
      (reduce
       (fn [acc [i chunk]]
         (let [path (str dir "/chunk-" i ".transit")
               done (when (.exists (io/file path)) (pipeline/read-transit path))
               sums (or done
                        (let [s (into {}
                                      (map (fn [[label overrides]]
                                             [label (with-redefs-fn (merge prior overrides)
                                                      #(reduce merge-sums {} (keep identity (pmap replay-league chunk))))]))
                                      configs)]
                          (reset! needs-cache {})
                          (io/make-parents path)
                          (pipeline/write-transit! path s)
                          s))]
           (binding [*out* *err*]
             (println (format "  chunk %d of %d%s" (inc i) (count chunks) (if done " (from disk)" ""))))
           (merge-with merge-sums acc sums)))
       {}
       (map-indexed vector chunks)))))

(defn total [sums] (reduce add-sums {} (vals (:by-league sums))))

(defn ratio [s k n] (let [d (get s n 0)] (when (pos? d) (/ (double (get s k 0.0)) d))))

(defn summary
  "One configuration's headline figures."
  [sums]
  (let [t (total sums)]
    {:who-ll  (ratio t :who-ll :who-n)
     :who-p   (ratio t :who-p :who-n)
     :who-y   (ratio t :who-y :who-n)
     :win-ll  (ratio t :win-ll :win-n)
     :none-p  (ratio t :none-p :win-n)
     :none-y  (ratio t :none-y :win-n)
     :p50     (ratio t :p50 :contest-n)
     :p90     (ratio t :p90 :contest-n)
     :gain    (ratio t :gain :gain-n)
     :kept    (ratio t :kept :gain-n)
     :leagues (count (:by-league sums))
     :bids    (get t :win-n 0)
     :winners (get t :gain-n 0)}))

(defn paired-rows
  "One row per block holding the candidate's and the baseline's sums, as
  `bm/block-bootstrap-ci` resamples them."
  [base cand]
  (mapv (fn [k]
          (let [b (get base k {}) c (get cand k {})
                d #(- (double (get c % 0.0)) (double (get b % 0.0)))]
            {:season k
             :who-n  (get b :who-n 0)  :who-d  (d :who-ll)
             :win-n  (get b :win-n 0)  :win-d  (d :win-ll)
             :gain-n (get b :gain-n 0) :gain-d (d :gain)}))
        (distinct (concat (keys base) (keys cand)))))

(defn ratio-stat
  "The pooled difference per row: a sum of differences over a sum of counts."
  [d-key n-key]
  (fn [rows]
    (let [n (reduce + 0 (map n-key rows))]
      (if (pos? n) (/ (reduce + 0.0 (map d-key rows)) n) 0.0))))

(defn wider
  "Of two intervals on one difference, the wider."
  [a b]
  (let [w #(if (:lo %) (- (:hi %) (:lo %)) ##Inf)]
    (if (>= (w a) (w b)) a b)))

(defn compare-runs
  "The candidate less the baseline on the three scored figures, each with the
  wider of its 95% intervals over leagues and over season-weeks."
  [base cand]
  (let [ci (fn [by d n]
             (bm/block-bootstrap-ci (paired-rows (by base) (by cand)) (ratio-stat d n) {:iterations 1000}))
        both (fn [d n] (wider (ci :by-league d n) (ci :by-week d n)))]
    {:who  (both :who-d :who-n)
     :win  (both :win-d :win-n)
     :gain (both :gain-d :gain-n)}))

(defn ci-str
  "`+0.0123 [-0.0010, +0.0200]`, starred when the interval excludes zero."
  [digits {:keys [point lo hi] :as ci}]
  (let [f (str "%+." digits "f")]
    (str (format f point)
         (when lo (format (str " [" f ", " f "]") lo hi))
         (when (and lo (not (bm/spans-zero? ci))) " *"))))

(defn print-header []
  (println (str "  value     who-bids LL Δ (lower better)       win-chance LL Δ (lower better)     "
                "value-bid gain Δ $/winner (higher better)   none pred/obs   top ≤p50/≤p90   kept")))

(defn print-row [label shipped? s cmp]
  (println (format "  %-8s%s %-38s %-38s %-38s %4.0f%%/%4.0f%%     %3.0f%%/%3.0f%%     %4.1f%%"
                   label (if shipped? "*" " ")
                   (if cmp (ci-str 4 (:who cmp)) (format "%.4f (abs)" (:who-ll s)))
                   (if cmp (ci-str 4 (:win cmp)) (format "%.4f (abs)" (:win-ll s)))
                   (if cmp (ci-str 2 (:gain cmp)) (format "%.2f (abs)" (:gain s)))
                   (* 100 (or (:none-p s) 0)) (* 100 (or (:none-y s) 0))
                   (* 100 (or (:p50 s) 0)) (* 100 (or (:p90 s) 0))
                   (* 100 (or (:kept s) 0)))))

(defn value-str [v] (if (== v (Math/rint v)) (str (long v)) (str v)))

(defn parse-set
  "`name=value` as `[var value]`."
  [s]
  (let [[n v] (str/split s #"=" 2)
        setting (first (filter #(= n (:name %)) settings))]
    (when-not setting (throw (ex-info (str "no such setting: " n) {:name n})))
    [(:var setting) (value-of setting (Double/parseDouble v))]))

(defn flag-values [args flag]
  (map second (filter #(= flag (first %)) (partition 2 1 args))))

(defn flag-value [args flag] (first (flag-values args flag)))

(defn replayable
  "The frozen set's `half`, its first `n` when given, less every league the
  fetch skipped or has not reached — so a run can start while a fetch is still
  under way, on the leagues it has."
  [half n]
  (let [skipped (leagues/skipped)
        ready?  #(and (not (skipped (:league-id %)))
                      (every? (fn [id] (.exists (io/file (str replay/cache-dir "/league-" id ".transit"))))
                              [(:league-id %) (:previous %)]))]
    (vec (cond->> (filter ready? (leagues/pick (leagues/load-set) half nil))
           n (take n)))))

(defn configs-for
  "`[label overrides]`: the shipped constants first, then either the one
  combination `sets` names or every value of every setting in `only` (all of
  them when empty) other than the shipped one."
  [only sets]
  (into [["shipped" {}]]
        (if (seq sets)
          [["combined" sets]]
          (for [{:keys [name var values] :as setting} settings
                :when (or (empty? only) (only name))
                v values
                :when (not (shipped? setting v))]
            [(str name "=" (value-str v)) {var (value-of setting v)}]))))

(defn -main [& args]
  (let [flags   (set args)
        half    (keyword (or (flag-value args "--half") "fit"))
        n       (some-> (flag-value args "--sample") parse-long)
        only    (set (flag-values args "--only"))
        sets    (into {} (map parse-set (flag-values args "--set")))
        rows    (replayable half n)
        configs (configs-for only sets)
        dir     (run-dir rows configs)]
    (when (flags "--fresh")
      (run! io/delete-file (reverse (file-seq (io/file dir)))))
    (println (format "%d leagues (%s half), %d configurations, chunks under %s"
                     (count rows) (name half) (count configs) dir))
    (let [out  (run-configs (half-prior half) rows configs dir)
          base (get out "shipped")
          bs   (summary base)]
      (println (format "\n%d leagues replayed, %d bids, %d winners priced" (:leagues bs) (:bids bs) (:winners bs)))
      (println (format "Shipped: who-bids LL %.4f (predicted %.2f%% bid, observed %.2f%%), win LL %.4f, gain $%.2f/winner"
                       (:who-ll bs) (* 100 (:who-p bs)) (* 100 (:who-y bs)) (:win-ll bs) (:gain bs)))
      (if (seq sets)
        (do (print-header)
            (print-row "shipped" true bs nil)
            (print-row "combined" false (summary (get out "combined")) (compare-runs base (get out "combined"))))
        (doseq [{:keys [name values] :as setting} settings
                :when (or (empty? only) (only name))]
          (println (str "\n-- " name " (shipped " (some #(when (shipped? setting %) (value-str %)) values) ") --"))
          (print-header)
          (doseq [v values]
            (if (shipped? setting v)
              (print-row (value-str v) true bs nil)
              (let [cand (get out (str name "=" (value-str v)))]
                (print-row (value-str v) false (summary cand) (compare-runs base cand))))))))
    (shutdown-agents)))
