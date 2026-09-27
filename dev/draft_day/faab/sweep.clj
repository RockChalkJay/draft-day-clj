(ns draft-day.faab.sweep
  "Score every CHOSEN constant the bid model reads, one at a time, against real
  waiver auctions: `draft-day.faab.replay` run over several finished leagues
  with the constant moved and everything else as shipped.

  Each value is scored against the shipped one on the same auctions, so every
  difference is paired, and its interval resamples whole league-weeks, since
  the auctions in a week share its news. Three things are scored, lower being
  better for the first two:

  - Who bids: log loss of each team's chance of bidding on each free agent
    that week, against whether he did.
  - Win chance: log loss of each real bid's P(win | its amount), against
    whether it won.
  - Value bid: over every auction's winner, what the value bid would have
    saved him against what he paid, a lost claim costed at `lost-claim`.

  Not swept: `faab/heat-weight`, since the replay has no past trending lists;
  `faab/sure-win` and `faab/threat-floor`, which define what is displayed
  rather than estimate anything; and the bidding-style thresholds, which are
  display only.

  The Sleeper-wide backbone is refit once without every replayed league, and
  the season each one continues, so no league is scored against a prior it
  helped measure.

    lein run -m draft-day.faab.sweep                     ; every setting
    lein run -m draft-day.faab.sweep -- --only pseudo-bids
    lein run -m draft-day.faab.sweep -- --set pseudo-bids=16 --set half-life-weeks=2"
  (:require [clojure.string :as str]
            [draft-day.benchmark.metrics :as bm]
            [draft-day.bid-history :as bid-history]
            [draft-day.faab.replay :as replay]
            [draft-day.rankings.faab :as faab]
            [draft-day.rankings.ros :as ros]
            [draft-day.rankings.waiver :as waiver]))

(def leagues
  "Finished 2025 redraft leagues on $100 budgets, each continuing a 2024 season:
  the author's, then the ten of the crawl's with the most contested auctions."
  [replay/default-league
   "1254136828143861760" "1257448686758150145" "1259629887963017216"
   "1257102513774022656" "1251648114230571008" "1212496537347690496"
   "1180572868652548096" "1211429358888030208" "1255201654840508416"
   "1263306727697154048"])

(def settings
  "Each swept constant, the values tried, the shipped one among them. A
  `half-life-weeks` of 1000 is no decay at all."
  [{:name "previous-season-weight" :var #'bid-history/previous-season-weight :values [0.0 0.25 0.5 0.75 1.0]}
   {:name "half-life-weeks"        :var #'bid-history/half-life-weeks        :values [1.0 2.0 4.0 8.0 1000.0]}
   {:name "pseudo-weeks"           :var #'bid-history/pseudo-weeks           :values [0.5 1.0 2.0 4.0 8.0]}
   {:name "pseudo-bids"            :var #'bid-history/pseudo-bids            :values [1.0 2.0 4.0 8.0 16.0 32.0]}
   {:name "pseudo-league-bids"     :var #'bid-history/pseudo-league-bids     :values [3.0 10.0 30.0 100.0 300.0]}
   {:name "cap-quantile"           :var #'faab/cap-quantile                  :values [0.5 0.75 0.9 0.95 0.99]}
   {:name "stash-share"            :var #'waiver/stash-share                 :values [0.0 0.05 0.15 0.3 0.5]}
   {:name "PRIOR-GAMES"            :var #'ros/PRIOR-GAMES                    :values [2.0 4.0 6.0 10.0 17.0]}])

(def lost-claim
  "What a claim the value bid would have lost costs, in dollars: the replay's
  figure, so the two report the same gain."
  10.0)

(def eps
  "How close to certain a chance is taken to be before its log loss is scored."
  1e-4)

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


(defn block-of [league-id week] (str league-id ":w" week))

(defn who-blocks
  "`{block {:n :ll :p :y}}` over the who-bids rows: sums, since a league-season
  holds some ninety thousand of them."
  [league-id who]
  (reduce (fn [acc {:keys [week p bid?]}]
            (let [y (if bid? 1.0 0.0)]
              (update acc (block-of league-id week)
                      (fn [s] (-> (or s {:n 0 :ll 0.0 :p 0.0 :y 0.0})
                                  (update :n inc)
                                  (update :ll + (log-loss p y))
                                  (update :p + p)
                                  (update :y + y))))))
          {} who))

(defn bid-row
  "What the sweep keeps of one scored bid."
  [league-id {:keys [week won? p-win value walk-away] :as r}]
  (let [y (if won? 1.0 0.0)]
    (merge (select-keys r [:week :won? :amount :top :top-range :p-none :value :walk-away])
           {:league  league-id
            :block   (block-of league-id week)
            :win-ll  (log-loss p-win y)
            :gain    (when (and won? value walk-away) (value-gain r))})))


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
  ids themselves, which for a sweep's worth of leagues is past the longest
  name a file may have."
  [league-ids]
  (let [ids (vec (sort league-ids))]
    (replay/cached! (format "%s/backbone-without-%d-%08x.transit"
                            replay/cache-dir (count ids) (hash ids))
                    #(with-redefs [replay/cached! (fn [_ f] (f))]
                       (replay/backbone-without ids)))))

(defn replay-league
  "One league's replay reduced to who-bids sums and bid rows."
  [league-id]
  (let [{:keys [who bids]} (replay/replay league-id {:league-prior? true})]
    {:who  (who-blocks league-id who)
     :bids (mapv #(bid-row league-id %) bids)}))

(defn run-all
  "Every league replayed with `overrides` (`{var value}`) in force, under the
  pooled backbone `prior`."
  [prior league-ids overrides]
  (with-redefs-fn (merge prior overrides)
    #(let [outs (doall (pmap replay-league league-ids))]
       {:who  (apply merge (map :who outs))
        :bids (vec (mapcat :bids outs))})))

(defn mean [xs] (when (seq xs) (/ (reduce + 0.0 xs) (count xs))))

(defn summary
  "One run's headline figures."
  [{:keys [who bids]}]
  (let [w       (vals who)
        n       (reduce + 0 (map :n w))
        contest (filter #(and (:top %) (:top-range %)) bids)
        gains   (keep :gain bids)]
    {:who-ll    (/ (reduce + 0.0 (map :ll w)) n)
     :who-p     (/ (reduce + 0.0 (map :p w)) n)
     :who-y     (/ (reduce + 0.0 (map :y w)) n)
     :win-ll    (mean (map :win-ll bids))
     :none-p    (mean (map :p-none bids))
     :none-y    (mean (map #(if (:top %) 0.0 1.0) bids))
     :p50       (mean (map #(if (<= (:top %) (first (:top-range %))) 1.0 0.0) contest))
     :p90       (mean (map #(if (<= (:top %) (second (:top-range %))) 1.0 0.0) contest))
     :gain      (mean gains)
     :kept      (mean (map #(replay/outcome (:value %) (:top %))
                           (filter :gain bids)))
     :bids      (count bids)
     :winners   (count gains)}))

(defn paired-blocks
  "One row per league-week holding the candidate's and the baseline's sums,
  for `bm/block-bootstrap-ci`: each row is a block, so resampling rows
  resamples blocks."
  [base cand]
  (let [bid-sums (fn [bids] (update-vals (group-by :block bids)
                                         (fn [rs] {:win-n  (count rs)
                                                   :win-ll (reduce + 0.0 (map :win-ll rs))
                                                   :gain-n (count (keep :gain rs))
                                                   :gain   (reduce + 0.0 (keep :gain rs))})))
        bb (bid-sums (:bids base))
        cb (bid-sums (:bids cand))]
    (mapv (fn [k]
            {:season k
             :who-n  (get-in base [:who k :n] 0)
             :who-d  (- (get-in cand [:who k :ll] 0.0) (get-in base [:who k :ll] 0.0))
             :win-n  (get-in bb [k :win-n] 0)
             :win-d  (- (get-in cb [k :win-ll] 0.0) (get-in bb [k :win-ll] 0.0))
             :gain-n (get-in bb [k :gain-n] 0)
             :gain-d (- (get-in cb [k :gain] 0.0) (get-in bb [k :gain] 0.0))})
          (distinct (concat (keys (:who base)) (keys bb))))))

(defn ratio-stat
  "The pooled difference per row: a sum of differences over a sum of counts."
  [d-key n-key]
  (fn [rows]
    (let [n (reduce + 0 (map n-key rows))]
      (if (pos? n) (/ (reduce + 0.0 (map d-key rows)) n) 0.0))))

(defn compare-runs
  "The candidate less the baseline on the three scored figures, each with a
  95% interval over league-weeks."
  [base cand]
  (let [rows (paired-blocks base cand)
        ci   #(bm/block-bootstrap-ci rows (ratio-stat %1 %2) {:iterations 1000})]
    {:who  (ci :who-d :who-n)
     :win  (ci :win-d :win-n)
     :gain (ci :gain-d :gain-n)}))


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
                   (* 100 (:none-p s)) (* 100 (:none-y s))
                   (* 100 (or (:p50 s) 0)) (* 100 (or (:p90 s) 0))
                   (* 100 (or (:kept s) 0)))))

(defn value-str [v] (if (== v (Math/rint v)) (str (long v)) (str v)))

(defn parse-set
  "`name=value` as `[var value]`."
  [s]
  (let [[n v] (str/split s #"=" 2)
        {:keys [var]} (first (filter #(= n (:name %)) settings))]
    (when-not var (throw (ex-info (str "no such setting: " n) {:name n})))
    [var (Double/parseDouble v)]))

(defn flag-values [args flag]
  (map second (filter #(= flag (first %)) (partition 2 1 args))))

(defn -main [& args]
  (let [only      (set (flag-values args "--only"))
        sets      (into {} (map parse-set (flag-values args "--set")))
        league-ids (or (seq (flag-values args "--league")) leagues)
        ;; Every replayed league and the season each continues, fetched here
        ;; one at a time rather than by the replays at once.
        excluded  (distinct (mapcat (fn [id]
                                      (let [prev (some-> (replay/league-docs id) :league
                                                         :previous_league_id str)]
                                        (when prev (replay/league-docs prev))
                                        (cond-> [id] prev (conj prev))))
                                    league-ids))
        prior     (replay/prior-vars (backbone-without excluded))
        timed     (fn [label f]
                    (let [t0 (System/nanoTime) v (f)]
                      (binding [*out* *err*]
                        (println (format "  [%s: %.0fs]" label (/ (- (System/nanoTime) t0) 1e9))))
                      v))]
    (with-redefs [waiver/rival-needs (cached-rival-needs waiver/rival-needs)]
      (let [base   (timed "baseline" #(run-all prior league-ids {}))
            bs     (summary base)]
        (println (format "\n%d leagues, %d bids, %d winners priced; backbone refit without %d league-seasons"
                         (count league-ids) (:bids bs) (:winners bs) (count excluded)))
        (println (format "Shipped: who-bids LL %.4f (predicted %.2f%% bid, observed %.2f%%), win LL %.4f, gain $%.2f/winner"
                         (:who-ll bs) (* 100 (:who-p bs)) (* 100 (:who-y bs)) (:win-ll bs) (:gain bs)))
        (if (seq sets)
          (let [cand (timed "combined" #(run-all prior league-ids sets))]
            (println (str "\nCombined: " (str/join ", " (map (fn [[v x]] (str (:name (meta v) (str v)) "=" x)) sets))))
            (print-header)
            (print-row "shipped" true bs nil)
            (print-row "combined" false (summary cand) (compare-runs base cand)))
          (doseq [{:keys [name var values]} settings
                  :when (or (empty? only) (only name))]
            ;; Each setting refills the cache once, so the boards a setting
            ;; moves do not outlive it.
            (reset! needs-cache {})
            (println (str "\n-- " name " (shipped " (value-str @var) ") --"))
            (print-header)
            (doseq [v values]
              (if (== v @var)
                (print-row (value-str v) true bs nil)
                (let [cand (timed (str name "=" (value-str v)) #(run-all prior league-ids {var v}))]
                  (print-row (value-str v) false (summary cand) (compare-runs base cand))))
              (flush))))))
    (shutdown-agents)))
