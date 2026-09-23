(ns draft-day.faab.report
  "Entry point: crawl (or load) the FAAB corpus and print what real Sleeper
  leagues pay for waiver claims — the measurements the bid model's backbone is
  built from.

    lein run -m draft-day.faab.report                 ; report on the cached corpus
    lein run -m draft-day.faab.report --crawl         ; crawl more, resuming, then report
    lein run -m draft-day.faab.report --fresh         ; crawl from scratch, then report
    lein run -m draft-day.faab.report --prior         ; also print the backbone as EDN

  A crawl takes its bounds from `--k=v` flags, all optional: `--max-seasons=N`,
  `--max-users=N`, `--max-seasons-per-user=N`.

  Every share is of the league's season budget. \"Bidders\" counts the managers
  who competed for a player in one run, the winner included, with four and up
  pooled. The phases are weeks 1-4, 5-10 and 11 on."
  (:require [clojure.pprint :as pp]
            [draft-day.faab.corpus :as corpus]
            [draft-day.faab.crawl :as crawl]))

(def seed-uid
  "Where a cold walk starts: the author's own account."
  "993960010998722560")

(defn quantile
  "The `p` quantile of `xs` by nearest rank, or nil for none."
  [xs p]
  (when (seq xs)
    (let [v (vec (sort xs))]
      (nth v (min (dec (count v)) (int (Math/floor (* p (count v)))))))))

(defn summarize
  "Count, share of zeros, and quantiles of `xs`."
  [xs]
  (let [pos (filter pos? xs)]
    {:n      (count xs)
     :p-zero (when (seq xs) (/ (count (remove pos? xs)) (double (count xs))))
     :p25    (quantile xs 0.25) :p50 (quantile xs 0.5)
     :p75    (quantile xs 0.75) :p90 (quantile xs 0.9)
     :pos-p50 (quantile pos 0.5)}))

(defn ranks
  "Average ranks, ties sharing their mean rank."
  [xs]
  (let [idx (sort-by #(nth xs %) (range (count xs)))
        groups (partition-by #(nth xs %) idx)]
    (loop [gs groups start 0 out (vec (repeat (count xs) 0.0))]
      (if-let [g (first gs)]
        (let [r (+ start (/ (dec (count g)) 2.0))]
          (recur (rest gs) (+ start (count g)) (reduce #(assoc %1 %2 r) out g)))
        out))))

(defn pearson [xs ys]
  (let [n  (count xs)
        mx (/ (reduce + xs) n) my (/ (reduce + ys) n)
        sxy (reduce + (map #(* (- %1 mx) (- %2 my)) xs ys))
        sxx (reduce + (map #(* (- % mx) (- % mx)) xs))
        syy (reduce + (map #(* (- % my) (- % my)) ys))]
    (when (and (pos? sxx) (pos? syy)) (/ sxy (Math/sqrt (* sxx syy))))))

(defn spearman
  "Rank correlation of paired values, or nil with fewer than three pairs."
  [pairs]
  (when (>= (count pairs) 3)
    (pearson (ranks (mapv first pairs)) (ranks (mapv second pairs)))))

(defn pct [x] (if (number? x) (format "%5.1f%%" (* 100.0 x)) "    –"))

(defn print-summary-row [label {:keys [n p-zero p25 p50 p75 p90 pos-p50]}]
  (println (format "  %-22s n=%-7d zero=%s  p25=%s p50=%s p75=%s p90=%s  pos-p50=%s"
                   label n (pct p-zero) (pct p25) (pct p50) (pct p75) (pct p90) (pct pos-p50))))

(defn print-shape [seasons rows]
  (let [metas (map :meta seasons)
        tally (fn [k] (->> metas (map k) frequencies (sort-by (comp str key))))]
    (println "\n-- Corpus --")
    (println (format "  league-seasons %d   auctions %d   bids %d"
                     (count seasons) (count rows) (reduce + (map :n rows))))
    (println "  seasons   " (pr-str (tally :season)))
    (println "  superflex " (pr-str (tally :superflex?)))
    (println "  kind      " (pr-str (tally :kind)))
    (println "  teams     " (pr-str (tally :num-teams)))
    (println "  budgets   " (pr-str (tally :budget)))
    (println "  daily     " (pr-str (tally :daily?)))
    (println "  bidders   " (pr-str (->> rows (map :bucket) frequencies (sort-by key))))))

(defn print-winners [rows]
  (println "\n-- Winning bid by bidders (FF Beacon's medians: 0 / 5 / 10.5 / 20%) --")
  (doseq [[b rs] (sort-by key (group-by :bucket rows))]
    (print-summary-row (str b (when (= 4 b) "+") " bidder(s)") (summarize (map :winner rs)))))

(defn print-split
  "Winning bid by bidders, split by `k`."
  [rows k label]
  (println (str "\n-- Winning bid by bidders × " label " --"))
  (doseq [[b rs] (sort-by key (group-by :bucket rows))
          [v vs] (sort-by (comp str key) (group-by k rs))]
    (print-summary-row (str b (when (= 4 b) "+") " · " v) (summarize (map :winner vs)))))

(defn typical-shares
  "`{[bucket phase] median positive bid share}` — what an ordinary bid in that
  kind of auction is, for measuring a manager against."
  [bids]
  (into {} (map (fn [[k bs]] [k (quantile (filter pos? (map :share bs)) 0.5)]))
        (group-by (juxt :bucket :phase) bids)))

(defn print-backbone [bids]
  (println "\n-- Individual competitive bids by bidders × phase (the backbone) --")
  (doseq [[[b ph] bs] (sort-by key (group-by (juxt :bucket :phase) bids))]
    (print-summary-row (str b (when (= 4 b) "+") " · " (name ph)) (summarize (map :share bs)))))

(defn spread-reduction
  "How much knowing `k` narrows positive bids within a bidder bucket: one minus
  the ratio of mean absolute log-deviation around each (bucket, k) median to
  that around each bucket median. Near 0 means `k` does not move prices."
  [bids k]
  (let [pos   (filter #(pos? (:share %)) bids)
        dev   (fn [group-key]
                (let [groups (group-by group-key pos)
                      meds   (update-vals groups #(quantile (map :share %) 0.5))]
                  (/ (reduce + (map #(Math/abs (Math/log (/ (:share %) (meds (group-key %))))) pos))
                     (max 1 (count pos)))))
        base  (dev :bucket)
        split (dev (juxt :bucket k))]
    (when (pos? base) (- 1.0 (/ split base)))))

(defn print-dimensions [bids]
  (println "\n-- Which dimensions move positive bids (spread reduction within bidder count) --")
  (let [dims {:phase :phase :position :position :superflex? :superflex? :kind :kind
              :teams #(corpus/team-bucket (:num-teams %)) :budget :budget :daily? :daily?}]
    (doseq [[label r] (sort-by (comp - (fnil identity 0.0) second)
                               (map (fn [[label k]] [label (spread-reduction bids k)]) dims))]
      (println (format "  %-12s %s" (name label) (if r (format "%5.1f%%" (* 100 r)) "–"))))))

(defn heaping
  "How often positive bids of at least `floor` dollars land on round numbers, in
  a `budget`-dollar league, beside the rate chance alone would give."
  [bids budget floor unit]
  (let [amts  (->> bids (filter #(= budget (:budget %))) (map :amount) (filter #(>= % floor)))
        n     (count amts)
        share (fn [pred] (when (pos? n) (/ (count (filter pred amts)) (double n))))]
    {:n n
     :round     (share #(zero? (mod % unit)))
     :round-2x  (share #(zero? (mod % (* 2 unit))))
     :one-over  (share #(= 1 (mod % unit)))
     :by-chance (/ 1.0 unit)}))

(defn print-heaping [bids]
  (println "\n-- Round numbers (positive bids; chance alone would put 1 in `unit` on each) --")
  (doseq [[budget floor unit] [[100 5 5] [1000 50 50] [1000 10 10]]]
    (let [{:keys [n round round-2x one-over by-chance]} (heaping bids budget floor unit)]
      (println (format "  $%-5d unit $%-3d n=%-6d on a multiple: %s  of twice it: %s  one over: %s  (chance %s)"
                       budget unit n (pct round) (pct round-2x) (pct one-over) (pct by-chance))))))

(defn print-overpay [rows]
  (println "\n-- What winners left on the table (contested auctions) --")
  (let [c      (filter #(>= (:n %) 2) rows)
        ratios (keep #(when (pos? (:second %)) (/ (:winner %) (:second %))) c)
        gaps   (map #(- (:winner %) (:second %)) c)]
    (println (format "  n=%d  winner/second median %.2f×   overpay p50=%s p75=%s p90=%s   won by 5%%+ of budget: %s"
                     (count c) (double (or (quantile ratios 0.5) 0)) (pct (quantile gaps 0.5))
                     (pct (quantile gaps 0.75)) (pct (quantile gaps 0.9))
                     (pct (if (seq gaps) (/ (count (filter #(>= % 0.05) gaps)) (double (count gaps))) 0.0))))))

(defn print-habits [managers]
  (println "\n-- Managers (manager-seasons with 10+ bids) --")
  (let [ms (filter #(>= (:bids %) 10) managers)
        q  (fn [k p] (quantile (keep k ms) p))]
    (println (format "  n=%d" (count ms)))
    (doseq [[k label f] [[:per-week "claims a week" #(format "%5.2f" (double %))]
                         [:zero-share "share at $0" pct]
                         [:aggression "aggression (log vs typical)" #(format "%+5.2f" (double %))]]]
      (println (format "  %-28s p10=%s p50=%s p90=%s" label
                       (f (q k 0.1)) (f (q k 0.5)) (f (q k 0.9)))))))

(defn season-pairs
  "A manager's season beside his previous season in the league it continues."
  [managers]
  (let [by (into {} (map (juxt (juxt :league-id :owner-id) identity)) managers)]
    (keep (fn [m] (when-let [prev (get by [(:previous-league-id m) (:owner-id m)])]
                    [prev m]))
          managers)))

(defn league-pairs
  "One manager's two leagues in the same season, one pair per manager-season."
  [managers]
  (->> (group-by (juxt :owner-id :season) managers)
       vals
       (keep (fn [ms] (let [[a b] (sort-by :league-id ms)] (when b [a b]))))))

(defn persistence
  "Rank correlation of each habit across `pairs`, over pairs where both sides
  have 10+ bids."
  [pairs]
  (let [ok (filter (fn [[a b]] (and (>= (:bids a) 10) (>= (:bids b) 10))) pairs)
        r  (fn [k] (spearman (keep (fn [[a b]] (when (and (k a) (k b)) [(k a) (k b)])) ok)))]
    {:pairs (count ok) :per-week (r :per-week) :zero-share (r :zero-share)
     :aggression (r :aggression)}))

(defn print-persistence [managers]
  (println "\n-- Do habits carry over? (rank correlation) --")
  (doseq [[label p] [["season to season, same league" (persistence (season-pairs managers))]
                     ["league to league, same season" (persistence (league-pairs managers))]]]
    (println (format "  %-32s pairs=%-5d claims/wk=%s  $0 share=%s  aggression=%s"
                     label (:pairs p)
                     (some->> (:per-week p) (format "%+.2f"))
                     (some->> (:zero-share p) (format "%+.2f"))
                     (some->> (:aggression p) (format "%+.2f"))))))

(defn shifts
  "Per value of `k`, how far its positive bids sit from its bidder bucket's: the
  median of `log(value median / bucket median)` across buckets, weighted by
  count. Zero means `k` adds nothing once the bidder count is known."
  [bids k]
  (let [pos       (filter #(pos? (:share %)) bids)
        bucket-md (update-vals (group-by :bucket pos) #(quantile (map :share %) 0.5))]
    (into (sorted-map)
          (map (fn [[v bs]]
                 (let [logs (mapcat (fn [[b cbs]]
                                      (repeat (count cbs)
                                              (Math/log (/ (quantile (map :share cbs) 0.5)
                                                           (bucket-md b)))))
                                    (group-by :bucket bs))]
                   [v {:n (count bs) :log-shift (quantile logs 0.5)}])))
          (group-by k pos))))

(defn position-shifts [bids] (shifts bids :position))

(def backbone-budget
  "The budget the backbone is measured in. The Sleeper default and the dominant
  format; bids are shares of it, and leagues on other budgets bid smaller shares
  at every quantile rather than only at the $1 grain, so pooling them would
  blur the one scale the model converts through. `:budget-shift` says how far
  the others sit from it."
  100)

(defn budget-class [b] (if (= backbone-budget (:budget b)) :base :other))

(defn budget-shifts
  "Per bidder bucket, how far positive bids in leagues on any other budget sit
  from `backbone-budget` leagues': the log of the ratio of their median shares.
  Measured one class against the other rather than through `shifts`, whose
  pooled median would put both classes off it and leave the distance between
  them to be subtracted by hand."
  [bids]
  (let [med (fn [bs] (quantile (map :share bs) 0.5))]
    (into (sorted-map)
          (keep (fn [[b bs]]
                  (let [{base :base other :other} (group-by budget-class bs)]
                    (when (and (seq base) (seq other))
                      [b {:n (count other) :log-shift (Math/log (/ (med other) (med base)))}]))))
          (group-by :bucket (filter #(pos? (:share %)) bids)))))

(defn backbone
  "The measured tables, as data for `draft-day.bid-prior`: the bid cells and the
  position, kind, superflex and team shifts over `backbone-budget` leagues; the
  budget shift, heaping, manager habits and their persistence over all of them."
  [bids managers]
  (let [base  (filter #(= :base (budget-class %)) bids)
        cells (group-by (juxt :bucket :phase) base)
        qs    [0.05 0.1 0.25 0.5 0.75 0.9 0.95 0.99]]
    {:bid-share (into (sorted-map)
                      (map (fn [[k bs]]
                             (let [shares (map :share bs)
                                   pos    (filter pos? shares)]
                               [k {:n        (count shares)
                                   :p-zero   (/ (count (remove pos? shares)) (double (count shares)))
                                   :positive (into (sorted-map) (map (fn [p] [p (quantile pos p)])) qs)}])))
                      cells)
     :position     (shifts base :position)
     :kind         (shifts base :kind)
     :superflex    (shifts base :superflex?)
     :teams        (shifts base #(corpus/team-bucket (:num-teams %)))
     :budget-shift (budget-shifts bids)
     :heaping      {:budget-100  (heaping bids 100 5 5)
                    :budget-1000 (heaping bids 1000 50 50)}
     :managers     (let [ms (filter #(>= (:bids %) 10) managers)
                         q  (fn [k p] (quantile (keep k ms) p))]
                     {:n          (count ms)
                      :per-week   {:p10 (q :per-week 0.1) :p50 (q :per-week 0.5) :p90 (q :per-week 0.9)}
                      :zero-share {:p10 (q :zero-share 0.1) :p50 (q :zero-share 0.5) :p90 (q :zero-share 0.9)}
                      :aggression {:p10 (q :aggression 0.1) :p50 (q :aggression 0.5) :p90 (q :aggression 0.9)}})
     :persistence  {:seasons (persistence (season-pairs managers))
                    :leagues (persistence (league-pairs managers))}}))

(defn build!
  "Crawl, resuming saved state unless `fresh`, saving as it goes."
  [{:keys [fresh] :as opts}]
  (let [prior (if fresh {} (crawl/load-state))
        st    (crawl/crawl [seed-uid]
                           (assoc opts
                                  :state prior
                                  :on-accept! crawl/save-season!
                                  :progress! (fn [{:keys [accepted seen-users frontier]}]
                                               (println (format "  visited=%d accepted=%d frontier=%d"
                                                                (count seen-users) (count accepted)
                                                                (count frontier))))
                                  :checkpoint! crawl/save-state!))]
    (crawl/save-state! st)
    (println (format "\ncrawl done: visited=%d examined=%d accepted=%d"
                     (count (:seen-users st)) (:examined st) (count (:accepted st))))
    (println "  reasons" (pr-str (sort-by (comp - val) (:reasons st))))))

(defn parse-bounds [args]
  (into {} (keep (fn [a]
                   (when-let [[_ k v] (re-matches #"--([a-z-]+)=(\d+)" a)]
                     [(keyword k) (parse-long v)])))
        args))

(defn -main [& args]
  (let [flags (set args)]
    (when (or (flags "--crawl") (flags "--fresh"))
      (build! (merge {:fresh (boolean (flags "--fresh"))} (parse-bounds args))))
    (let [seasons  (corpus/load-seasons)
          rows     (corpus/corpus-rows seasons (corpus/fetch-positions!))
          bids     (corpus/bid-rows rows)
          managers (corpus/manager-seasons rows (typical-shares bids))]
      (if (empty? rows)
        (println "no corpus yet — run with --crawl")
        (do
          (print-shape seasons rows)
          (print-winners rows)
          (print-split rows :phase "phase")
          (print-split rows :position "position")
          (print-split rows :superflex? "superflex")
          (print-split rows :kind "league kind")
          (print-backbone bids)
          (print-dimensions bids)
          (doseq [[label k] [["position" :position] ["league kind" :kind] ["superflex" :superflex?]]]
            (println (str "\n-- Shift by " label " within bidder count (log of median ratio) --"))
            (doseq [[v {:keys [n log-shift]}] (shifts bids k)]
              (println (format "  %-9s n=%-7d %+.2f  (×%.2f)" v n log-shift (Math/exp log-shift)))))
          (print-heaping bids)
          (print-overpay rows)
          (print-habits managers)
          (print-persistence managers)
          (when (flags "--prior")
            (println "\n-- Backbone (EDN) --")
            (pp/pprint (backbone bids managers)))))))
  (shutdown-agents))
