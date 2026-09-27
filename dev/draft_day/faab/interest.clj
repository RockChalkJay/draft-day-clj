(ns draft-day.faab.interest
  "Measure how a rival aims his claims: fit `faab/claim-weights`, the conditional
  logit a rival's claims are shared out by, to the claims real managers made.

  Each week of each replayed league, at the week's first waiver run, every
  rival faces the free agents the board would have shown, each described by
  `faab/claim-features` and, for that rival alone, `faab/need-features`. The
  players he bid on that week — at any run — are his choices, and the weights
  are the ones that make those choices likeliest: a rival's claims land on a
  player in proportion to e^utility. How *many* claims he makes is his
  `bid-history` profile's business, not this fit's; a rival who bid on nobody
  that week says nothing about where claims land and is left out.

  The fit is Newton's method on the log likelihood, which is concave, with a
  small ridge so a feature nobody varies cannot run off. It is scored out of
  sample, one league at a time: each league's choices are scored under weights
  fit to the others, per choice against the uniform guess, and beside fits
  with a family of features left out, which is what says each earns its place.

  How far bids pile onto one player beyond what the features see is
  `faab/cluster-spread`: every free agent's week multiplies every rival's rate
  on him by one gamma draw of mean one, its shape his total rate over the
  spread, and the spread is the one that makes the weeks' bidder counts
  likeliest — a negative binomial on each, whose variance is its mean times
  one plus the spread. A shape that did not grow with the rate was measured
  first and fit worse: it made a claim on a player nobody expected bring a
  quarter of the company a claim on the week's obvious target does, where
  real claims bring about the same company either way.

  `--replay PATH` then runs `draft-day.faab.sweep`'s leagues through the
  replay with those held-out weights and shape in place, and scores them, paired, against
  a run saved at PATH (a `sweep/run-all` result, as Transit). The backbone is
  refit without every replayed league, as the sweep does.

    lein run -m draft-day.faab.interest                     ; fit, ablate, print
    lein run -m draft-day.faab.interest -- --replay base.transit"
  (:require [draft-day.faab.replay :as replay]
            [draft-day.faab.sweep :as sweep]
            [draft-day.ingestion.pipeline :as pipeline]
            [draft-day.rankings.faab :as faab]
            [draft-day.rankings.waiver :as waiver]))

(def ridge
  "The penalty on the squared weights: small against a log likelihood summed
  over thousands of choices, there only to keep the Hessian invertible."
  1.0)


(defn week-choices
  "One league-week at its first run: every free agent's shared features, and
  per rival his claims a week, his need features and the free agents he bid
  on that week, as indices into `:features`."
  [board ctx week-bids week]
  (let [{:keys [fas xwalk by-id seats habits]} (waiver/market-inputs board (dissoc ctx :my-roster-id))
        needs (waiver/rival-needs (get-in ctx [:league :teams]) nil xwalk by-id seats
                                  (:starting-slots ctx) fas)
        index (zipmap (map :player-id fas) (range))]
    {:week     week
     :features (mapv #(faab/claim-features % week) fas)
     :rivals   (mapv (fn [{:keys [roster-id] :as r}]
                       {:roster-id roster-id
                        :per-week  (:per-week (faab/rival-habits r habits))
                        :needs     (mapv #(faab/need-features (get (:needs r) (:player-id %))) fas)
                        :chosen    (vec (keep (fn [[rid pid]] (when (= rid roster-id) (index pid)))
                                              week-bids))})
                     needs)}))

(defn league-choices
  "Every week's choices in one replayed league, through the replay's own
  reconstruction of what the board knew. Nothing is priced, so it is quick."
  [league-id]
  (let [out (atom [])]
    (with-redefs [replay/who-bids-rows (fn [board ctx week-bids w]
                                         (swap! out conj (week-choices board ctx week-bids w))
                                         [])
                  replay/price-for     (fn [& _] {})]
      (replay/replay league-id {:league-prior? true}))
    (mapv #(assoc % :league league-id) @out)))


(defn feature-keys
  "Every feature any choice set carries, in a fixed order."
  [data]
  (vec (sort (into #{} (mapcat (fn [{:keys [features rivals]}]
                                 (concat (mapcat keys features)
                                         (mapcat keys (:needs (first rivals))))))
                       data))))

(defn choice-sets
  "Every rival-week with a choice as `{:x double[][] :chosen [idx]}`, its rows
  being each free agent's features in `ks` order, restricted to `ks`."
  [data ks]
  (let [ix (zipmap ks (range))
        k  (count ks)]
    (into []
          (mapcat (fn [{:keys [features rivals]}]
                    (keep (fn [{:keys [needs chosen]}]
                            (when (seq chosen)
                              {:chosen chosen
                               :x      (into-array
                                        (map (fn [f n]
                                               (let [row (double-array k)]
                                                 (doseq [[fk v] (concat f n)]
                                                   (when-let [i (ix fk)] (aset row (long i) (double v))))
                                                 row))
                                             features needs))}))
                          rivals)))
          data)))

(defn set-stats
  "One choice set's log likelihood, gradient and Hessian contribution at
  `theta`: `[ll grad hess]`."
  [^doubles theta {:keys [x chosen]}]
  (let [k     (alength theta)
        n     (alength ^objects x)
        u     (double-array n)
        _     (dotimes [j n]
                (let [^doubles row (aget ^objects x j)]
                  (aset u j (double (loop [i 0 acc 0.0]
                                      (if (< i k)
                                        (recur (inc i) (+ acc (* (aget theta i) (aget row i))))
                                        acc))))))
        top   (reduce max (seq u))
        z     (loop [j 0 acc 0.0] (if (< j n) (recur (inc j) (+ acc (Math/exp (- (aget u j) top)))) acc))
        lse   (+ top (Math/log z))
        m     (double (count chosen))
        mean  (double-array k)
        outer (make-array Double/TYPE k k)]
    (dotimes [j n]
      (let [s (Math/exp (- (aget u j) lse))
            ^doubles row (aget ^objects x j)]
        (dotimes [a k]
          (let [sa (* s (aget row a))]
            (aset mean a (+ (aget mean a) sa))
            (let [^doubles oa (aget ^objects outer a)]
              (dotimes [b k]
                (aset oa b (+ (aget oa b) (* sa (aget row b))))))))))
    (let [grad (double-array k)
          hess (make-array Double/TYPE k k)]
      (doseq [c chosen]
        (let [^doubles row (aget ^objects x c)]
          (dotimes [a k] (aset grad a (+ (aget grad a) (aget row a))))))
      (dotimes [a k]
        (aset grad a (- (aget grad a) (* m (aget mean a))))
        (let [^doubles ha (aget ^objects hess a) ^doubles oa (aget ^objects outer a)]
          (dotimes [b k]
            (aset ha b (- (* m (- (aget oa b) (* (aget mean a) (aget mean b)))))))))
      [(- (reduce + (map #(aget u (long %)) chosen)) (* m lse)) grad hess])))

(defn solve
  "x with `a` x = `b`, by Gaussian elimination with partial pivoting."
  [a b]
  (let [n (count b)
        m (mapv (fn [row bi] (conj (vec row) bi)) a b)
        m (reduce (fn [m col]
                    (let [p  (apply max-key #(Math/abs (double (get-in m [% col]))) (range col n))
                          m  (assoc m col (m p) p (m col))
                          pv (get-in m [col col])]
                      (reduce (fn [m r]
                                (if (= r col)
                                  m
                                  (let [f (/ (get-in m [r col]) pv)]
                                    (assoc m r (mapv - (m r) (map #(* f %) (m col)))))))
                              m (range n))))
                  m (range n))]
    (mapv (fn [i] (/ (get-in m [i n]) (get-in m [i i]))) (range n))))

(defn totals
  "Log likelihood, gradient and Hessian over every choice set, ridge included."
  [theta sets]
  (let [k (count theta)
        th (double-array theta)
        [ll g h] (reduce (fn [[ll g h] s]
                           (let [[l gs hs] (set-stats th s)]
                             [(+ ll l) (mapv + g gs) (mapv (fn [r hr] (mapv + r hr)) h (map vec hs))]))
                         [0.0 (vec (repeat k 0.0)) (vec (repeat k (vec (repeat k 0.0))))]
                         sets)]
    [(- ll (* 0.5 ridge (reduce + (map #(* % %) theta))))
     (mapv - g (map #(* ridge %) theta))
     (vec (map-indexed (fn [i r] (update r i - ridge)) h))]))

(defn fit
  "The weights, keyed by feature, maximizing the likelihood of `sets`' choices;
  `:se` their standard errors off the Hessian. A Newton step that would lower
  the likelihood is halved until it does not, since from a cold start a full
  step can overshoot into a softmax that has put everything on one player."
  [ks sets]
  (loop [theta (vec (repeat (count ks) 0.0)) iter 0]
    (let [[ll g h] (totals theta sets)
          step     (solve h g)
          [theta' ll'] (loop [scale 1.0]
                         (let [t (mapv (fn [x d] (- x (* scale d))) theta step)
                               l (first (totals t sets))]
                           (if (or (>= l ll) (< scale 1e-4)) [t l] (recur (/ scale 2.0)))))]
      (if (or (< (- ll' ll) 1e-6) (= iter 50))
        (let [[_ _ h'] (totals theta' sets)
              cov      (map (fn [i] (solve h' (assoc (vec (repeat (count ks) 0.0)) i -1.0))) (range (count ks)))]
          {:weights (zipmap ks theta')
           :se      (zipmap ks (map-indexed (fn [i col] (Math/sqrt (max 0.0 (nth col i)))) cov))
           :ll      ll'
           :iterations iter})
        (recur theta' (inc iter))))))

(defn per-choice
  "Mean log likelihood a choice, under `weights`, beside the uniform guess's."
  [weights ks sets]
  (let [theta (double-array (map #(get weights % 0.0) ks))
        n     (reduce + (map (comp count :chosen) sets))]
    {:model   (/ (reduce + (map #(first (set-stats theta %)) sets)) n)
     :uniform (/ (reduce + (map #(* (count (:chosen %)) (- (Math/log (alength ^objects (:x %))))) sets)) n)
     :choices n}))


(defn lgamma
  "The log gamma function, by Lanczos' approximation."
  [x]
  (let [c [0.99999999999980993 676.5203681218851 -1259.1392167224028 771.32342877765313
           -176.61502916214059 12.507343278686905 -0.13857109526572012 9.9843695780195716e-6
           1.5056327351493116e-7]]
    (if (< x 0.5)
      (- (Math/log (/ Math/PI (Math/sin (* Math/PI x)))) (lgamma (- 1.0 x)))
      (let [x (dec x)
            a (reduce + (first c) (map-indexed (fn [i ci] (/ ci (+ x i 1.0))) (rest c)))
            t (+ x 7.5)]
        (+ (* 0.5 (Math/log (* 2.0 Math/PI))) (* (+ x 0.5) (Math/log t)) (- t) (Math/log a))))))

(defn player-weeks
  "Every free agent's week: `:rate`, every rival's claims on him together under
  `weights` and each rival's claims a week, and `:bidders`, how many bid."
  [data weights]
  (mapcat (fn [{:keys [features rivals]}]
            (let [shares (mapv (fn [{:keys [needs]}]
                                 (let [us  (mapv #(faab/utility weights (merge %1 %2)) features needs)
                                       top (reduce max us)
                                       ws  (mapv #(Math/exp (- % top)) us)
                                       t   (reduce + ws)]
                                   (mapv #(/ % t) ws)))
                               rivals)
                  counts (frequencies (mapcat :chosen rivals))]
              (map (fn [j]
                     {:rate    (reduce + (map (fn [r sh] (* (double (or (:per-week r) 0.0)) (nth sh j)))
                                              rivals shares))
                      :bidders (get counts j 0)})
                   (range (count features)))))
          data))

(defn nb-ll
  "The log likelihood of the bidder counts when a player-week's rate r is
  multiplied by a gamma of mean one and shape r / `spread`: a negative
  binomial whose variance is r (1 + `spread`)."
  [spread rows]
  (reduce + (map (fn [{:keys [rate bidders]}]
                   (let [r (max 1e-9 (double rate)) a (/ r spread) n (double bidders)]
                     (+ (lgamma (+ n a)) (- (lgamma a)) (- (lgamma (inc n)))
                        (* a (Math/log (/ a (+ a r)))) (* n (Math/log (/ r (+ a r)))))))
                 rows)))

(defn fit-spread
  "The `faab/cluster-spread` maximizing `nb-ll`, by golden-section search on
  its log."
  [rows]
  (let [g  (/ (- (Math/sqrt 5.0) 1.0) 2.0)
        f  #(nb-ll (Math/exp %) rows)]
    (loop [lo (Math/log 0.001) hi (Math/log 100.0) n 0]
      (if (= n 40)
        (Math/exp (/ (+ lo hi) 2.0))
        (let [a (- hi (* g (- hi lo)))
              b (+ lo (* g (- hi lo)))]
          (if (> (f a) (f b)) (recur lo b (inc n)) (recur a hi (inc n))))))))


(def families
  "Families of features to leave out one at a time, and what each is."
  {"form"     #{:played :last-game :season}
   "value"    #{:vorp :vorp-sq}
   "position" #{:pos/QB :pos/RB :pos/TE :pos/K :pos/DST}
   "dropped"  #{:dropped}
   "need"     #{:need?}})

(defn held-out
  "Each league's choices scored under weights fit to every other league's:
  `{league per-choice}`, with the weights each was scored under and the
  `faab/cluster-spread` fit, under them, to the other leagues' bidder counts."
  [data ks]
  (let [by-league (group-by :league data)]
    (into {}
          (pmap (fn [[league rows]]
                  (let [others (mapcat val (dissoc by-league league))
                        w      (:weights (fit ks (choice-sets others ks)))]
                    [league (assoc (per-choice w ks (choice-sets rows ks))
                                   :weights w
                                   :spread (fit-spread (player-weeks others w)))]))
                by-league))))

(defn pooled [scores]
  (let [n (reduce + (map :choices (vals scores)))]
    {:model   (/ (reduce + (map #(* (:choices %) (:model %)) (vals scores))) n)
     :uniform (/ (reduce + (map #(* (:choices %) (:uniform %)) (vals scores))) n)
     :choices n}))

(defn -main [& args]
  (let [data  (vec (mapcat league-choices sweep/leagues))
        ks    (feature-keys data)
        all   (fit ks (choice-sets data ks))
        cv    (held-out data ks)
        p     (pooled cv)]
    (println (format "\n%d league-weeks, %d choices" (count data) (:choices p)))
    (println "\nWeights, fit to every league (± standard error):")
    (doseq [k ks]
      (println (format "  %-12s %+7.3f  ± %.3f" (str k) (get-in all [:weights k]) (get-in all [:se k]))))
    (println (format "\nHeld out, a league at a time: %.4f a choice, against %.4f for the uniform guess (%+.4f)"
                     (:model p) (:uniform p) (- (:model p) (:uniform p))))
    (println "\nLeaving a family out, held out (a lower likelihood means it earns its place):")
    (doseq [[family drop] families]
      (let [ks' (vec (remove drop ks))
            q   (pooled (held-out data ks'))]
        (println (format "  without %-9s %.4f a choice  (%+.4f against the full model)"
                         family (:model q) (- (:model q) (:model p))))))
    (let [rows   (player-weeks data (:weights all))
          spread (fit-spread rows)]
      (println (format "\nBids pile up: cluster-spread %.3f, log likelihood of the bidder counts %.1f against %.1f with rivals independent"
                       spread (nb-ll spread rows) (nb-ll 1e-6 rows)))
      (println "Held-out spreads:" (sort (map #(format "%.3f" (:spread %)) (vals cv)))))
    (println "\nclaim-weights:" (into (sorted-map) (update-vals (:weights all) #(/ (Math/round (* 1000.0 %)) 1000.0))))
    (when-let [baseline (second (drop-while #(not= "--replay" %) args))]
      (let [excluded (distinct (mapcat (fn [id]
                                         (let [prev (some-> (replay/league-docs id) :league :previous_league_id str)]
                                           (cond-> [id] prev (conj prev))))
                                       sweep/leagues))
            prior    (replay/prior-vars (sweep/backbone-without excluded))
            base     (pipeline/read-transit baseline)
            run      (with-redefs [waiver/rival-needs (sweep/cached-rival-needs waiver/rival-needs)]
                       (reduce (fn [acc league]
                                 (let [out (sweep/run-all prior [league]
                                                          {#'faab/claim-weights (get-in cv [league :weights])
                                                           #'faab/cluster-spread (get-in cv [league :spread])})]
                                   (-> acc (update :who merge (:who out)) (update :bids into (:bids out)))))
                               {:who {} :bids []} sweep/leagues))
            s        (sweep/summary run)
            bs       (sweep/summary base)
            cmp      (sweep/compare-runs base run)]
        (println "\nReplayed with held-out weights, against the shipped model's run:")
        (sweep/print-header)
        (sweep/print-row "shipped" true bs nil)
        (sweep/print-row "fitted" false s cmp)))
    (shutdown-agents)))
