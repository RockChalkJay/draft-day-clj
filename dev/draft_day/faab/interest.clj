(ns draft-day.faab.interest
  "Measure how a rival aims his claims: fit `faab/claim-weights`, the conditional
  logit a rival's claims are shared out by, and `faab/cluster-spread`, how far
  claims pile onto one player, to the claims real managers made.

  Each week of each replayed league (`draft-day.faab.leagues`), at the week's
  first waiver run, every rival faces the free agents the board would have
  shown, each described by `faab/claim-features` and, for that rival alone,
  `faab/need-features`. The players he bid on that week — at any run — are his
  choices, and the weights are the ones that make those choices likeliest: a
  rival's claims land on a player in proportion to e^utility. How *many* claims
  he makes is his `bid-history` profile's business, not this fit's; a rival who
  bid on nobody that week says nothing about where claims land and is left out.

  The fit is Newton's method on the log likelihood, which is concave, with a
  small ridge so a feature nobody varies cannot run off. It is fit on the
  frozen set's fit half and scored on its score half, which it never sees: per
  choice, against the uniform guess, by stratum, and beside fits with a family
  of features left out, which is what says each family earns its place.
  `--curve` refits on nested samples of the fit half, so the score says how
  much another doubling of leagues is worth; `--by-kind` fits redraft, keeper
  and dynasty apart, to say whether one set of weights serves them all.

  How far bids pile onto one player beyond what the features see is
  `faab/cluster-spread`: every free agent's week multiplies every rival's rate
  on him by one gamma draw of mean one, its shape his total rate over the
  spread, and the spread is the one that makes the weeks' bidder counts
  likeliest — a negative binomial on each, whose variance is its mean times
  one plus the spread. A shape that did not grow with the rate was measured
  first and fit worse: it made a claim on a player nobody expected bring a
  quarter of the company a claim on the week's obvious target does, where
  real claims bring about the same company either way.

  A league's choices are extracted once, through the replay, and kept under
  `choices-dir` in a compact binary form: the shared features once a week as
  floats, and each rival's own feature as a byte per free agent. Changing
  `faab/claim-features` means bumping `choices-version`.

  Each half is extracted under a backbone refit without it
  (`sweep/half-prior`). `--sample N` takes the first N leagues of each half.
  `--replay` then runs the score half through `draft-day.faab.sweep` with the
  shipped constants and with the refit ones, and scores them paired.

    JVM_OPTS=-Xmx10g lein run -m draft-day.faab.interest
    JVM_OPTS=-Xmx10g lein run -m draft-day.faab.interest -- --curve --by-kind --replay"
  (:require [clojure.java.io :as io]
            [draft-day.faab.replay :as replay]
            [draft-day.faab.sweep :as sweep]
            [draft-day.rankings.faab :as faab]
            [draft-day.rankings.waiver :as waiver])
  (:import [java.io DataInputStream DataOutputStream]
           [java.util.zip GZIPInputStream GZIPOutputStream]))

(def ridge
  "The penalty on the squared weights: small against a log likelihood summed
  over thousands of choices, there only to keep the Hessian invertible."
  1.0)

(def common-keys
  "`faab/claim-features`' features, in the order a row holds them."
  [:played :last-game :season :vorp :vorp-sq :dropped :pos/QB :pos/RB :pos/TE :pos/K :pos/DST])

(def need-key
  "`faab/need-features`' one feature, the last column."
  :need?)

(def all-keys (conj common-keys need-key))

(def choices-version 1)

(def choices-dir (str "data/faab_cache/choices/v" choices-version))

(defn compact-week
  "A league-week's choices as the fit reads them: `:x`, a float row of `common`
  features per free agent, and per rival `:extra`, his `need` feature as a
  byte per free agent, his `:chosen` indices and his `:per-week` claims."
  [{:keys [features rivals week]} common need]
  {:week   week
   :x      (into-array (map (fn [f] (float-array (map #(float (get f % 0.0)) common))) features))
   :rivals (mapv (fn [{:keys [needs chosen per-week]}]
                   {:extra    (byte-array (map #(byte (if (pos? (double (get % need 0.0))) 1 0)) needs))
                    :chosen   (vec chosen)
                    :per-week (double (or per-week 0.0))})
                 rivals)})

(defn week-choices
  "One league-week at its first run, compact: every free agent's shared
  features, and per rival his claims a week, whether he would start each, and
  the free agents he bid on that week."
  [board ctx week-bids week]
  (let [{:keys [fas xwalk by-id seats habits]} (waiver/market-inputs board (dissoc ctx :my-roster-id))
        needs (waiver/rival-needs (get-in ctx [:league :teams]) nil xwalk by-id seats
                                  (:starting-slots ctx) fas)
        index (zipmap (map :player-id fas) (range))]
    (compact-week
     {:week     week
      :features (mapv #(faab/claim-features % week) fas)
      :rivals   (mapv (fn [{:keys [roster-id] :as r}]
                        {:per-week (:per-week (faab/rival-habits r habits))
                         :needs    (mapv #(faab/need-features (get (:needs r) (:player-id %))) fas)
                         :chosen   (vec (keep (fn [[rid pid]] (when (= rid roster-id) (index pid)))
                                              week-bids))})
                      needs)}
     common-keys need-key)))

(defn write-choices!
  "`weeks` to `path`, gzipped: counts, then floats, bytes and ints."
  [path weeks]
  (io/make-parents path)
  (with-open [out (DataOutputStream. (GZIPOutputStream. (io/output-stream path)))]
    (.writeInt out (count weeks))
    (doseq [{:keys [week x rivals]} weeks]
      (let [rows ^objects x
            n    (alength rows)
            k    (if (pos? n) (alength ^floats (aget rows 0)) (count common-keys))]
        (.writeInt out (int week))
        (.writeInt out n)
        (.writeInt out k)
        (dotimes [j n]
          (let [^floats row (aget rows j)]
            (dotimes [i k] (.writeFloat out (aget row i)))))
        (.writeInt out (count rivals))
        (doseq [{:keys [extra chosen per-week]} rivals]
          (.writeDouble out per-week)
          (.write out ^bytes extra)
          (.writeInt out (count chosen))
          (doseq [c chosen] (.writeInt out (int c))))))))

(defn read-choices
  "What `write-choices!` wrote."
  [path]
  (with-open [in (DataInputStream. (GZIPInputStream. (io/input-stream path)))]
    (vec (repeatedly (.readInt in)
                     (fn []
                       (let [week (.readInt in)
                             n    (.readInt in)
                             k    (.readInt in)
                             x    (into-array (repeatedly n #(let [row (float-array k)]
                                                               (dotimes [i k] (aset row i (.readFloat in)))
                                                               row)))]
                         {:week   week
                          :x      x
                          :rivals (vec (repeatedly (.readInt in)
                                                   (fn []
                                                     (let [per-week (.readDouble in)
                                                           extra    (byte-array n)]
                                                       (.readFully in extra)
                                                       {:per-week per-week
                                                        :extra    extra
                                                        :chosen   (vec (repeatedly (.readInt in) #(.readInt in)))}))))}))))))

(def ^:dynamic *sink*
  "Where this thread's replay puts the weeks it extracts. Dynamic because the
  replay's hooks are installed with `with-redefs`, which every thread shares,
  while leagues are extracted several at once."
  nil)

(defn capture
  "The replay hooks `league-choices` extracts through: each week's first run
  into `*sink*`, and nothing priced."
  []
  {#'replay/who-bids-rows (fn [board ctx week-bids w]
                            (swap! *sink* conj (week-choices board ctx week-bids w))
                            [])
   #'replay/price-for     (fn [& _] {})})

(defn league-choices
  "Every week's choices in one replayed league, read from `choices-dir` or
  extracted through the replay's own reconstruction of what the board knew
  and written there. Extracting needs `capture` installed."
  [league-id]
  (let [path (str choices-dir "/" league-id ".bin")]
    (if (.exists (io/file path))
      (read-choices path)
      (binding [*sink* (atom [])]
        (replay/replay league-id {:league-prior? true})
        (write-choices! path @*sink*)
        @*sink*))))

(defn load-leagues
  "`{league-id weeks}` for `rows`, extracted in parallel under the backbone
  `prior`; a league the replay cannot read is logged and left out."
  [prior rows]
  (with-redefs-fn (merge prior (capture))
    #(into {}
           (keep identity)
           (pmap (fn [{:keys [league-id]}]
                   (try [league-id (league-choices league-id)]
                        (catch Exception e
                          (binding [*out* *err*] (println "  skipped" league-id (ex-message e)))
                          nil)))
                 rows))))

(defn choice-sets
  "Every rival-week in `weeks` that chose somebody, as `{:x :extra :chosen}`:
  the week's shared rows, his own column and his choices."
  [weeks]
  (into [] (mapcat (fn [{:keys [x rivals]}]
                     (keep (fn [{:keys [extra chosen]}]
                             (when (seq chosen) {:x x :extra extra :chosen chosen}))
                           rivals)))
        weeks))

(defn row-into!
  "Free agent `j`'s features into `out`: his shared floats, then his `extra`
  byte when there is one."
  [^doubles out ^objects rows ^bytes extra ^long j]
  (let [^floats row (aget rows j)
        kx          (alength row)]
    (dotimes [i kx] (aset out i (double (aget row i))))
    (when extra (aset out kx (double (aget extra j))))
    out))

(defn set-stats
  "One choice set's log likelihood, gradient and Hessian contribution at
  `theta`: `[ll grad hess]`. A row is the free agent's shared floats, then
  his `:extra` byte when the set has one, which `theta`'s last weight reads.
  The Hessian is symmetric, so only its upper half is summed."
  [^doubles theta {:keys [x extra chosen]}]
  (let [rows  ^objects x
        ex    ^bytes extra
        k     (alength theta)
        n     (alength rows)
        xj    (double-array k)
        u     (double-array n)
        _     (dotimes [j n]
                (row-into! xj rows ex j)
                (aset u j (double (loop [i 0 acc 0.0]
                                    (if (< i k) (recur (inc i) (+ acc (* (aget theta i) (aget xj i)))) acc)))))
        top   (areduce u j m Double/NEGATIVE_INFINITY (max m (aget u j)))
        z     (areduce u j acc 0.0 (+ acc (Math/exp (- (aget u j) top))))
        lse   (+ top (Math/log z))
        m     (double (count chosen))
        mean  (double-array k)
        outer (double-array (* k k))]
    (dotimes [j n]
      (let [s (Math/exp (- (aget u j) lse))]
        (row-into! xj rows ex j)
        (dotimes [a k]
          (let [sa (* s (aget xj a))
                ak (* a k)]
            (aset mean a (+ (aget mean a) sa))
            (loop [b a]
              (when (< b k)
                (aset outer (+ ak b) (+ (aget outer (+ ak b)) (* sa (aget xj b))))
                (recur (inc b))))))))
    (let [grad (double-array k)
          hess (make-array Double/TYPE k k)]
      (doseq [c chosen]
        (row-into! xj rows ex c)
        (dotimes [a k] (aset grad a (+ (aget grad a) (aget xj a)))))
      (dotimes [a k]
        (aset grad a (- (aget grad a) (* m (aget mean a))))
        (dotimes [b k]
          (let [o (if (<= a b) (aget outer (+ (* a k) b)) (aget outer (+ (* b k) a)))]
            (aset ^doubles (aget ^objects hess a) b
                  (- (* m (- o (* (aget mean a) (aget mean b)))))))))
      [(- (reduce + (map #(aget u (long %)) chosen)) (* m lse)) grad hess])))

(defn set-ll
  "One choice set's log likelihood alone, for a line search."
  [^doubles theta {:keys [x extra chosen]}]
  (let [rows ^objects x
        ex   ^bytes extra
        k    (alength theta)
        kx   (if ex (dec k) k)
        u    (double-array (alength rows))]
    (dotimes [j (alength rows)]
      (let [^floats row (aget rows j)]
        (aset u j (+ (double (loop [i 0 acc 0.0]
                               (if (< i kx) (recur (inc i) (+ acc (* (aget theta i) (aget row i)))) acc)))
                     (if ex (* (aget theta kx) (aget ex j)) 0.0)))))
    (let [top (areduce u j m Double/NEGATIVE_INFINITY (max m (aget u j)))
          lse (+ top (Math/log (areduce u j acc 0.0 (+ acc (Math/exp (- (aget u j) top))))))]
      (- (reduce + (map #(aget u (long %)) chosen)) (* (count chosen) lse)))))

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

(def parts
  "How many pieces a pass over the choice sets is split into for the cores."
  32)

(defn totals
  "Log likelihood, gradient and Hessian over every choice set, ridge included,
  summed in parallel into primitive arrays."
  [theta sets]
  (let [k    (count theta)
        th   (double-array theta)
        size (max 1 (long (Math/ceil (/ (count sets) (double parts)))))
        sums (pmap (fn [piece]
                     (let [g (double-array k) h (make-array Double/TYPE k k)]
                       (loop [ll 0.0 [s & more] piece]
                         (if s
                           (let [[l gs hs] (set-stats th s)]
                             (dotimes [a k]
                               (aset g a (+ (aget g a) (aget ^doubles gs a)))
                               (let [^doubles ha (aget ^objects h a) ^doubles sa (aget ^objects hs a)]
                                 (dotimes [b k] (aset ha b (+ (aget ha b) (aget sa b))))))
                             (recur (+ ll l) more))
                           [ll g h]))))
                   (partition-all size sets))
        ll   (reduce + (map first sums))
        g    (apply mapv + (map (comp vec second) sums))
        h    (apply mapv (fn [& rows] (apply mapv + rows)) (map (fn [[_ _ h]] (mapv vec h)) sums))]
    [(- ll (* 0.5 ridge (reduce + (map #(* % %) theta))))
     (mapv - g (map #(* ridge %) theta))
     (vec (map-indexed (fn [i r] (update r i - ridge)) h))]))

(defn total-ll
  "`totals`' log likelihood alone."
  [theta sets]
  (let [th (double-array theta)]
    (- (reduce + (pmap (fn [piece] (reduce + (map #(set-ll th %) piece)))
                       (partition-all (max 1 (long (Math/ceil (/ (count sets) (double parts))))) sets)))
       (* 0.5 ridge (reduce + (map #(* % %) theta))))))

(defn restrict [v idx] (mapv #(nth v %) idx))

(defn fit
  "The weights, keyed by `ks`, maximizing the likelihood of `sets`' choices,
  with `:se` their standard errors off the Hessian. Only the columns named in
  `active` (every one by default) are fit; the rest stay at zero, which is how
  a family is left out without rebuilding the rows. A Newton step that would
  lower the likelihood is halved until it does not, since from a cold start a
  full step can overshoot; when no halving helps, the weights found are the
  answer."
  ([ks sets] (fit ks sets (set ks)))
  ([ks sets active]
   (let [idx (vec (keep-indexed (fn [i k] (when (active k) i)) ks))
         put (fn [theta sub] (reduce (fn [t [i v]] (assoc t i v)) theta (map vector idx sub)))]
     (loop [theta (vec (repeat (count ks) 0.0)) iter 0]
       (let [[ll g h] (totals theta sets)
             step     (solve (mapv #(restrict (nth h %) idx) idx) (restrict g idx))
             [theta' ll'] (loop [scale 1.0]
                            (let [t (put theta (mapv (fn [i d] (- (nth theta i) (* scale d))) idx step))
                                  l (total-ll t sets)]
                              (cond (>= l ll)      [t l]
                                    (< scale 1e-4) [theta ll]
                                    :else          (recur (/ scale 2.0)))))]
         (if (or (< (- ll' ll) 1e-6) (= iter 50))
           (let [[_ _ h'] (totals theta' sets)
                 hs       (mapv #(restrict (nth h' %) idx) idx)
                 se       (map-indexed (fn [j _] (Math/sqrt (max 0.0 (nth (solve hs (assoc (vec (repeat (count idx) 0.0)) j -1.0)) j))))
                                       idx)]
             {:weights    (zipmap ks theta')
              :se         (zipmap (map ks idx) se)
              :ll         ll'
              :iterations iter})
           (recur theta' (inc iter))))))))

(defn per-choice
  "Mean log likelihood a choice, under `weights`, beside the uniform guess's;
  zeros for sets with no choice in them."
  [weights ks sets]
  (let [theta (double-array (map #(get weights % 0.0) ks))
        n     (max 1 (reduce + (map (comp count :chosen) sets)))]
    {:model   (/ (reduce + (pmap (fn [piece] (reduce + (map #(set-ll theta %) piece)))
                                 (partition-all (max 1 (long (Math/ceil (/ (count sets) (double parts))))) sets)))
                 n)
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
  [weeks weights ks]
  (let [theta (double-array (map #(get weights % 0.0) ks))
        kx    (dec (count ks))]
    (mapcat (fn [{:keys [x rivals]}]
              (let [rows   ^objects x
                    n      (alength rows)
                    base   (double-array n)
                    _      (dotimes [j n]
                             (let [^floats row (aget rows j)]
                               (aset base j (double (loop [i 0 acc 0.0]
                                                      (if (< i kx) (recur (inc i) (+ acc (* (aget theta i) (aget row i)))) acc))))))
                    rate   (double-array n)
                    counts (frequencies (mapcat :chosen rivals))]
                (doseq [{:keys [extra per-week]} rivals]
                  (let [us  (double-array n)
                        _   (dotimes [j n] (aset us j (+ (aget base j) (* (aget theta kx) (aget ^bytes extra j)))))
                        top (areduce us j m Double/NEGATIVE_INFINITY (max m (aget us j)))
                        z   (areduce us j acc 0.0 (+ acc (Math/exp (- (aget us j) top))))]
                    (dotimes [j n]
                      (aset rate j (+ (aget rate j) (* per-week (/ (Math/exp (- (aget us j) top)) z)))))))
                (map (fn [j] {:rate (aget rate j) :bidders (get counts j 0)}) (range n))))
            weeks)))

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

(def curve-sizes [25 50 100 200 400 800])

(defn weeks-of [data rows] (mapcat #(get data (:league-id %)) rows))

(defn sets-of [data rows] (choice-sets (weeks-of data rows)))

(defn stratum-label [{:keys [season kind budget superflex?]}]
  (format "%s %-8s %-6s %s" season (name kind) (name budget) (if superflex? "superflex" "1QB")))

(defn -main [& args]
  (let [flags  (set args)
        n      (some-> (sweep/flag-value args "--sample") parse-long)
        fit-r  (sweep/replayable :fit n)
        score-r (sweep/replayable :score n)
        _      (println (format "Extracting choices: %d fit and %d score leagues" (count fit-r) (count score-r)))
        data   (merge (load-leagues (sweep/half-prior :fit) fit-r)
                      (load-leagues (sweep/half-prior :score) score-r))
        fit-r  (filterv #(data (:league-id %)) fit-r)
        score-r (filterv #(data (:league-id %)) score-r)
        fit-s  (sets-of data fit-r)
        score-s (sets-of data score-r)
        all    (fit all-keys fit-s)
        w      (:weights all)
        p      (per-choice w all-keys score-s)
        spread (fit-spread (player-weeks (weeks-of data fit-r) w all-keys))]
    (println (format "\nFit on %d leagues (%d choices), scored on %d leagues (%d choices)"
                     (count fit-r) (reduce + (map (comp count :chosen) fit-s)) (count score-r) (:choices p)))
    (println "\nWeights (± standard error):")
    (doseq [k all-keys]
      (println (format "  %-12s %+7.3f  ± %.3f" (str k) (get w k) (get-in all [:se k]))))
    (println (format "\nScore half: %.4f a choice, against %.4f for the uniform guess (%+.4f)"
                     (:model p) (:uniform p) (- (:model p) (:uniform p))))
    (println (format "Shipped weights on the score half: %.4f a choice" (:model (per-choice faab/claim-weights all-keys score-s))))
    (println "\nBy stratum, on the score half:")
    (doseq [[s rows] (sort-by (comp stratum-label key) (group-by :stratum score-r))]
      (let [q (per-choice w all-keys (sets-of data rows))]
        (println (format "  %-34s %4d leagues %7d choices  %.4f  (uniform %.4f, %+.4f)"
                         (stratum-label s) (count rows) (:choices q) (:model q) (:uniform q) (- (:model q) (:uniform q))))))
    (println "\nLeaving a family out, on the score half (a lower likelihood means it earns its place):")
    (doseq [[family drop] families]
      (let [q (per-choice (:weights (fit all-keys fit-s (set (remove drop all-keys)))) all-keys score-s)]
        (println (format "  without %-9s %.4f a choice  (%+.4f against the full model)" family (:model q) (- (:model q) (:model p))))))
    (println (format "\nBids pile up: cluster-spread %.3f (shipped %.3f)" spread faab/cluster-spread))
    (when (flags "--curve")
      (println "\nLearning curve: fit on the first n fit-half leagues, scored on the whole score half")
      (doseq [n (concat (filter #(< % (count fit-r)) curve-sizes) [(count fit-r)])]
        (let [rows (take n fit-r)
              f    (fit all-keys (sets-of data rows))
              q    (per-choice (:weights f) all-keys score-s)]
          (println (format "  %5d leagues  %.4f a choice  spread %.3f" n (:model q)
                           (fit-spread (player-weeks (weeks-of data rows) (:weights f) all-keys)))))))
    (when (flags "--by-kind")
      (println "\nBy kind: each kind's score half under its own fit and under the pooled one")
      (doseq [[kind rows] (group-by (comp :kind :stratum) fit-r)]
        (let [own    (:weights (fit all-keys (sets-of data rows)))
              scored (sets-of data (filter #(= kind (get-in % [:stratum :kind])) score-r))]
          (when (seq scored)
            (println (format "  %-8s own %.4f  pooled %.4f a choice" (name kind)
                             (:model (per-choice own all-keys scored)) (:model (per-choice w all-keys scored))))))))
    (println "\nclaim-weights:" (into (sorted-map) (update-vals w #(/ (Math/round (* 1000.0 %)) 1000.0))))
    (when (flags "--replay")
      (let [configs [["shipped" {}]
                     ["refit" {#'faab/claim-weights w #'faab/cluster-spread spread}]]
            out     (sweep/run-configs (sweep/half-prior :score) score-r configs (sweep/run-dir score-r configs))
            base    (get out "shipped")]
        (println "\nReplayed on the score half, the refit against the shipped constants:")
        (sweep/print-header)
        (sweep/print-row "shipped" true (sweep/summary base) nil)
        (sweep/print-row "refit" false (sweep/summary (get out "refit")) (sweep/compare-runs base (get out "refit")))))
    (shutdown-agents)))
