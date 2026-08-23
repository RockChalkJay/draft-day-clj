(ns draft-day.replay.price-curve
  "What share of a room's money the Nth most expensive player takes, measured
  across the collected corpus of real auctions.

  The benchmark's draft simulator needs opponents that bid, and there is nothing
  to tell it what a player costs: Fantasy Football Calculator publishes ADP but
  no auction endpoint, ESPN and FantasyPros publish auction values only for the
  current season (so using them for 2015 is hindsight), and the vintage benchmark
  rows carry no dollars at all. A synthesized price curve is unavoidable, and the
  curve is then the single assumption the whole simulated market rests on.

  So it is measured rather than invented. A real auction cannot say what *this
  player* cost in *that season* — the corpus is 2021-2025 Sleeper leagues and the
  benchmark reaches 2011 — but it can say how auction money distributes by rank,
  and that shape is a property of the format rather than of a season. Rank is
  what transfers.

  Everything here is normalized twice over, because the corpus is deliberately
  heterogeneous (8 to 14 teams, $100 to $400 budgets, 120 to 280 picks):

    price -> share of the room's pool   (num-teams * budget)
    rank  -> fraction of picks made

  which makes a $400 fourteen-team room and a $100 ten-team room comparable."
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defn pool
  "Total money in the room."
  [{:keys [num-teams budget]}]
  (* (or num-teams 0) (double (or budget 0))))

(defn shares
  "One draft's prices as descending shares of its pool.

  Descending by price rather than by nomination order: the curve answers 'what
  does the most expensive player cost', which is what a bidder needs, and
  nomination order is close to arbitrary."
  [draft]
  (let [p (pool draft)]
    (if-not (pos? p)
      []
      (->> (:picks draft)
           (keep :price)
           (sort >)
           (mapv #(/ (double %) p))))))

(defn- cumulative
  "Running total of `xs`, length (inc (count xs)), starting at 0."
  [xs]
  (vec (reductions + 0.0 xs)))

(defn resample
  "Re-express `shares` — descending, as `shares` returns them — on a grid of `n`
  ranks, preserving the total exactly.

  Descending is a precondition, not a preference: the cumulative interpolation
  below will answer an unsorted vector confidently and meaninglessly.

  Only ever asked to *coarsen* in practice (real drafts run 120-280 picks onto a
  ~144 grid). Upsampling spreads one rank's money across ranks that did not
  exist — `[1.0]` onto two points gives `[0.5 0.5]`, right for a distribution,
  arguable for \"what the priciest player costs\" — so a shallow corpus on a fine
  grid would report a flatter, cheaper top of the board than any real room.

  Interpolating the shares directly would not: a 280-pick draft resampled onto
  144 points would report each point's share while silently dropping half the
  money. Interpolating the *cumulative* distribution and differencing adjacent
  points makes each output bucket the real money spent across the span of input
  ranks it covers, so the sum is conserved by construction."
  [shares n]
  (let [m   (count shares)
        cum (cumulative shares)]
    (if (or (zero? m) (not (pos? n)))
      (vec (repeat (max 0 n) 0.0))
      (let [at (fn [x]                       ; cumulative share at fractional rank x
                 (let [x (-> x (max 0.0) (min (double m)))
                       i (int (Math/floor x))]
                   (if (>= i m)
                     (nth cum m)
                     (+ (nth cum i) (* (- x i) (nth shares i))))))]
        (mapv (fn [j]
                (- (at (/ (* (inc j) (double m)) n))
                   (at (/ (* j (double m)) n))))
              (range n))))))

(defn curve
  "Mean share-of-pool by rank across `drafts`, on a grid of `n` ranks.

  Returns `{:shares [...] :n-drafts k :spend-share s}`. `:spend-share` is the
  mean fraction of the pool these rooms actually spent, and is deliberately not
  normalized away — real rooms finish with money on the table, and a simulated
  field that spends every dollar would clear prices no real auction clears."
  [drafts n]
  (let [ss (->> drafts (map shares) (remove empty?) (mapv #(resample % n)))
        k  (count ss)]
    (if (zero? k)
      {:shares (vec (repeat n 0.0)) :n-drafts 0 :spend-share 0.0}
      {:shares (mapv (fn [j] (/ (reduce + 0.0 (map #(nth % j) ss)) k)) (range n))
       :n-drafts k
       :spend-share (/ (reduce + 0.0 (map #(reduce + 0.0 %) ss)) k)})))

(defn spread
  "How much the price at each rank actually scatters, as a coefficient of
  variation across `drafts`, on a grid of `n` ranks.

  The mean curve alone describes a room where every manager agrees to the
  dollar, and a simulated field built from it has no dispersion at all: the
  marginal bidder sits at exactly the mean, so a seat bidding a dollar more wins
  every contest it enters and a seat bidding a dollar less wins none. That is a
  step function where real auctions have a slope, and it makes a valuation that
  is *close* to the market score worse than one that is wildly wrong.

  Real rooms disagree by about 15% at the top of the board and 40% in the tail
  (45 standard drafts: $73.70 +/- $12.20 at rank 1, $3.60 +/- $1.40 at rank
  100). The scatter is what turns a small edge in valuation into a small edge in
  outcome instead of an all-or-nothing one."
  [drafts n]
  (let [ss (->> drafts (map shares) (remove empty?) (mapv #(resample % n)))
        k  (count ss)]
    (if (zero? k)
      (vec (repeat n 0.0))
      (mapv (fn [j]
              (let [xs (map #(nth % j) ss)
                    m  (/ (reduce + 0.0 xs) k)
                    v  (/ (reduce + 0.0 (map #(let [d (- % m)] (* d d)) xs)) k)]
                (if (pos? m) (/ (Math/sqrt v) m) 0.0)))
            (range n)))))

(defn clearing-prices
  "The curve as whole dollars for a room holding `pool`.

  **Indexed in rank-fraction space, not by absolute rank.** Slot `j` of the
  result is the player at rank fraction `(j+0.5)/n`, so it means \"absolute rank
  j\" only when the curve was built with `n` equal to the caller's own number of
  picks. Build a 12-bucket curve, index it by ADP rank across a 144-pick draft,
  and every price is drawn from the wrong twelfth of the distribution — with no
  error and an entirely plausible-looking result. `for-picks` exists so a caller
  does not have to remember this.

  Floored at $1: every rank in the grid is a roster slot somebody fills, and a
  simulated field that bids $0 would hand the model seat free players — the same
  reasoning that put a minimum bid in `rankings.value`."
  [{:keys [shares]} pool]
  (mapv #(max 1 (Math/round (* (double %) (double pool)))) shares))

;; ---- corpus ----------------------------------------------------------------

(def ^:private cache-dir "data/replay_cache")

(defn load-drafts
  "Every normalized draft cached by the replay harness.

  Reads the v2 cache only. A v1 file carries no league type, which would read as
  `:superflex? false` and quietly file superflex rooms among the standard ones —
  the trap that put the version token on the path in the first place."
  []
  (->> (file-seq (io/file cache-dir))
       (map #(.getName %))
       (filter #(re-matches #"draft-v2-\d+\.edn" %))
       (keep (fn [f] (try (edn/read-string (slurp (str cache-dir "/" f)))
                          (catch Exception _ nil))))
       (filter #(seq (:picks %)))
       vec))

(defn for-picks
  "The curve a room making `n-picks` should price against, from `drafts`.

  The pairing `clearing-prices` needs, in one call: builds the grid at the
  caller's own pick count so rank fraction and absolute rank coincide.

  Throws on an empty corpus rather than returning a curve of zeros. That is the
  one failure mode worth being loud about — every rank would floor at $1, the
  simulated field would bid nothing, and the model seat would win an entire draft
  for pocket change, reported as a spectacular edge rather than as a missing
  cache."
  [drafts n-picks]
  (let [c (curve drafts n-picks)]
    (when (zero? (:n-drafts c))
      (throw (ex-info "no priced drafts to build a price curve from"
                      {:n-picks n-picks :cache-dir cache-dir})))
    c))

(defn standard-drafts
  "The 1-QB drafts. The benchmark simulates a single-quarterback lineup, and a
  superflex room spends a visibly different share of its pool on quarterbacks —
  so the curve the simulator uses should come from rooms shaped like the one it
  is simulating, even though the corpus holds both."
  [drafts]
  (remove :superflex? drafts))
