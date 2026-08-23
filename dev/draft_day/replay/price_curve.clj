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
  "Re-express `shares` on a grid of `n` ranks, preserving the total exactly.

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

(defn clearing-prices
  "The curve as whole dollars for a room holding `pool`.

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

(defn standard-drafts
  "The 1-QB drafts. The benchmark simulates a single-quarterback lineup, and a
  superflex room spends a visibly different share of its pool on quarterbacks —
  so the curve the simulator uses should come from rooms shaped like the one it
  is simulating, even though the corpus holds both."
  [drafts]
  (remove :superflex? drafts))
