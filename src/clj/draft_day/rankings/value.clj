(ns draft-day.rankings.value
  "VBD -> dollars. Value is the stable salary-cap price; Price (:worth) is Value
  scaled by live inflation.

  Dollar rounding uses Math/rint (round half to even) to match the numpy/pandas
  behavior the reference tests pin, e.g. 1 + (40-1)*0.5 = 20.5 -> 20."
  (:require [draft-day.db :as db]))

(def priced-positions
  "The positions a dollar can attach to — `db/priced-positions`, not a fifth copy
  of the same four strings. `src/cljc` is on the backend classpath for exactly
  this."
  db/priced-positions)

(def MIN-BID
  "What one roster slot costs at the floor of an auction."
  1)

(defn- to-dollars ^long [x]
  (max 0 (long (Math/rint (double x)))))

(defn priced-vorp?
  "Does this player earn a share of the discretionary money? A priced position
  with VORP strictly above replacement. Everything else either takes the $1
  minimum (`min-bid-ids`) or nothing.

  Returns a real boolean. `(and (some-set x) ...)` yields nil rather than false
  on a miss, and this is a `group-by` key in `min-bid-ids` — nil and false are
  different groups, so K and DST used to land in a third bucket that the
  destructuring silently dropped."
  [p]
  (boolean
   (and (priced-positions (:position p))
        (> (double (or (:vorp p) 0.0)) 0.0))))

(defn- largest-remainder
  "Apportion `n` whole units across `weights` {k w} in proportion to w, giving the
  leftover to the largest fractional parts. Standard largest-remainder
  apportionment, so the parts always sum to exactly `n`."
  [n weights]
  (let [total (reduce + 0.0 (vals weights))]
    (if-not (pos? total)
      {}
      (let [exact  (into {} (map (fn [[k w]] [k (* n (/ (double w) total))])) weights)
            floors (into {} (map (fn [[k x]] [k (long (Math/floor x))])) exact)
            short  (- n (reduce + 0 (vals floors)))
            order  (->> exact
                        (sort-by (fn [[k x]] [(- (- x (Math/floor x))) (str k)]))
                        (map first))]
        (reduce (fn [m k] (update m k inc)) floors (take short order))))))

(defn- best-n
  "The `n` highest-scoring players at `pos`, by :points. Streamed positions carry
  no VORP to rank on, and inside one position :points is the same ordering VORP
  would give anyway."
  [board pos n]
  (->> board
       (filter #(= pos (:position %)))
       (sort-by #(- (double (or (:points %) 0.0))))
       (take n)
       (map :player-id)))

(defn min-bid-ids
  "Ids of every player who fills a roster slot without earning a share of the
  discretionary money, and so costs the $1 league minimum.

  Two groups end up here for different reasons.

  **Below-replacement skill players.** The reserve `budget - total-slots` sets a
  dollar aside for every slot, but it was only ever paid to players above
  replacement — at the 12-team default that is $1 x 180 reserved against 84
  players paid, so 96 roster slots carried no price at all and the board said
  'undraftable' about half a draft. Their minimums are apportioned **per
  position**, in proportion to how many of that position finished above
  replacement: below replacement each position's points curve has its own slope,
  so ranking the tail globally by VORP buys the flattest curves. It gave TE 27 of
  96 minimums against 12 TE starters, pricing a tight end at ADP 251 while a back
  at ADP 147 read as undraftable.

  **Kickers and defenses.** Exactly as many of each as the roster drafts, best
  first. They are streamed, so the engine declines to rank them on VORP — a real
  replacement level would put the top defense 55th overall against a room that
  pays $1 for it. But declining to rank them is not the same as calling them
  free: every team fills those seats and pays at least the minimum for them, and
  pricing them at $0 both understated the board by the room's whole K/DST spend
  and handed those dollars to skill players who will not spend them.

  `streamed-slots` is `{\"K\" n \"DST\" m}` from `league-state/streamed-slots`;
  omit it (or pass `{}`) and no streamed seat is priced."
  ([board total-slots] (min-bid-ids board total-slots {}))
  ([board total-slots streamed-slots]
   (let [{priced true tail false} (group-by priced-vorp? board)
         streamed (into #{} (mapcat (fn [[pos n]] (best-n board pos n))) streamed-slots)
         cand     (->> tail
                       (filter #(and (priced-positions (:position %)) (number? (:vorp %))))
                       (sort-by #(- (double (:vorp %)))))
         n        (- (long total-slots)
                     (reduce + 0 (vals streamed-slots))
                     (count priced))]
     (if-not (pos? n)
       streamed
       (let [demand (frequencies (map :position priced))
             quota  (largest-remainder n demand)
             by-pos (group-by :position cand)
             taken  (into #{}
                          (mapcat (fn [[pos q]] (map :player-id (take q (get by-pos pos)))))
                          quota)
             ;; a position whose tail ran dry leaves the count short; top it up in
             ;; VORP order so the board still prices exactly `n` skill slots
             fill   (->> cand
                         (remove #(contains? taken (:player-id %)))
                         (map :player-id)
                         (take (- n (count taken))))]
         (into streamed (into taken fill)))))))

(defn calculate-value
  "Stable salary-cap Value (VBD -> dollars): reserve $1 per rostered slot, then
  spread the discretionary money (budget - total-slots) across positive-VORP
  priced players by VORP share. Everyone else who still fills a roster slot —
  the below-replacement tail, and the league's kickers and defenses — prices at
  the $1 minimum (`min-bid-ids`); everyone past the last roster slot -> $0.
  Assocs :value on each player.

  Sums to the budget to **within per-player rounding**: `to-dollars` rounds each
  priced player independently, so the total drifts up to half a dollar each way
  per player (on the sample board, $2398 of $2400). It does *not* sum to
  `priced + discretionary`, which is what it summed to before the minimum-bid
  tail was priced.

  `streamed-slots` (4-arity) is `{\"K\" n \"DST\" m}`; `engine/live-valuation`
  passes the league's real counts."
  ([board budget total-slots] (calculate-value board budget total-slots {}))
  ([board budget total-slots streamed-slots]
   (let [disc     (max 0.0 (- (double budget) total-slots))
         streamed-total (reduce + 0 (vals streamed-slots))
         ;; A row cannot be priced without a seat to fill or a dollar to fill it
         ;; with. `:teams` and `:replacement-config` come from different snapshots
         ;; — `:apply-config` deliberately keeps the old `:teams` once picks exist
         ;; — so editing roster or team count mid-draft can put more players above
         ;; replacement than there are seats. Without this the board over-sums the
         ;; room and rows past the last slot read a real price. `routes` rejects a
         ;; room too poor to pay a dollar a slot, so the budget term is a floor
         ;; under the pure function rather than a state the app reaches.
         payable  (min (- (long total-slots) streamed-total)
                       (max 0 (- (long budget) streamed-total)))
         ranked   (->> board (filter priced-vorp?) (sort-by #(- (double (:vorp %)))))
         priced   (set (map :player-id (take payable ranked)))
         total-vorp (reduce + 0.0 (map #(if (contains? priced (:player-id %))
                                          (double (:vorp %)) 0.0)
                                       board))
         min-bid  (min-bid-ids board (+ payable streamed-total) streamed-slots)]
     (mapv (fn [p]
             (assoc p :value
                    (cond
                      (and (contains? priced (:player-id p)) (> total-vorp 0.0))
                      (to-dollars (+ 1.0 (* (/ (double (:vorp p)) total-vorp) disc)))

                      (contains? min-bid (:player-id p)) MIN-BID
                      :else                              0)))
           board))))

(defn- inflation-for
  "`inflation` may be a scalar or a per-player fn (for per-position inflation)."
  [inflation player]
  (if (fn? inflation) (inflation player) inflation))

(defn calculate-price
  "Live Price (:worth) = 1 + (value - 1) * inflation. At inflation 1 Price equals
  Value; the $1 base keeps min-bid players at $1. Priced for any undrafted player
  the board valued at $1 or more — including the kickers and defenses holding a
  roster slot, whose $1 the base leaves untouched at any inflation. Drafted and
  worthless -> $0. `inflation` is a scalar or a fn of the player (per-position
  inflation)."
  [board inflation drafted-ids]
  (mapv (fn [p]
          (let [value  (double (or (:value p) 0))
                priced (and (not (contains? drafted-ids (:player-id p)))
                            (>= value 1.0))]
            (assoc p :worth
                   (if priced
                     (to-dollars (+ 1.0 (* (- value 1.0) (inflation-for inflation p))))
                     0))))
        board))
