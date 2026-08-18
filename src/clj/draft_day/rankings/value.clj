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

(defn min-bid-ids
  "Ids of the below-replacement players who still fill a roster slot, and so cost
  the $1 minimum rather than nothing.

  The reserve `budget - total-slots` sets a dollar aside for every *slot*, but the
  dollar was only ever paid out to players above replacement. At the 12-team
  default that is $1 x 180 reserved against 84 players paid, so the board summed
  to $2304 of a $2400 room and 96 roster slots carried no price at all. Those
  slots get filled at the auction; a board that says $0 is saying 'undraftable'
  about half a draft, and saying it about the half where the manager has the
  least to go on.

  `slots` is the count of slots a *priced* position can fill (`league-state/
  priced-slots`), not the league total: K and DST take roster slots but never a
  dollar, so counting their slots here hands their minimum bids to skill players.

  The minimums are apportioned **per position**, in proportion to how many of that
  position finished above replacement, rather than handed to the best of the tail
  by raw VORP. Below replacement each position's points curve has its own slope,
  so a global VORP ranking buys the flattest curves: it gave TE 27 of 96 minimums
  against 12 TE starters, and priced a TE at ADP 251 while leaving an RB at ADP
  147 unpriced. Proportional shares track the roster the league actually fills.
  Any shortfall at a position (its tail ran out) is redistributed by VORP so the
  count still lands on `slots`."
  [board slots]
  (let [{priced true tail false} (group-by priced-vorp? board)
        ;; `tail` is every non-priced row, K/DST included: they take roster slots
        ;; but never a dollar, so they are dropped here and their slots were
        ;; already excluded from `slots`.
        cand   (->> tail
                    (filter #(and (priced-positions (:position %)) (number? (:vorp %))))
                    (sort-by #(- (double (:vorp %)))))
        n      (- (long slots) (count priced))]
    (if-not (pos? n)
      #{}
      (let [demand (frequencies (map :position priced))
            quota  (largest-remainder n demand)
            by-pos (group-by :position cand)
            taken  (into #{}
                         (mapcat (fn [[pos q]] (map :player-id (take q (get by-pos pos)))))
                         quota)
            ;; a position whose tail ran dry leaves the count short; top it up in
            ;; VORP order so the board still prices exactly `n` slots
            fill   (->> cand
                        (remove #(contains? taken (:player-id %)))
                        (map :player-id)
                        (take (- n (count taken))))]
        (into taken fill)))))

(defn calculate-value
  "Stable salary-cap Value (VBD -> dollars): reserve $1 per rostered slot, then
  spread the discretionary money (budget - total-slots) across positive-VORP
  priced players by VORP share. The below-replacement players who still fill a
  slot price at the $1 minimum (`min-bid-ids`); K/DST and everyone past the last
  roster slot -> $0. Assocs :value on each player.

  Two things keep the total under the room's cash rather than exactly on it, and
  both are deliberate:

  - a dollar is reserved for **every** slot (`budget - total-slots`) but only a
    priced position can collect one, so the league's K/DST slots hold back their
    own count — $24 at the 12-team default. That reserve is right: those slots
    really do cost a dollar or two apiece, the board just does not price them yet.
  - `to-dollars` rounds each priced player independently, so the total drifts up
    to half a dollar each way per player.

  On the sample board that is $2374 of $2400: $24 reserved for K/DST, the rest
  rounding. It does *not* sum to `priced + discretionary`, which is what it summed
  to before the minimum-bid tail was priced.

  `priced-slots` (4-arity) is the number of slots a priced position can fill; it
  defaults to `total-slots`, which over-counts by the league's K/DST slots.
  `engine/live-valuation` passes the real figure."
  ([board budget total-slots] (calculate-value board budget total-slots total-slots))
  ([board budget total-slots priced-slots]
   (let [disc     (max 0.0 (- (double budget) total-slots))
         ;; A board may not price more players than the league has slots to fill.
         ;; `:teams` and `:replacement-config` come from different snapshots of app
         ;; state — `:apply-config` deliberately keeps the old `:teams` once picks
         ;; exist — so editing roster or team count mid-draft can put more players
         ;; above replacement than there are seats. Without this the board
         ;; over-sums the room and rows past the last slot read a real price.
         ;; A row cannot be priced without a seat to fill or a dollar to fill it
         ;; with. `routes` rejects a room too poor to pay a dollar a slot, so the
         ;; budget term here is a floor under the pure function rather than a
         ;; state the app reaches.
         payable  (min (long priced-slots) (long budget))
         ranked   (->> board (filter priced-vorp?) (sort-by #(- (double (:vorp %)))))
         priced   (set (map :player-id (take payable ranked)))
         total-vorp (reduce + 0.0 (map #(if (contains? priced (:player-id %))
                                          (double (:vorp %)) 0.0)
                                       board))
         min-bid  (min-bid-ids board payable)]
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
  Value; the $1 base keeps min-bid players at $1. Priced only for undrafted skill
  players with value >= 1; K/DST, drafted, worthless -> $0. `inflation` is a
  scalar or a fn of the player (per-position inflation)."
  [board inflation drafted-ids]
  (mapv (fn [p]
          (let [value  (double (or (:value p) 0))
                priced (and (not (contains? drafted-ids (:player-id p)))
                            (priced-positions (:position p))
                            (>= value 1.0))]
            (assoc p :worth
                   (if priced
                     (to-dollars (+ 1.0 (* (- value 1.0) (inflation-for inflation p))))
                     0))))
        board))
