(ns draft-day.rankings.value
  "VBD -> dollars. Value is the stable salary-cap price; Price (:worth) is Value
  scaled by live inflation.

  Dollar rounding uses Math/rint (round half to even) to match the numpy/pandas
  behavior the reference tests pin, e.g. 1 + (40-1)*0.5 = 20.5 -> 20.")

(def priced-positions #{"QB" "RB" "WR" "TE"})

(defn- to-dollars ^long [x]
  (max 0 (long (Math/rint (double x)))))

(defn priced-vorp?
  "Does this player earn a share of the discretionary money? A priced position
  with VORP strictly above replacement. Everything else either takes the $1
  minimum (`min-bid-ids`) or nothing."
  [p]
  (and (priced-positions (:position p))
       (> (double (or (:vorp p) 0.0)) 0.0)))

(defn min-bid-ids
  "Ids of the below-replacement players who still fill a roster slot, and so cost
  the $1 minimum rather than nothing.

  The reserve `budget - total-slots` sets a dollar aside for every *slot*, but
  the dollar was only ever paid out to players above replacement. At the 12-team
  default that is $1 x 180 reserved against 84 players paid, so the board summed
  to $2304 of a $2400 room and 96 roster slots carried no price at all. Those 96
  slots get filled at the auction; a board that says $0 is saying 'undraftable'
  about half a draft, and it is saying it about the half where the manager has
  the least to go on.

  The best `total-slots - priced` of them by VORP take the dollar already set
  aside for them, which is what makes the board conserve to the budget. Players
  the model never scored are left out — no :vorp means no opinion, not $1.

  K and DST are left out too, as they are from pricing generally. That overstates
  the count by however many K/DST slots a roster carries (24 of 180 at the
  default), so 24 skill players get a dollar that a kicker will really spend. The
  alternative is teaching this function the roster's shape for $24 of $2400."
  [board total-slots]
  (let [{priced true tail false} (group-by priced-vorp? board)
        n (- (long total-slots) (count priced))]
    (if-not (pos? n)
      #{}
      (into #{}
            (comp (filter #(and (priced-positions (:position %)) (number? (:vorp %))))
                  (map :player-id)
                  (take n))
            (sort-by #(- (double (or (:vorp %) 0.0))) tail)))))

(defn calculate-value
  "Stable salary-cap Value (VBD -> dollars): reserve $1 per rostered slot, then
  spread the discretionary money (budget - total-slots) across positive-VORP
  priced players by VORP share. The below-replacement players who still fill a
  slot price at the $1 minimum (`min-bid-ids`); K/DST and everyone past the last
  roster slot -> $0. Assocs :value on each player.

  Sums to the budget rather than to `priced + discretionary`, which is what it
  summed to before the minimum-bid tail was priced."
  [board budget total-slots]
  (let [total-vorp (reduce + 0.0 (map #(if (priced-vorp? %) (double (:vorp %)) 0.0) board))
        disc       (max 0.0 (- (double budget) total-slots))
        min-bid    (min-bid-ids board total-slots)]
    (mapv (fn [p]
            (assoc p :value
                   (cond
                     (and (priced-vorp? p) (> total-vorp 0.0))
                     (to-dollars (+ 1.0 (* (/ (double (:vorp p)) total-vorp) disc)))

                     (contains? min-bid (:player-id p)) 1
                     :else                              0)))
          board)))

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
