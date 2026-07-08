(ns draftday.rankings.value
  "VBD -> dollars. Value is the stable salary-cap price; Price (:worth) is Value
  scaled by live inflation.

  Dollar rounding uses Math/rint (round half to even) to match the numpy/pandas
  behavior the reference tests pin, e.g. 1 + (40-1)*0.5 = 20.5 -> 20.")

(def priced-positions #{"QB" "RB" "WR" "TE"})

(defn- to-dollars ^long [x]
  (max 0 (long (Math/rint (double x)))))

(defn- priced-vorp? [p]
  (and (priced-positions (:position p))
       (> (double (or (:vorp p) 0.0)) 0.0)))

(defn calculate-value
  "Stable salary-cap Value (VBD -> dollars): reserve $1 per rostered slot, then
  spread the discretionary money (budget - total-slots) across positive-VORP
  priced players by VORP share. K/DST and replacement-level players -> $0. Assocs
  :value on each player."
  [board budget total-slots]
  (let [total-vorp (reduce + 0.0 (map #(if (priced-vorp? %) (double (:vorp %)) 0.0) board))
        disc       (max 0.0 (- (double budget) total-slots))]
    (mapv (fn [p]
            (assoc p :value
                   (if (and (priced-vorp? p) (> total-vorp 0.0))
                     (to-dollars (+ 1.0 (* (/ (double (:vorp p)) total-vorp) disc)))
                     0)))
          board)))

(defn calculate-price
  "Live Price (:worth) = 1 + (value - 1) * inflation. At inflation 1 Price equals
  Value; the $1 base keeps min-bid players at $1. Priced only for undrafted skill
  players with value >= 1; K/DST, drafted, worthless -> $0."
  [board inflation drafted-ids]
  (mapv (fn [p]
          (let [value  (double (or (:value p) 0))
                priced (and (not (contains? drafted-ids (:player-id p)))
                            (priced-positions (:position p))
                            (>= value 1.0))]
            (assoc p :worth
                   (if priced
                     (to-dollars (+ 1.0 (* (- value 1.0) inflation)))
                     0))))
        board))
