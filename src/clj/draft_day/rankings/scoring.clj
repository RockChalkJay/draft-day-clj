(ns draft-day.rankings.scoring
  "Piece 0: configurable scoring -> a single :points value per player.

  A player carries its projected stat line under :stats {stat-key value}. A
  ScoringConfig is a flat {stat-key weight} map over those same keys, so points
  is simply Σ(stat * weight). Using Sleeper's stat keys (`pass_yd`, `rush_td`,
  `rec`, ...) means the league's Sleeper scoring_settings map in directly, and a
  position that lacks a stat (a TE has no :pass_yd) contributes 0 — never an
  error.")

(defn- preset [reception-pts]
  {:pass_yd 0.04 :pass_td 4.0 :pass_int -2.0
   :rush_yd 0.1 :rush_td 6.0
   :rec reception-pts :rec_yd 0.1 :rec_td 6.0
   :fum_lost -2.0
   ;; kicking
   :fgm 3.0 :xpm 1.0
   ;; team defense (linear stats only)
   :sack 1.0 :int 2.0 :fum_rec 2.0 :ff 1.0 :def_td 6.0 :safe 2.0})

(def presets
  {:standard (preset 0.0)
   :half-ppr (preset 0.5)
   :ppr      (preset 1.0)})

(defn player-points
  "Σ over the scoring map of (stat weight * player's projected stat), defaulting
  missing stats to 0."
  [player scoring]
  (let [stats (:stats player)]
    (reduce-kv (fn [acc stat weight]
                 (if (zero? weight)
                   acc
                   (+ acc (* weight (double (get stats stat 0))))))
               0.0 scoring)))

(defn with-points
  "Return board with a :points value on each player."
  [board scoring]
  (mapv #(assoc % :points (player-points % scoring)) board))
