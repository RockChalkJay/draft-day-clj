(ns draft-day.scoring
  "Piece 0: configurable scoring -> a single :points value per player.

  A player carries its projected stat line under :stats {stat-key value}. A
  ScoringConfig is a flat {stat-key weight} map over those same keys, so points
  is simply Σ(stat * weight). Using Sleeper's stat keys (`pass_yd`, `rush_td`,
  `rec`, ...) means the league's Sleeper scoring_settings map in directly, and a
  position that lacks a stat (a TE has no :pass_yd) contributes 0 — never an
  error.

  Shared cljc rather than backend-only (the `db.cljc` precedent) because the
  frontend needs `presets` and `stat-keys` synchronously: seeding the custom
  scoring editor from an async `/api/scoring/presets` fetch meant a manager who
  picked Custom before it landed got a nil scoring config and a Settings page
  that threw. One definition, both sides, no fetch.")

(defn- preset [reception-pts]
  {:pass_yd 0.04 :pass_td 4.0 :pass_int -2.0 :pass_2pt 0.0
   :rush_yd 0.1 :rush_td 6.0 :rush_2pt 0.0
   :rec reception-pts :rec_yd 0.1 :rec_td 6.0 :rec_2pt 0.0
   :fum_lost -2.0
   ;; kicking
   :fgm 3.0 :xpm 1.0 :blk_kick 0.0
   ;; team defense (linear stats only)
   :sack 1.0 :int 2.0 :fum_rec 2.0 :ff 1.0 :def_td 6.0 :safe 2.0})

(def presets
  {:standard (preset 0.0)
   :half-ppr (preset 0.5)
   :ppr      (preset 1.0)})

(def stat-keys
  "Every stat key the custom scoring editor and league import may touch."
  (vec (keys (:standard presets))))

(defn usable-weight
  "`x` as a number we can safely multiply by, or 0.0.

  Anything unusable costs that one stat its contribution rather than taking the
  whole board down. This matters because a cleared input box in the custom
  scoring editor sends NaN, which `JSON.stringify` writes as null: the previous
  `(zero? weight)` threw on it, surfaced as an HTTP 400, and blanked the board."
  [x]
  (if (and (number? x)
           #?(:clj  (let [d (double x)]
                      (not (or (Double/isNaN d) (Double/isInfinite d))))
              :cljs (js/isFinite x)))
    (double x)
    0.0))

(def formats
  "The scoring formats outside vendors publish against, in reception order.
  FantasyPros' ECR and auction calculator and Sleeper's ADP all vary by these."
  [:standard :half-ppr :ppr])

(def ^:private half-ppr-cutoff 0.25)
(def ^:private ppr-cutoff 0.75)

(defn format-of
  "The published format closest to this scoring config.

  Vendor columns are published per format and a custom config does not name one,
  but receptions are what actually separate the three — so the :rec weight picks
  the nearest. A custom 0.4-per-catch league reads as half-PPR; a 1.5-per-catch
  TE-premium league reads as PPR."
  [scoring]
  (let [rec (usable-weight (:rec scoring))]
    (cond (< rec half-ppr-cutoff) :standard
          (< rec ppr-cutoff)      :half-ppr
          :else                   :ppr)))

(defn scores-anything?
  "True when at least one weight can actually move a player's points. An empty
  or all-zero config is not a league, it is a board where everyone is worth $0."
  [scoring]
  (boolean (some #(not (zero? (usable-weight %))) (vals scoring))))

(defn player-points
  "Σ over the scoring map of (stat weight * player's projected stat), defaulting
  missing stats to 0."
  [player scoring]
  (let [stats (:stats player)]
    (reduce-kv (fn [acc stat weight]
                 (let [w (usable-weight weight)]
                   (if (zero? w)
                     acc
                     (+ acc (* w (usable-weight (get stats stat 0)))))))
               0.0 scoring)))

(defn with-points
  "Return board with a :points value on each player."
  [board scoring]
  (mapv #(assoc % :points (player-points % scoring)) board))
