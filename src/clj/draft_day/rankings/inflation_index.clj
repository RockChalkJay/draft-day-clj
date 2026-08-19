(ns draft-day.rankings.inflation-index
  "The inflation revamp: a live Inflation Index (headline diagnostic) and a
  per-position inflation multiplier that replaces the POC's single global scalar.

  Both consume the pick log — league-state :picks [{:player-id :position :price}]
  — where par value is the player's intrinsic :value on the current board.
  With no picks yet, per-position inflation reduces to the global conserving
  factor for every position, so opening-at-par behavior is preserved."
  (:require [draft-day.db :as db]))

(def priced-positions
  "The positions this namespace reports a multiplier for — `db/priced-positions`.

  It used to be a private vector of the same four strings under the same name as
  `value/priced-positions`, which is a set. `(priced-positions pos)` reads as a
  membership test everywhere else in the codebase and threw a ClassCastException
  here."
  db/priced-positions)

(def BETA
  "How strongly a position's realized overpay tilts its inflation away from the
  global factor."
  0.5)

(def SHRINK-BASE
  "Dollars of par a position must have on the board before its realized ratio
  speaks at full volume.

  The tilt is a scale-free ratio, so without this a trivial pick swings it as hard
  as a real one: $3 for a $1 flier reads as a 3x overpay and pinned the position
  at the top of the band on the first nomination of the draft, pricing a $40 RB
  at $63. A hard minimum-par threshold does not fix that — it just moves the
  cliff, since $6 on a $2 player is the same 3x and a dollar of par difference
  would swing the multiplier from end to end with nothing in between.

  Shrinking toward 1.0 by how much par is actually at stake grades the same
  judgement continuously: one $1 flier bought at 3x moves its position 5%, while
  a genuine run over $150 of par moves it nearly the full BETA. It also keeps the
  signal a threshold threw away — twenty cheap players bought at 6x is a real
  endgame run, and filtering those picks out reported it as nothing at all."
  20.0)

(defn- par-values [board]
  (into {} (map (juxt :player-id #(double (or (:value %) 0.0))) board)))

(defn inflation-index
  "Headline diagnostic: running Σ(price_paid - par_value) over drafted picks at
  priced positions. Rising/positive => the room is overpaying, so value is
  available later.

  K and DST are excluded because the board never prices them, so every dollar
  spent on one scored as pure overpay: a room that paid par on all 180 picks
  still reported +$24, and `core.cljs` paints any positive index as a warning."
  [board picks]
  (let [par (par-values board)]
    (reduce (fn [acc {:keys [player-id position price]}]
              (if-not (priced-positions position)
                acc
                (+ acc (- (double price) (get par player-id 0.0)))))
            0.0 picks)))

(defn per-position-inflation
  "Return {pos multiplier} tilting the global conserving inflation by each
  position's realized overpay ratio (Σpaid/Σpar among that position's picks):

    ratio_p = Σpaid / Σpar
    shrink  = Σpar / (Σpar + SHRINK-BASE)
    infl_p  = global * (1 + BETA*(ratio_p - 1)*shrink)

  Positions with no picks pass the global factor straight through, and so, very
  nearly, do positions whose only picks were minimum bids — see `SHRINK-BASE`.

  Deliberately unclamped. This is one factor in the multiplier Worth is scaled
  by, not the multiplier itself: phase decay still multiplies in afterwards, and
  `inflation/clamp-to-band` holds the finished product inside a single band. A
  band here bounded the wrong quantity — it capped a position at 1.6 while the
  global factor alone was allowed to reach 1.8, so a position with no picks came
  out *below* the factor it was supposed to be passing straight through."
  [board league-state global-infl]
  (let [par    (par-values board)
        by-pos (group-by :position (:picks league-state))]
    (reduce (fn [m pos]
              (let [ps     (get by-pos pos)
                    paid   (reduce + 0.0 (map #(double (:price %)) ps))
                    base   (reduce + 0.0 (map #(get par (:player-id %) 0.0) ps))
                    shrink (/ base (+ base SHRINK-BASE))
                    tilt   (if (pos? base)
                             (+ 1.0 (* BETA (- (/ paid base) 1.0) shrink))
                             1.0)]
                (assoc m pos (double (* global-infl tilt)))))
            {} priced-positions)))
