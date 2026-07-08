(ns draftday.rankings.inflation-index
  "The inflation revamp: a live Inflation Index (headline diagnostic) and a
  per-position inflation multiplier that replaces the POC's single global scalar.

  Both consume the pick log — league-state :picks [{:player-id :position :price}]
  — where par value is the player's intrinsic :value on the current board.
  With no picks yet, per-position inflation reduces to the global conserving
  factor for every position, so opening-at-par behavior is preserved.")

(def priced-positions ["QB" "RB" "WR" "TE"])

(def BETA
  "How strongly a position's realized overpay tilts its inflation away from the
  global factor."
  0.5)

;; Softer band than the global [0.5,1.8] so a positional run can register.
(def POS-MIN 0.6)
(def POS-MAX 1.6)

(defn- par-values [board]
  (into {} (map (juxt :player-id #(double (or (:value %) 0.0))) board)))

(defn inflation-index
  "Headline diagnostic: running Σ(price_paid - par_value) over drafted picks.
  Rising/positive => the room is overpaying, so value is available later."
  [board picks]
  (let [par (par-values board)]
    (reduce (fn [acc {:keys [player-id price]}]
              (+ acc (- (double price) (get par player-id 0.0))))
            0.0 picks)))

(defn per-position-inflation
  "Return {pos multiplier} tilting the global conserving inflation by each
  position's realized overpay ratio (Σpaid/Σpar among that position's picks):
    infl_p = clamp(global * (1 + BETA*(ratio_p - 1))).
  Positions with no picks pass the global factor straight through."
  [board league-state global-infl]
  (let [par    (par-values board)
        by-pos (group-by :position (:picks league-state))]
    (reduce (fn [m pos]
              (let [ps   (get by-pos pos)
                    paid (reduce + 0.0 (map #(double (:price %)) ps))
                    base (reduce + 0.0 (map #(get par (:player-id %) 0.0) ps))
                    tilt (if (pos? base) (+ 1.0 (* BETA (- (/ paid base) 1.0))) 1.0)]
                (assoc m pos (double (min POS-MAX (max POS-MIN (* global-infl tilt)))))))
            {} priced-positions)))
