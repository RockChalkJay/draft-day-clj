(ns draftday.rankings.profiles
  "Strategy profiles as named weight vectors. Each preset is an instance of the
  same {:risk :ceiling :scarcity :inflation-sensitivity} vector, so moving to
  user-tunable sliders later is just 'edit the vector' — no engine change.

  - :risk    blends effective points toward the floor (safe)
  - :ceiling blends effective points toward the ceiling (upside)
  - :scarcity folds tier-cliff x positional-demand into VORP (applied live)
  - :inflation-sensitivity scales how hard live inflation moves Worth")

(def profiles
  {:balanced {:label "Balanced" :risk 0.0 :ceiling 0.0 :scarcity 0.0 :inflation-sensitivity 1.0}
   :floor    {:label "Floor"    :risk 0.5 :ceiling 0.0 :scarcity 0.0 :inflation-sensitivity 1.0}
   :ceiling  {:label "Ceiling"  :risk 0.0 :ceiling 0.5 :scarcity 0.0 :inflation-sensitivity 1.0}
   :scarcity {:label "Scarcity" :risk 0.0 :ceiling 0.0 :scarcity 1.0 :inflation-sensitivity 1.0}})

(def default-profile :balanced)

(defn resolve-profile
  "Accept a preset keyword or a raw weight-vector map (future sliders); always
  return a full vector with defaults filled in."
  [p]
  (cond
    (map? p)     (merge (:balanced profiles) p)
    (keyword? p) (get profiles p (:balanced profiles))
    :else        (:balanced profiles)))

(defn effective-points
  "Blend the mean projection toward floor/ceiling per the profile:
     eff = mean + w_ceiling*(ceiling - mean) - w_risk*(mean - floor).
  Balanced (both 0) returns the mean unchanged."
  [player profile]
  (let [prof    (resolve-profile profile)
        mean    (double (or (:points player) 0))
        floor   (double (or (:floor player) mean))
        ceiling (double (or (:ceiling player) mean))]
    (- (+ mean (* (:ceiling prof) (- ceiling mean)))
       (* (:risk prof) (- mean floor)))))

(defn with-effective-points
  "Assoc :eff-points (profile-adjusted projection that feeds replacement/VORP)."
  [board profile]
  (mapv #(assoc % :eff-points (effective-points % profile)) board))
