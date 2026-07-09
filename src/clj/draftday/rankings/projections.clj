(ns draftday.rankings.projections
  "Derive a floor (p10) / ceiling (p90) band around each player's mean projection
  from data we already fetch free: expert-rank disagreement (FantasyPros
  rank_std) scaled by a per-position historical volatility. Powers the Floor and
  Ceiling strategy profiles.

  band = k_pos * min(1, rank_std / STD_SCALE)
  ceiling = mean * (1 + band),  floor = mean * (1 - band)

  rank_std (spread of expert ranks) is used directly rather than divided by
  rank_ave — the ratio perversely inflates for top players (tiny rank_ave), which
  would make studs look boom/bust. Raw std tracks real disagreement.

  More expert disagreement -> a wider band. When a player has no rank spread
  (e.g. not covered by FantasyPros ECR), a per-position default spread is used so
  Floor/Ceiling still differ from the mean.")

(def position-volatility
  "k_pos: the fraction of the mean a full (rel=1) expert disagreement implies.
  RB/WR/TE are boomier than QB; K/DST are steadier season-long."
  {"QB" 0.20 "RB" 0.35 "WR" 0.35 "TE" 0.40 "K" 0.15 "DST" 0.25})

(def ^:private default-volatility 0.30)
(def ^:private default-spread 0.5)
;; A rank_std of ~10 (experts spread ~10 slots) reads as maximum disagreement.
(def ^:private std-scale 10.0)

(defn- relative-spread
  "Expert disagreement as a 0..1 signal from the spread of expert ranks; nil when
  no usable rank data is present."
  [player]
  (let [std (:fantasypros/rank-std player)]
    (when (number? std)
      (min 1.0 (/ (double std) std-scale)))))

(defn with-floor-ceiling
  "Assoc :floor and :ceiling on each player. Requires :points (run after
  scoring). Options: :volatility (per-position k_pos map), :default-spread."
  ([board] (with-floor-ceiling board {}))
  ([board {:keys [volatility default-spread]
           :or   {volatility position-volatility default-spread default-spread}}]
   (mapv (fn [p]
           (let [mean (double (or (:points p) 0))
                 kpos (get volatility (:position p) default-volatility)
                 rel  (or (relative-spread p) default-spread)
                 band (* kpos rel)]
             (assoc p
                    :floor   (* mean (- 1.0 band))
                    :ceiling (* mean (+ 1.0 band)))))
         board)))
