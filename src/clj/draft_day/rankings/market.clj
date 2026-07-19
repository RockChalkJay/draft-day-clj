(ns draft-day.rankings.market
  "Reference market price: what the room is likely to charge, from external
  auction sources (ESPN, FantasyPros), normalized to *this* league's money pool.

  Purely a display/reference layer — this never feeds Value or Worth (see
  engine.clj). Sources publish dollars against their own baseline pool (e.g. ESPN
  vs 10 teams x $200 = $2000); we rescale each to the request league's pool so
  they're comparable, average the ones a player has, and expose the gap to Worth
  as :edge.")

(def source-baselines
  "Player-map key -> the total money pool that source's raw dollars are quoted
  against. ESPN publishes vs 10-team/$200; FantasyPros' calculator default is
  12-team/$200 (see ingestion.fantasypros/aav-url)."
  {:espn/auction-value 2000.0
   :fantasypros/aav    2400.0})

(defn- normalize
  "Rescale a source's raw dollars to `league-pool`: a player worth 5% of ESPN's
  $2000 pool is worth 5% of this league's pool."
  [raw baseline league-pool]
  (* (/ (double raw) baseline) league-pool))

(defn market-price
  "Mean of the player's available normalized source prices, rounded to the
  nearest whole dollar. nil when the player has no source price or the league
  pool is non-positive."
   [player league-pool]
    (when (pos? league-pool)
      (let [prices (keep (fn [[k baseline]]
                           (when-let [raw (get player k)]
                             (when (pos? raw)
                               (normalize raw baseline league-pool))))
                         source-baselines)]
        (when (seq prices)
          (Math/round (/ (reduce + prices) (count prices)))))))

(defn edge
  "Worth minus market — positive means the model values the player above the
  room. nil unless the player has a positive Worth and a market price."
  [worth market]
  (when (and (number? worth) (pos? worth) (number? market))
    (- worth market)))

(defn with-market
  "Assoc :market (normalized consensus) and :edge (worth - market) onto each
  player. `league-pool` is the league's total money (num-teams * bankroll)."
  [players league-pool]
  (mapv (fn [p]
          (let [m (market-price p league-pool)]
            (assoc p :market m :edge (edge (:worth p) m))))
        players))
