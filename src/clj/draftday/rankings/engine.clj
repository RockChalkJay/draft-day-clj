(ns draftday.rankings.engine
  "Static/live orchestration — the core answer to 'some of this is live state,
  some isn't'. `static-rankings` (points -> tiers -> vorp) computes once per
  scoring/roster-size config; `live-valuation` (value -> inflation -> worth ->
  bargain, + tcm/pdm signals) recomputes after every pick."
  (:require [draftday.rankings.scoring :as scoring]
            [draftday.rankings.tiers :as tiers]
            [draftday.rankings.replacement :as replacement]
            [draftday.rankings.value :as value]
            [draftday.rankings.inflation :as inflation]
            [draftday.rankings.tcm :as tcm]
            [draftday.rankings.pdm :as pdm]
            [draftday.rankings.league-state :as ls]))

(defn- apply-expert-tier
  "The live-fetched FantasyPros expert tier (:fantasypros/ecr-tier) takes
  precedence over the computed cliff tier when present and >= 1."
  [player]
  (let [et (:fantasypros/ecr-tier player)]
    (if (and (number? et) (>= et 1))
      (assoc player :tier (long et))
      player)))

(defn- dense-rank-per-position
  "Re-anchor :tier to the per-position scale: within each position, dense-rank
  the tier values so the best cluster is tier 1. Identity for computed cliff
  tiers (already 1..k); collapses expert *overall* tiers (a TE's best cluster may
  be overall tier 5) back to per-position 1..k that the board/PDM think in."
  [players]
  (mapcat (fn [[_ grp]]
            (let [rank-map (into {} (map-indexed (fn [i t] [t (inc i)])
                                                 (sort (distinct (map :tier grp)))))]
              (map #(update % :tier rank-map) grp)))
          (group-by :position players)))

(defn static-rankings
  "Steps 0-2: points -> per-position tiers (expert override + re-anchor) -> vorp.
  Returns {:players [...] :replacement-levels {...}}. Never mutated by the live
  layer."
  ([board scoring num-teams] (static-rankings board scoring num-teams {}))
  ([board scoring num-teams {:keys [num-tiers replacement-config]
                             :or   {num-tiers 5}}]
   (let [scored     (scoring/with-points board scoring)
         tiered     (mapcat (fn [[_ grp]] (tiers/tiers-by-cliffs grp num-tiers))
                            (group-by :position scored))
         reanchored (->> tiered
                         (map apply-expert-tier)
                         dense-rank-per-position
                         vec)
         levels     (replacement/replacement-levels reanchored num-teams
                                                    (or replacement-config {}))]
     {:players            (replacement/with-vorp reanchored levels)
      :replacement-levels levels})))

(defn live-valuation
  "Live layer: Value (VBD->$), Price (:worth, Value scaled by inflation*phase),
  Bargain (value - worth), plus the tcm/pdm analytical signals. Call after each
  pick. Returns {:players ... :replacement-levels ... :pdm-map ... :inflation ...
  :market-heat ...}."
  [static-result league-state]
  (let [board       (:players static-result)
        budget      (ls/initial-cash league-state)
        total-slots (reduce + 0 (map #(count (:roster %)) (:teams league-state)))
        valued      (value/calculate-value board budget total-slots)
        infl        (inflation/auction-inflation valued league-state)
        heat        (inflation/draft-phase-decay league-state)
        priced      (value/calculate-price valued (* infl heat)
                                           (:drafted-player-ids league-state))
        with-barg   (mapv (fn [p]
                            (assoc p :bargain
                                   (if (> (:worth p) 0) (- (:value p) (:worth p)) 0)))
                          priced)
        with-signals (tcm/with-tcm with-barg league-state)]
    {:players            with-signals
     :replacement-levels (:replacement-levels static-result)
     :pdm-map            (pdm/calculate-pdm with-barg league-state)
     :inflation          infl
     :market-heat        heat}))
