(ns draft-day.rankings.engine
  "Static/live orchestration — the core answer to 'some of this is live state,
  some isn't'. `static-rankings` (points -> tiers -> vorp) computes once per
  scoring/roster-size config; `live-valuation` (value -> inflation -> worth ->
  bargain, + the tcm display signal) recomputes after every pick."
  (:require [draft-day.rankings.scoring :as scoring]
            [draft-day.rankings.projections :as projections]
            [draft-day.rankings.tiers :as tiers]
            [draft-day.rankings.replacement :as replacement]
            [draft-day.rankings.value :as value]
            [draft-day.rankings.inflation :as inflation]
            [draft-day.rankings.inflation-index :as idx]
            [draft-day.rankings.tcm :as tcm]
            [draft-day.rankings.league-state :as ls]))

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
  "Steps 0-2: points -> floor/ceiling -> per-position tiers (on mean points;
  expert override + re-anchor) -> replacement + VORP (on points). Returns
  {:players [...] :replacement-levels {...}}. Never mutated by the live layer."
  ([board scoring num-teams] (static-rankings board scoring num-teams {}))
  ([board scoring num-teams {:keys [num-tiers replacement-config]
                             :or   {num-tiers 5}}]
   (let [enriched   (-> (scoring/with-points board scoring)
                        projections/with-floor-ceiling)
         tiered     (mapcat (fn [[_ grp]] (tiers/tiers-by-cliffs grp num-tiers))
                            (group-by :position enriched))
         reanchored (->> tiered
                         (map apply-expert-tier)
                         dense-rank-per-position
                         vec)
         levels     (replacement/replacement-levels reanchored num-teams
                                                    (or replacement-config {}) :points)]
     {:players            (replacement/with-vorp reanchored levels :points)
      :replacement-levels levels})))

(defn live-valuation
  "Live layer: Value (VBD->$), Price (:worth, Value scaled by inflation*phase),
  Bargain (value - worth), plus the per-player :tcm cliff display signal. Call
  after each pick. Returns {:players ... :replacement-levels ... :inflation ...
  :inflation-index ... :market-heat ...}."
  [static-result league-state]
  (let [base        (:players static-result)
         board       (tcm/with-tcm base league-state)
         budget      (ls/initial-cash league-state)
         total-slots (reduce + 0 (map #(count (:roster %)) (:teams league-state)))
         valued      (value/calculate-value board budget total-slots)
         infl        (inflation/auction-inflation valued league-state)
         heat        (inflation/draft-phase-decay league-state)
         ;; per-position inflation replaces the single global scalar; reduces to
         ;; `infl` for positions with no picks.
         pos-infl    (idx/per-position-inflation valued league-state infl)
         infl-fn     (fn [p] (* (get pos-infl (:position p) infl) heat))
         priced      (value/calculate-price valued infl-fn
                                            (:drafted-player-ids league-state))
         with-barg   (mapv (fn [p]
                             (assoc p :bargain
                                    (if (> (:worth p) 0) (- (:value p) (:worth p)) 0)))
                           priced)]
     {:players             with-barg
      :replacement-levels  (:replacement-levels static-result)
      :inflation           infl
      :inflation-index     (idx/inflation-index valued (:picks league-state))
      :market-heat         heat}))
