(ns draft-day.rankings.engine
  "Static/live orchestration — the core answer to 'some of this is live state,
  some isn't'. `static-rankings` (points -> tiers -> vorp) computes once per
  scoring/roster-size config; `live-valuation` (value -> inflation -> worth ->
  bargain, + tcm/pdm signals) recomputes after every pick."
  (:require [draft-day.rankings.scoring :as scoring]
            [draft-day.rankings.projections :as projections]
            [draft-day.rankings.profiles :as profiles]
            [draft-day.rankings.tiers :as tiers]
            [draft-day.rankings.replacement :as replacement]
            [draft-day.rankings.value :as value]
            [draft-day.rankings.inflation :as inflation]
            [draft-day.rankings.inflation-index :as idx]
            [draft-day.rankings.tcm :as tcm]
            [draft-day.rankings.pdm :as pdm]
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
  "Steps 0-2: points -> floor/ceiling -> profile-adjusted effective points ->
  per-position tiers (on mean points; expert override + re-anchor) -> replacement
  + VORP (on effective points). The active :profile re-prices the board by moving
  effective points toward floor/ceiling. Returns {:players [...]
  :replacement-levels {...} :profile <keyword-or-map>}. Never mutated by the live
  layer."
  ([board scoring num-teams] (static-rankings board scoring num-teams {}))
  ([board scoring num-teams {:keys [num-tiers replacement-config profile]
                             :or   {num-tiers 5 profile profiles/default-profile}}]
   (let [enriched   (-> (scoring/with-points board scoring)
                        projections/with-floor-ceiling
                        (profiles/with-effective-points profile))
         ;; tiers use mean :points so a position's clusters stay stable across
         ;; lenses (PDM's elite counts and the badge don't jump when you switch).
         tiered     (mapcat (fn [[_ grp]] (tiers/tiers-by-cliffs grp num-tiers))
                            (group-by :position enriched))
         reanchored (->> tiered
                         (map apply-expert-tier)
                         dense-rank-per-position
                         vec)
         ;; replacement + VORP run on profile-adjusted :eff-points.
         levels     (replacement/replacement-levels reanchored num-teams
                                                    (or replacement-config {}) :eff-points)]
     {:players            (replacement/with-vorp reanchored levels :eff-points)
      :replacement-levels levels
      :profile            profile})))

(defn- scarcity-adjust-vorp
  "For the Scarcity profile (:scarcity weight), fold tier-cliff x positional-
  demand into VORP: vorp' = vorp * (1 + w*(tcm*pdm_pos - 1)). Identity when w=0,
  so every other profile is untouched. Keeps the raw static VORP under
  :vorp-base for display."
  [board pdm-map w]
  (mapv (fn [p]
          (let [factor (* (double (or (:tcm p) 1.0)) (double (get pdm-map (:position p) 1.0)))]
            (-> p
                (assoc :vorp-base (:vorp p))
                (assoc :vorp (* (double (:vorp p)) (+ 1.0 (* w (- factor 1.0))))))))
        board))

(defn live-valuation
  "Live layer: Value (VBD->$), Price (:worth, Value scaled by inflation*phase),
  Bargain (value - worth), plus the tcm/pdm signals. The active :profile applies
  the scarcity VORP fold and scales inflation sensitivity. Call after each pick.
  Returns {:players ... :replacement-levels ... :pdm-map ... :inflation ...
  :market-heat ... :profile ...}."
  ([static-result league-state] (live-valuation static-result league-state {}))
  ([static-result league-state {:keys [profile]}]
   (let [prof        (profiles/resolve-profile (or profile (:profile static-result)))
         base        (:players static-result)
         ;; live signals first — the scarcity fold consumes them.
         with-tcm    (tcm/with-tcm base league-state)
         pdm         (pdm/calculate-pdm base league-state)
         board       (scarcity-adjust-vorp with-tcm pdm (:scarcity prof))
         budget      (ls/initial-cash league-state)
         total-slots (reduce + 0 (map #(count (:roster %)) (:teams league-state)))
         valued      (value/calculate-value board budget total-slots)
         infl        (inflation/auction-inflation valued league-state)
         heat        (inflation/draft-phase-decay league-state)
         ;; per-position inflation replaces the single global scalar; reduces to
         ;; `infl` for positions with no picks. inflation-sensitivity scales how
         ;; hard the resulting live market moves Worth.
         pos-infl    (idx/per-position-inflation valued league-state infl)
         sens        (:inflation-sensitivity prof)
         infl-fn     (fn [p]
                       (let [market (* (get pos-infl (:position p) infl) heat)]
                         (+ 1.0 (* sens (- market 1.0)))))
         priced      (value/calculate-price valued infl-fn
                                            (:drafted-player-ids league-state))
         with-barg   (mapv (fn [p]
                             (assoc p :bargain
                                    (if (> (:worth p) 0) (- (:value p) (:worth p)) 0)))
                           priced)]
     {:players             with-barg
      :replacement-levels  (:replacement-levels static-result)
      :pdm-map             pdm
      :inflation           infl
      :position-inflation  pos-infl
      :inflation-index     (idx/inflation-index valued (:picks league-state))
      :market-heat         heat
      :profile             (or profile (:profile static-result))})))
