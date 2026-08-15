(ns draft-day.rankings.engine
  "Static/live orchestration — the core answer to 'some of this is live state,
  some isn't'. `static-rankings` (points -> tiers -> vorp) computes once per
  scoring/roster-size config; `live-valuation` (value -> inflation -> worth ->
  bargain, + the tcm display signal) recomputes after every pick."
  (:require [draft-day.rankings.model :as model]
            [draft-day.rankings.projections :as projections]
            [draft-day.rankings.tiers :as tiers]
            [draft-day.rankings.replacement :as replacement]
            [draft-day.rankings.value :as value]
            [draft-day.rankings.inflation :as inflation]
            [draft-day.rankings.inflation-index :as idx]
            [draft-day.rankings.tcm :as tcm]
            [draft-day.rankings.league-state :as ls]))

(defn tier-floor
  "Points below which a position's tail collapses into one tier.

  Valuation's replacement level where there is one. K and DST are deliberately
  absent from that map so they price at $0, but they still need a floor: kicker
  points are small enough that a 2-point drop down the tail is a 20% drop, and
  tiering the whole pool split 44 kickers into 12 tiers of mostly one. Exactly
  one of each starts, so the num-teams-th best is the same boundary the priced
  positions get.

  `sorted` is the position group already in descending :points order. It used to
  sort a second copy of the group for itself; handing the same vector to
  `tiers-by-cliffs` leaves one real sort per position, since the defensive sort
  there costs a linear pass on input that is already ordered."
  [sorted level num-teams]
  (or level
      (when (seq sorted)
        (double (:points (nth sorted (min num-teams (dec (count sorted)))))))))

(defn static-rankings
  "Steps 0-2: points -> floor/ceiling -> replacement level -> per-position tiers
  (cliffs above replacement) -> VORP. Returns {:players [...]
  :replacement-levels {...}}. Never mutated by the live layer.

  Replacement runs before tiering because the tier cut needs to know where a
  position stops being draftable; both stages read only :points, so the order
  between them is free.

  :tier is deliberately computed here rather than taken from
  :fantasypros/ecr-tier, which used to override it. That tier covers ~75% of the
  board, so adopting it left a quarter of the players untiered — and dense-ranking
  expert *overall* tiers (1-16) together with computed *per-position* tiers made
  the two scales one incoherent number. (It is no longer PPR-only: ingestion
  scrapes all three cheatsheets and `rankings.vendor` picks the league's format,
  so the expert tier does follow the league's scoring now. The two reasons above
  are what still rule it out.) It is still shipped for the :fp-tier column.

  Opts :model (default :points, the raw scored projection) and :weights select
  which `rankings.model` produces :points; every later stage is indifferent to
  the choice. See `draft-day.rankings.model`."
  ([board scoring num-teams] (static-rankings board scoring num-teams {}))
  ([board scoring num-teams {:keys [replacement-config model weights]
                             :or   {model :points}}]
   (let [enriched (-> (model/score-board model {:scoring scoring :weights weights} board)
                      projections/with-floor-ceiling)
         levels   (replacement/replacement-levels enriched num-teams
                                                  (or replacement-config {}) :points)
         tiered   (into [] (mapcat (fn [[pos grp]]
                                     (let [sorted (vec (sort-by :points > grp))]
                                       (tiers/tiers-by-cliffs
                                        sorted (tier-floor sorted (get levels pos) num-teams)))))
                        (group-by :position enriched))]
     {:players            (replacement/with-vorp tiered levels :points)
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
