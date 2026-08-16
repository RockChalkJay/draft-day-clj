(ns draft-day.rankings.engine
  "Static/live orchestration — the core answer to 'some of this is live state,
  some isn't'. `static-rankings` (points -> tiers -> vorp) computes once per
  scoring/roster-size config; `live-valuation` (value -> inflation -> worth ->
  bargain, + the tcm display signal) recomputes after every pick."
  (:require [draft-day.rankings.model :as model]
            [draft-day.rankings.projections :as projections]
            [draft-day.rankings.tiers :as tiers]
            ;; registers the :ecr tier strategy
            [draft-day.rankings.tiers.ecr]
            [draft-day.rankings.replacement :as replacement]
            [draft-day.rankings.value :as value]
            [draft-day.rankings.inflation :as inflation]
            [draft-day.rankings.inflation-index :as idx]
            [draft-day.rankings.tcm :as tcm]
            [draft-day.rankings.league-state :as ls]))

(defn static-rankings
  "Steps 0-2: points -> floor/ceiling -> replacement level -> VORP -> tiers.
  Returns {:players [...] :replacement-levels {...}}. Never mutated by the live
  layer.

  Replacement runs before tiering because the tier cut needs to know where a
  position stops being draftable. VORP runs before tiering too, and that is the
  one ordering constraint here: the overall tier scale is cut on :vorp, the only
  score that compares a QB to an RB. `with-vorp` reads nothing tiering writes, so
  the swap is free.

  Every registered tier strategy is computed, at both an overall and a
  per-position scale, and shipped side by side under :tiers — the board picks a
  technique and a scale without a round trip. :tier remains the default
  strategy's positional tier.

  The expert tier is a peer strategy (:ecr) rather than an override of the
  computed one. Overriding was wrong for two reasons this shape answers directly:
  it covers ~75% of the board, which now reads as an explicit untiered bucket
  instead of a quarter of the players silently borrowing a cliff tier; and its
  overall scale is not the computed per-position scale, which now are two
  separate keys instead of one dense-ranked number meaning neither. Both of its
  scales are scraped — see `rankings.tiers.ecr`. Ingestion fetches every
  FantasyPros page per format and `rankings.vendor` picks the league's, so the
  expert tiers follow the league's scoring.

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
         vorped   (replacement/with-vorp enriched levels :points)]
     {:players            (tiers/with-tiers vorped {:replacement-levels levels
                                                    :num-teams          num-teams})
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
