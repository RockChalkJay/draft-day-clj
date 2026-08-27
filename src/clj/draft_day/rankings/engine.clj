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
            [draft-day.rankings.injury :as injury]
            [draft-day.rankings.pos-rank :as pos-rank]
            [draft-day.rankings.league-state :as ls]))

(defn static-rankings
  "Steps 0-2: points -> floor/ceiling -> replacement level -> VORP -> tiers.
  Returns {:players [...] :replacement-levels {...}}. Never mutated by the live
  layer.

  Replacement runs before tiering because the tier cut needs to know where a
  position stops being draftable. VORP runs before tiering too, and that is the
  one real ordering constraint here: the overall tier scale is cut on :vorp, the
  only score that compares a QB to an RB. `with-vorp` reads nothing tiering
  writes, so the swap is free.

  :tier is deliberately computed here rather than taken from
  :fantasypros/ecr-tier, which used to override it. That tier covers ~75% of the
  board, so adopting it left a quarter of the players untiered — and dense-ranking
  expert *overall* tiers together with computed *per-position* tiers made the two
  scales one incoherent number. (It is no longer PPR-only: ingestion scrapes all
  three cheatsheets and `rankings.vendor` picks the league's format, so the
  expert tier does follow the league's scoring now. The two reasons above are
  what still rule it out.) Both of its scales ship as display columns instead —
  see `db/column-catalog`.

  :injury-risk and :pos-rank ride along last, after everything that reads
  :points — they are board columns, not terms in any score, and nothing
  downstream consumes either.

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
     {:players            (-> (tiers/with-tiers vorped {:replacement-levels levels
                                                        :num-teams          num-teams})
                              ;; Injury risk is static — completed seasons and a
                              ;; preseason designation, neither of which a pick
                              ;; moves — so it belongs here rather than beside
                              ;; `tcm` in the live layer. Display only: it feeds
                              ;; no later stage. See `rankings.injury`.
                              injury/with-injury-risk
                              ;; Same shelf as :injury-risk — static, display
                              ;; only, read by nothing downstream. It belongs
                              ;; here and not in `live-valuation` precisely so
                              ;; it does *not* renumber as players are drafted:
                              ;; RB1 is an identifier for the whole draft, and
                              ;; the `#` column already answers the live
                              ;; question. See `rankings.pos-rank`.
                              pos-rank/with-pos-rank)
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
         total-slots (ls/total-slots league-state)
         valued      (value/calculate-value board budget total-slots
                                            (ls/streamed-slots league-state))
         infl        (inflation/auction-inflation valued league-state)
         heat        (inflation/draft-phase-decay league-state)
         ;; per-position inflation replaces the single global scalar; reduces to
         ;; `infl` for positions with no picks.
         pos-infl    (idx/per-position-inflation valued league-state infl)
         ;; position tilt x phase decay, held inside one band at the end — see
         ;; `inflation/clamp-to-band` for why the band lives here and nowhere else
         infl-fn     (fn [p] (inflation/clamp-to-band
                              (* (get pos-infl (:position p) infl) heat)))
         ;; The banded figure the board actually prices at, shipped rather than
         ;; left for the client to recompose: the header used to multiply
         ;; :inflation by :market-heat itself and skip the band, so it read ×0.40
         ;; where the board was pricing at 0.50.
         mult        (inflation/clamp-to-band (* infl heat))
         priced      (value/calculate-price valued infl-fn
                                            (:drafted-player-ids league-state))
         with-barg   (mapv (fn [p]
                             (assoc p :bargain
                                    (if (> (:worth p) 0) (- (:value p) (:worth p)) 0)))
                           priced)]
     {:players             with-barg
      :replacement-levels  (:replacement-levels static-result)
      :inflation           infl
      :market-multiplier   mult
      :inflation-index     (idx/inflation-index valued (:picks league-state))
      :market-heat         heat}))
