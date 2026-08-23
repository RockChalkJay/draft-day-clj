(ns draft-day.benchmark.auction
  "Draft the season by auction instead of by snake, and score the team that
  results.

  The snake simulator can never measure most of the engine. A snake pick is a
  choice of *player* and nothing else, so value, inflation and phase decay cannot
  affect it — the only thing under test is board order. An auction is a choice of
  *how much*, which is what those three actually compute, so this is where the
  dollar math becomes falsifiable.

  The model seat bids up to `:worth`, recomputed before every nomination against
  the live state of the room. That is not an approximation of the shipped
  behaviour, it is the shipped behaviour: `rankings.value/calculate-price`
  already returns whole dollars, inflation-adjusted, floored at the minimum bid —
  a max bid in everything but name.

  The field bids a price curve measured from real auctions
  (`replay.price-curve`), because nothing published carries vintage auction
  dollars and an invented curve would make the simulator reward whoever deviated
  least from the invention.

  Field seats do not all read the curve the same way. They disagree by as much
  as real rooms disagree — `price-curve/spread`, about 15% at the top of the
  board and 40% in the tail — and getting that wrong is what a first version of
  this got wrong. With eleven seats naming one number to the dollar, the
  marginal bidder sits exactly at the mean: a seat bidding $1 more wins every
  contest it enters, a seat bidding $1 less wins none. Measured, that step
  function cost a market+$1 seat 13 points a game and a market-$1 seat 15 —
  both worse than bidding Worth — so the simulator was scoring *being different*
  rather than being wrong, and the deviation it punished hardest was the one
  closest to correct.

  What it says with a field that disagrees, over 2021-2025: bidding Worth is
  **indistinguishable from simply paying the going rate**, +70 points a season,
  CI [-93, +233]. The report's top-buy column says the seat is doing something —
  it pays $75 for its most expensive player where the average field seat pays
  $52, so Worth does sit above the market at the top of the board — but buying
  there neither wins nor loses over five seasons.

  The controls that make that readable, all on the same five seasons, per game:

    market (identity)   +0.07  [-0.61, +0.52]   the null
    market +/- $1       +0.83 / -0.33           deviation itself is ~free
    market x 1.1        +0.19  [-2.12, +2.36]
    market x 0.9        -8.83  [-12.15, -5.50]  the metric still has teeth
    worth               +2.71  [-3.15, +8.97]

  The null is `:model-bid` set to the identity, which makes the model an
  ordinary field seat. It shows the seat is not special-cased in the bidding
  path and nothing more — a seat bidding the market is not deviating, so it
  cannot speak to whether deviation is treated fairly. The +/-$1 controls are
  what does that, and they are the reason to believe the Worth number now and
  not before. (The null lands at +0.07 rather than 0 because an obligated seat
  bids a dollar where a field seat would bid nothing; see `must-bid?`.)

  ## Next: does inflation earn its keep?

  Worth is Value scaled by live inflation and phase decay. This run says the
  three of them together come out level with the market, which is not the same
  as saying each earns its place, and the seam to find out is already here —
  `:model-bid` receives `[market worth]`, and the board carries `:value`
  alongside `:worth`.

  Three seats over the same seasons and the same curve:

    market   `(fn [m _] m)`   the null, and the baseline the other two answer to
    worth    the default                    Value x inflation x phase decay
    value    bid `:value`                   the same dollars before either scaling

  If `value` beats `worth`, the live scaling is subtracting; if they tie, it is
  free motion the shipped board pays for on every pick; if `worth` wins, the
  scaling is what is holding the seat level with the market. Roughly 3x the
  runtime of one mode, since each seat is a full re-simulation of every season.
  Five seasons put the interval at about +/-160 points, so a tie there means
  too thin to call, not no difference.

  Two things to get right when it runs. Passing `:value` through `:model-bid`
  means threading the board's `:value` into the callback, which today only sees
  the market and Worth — widen the callback rather than reaching for the static
  board inside the seat, or the value seat will price against a stale pool.
  And `:value` is not inflation-adjusted, so it is calibrated to the pool at the
  *start* of the draft: a value seat necessarily overbids late, which is the
  behaviour under test and not a bug to correct on the way in."
  (:require [draft-day.benchmark.metrics :as metrics]
            [draft-day.benchmark.simulate :as simulate]
            [draft-day.replay.price-curve :as price-curve]
            [draft-day.replay.core :as replay]
            [draft-day.rankings.engine :as engine]
            [draft-day.rankings.replacement :as replacement]))

(def default-config
  "The snake simulator's league, plus money.

  Roster size is `:rounds`, so an auction fills the same number of seats the
  snake draft would — the two metrics stay comparable.

  `:vorp?` is dropped rather than inherited. It selects the snake seat's board
  ordering, and an auction has no board order to select: `static-board` computes
  VORP unconditionally because the valuation chain is built on it. Left in place
  it would read as a switch that does nothing."
  (-> simulate/default-config (dissoc :vorp?) (assoc :budget 200)))

(defn roster-slots
  "Slot counts for one team: the simulated starting lineup, with the remaining
  rounds as bench."
  [{:keys [rounds] :as config}]
  (let [{:keys [qb rb wr te flex]} (simulate/replacement-config config)
        starters (+ qb rb wr te flex)]
    {:qb qb :rb rb :wr wr :te te :flex flex
     :k 0 :dst 0                                  ; the benchmark pool has neither
     :bench (max 0 (- rounds starters))}))

(defn static-board
  "The static half of the valuation chain over an already-scored pool.

  `engine/static-rankings` would re-run the ranking model, discarding the
  `:points` the benchmark just computed for the model under test. Only
  replacement and VORP are needed downstream — tiers are display-only and
  floor/ceiling feeds nothing here — so those two run directly."
  [players config]
  (let [levels (replacement/replacement-levels
                players (:teams config) (simulate/replacement-config config) :points)]
    {:players (replacement/with-vorp players levels :points)
     :replacement-levels levels}))

;; ---- what a team may do ------------------------------------------------------

(defn open-slots [team]
  (count (remove :player-id (:roster team))))

(defn max-bid
  "The most this team can commit and still put a dollar on every remaining seat.

  The reserve is what stops a bidder buying a star and fielding an incomplete
  roster, and it is also the guard that keeps `replay/apply-pick` — which will
  happily drive a bankroll negative — from ever being handed a price a team
  cannot pay."
  [team]
  (let [open (open-slots team)]
    (if (pos? open)
      (max 0 (- (long (:bankroll team)) (dec open)))
      0)))

(defn- position-counts [team by-id]
  (frequencies (keep #(some-> (:player-id %) by-id :position) (:roster team))))

(defn open-starter-slots
  "Starting slots a roster of `counts` still has to fill.

  Flex is counted from the spare bodies the fixed slots did not claim, so a team
  holding three running backs against two RB slots has already covered its flex."
  [counts {:keys [starters flex flex-positions]}]
  (let [exact (reduce + 0 (map (fn [[pos n]] (max 0 (- n (get counts pos 0)))) starters))
        spare (reduce + 0 (map (fn [pos] (max 0 (- (get counts pos 0) (get starters pos 0))))
                               flex-positions))]
    (+ exact (max 0 (- flex (min flex spare))))))

(defn can-take?
  "May this team bid on this player?

  Three conditions, and the third is the one the snake simulator never needed.
  There must be an empty seat; the position must be under its cap, which stops a
  greedy board buying six quarterbacks the way `simulate/pick` does; and the
  purchase must leave enough seats to still field a legal lineup.

  Without that last rule the model seat can draft itself out of the metric.
  Worth is position-blind by construction — dollars per VORP, so a back and a
  receiver of equal VORP are worth the same — and a seat spending purely on that
  will happily fill its last seats at a position it is already deep in. What it
  scores then is roster construction, not the dollar math this exists to test.

  Not a thumb on the scale: no manager drafts a lineup they cannot start, so
  this is a constraint the shipped board is already used under rather than one
  invented here. It binds only near the end, when open seats run short of
  starter holes."
  [team player by-id {:keys [rounds caps] :as config}]
  (let [counts (position-counts team by-id)
        pos    (:position player)]
    (and (pos? (open-slots team))
         (< (get counts pos 0) (get caps pos 99))
         (let [after (update counts pos (fnil inc 0))
               total (reduce + 0 (vals after))]
           (<= (open-starter-slots after config) (max 0 (- rounds total)))))))

(defn must-bid?
  "Would this player fill a starting slot the team still owes?

  `value/calculate-price` prices only what the board valued at a dollar or more
  and returns 0 for the rest, so Worth alone says nothing at all about the whole
  below-replacement tail — and a seat reading only Worth declines it, then
  starts nobody at the position it never got round to. No manager does that:
  with an empty RB2 slot and a dollar, they bid the dollar.

  Only a floor, never a raise. The obligation puts $1 behind a hole-filling
  player and stops there, so the seat still loses every contested bid and fills
  its holes with what the room did not want — which is exactly what a manager
  short of money does, and what makes holding budget back pay off or not."
  [team player by-id config]
  (let [counts (position-counts team by-id)]
    (< (open-starter-slots (update counts (:position player) (fnil inc 0)) config)
       (open-starter-slots counts config))))

;; ---- the auction -------------------------------------------------------------

(defn nomination-order
  "Who gets nominated, best first. Real rooms put the expensive players up early,
  and the price curve is indexed by exactly that ordering."
  [players]
  (sort-by #(double (or (:adp %) (:ecr %) Double/MAX_VALUE)) players))

(defn jitter
  "One seat's private opinion of the going rate, as a multiplier.

  Deterministic in `(team-id, rank)` — the benchmark has to be reproducible, and
  a fresh RNG per run would make two identical invocations disagree by more than
  the effects being measured.

  Uniform with standard deviation `cv`, shifted down so that the *second highest*
  of `n-bidders` draws sits on the curve rather than above it. The measured
  spread is the scatter of what rooms actually *paid*, which is already a
  winning bid; centring the field on it and then taking the top of eleven draws
  would price every player above what any real room paid. For a uniform of
  half-width w, the second-highest of n has mean 1 + w(1 - 4/(n+1)), so the
  centre comes down by exactly that."
  [team-id rank cv n-bidders]
  (if-not (pos? cv)
    1.0
    (let [w (* (Math/sqrt 3.0) cv)
          c (* w (- 1.0 (/ 4.0 (inc (double n-bidders)))))
          u (/ (double (mod (bit-and (hash [team-id rank]) 0x7fffffff) 10007)) 10007.0)]
      (max 0.0 (+ 1.0 (- c) (* w (- (* 2.0 u) 1.0)))))))

(defn willingness
  "The most a seat would go to for this player, before its bankroll is consulted.

  A field seat wants the going rate as *it* sees it — the curve, moved by this
  seat's private opinion. Eleven seats reading one number to the dollar is what
  made the simulator a step function; see `price-curve/spread`.

  The model seat wants its Worth, except on a seat it is obligated to fill,
  where a dollar beats an empty starting slot.

  One function for both sides on purpose. `:model-bid` receives *this seat's*
  jittered market, so passing it `(fn [m _] m)` makes the model an ordinary
  field seat — which is the null this simulator has to be able to run. Note what
  that null does and does not show: it confirms the model seat is not
  special-cased anywhere in the bidding path, and nothing more. It cannot prove
  the design treats a *deviating* seat fairly, because a seat bidding the market
  is not deviating. The controls that test that are the ones a dollar either
  side of the market."
  [team model-id market cv n-bidders rank worth player by-id {:keys [model-bid] :as config}]
  (let [mine (Math/round (* (double market) (jitter (:team-id team) rank cv n-bidders)))]
    (if (= model-id (:team-id team))
      (let [want (if model-bid (model-bid mine worth) worth)]
        (if (must-bid? team player by-id config) (max 1 want) want))
      mine)))

(defn- winner
  "Resolve one nomination: `[team-id price]`, or nil if nobody bids.

  Every eligible seat names a price, capped by what its bankroll can cover with a
  dollar left on each remaining seat. Highest wants it; ties go to the deepest
  pocket, which is what keeps a field that happens to agree from clustering its
  buys on one team.

  The winner pays the runner-up plus a dollar, not its own number — an auction
  charges what it took to win, not what you would have gone to. That is also
  what makes the endgame informative: once the field is down to its reserve, the
  runner-up bid collapses and a seat that held money back buys the tail cheaply."
  [state model-id market cv rank worth by-id config player]
  (let [able (filter #(can-take? % player by-id config) (:teams state))
        ;; at least two: the centring in `jitter` is defined by where the
        ;; runner-up lands, and one bidder has no runner-up.
        n    (max 2 (dec (count (:teams state))))
        bids (->> able
                  (map (fn [t]
                         {:team t
                          :bid  (min (max-bid t)
                                     (long (max 0 (willingness t model-id market cv n rank
                                                               worth player by-id config))))}))
                  (filter #(pos? (:bid %)))
                  (sort-by (juxt #(- (:bid %)) #(- (max-bid (:team %))))))]
    (when-let [{:keys [team bid]} (first bids)]
      [(:team-id team) (max 1 (min bid (inc (long (:bid (second bids) 0)))))])))

(defn simulate-season
  "One season, model in one seat.

  Returns `{:model-points :field-mean :edge :model-top-buy :field-top-buy}`. The
  top-buy pair is the mechanism, not decoration: the two ways to lose an auction
  are paying too much for stars and fading them, and the edge alone cannot say
  which happened. It is the priciest single player each seat bought, averaged
  over the field."
  [players model-seat config truth-key clearing spread]
  (let [{:keys [teams budget]} config
        by-id    (into {} (map (juxt :player-id identity)) players)
        static   (static-board players config)
        order    (nomination-order players)
        last-i   (dec (count clearing))
        start    (replay/base-state teams budget (roster-slots config))
        ;; read back rather than rebuilt: `replay/base-state` numbers teams by
        ;; Sleeper's roster_id, a convention that belongs to replaying real
        ;; drafts. Reconstructing it here would fail silently if it changed --
        ;; no seat would match, every seat would bid the market, and the run
        ;; would report a clean 0.0 that looks exactly like the null.
        model-id (:team-id (nth (:teams start) model-seat))]
    (loop [state start
           [p & more] order
           spend {}
           i 0]
      (if (or (nil? p)
              (every? #(zero? (open-slots %)) (:teams state)))
        (let [pts (into {} (map (fn [t]
                                  [(:team-id t)
                                   (simulate/best-lineup-points
                                    (keep #(some-> (:player-id %) by-id) (:roster t))
                                    config truth-key)]))
                        (:teams state))
              top (into {} (map (fn [[id ps]] [id (reduce max 0.0 ps)])) spend)
              mine (get pts model-id 0.0)
              field (metrics/mean (map val (dissoc pts model-id)))]
          {:model-points  mine
           :field-mean    field
           :edge          (- mine field)
           :model-top-buy (get top model-id 0.0)
           :field-top-buy (metrics/mean (map val (dissoc top model-id)))})
        (let [live   (engine/live-valuation static state)
              worth  (or (:worth (first (filter #(= (:player-id %) (:player-id p))
                                                (:players live))))
                         0)
              j      (min i last-i)]
          (if-let [[tid price] (winner state model-id (nth clearing j) (nth spread j)
                                       j worth by-id config p)]
            (recur (replay/apply-pick state {:player-id (:player-id p)
                                             :position  (:position p)
                                             :price     (double price)
                                             :team-id   tid})
                   more
                   (update spend tid (fnil conj []) (double price))
                   (inc i))
            (recur state more spend (inc i))))))))

(defn simulate-all-seats
  "Average edge over every seat, so nomination luck and seat order cancel."
  [players config truth-key clearing spread]
  (let [runs (mapv #(simulate-season players % config truth-key clearing spread)
                   (range (:teams config)))
        avg  (fn [k] (metrics/mean (map k runs)))]
    {:edge          (avg :edge)
     :model-points  (avg :model-points)
     :field-mean    (avg :field-mean)
     :model-top-buy (avg :model-top-buy)
     :field-top-buy (avg :field-top-buy)
     :by-seat       (mapv :edge runs)}))

(defn market-prices
  "What the room pays by rank, and how much it disagrees: `{:clearing :spread}`.

  Both measured from the corpus of collected real auctions and both built at the
  simulation's own pick count via `price-curve/for-picks`, because the curve is
  indexed in rank-fraction space: a grid of any other size prices every rank
  from the wrong slice of the distribution, silently and plausibly."
  [{:keys [teams rounds budget]}]
  (let [picks  (* teams rounds)
        drafts (price-curve/standard-drafts (price-curve/load-drafts))]
    {:clearing (price-curve/clearing-prices (price-curve/for-picks drafts picks)
                                            (* teams budget))
     :spread   (price-curve/spread drafts picks)}))

(defn run
  "Per season, the model's auctioned-team edge over the field.
  `results` is a `core/run` output. Returns [{:season :edge ...}].

  The 4-arity takes the market directly. `market-prices` reads a corpus that
  lives in a gitignored cache, so anything that must run without it — the tests
  — supplies its own."
  ([results truth-key] (run results truth-key default-config))
  ([results truth-key config] (run results truth-key config (market-prices config)))
  ([results truth-key config {:keys [clearing spread]}]
   (into []
         (comp (remove :skipped?)
               (map (fn [{:keys [season players]}]
                      (assoc (simulate-all-seats players config truth-key clearing spread)
                             :season season))))
         results)))
