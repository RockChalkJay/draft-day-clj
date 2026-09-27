(ns draft-day.rankings.faab
  "What to bid on a waiver claim, given the managers who will bid against you.

  `rankings.waiver` prices a free agent's `:walk-away`, his worth to you, and
  never looks at anybody else. FAAB is a blind first-price auction — you pay
  what you bid, and a tie goes to waiver order — so the bid that makes a budget
  go furthest is just over the highest rival's, and this namespace predicts
  that bid. It is dollars and chances only: who would start for whom arrives
  from `waiver/rival-needs`, and nothing here requires `rankings.waiver`, which
  requires this.

  Per free agent, `:bid` is the value bid, the one maximizing
  P(win | b) · (walk-away − b): the league minimum when nobody else wants him,
  otherwise about a dollar over the likeliest top rival bid, stepping over the
  round numbers bids pile onto. It never exceeds the walk-away unless the league
  minimum does. `:win-prob` is its chance, `:bid-sure` the cheapest bid that
  wins `sure-win` of the time (it may exceed the walk-away), `:rivals` how many
  others are expected to bid, and `:competition` the top rival bid's spread,
  the chance nobody bids and the likeliest bidders. Not `:market`: that is the
  draft board's dollar price, and one key on two scales is what `:ros-vorp` was
  renamed to avoid.

  Who bids: a rival makes `:per-week` claims (his `bid-history` profile) and
  shares them out over the whole wire in proportion to e^utility, a
  conditional logit whose `claim-weights` were fit to real claims. Most of the
  utility is the same for every rival — last week's game, the season so far,
  value over replacement, position, a player just dropped, and his `heat` on
  Sleeper's trending list — and a rival's own part is only whether he would
  start him. That is what real claims look like: a league chases the same few
  players, and aiming claims by each rival's own lineup gain left four bids in
  five on players the model gave no chance. The count is Poisson, so he bids
  on a player with chance 1 − e^(−rate); a fixed count, 1 − (1 − share)^λ,
  would make a rival with one target certain to bid however rarely he bids.

  Bids also pile up beyond anything the board can see, because a claim is
  usually news — an injury, a depth chart — that every rival reads too. So a
  player's week multiplies every rival's rate on him by one shared gamma draw
  (`cluster-spread`), and a price, being for a player I am claiming, averages
  over that draw updated by my claim (`hotness`): rivals sit out together in a
  quiet week and bid together in a hot one.

  What he bids: `bid-prior/bid-share` for the predicted bidder count and the
  phase, so a player everybody wants is priced like one, scaled by position,
  budget and his `:log-multiplier`. His $0 share moves the cell's $0 rate on the
  log-odds scale, round numbers are heaped as `bid-prior/heaping` measured, and
  his bids are capped at his `:faab-left` and floored at the league minimum.

  Who wins: given the player's week, rivals bid independently, so a bid's
  chance is a product over them averaged over the week (`joint`). Each sits it
  out, bids under it, or ties and loses on waiver order — the lower
  `:waiver-position` first, even odds when either is unknown.

  Every CHOSEN constant, and the estimate as a whole, stands until the replay
  backtest scores it."
  (:require [draft-day.bid-history :as bid-history]
            [draft-day.bid-prior :as prior]))

(def claim-weights
  "How a rival aims his claims, by league kind: the weights of a conditional
  logit over `claim-features` and `need-features`. MEASURED by
  `draft-day.faab.interest` — see its docstring for the leagues, the fit and
  its held-out score."
  {:redraft {:played    1.316
     :last-game 0.459
     :season    0.627
     :vorp      -0.865
     :vorp-sq   -0.788
     :dropped   0.892
     :pos/QB    -0.994
     :pos/RB    0.610
     :pos/TE    -0.326
     :pos/K     -1.921
     :pos/DST   -0.038
     :need?     0.644}
   :keeper  {:played    1.316
     :last-game 0.459
     :season    0.627
     :vorp      -0.865
     :vorp-sq   -0.788
     :dropped   0.892
     :pos/QB    -0.994
     :pos/RB    0.610
     :pos/TE    -0.326
     :pos/K     -1.921
     :pos/DST   -0.038
     :need?     0.644}
   :dynasty {:played    1.316
     :last-game 0.459
     :season    0.627
     :vorp      -0.865
     :vorp-sq   -0.788
     :dropped   0.892
     :pos/QB    -0.994
     :pos/RB    0.610
     :pos/TE    -0.326
     :pos/K     -1.921
     :pos/DST   -0.038
     :need?     0.644}})

(def cluster-spread
  "How many more bidders a claim draws than the claim rates alone would say,
  by league kind: one player's week multiplies every claim rate on him by a
  gamma of mean one and shape (the league's total rate on him) / the spread,
  so once somebody claims him the others expect about this many more bids,
  spread by their share of the interest, however likely he looked beforehand —
  which is what real bids do. MEASURED by `draft-day.faab.interest`."
  {:redraft 1.43 :keeper 1.43 :dynasty 1.43})

(defn kind-of
  "A league kind as `claim-weights` keys it: redraft, keeper or dynasty,
  whichever spelling it crossed the wire in, and redraft for a league that
  did not say — an ESPN league, or a sync stored before syncs said."
  [kind]
  (case (some-> kind name)
    "dynasty" :dynasty
    "keeper"  :keeper
    :redraft))

(defn model-for
  "`{:weights :spread}` for a league of `kind`."
  [kind]
  (let [k (kind-of kind)]
    {:weights (claim-weights k) :spread (cluster-spread k)}))

(def hotness-points
  "How many equally likely values of that multiplier a price averages over."
  24)

(def sure-win
  "The chance of winning that `:bid-sure` buys."
  0.9)

(def heat-weight
  "What the top of Sleeper's trending list adds to a player's claim utility,
  on `claim-weights`' scale. CHOSEN, and the replay backtest cannot rebuild
  past trending lists, so it waits on the snapshots `ingestion.sleeper-trending`
  keeps."
  1.0)

(def threat-floor
  "The least chance of bidding that names a rival in `:competition`. CHOSEN."
  0.05)

(defn round-to [places x]
  (let [k (Math/pow 10.0 places)]
    (/ (Math/round (* (double x) k)) k)))

(defn heat-of
  "`player -> 0..1`: where his `:trending/adds` sits on Sleeper's list, on a log
  scale from its least-added player to its most. Off the list, 0. Read over the
  whole `board` and not the free agents, since the most-added are mostly
  rostered and the hottest free agent would otherwise always read as the top."
  [board]
  (let [adds (keep :trending/adds board)]
    (if (< 1 (count (distinct adds)))
      (let [lo (Math/log (apply min adds))
            span (- (Math/log (apply max adds)) lo)]
        (fn [p] (if-let [n (:trending/adds p)] (/ (- (Math/log n) lo) span) 0.0)))
      (fn [p] (if (:trending/adds p) 1.0 0.0)))))

(defn clamp [lo hi x] (min hi (max lo (double x))))

(defn per-ten
  "A points figure over ten, a nil or a negative one as none."
  [x]
  (/ (max 0.0 (double (or x 0.0))) 10.0))

(defn claim-features
  "What every rival reads off free agent `p` the same way, for claims decided
  in `week`. Points a game are over ten; `:played` is a game in the last few
  weeks, and three-week form itself measured as nothing beside last week and
  the season. Value over replacement is clamped and squared, since interest
  peaks just below replacement: players well above it are rarely on waivers.
  A kicker or defense has none and is read by position, WR being the position
  with no feature. `:dropped` is a player let go this week or last."
  [{:keys [form-points last-points last-week season-ppg ros-vorp position dropped?]} week]
  (let [v (if ros-vorp (/ (clamp -60.0 30.0 ros-vorp) 30.0) 0.0)]
    (cond-> {:played    (if form-points 1.0 0.0)
             :last-game (per-ten (when (= last-week (dec week)) last-points))
             :season    (per-ten season-ppg)
             :vorp      v
             :vorp-sq   (* v v)
             :dropped   (if dropped? 1.0 0.0)}
      (and position (not= "WR" position)) (assoc (keyword "pos" position) 1.0))))

(defn need-features
  "What one rival reads off a free agent for himself: whether he would start
  him. How much he would add measured as nothing beyond that."
  [need]
  {:need? (if (pos? (double (or need 0.0))) 1.0 0.0)})

(defn utility
  "`features` weighed by `weights`, a feature with no weight counting nothing."
  [weights features]
  (reduce-kv (fn [acc k v] (+ acc (* (double (get weights k 0.0)) v))) 0.0 features))

(defn shared-utility
  "The part of every rival's utility for `p` that is the same for all of them,
  heat included, under one kind's `weights`."
  [weights p week heat]
  (+ (utility weights (claim-features p week))
     (* heat-weight (heat p))))

(defn claim-rates
  "`{player-id rate}`: how many of a rival's `per-week` claims land on each free
  agent on average — shared out in proportion to e^utility, his own need added
  to the `shared` utilities (one per free agent, in order), so they sum to
  `per-week` and no free agent draws none."
  [weights needs fas per-week shared]
  (if (and (seq fas) (pos? (double (or per-week 0.0))))
    (let [us    (mapv (fn [p s] (+ s (utility weights (need-features (get needs (:player-id p))))))
                      fas shared)
          top   (reduce max us)
          ws    (mapv #(Math/exp (- % top)) us)
          total (reduce + 0.0 ws)]
      (zipmap (map :player-id fas) (map #(* per-week (/ % total)) ws)))
    {}))

(defn bid-chance
  "The chance of at least one claim when claims arrive at `rate`."
  [rate]
  (- 1.0 (Math/exp (- (double rate)))))

(defn hotness-shape
  "The shape of a player's week multiplier when the league claims him at
  `total` a week, under a kind's `spread`: see `cluster-spread`."
  [spread total]
  (max 1e-6 (/ (double (or total 0.0)) spread)))

(defn marginal-bid-chance
  "The chance of at least one claim at `rate` before anything is known about
  the player's week: `bid-chance` averaged over his multiplier, `total` being
  the league's rate on him."
  [spread rate total]
  (let [a (hotness-shape spread total)]
    (- 1.0 (Math/pow (/ a (+ a (double rate))) a))))

(defn equal-mass
  "`k` equally likely points standing for a density given as `weights` at
  `xs`: the mean x within each k-th of the mass, a point's mass split where it
  straddles two."
  [xs weights k]
  (let [per (/ (reduce + 0.0 weights) k)]
    (loop [xs (seq xs) ws (seq weights) room per sum 0.0 out []]
      (let [x (first xs) w (first ws)]
        (cond
          (= (count out) k) out
          (nil? x)          (conj out (/ sum (- per room)))
          (<= w room)       (recur (next xs) (next ws) (- room w) (+ sum (* w x)) out)
          :else             (recur xs (cons (- w room) (next ws)) per 0.0
                                   (conj out (/ (+ sum (* room x)) per))))))))

(def claimed-points
  "`hotness-points` equally likely values of x = a·η once a claim at rate c·a
  has been made: the unit-rate gamma prior x^(a-1)e^(-x) times the claim's
  chance 1 - e^(-cx), which as c goes to 0 is proportional to x."
  (memoize
   (fn [a c]
     (let [n  800
           lo (Math/log 1e-6)
           hi (Math/log (+ a 45.0 (* 8.0 (Math/sqrt (+ a 1.0)))))
           xs (mapv #(Math/exp (+ lo (* % (/ (- hi lo) (dec n))))) (range n))
           ;; On a log grid each point carries x times the density.
           ws (mapv (fn [x]
                      (* (Math/pow x a) (Math/exp (- x))
                         (if (zero? c) x (- 1.0 (Math/exp (- (* c x)))))))
                    xs)]
       (equal-mass xs ws hotness-points)))))

(defn round-figures
  "`x` to three significant figures."
  [x]
  (let [k (Math/pow 10.0 (- 2 (Math/floor (Math/log10 x))))]
    (/ (Math/round (* x k)) k)))

(defn hotness
  "`hotness-points` equally likely values of the multiplier on every rival's
  claim rate for a player I am claiming at `mine` a week, the league, me
  included, at `total`: his week's gamma of shape a (`hotness-shape`) and mean
  one, conditioned on my claim. Tabulated on a to three figures and my rate
  over a to two places, which `spread` bounds, so a board computes a few
  hundred tables rather than one a player."
  [spread total mine]
  (let [a (round-figures (hotness-shape spread total))
        c (/ (Math/round (* 100.0 (/ (max 0.0 (double (or mine 0.0))) a))) 100.0)]
    (mapv #(/ % a) (claimed-points a c))))

(defn logit [p] (Math/log (/ p (- 1.0 p))))

(defn logistic [x] (/ 1.0 (+ 1.0 (Math/exp (- x)))))

(defn clamp-p [p] (clamp 0.01 0.99 p))

(defn zero-chance
  "How often a rival's bid is $0: his cell's rate, moved by how far his own $0
  share sits from the typical manager's."
  [cell-p-zero zero-share]
  (logistic (+ (logit (clamp-p cell-p-zero))
               (logit (clamp-p zero-share))
               (- (logit (clamp-p (get-in prior/managers [:zero-share :p50])))))))

(defn cdf-points
  "`[[dollars probability] ...]`, increasing in both, through which a positive
  bid's CDF runs. A cell's quantiles become `scale` times their share of
  `budget`, and where several land on one amount the highest is kept. Below the
  lowest, a tail reaches down to half of it; above the highest, one reaches as
  far as the last step would carry. Nothing goes past the budget."
  [quantiles scale budget]
  (let [cap    (double budget)
        pts    (->> (sort-by key quantiles)
                    (map (fn [[q share]] [(min cap (* scale share cap)) q]))
                    (partition-by first)
                    (mapv last))
        [lo]   (first pts)
        [hi]   (peek pts)
        before (first (peek (pop pts)))
        top    (min cap (if before (/ (* hi hi) before) hi))]
    (cond-> (into [[(/ lo 2.0) 0.0]] (pop pts))
      (> top hi)       (conj (peek pts) [top 1.0])
      (not (> top hi)) (conj [hi 1.0]))))

(defn cdf-at
  "The CDF through `pts` at `x` dollars, log-linear between points."
  [pts x]
  (let [[x0] (first pts)
        [xn] (peek pts)]
    (cond
      (<= x x0) 0.0
      (>= x xn) 1.0
      :else     (some (fn [[[xa ya] [xb yb]]]
                        (when (<= xa x xb)
                          (+ ya (* (- yb ya) (/ (Math/log (/ x xa)) (Math/log (/ xb xa)))))))
                      (partition 2 1 pts)))))

(defn positive-pmf
  "P(bid = b) for whole dollars b in 0..budget, from `cdf-points`. Each dollar
  takes the CDF within half a dollar of it, $1 everything under $1.50, and the
  whole budget everything from half a dollar short of it."
  ^doubles [pts budget]
  (let [out (double-array (inc budget))]
    (loop [b 1 below 0.0]
      (when (<= b budget)
        (let [upto (if (== b budget) 1.0 (double (cdf-at pts (+ b 0.5))))]
          (aset out b (- upto below))
          (recur (inc b) upto))))
    out))

(defn heap-scale
  "The round-number grain and rates for a budget. `bid-prior/heaping` measured
  $100 and $1000 leagues, and a budget reads as whichever it is nearer to on a
  log scale."
  [budget]
  (if (< budget (Math/sqrt (* 100.0 1000.0)))
    {:unit 5  :rates (:budget-100 prior/heaping)}
    {:unit 50 :rates (:budget-1000 prior/heaping)}))

(defn heap-group
  "Which heap a bid of `b` falls in, or nil under one grain."
  [b unit]
  (cond
    (< b unit)                 nil
    (zero? (mod b (* 2 unit))) :round-2x
    (zero? (mod b unit))       :round
    (= 1 (mod b unit))         :one-over
    :else                      :other))

(defn heap-shares
  "The share of bids from one grain up that lands in each heap group."
  [{:keys [round round-2x one-over]}]
  {:round-2x round-2x
   :round    (- round round-2x)
   :one-over one-over
   :other    (- 1.0 round one-over)})

(defn with-heaps
  "`pmf` with the round-number heaps put back: from one grain up, each group of
  amounts is scaled to carry the share of that mass `bid-prior/heaping`
  measured, and a group the distribution never reaches stays empty while the
  others share its mass. Shares rather than ratios to chance, because a falling
  distribution already puts more than chance on its first grain — scaled by
  ratio, a typical cell lands 48% on multiples of $5 against the 39.5%
  measured."
  ^doubles [^doubles pmf budget]
  (let [{:keys [unit rates]} (heap-scale budget)
        share   (heap-shares rates)
        n       (alength pmf)
        mass    (reduce (fn [m b]
                          (if-let [g (heap-group b unit)]
                            (update m g (fnil + 0.0) (aget pmf b))
                            m))
                        {} (range n))
        total   (reduce + 0.0 (vals mass))
        reached (reduce + 0.0 (keep (fn [[g m]] (when (pos? m) (share g))) mass))
        out     (aclone pmf)]
    (when (and (pos? total) (pos? reached))
      (doseq [b (range unit n)]
        (let [g (heap-group b unit)
              m (double (mass g))]
          (when (pos? m)
            (aset out b (* (aget pmf b) (/ (* total (/ (double (share g)) reached)) m)))))))
    out))

(defn bid-pmf
  "P(a rival bids b | he bids) for b in 0..budget, or nil when the league
  minimum is more than he has. `scale` multiplies the cell's positive shares.
  Mass under the minimum moves up to it, and mass over his `cap` moves down to
  it: a manager short of what he would bid bids what he has."
  ^doubles [{:keys [p-zero positive]} scale zero-share budget min-bid cap]
  (when (<= min-bid cap)
    (let [pos   (with-heaps (positive-pmf (cdf-points positive scale budget) budget) budget)
          z     (double (zero-chance p-zero zero-share))
          out   (double-array (inc budget))
          _     (aset out 0 z)
          _     (doseq [b (range 1 (inc budget))]
                  (aset out b (* (- 1.0 z) (aget pos b))))
          under (reduce + 0.0 (map #(aget out %) (range 0 min-bid)))
          over  (reduce + 0.0 (map #(aget out %) (range (inc cap) (inc budget))))]
      (doseq [b (concat (range 0 min-bid) (range (inc cap) (inc budget)))]
        (aset out b 0.0))
      (aset out min-bid (+ (aget out min-bid) under))
      (aset out cap (+ (aget out cap) over))
      out)))

(defn cumulative
  "Running sums of a pmf: P(bid <= b)."
  ^doubles [^doubles pmf]
  (let [n   (alength pmf)
        out (double-array n)]
    (loop [b 0 acc 0.0]
      (when (< b n)
        (let [acc (+ acc (aget pmf b))]
          (aset out b acc)
          (recur (inc b) acc))))
    out))

(defn team-log-shift
  "What the backbone expects of a league of `teams`, as a natural log."
  [teams]
  (let [size (cond (<= teams 8) :small (<= teams 12) :standard :else :large)]
    (get-in prior/team-shift [size :log-shift] 0.0)))

(defn bid-scale
  "What a rival's positive bids are multiplied by, off the backbone's $100
  cell: his league's budget, the player's position, and his own
  `:log-multiplier`."
  [bucket position budget log-multiplier]
  (Math/exp (+ (if (== 100 budget) 0.0 (get-in prior/budget-shift [bucket :log-shift]))
               (get-in prior/position-shift [position :log-shift] 0.0)
               log-multiplier)))

(def cap-quantile
  "Which positive bid, in an auction four or more managers competed for, a
  walk-away may not exceed: near the expensive end of what the market pays.
  CHOSEN."
  0.9)

(defn market-cap
  "The most a claim at `position` should be worth, in the league's dollars:
  `cap-quantile` of the backbone's four-or-more-bidder cell for the week's
  phase, moved by the position's and the budget's measured shifts. A kicker or
  a defense lands near a sixth of a $100 budget, a quarterback over half. nil
  without a budget."
  [position budget week]
  (when (and (number? budget) (pos? budget))
    (let [share (get-in prior/bid-share [[4 (prior/phase week)] :positive cap-quantile])
          shift (+ (get-in prior/position-shift [position :log-shift] 0.0)
                   (if (== 100 budget) 0.0 (get-in prior/budget-shift [4 :log-shift])))]
      (* budget share (Math/exp shift)))))

(defn tie-chance
  "The chance I win a tie with a rival: waiver order, the lower position first,
  and even odds when either side's is unknown."
  [mine theirs]
  (if (and (number? mine) (number? theirs))
    (if (< mine theirs) 1.0 0.0)
    0.5))

(defn joint
  "The chance that every rival either sits it out or, bidding, does what `f`
  gives the chance of — averaged over the player's hotness, since the rivals
  move together with it. Each rival carries `:ps`, his chance of bidding at
  each hotness point, or a single `:p` that holds at every point."
  [rivals f]
  (if (empty? rivals)
    1.0
    (let [k   (reduce max 1 (keep #(some-> ^doubles (:ps %) alength) rivals))
          acc (double-array k 1.0)]
      (doseq [r rivals]
        (let [x  (double (f r))
              ^doubles ps (:ps r)
              p1 (double (or (:p r) 0.0))]
          (dotimes [i k]
            (let [p (if ps (aget ps i) p1)]
              (aset acc i (* (aget acc i) (+ (- 1.0 p) (* p x))))))))
      (/ (areduce acc i sum 0.0 (+ sum (aget acc i))) k))))

(defn uncontested
  "The chance no rival bids at all."
  [rivals]
  (joint rivals (constantly 0.0)))

(defn win-chance
  "The chance a bid of `b` wins. Every rival either sits it out, bids under it,
  or ties it and loses the tie. Each rival carries `:pmf` and `:cdf` over his
  bid and `:tie`, beside his chances of bidding (see `joint`)."
  [rivals b]
  (joint rivals
         (fn [{:keys [^doubles pmf ^doubles cdf tie]}]
           (let [n     (alength pmf)
                 below (cond (<= b 0) 0.0 (<= b n) (aget cdf (dec b)) :else 1.0)
                 at    (if (< b n) (aget pmf b) 0.0)]
             (+ below (* tie at))))))

(defn value-bid
  "The bid maximizing P(win | b) · (worth − b), from the league minimum up to
  the walk-away and what is left. It is the minimum itself when the walk-away
  does not reach it, the cheaper bid on a tie, and nil when even the minimum is
  more than is left."
  [rivals worth min-bid left]
  (when (<= min-bid left)
    (let [cap (max min-bid (min worth left))]
      (first (reduce (fn [[_ best :as acc] b]
                       (let [v (* (win-chance rivals b) (- worth b))]
                         (if (> v best) [b v] acc)))
                     [nil ##-Inf]
                     (range min-bid (inc cap)))))))

(defn first-true
  "The least whole number in `lo`..`hi` that `pred` holds for, or nil, `pred`
  being false and then true across that range: a binary search, since each
  test here averages every rival over every hotness point."
  [pred lo hi]
  (when (and (<= lo hi) (pred hi))
    (loop [lo lo hi hi]
      (if (= lo hi)
        lo
        (let [mid (quot (+ lo hi) 2)]
          (if (pred mid) (recur lo mid) (recur (inc mid) hi)))))))

(defn sure-bid
  "The cheapest bid that wins `sure-win` of the time, or nil when nothing up to
  what is left does. The chance only rises with the bid."
  [rivals min-bid left]
  (first-true #(>= (win-chance rivals %) sure-win) min-bid left))

(defn top-bid
  "`[p50 p90]` of the highest rival bid, given that somebody bids, or nil when
  nobody is expected to."
  [rivals budget]
  (let [none (uncontested rivals)]
    (when (< none 1.0)
      (let [upto (fn [b]
                   (/ (- (joint rivals (fn [{:keys [^doubles cdf]}]
                                         (aget cdf (min b (dec (alength cdf))))))
                         none)
                      (- 1.0 none)))
            at   (fn [q] (first-true #(>= (upto %) (- q 1e-9)) 0 budget))]
        [(at 0.5) (at 0.9)]))))

(defn threats
  "The rivals likeliest to bid, at most three, as the tooltip names them."
  [rivals]
  (->> rivals
       (filter #(>= (:p %) threat-floor))
       (sort-by (juxt (comp - :p) #(- (or (:faab-left %) 0))))
       (take 3)
       (mapv #(-> (select-keys % [:roster-id :name :faab-left :style])
                  (assoc :p (round-to 3 (:p %)))))))

(defn league-habits
  "Every manager's bidding habits off the league's history, and the league's
  own price level: `{:league-log :profiles :history}`. The level starts where
  the backbone expects a league of `teams` to sit, so a league with no history
  is priced like any Sleeper league its size."
  [history teams]
  (let [center (team-log-shift teams)
        {:keys [log-multiplier profiles]} (bid-history/profiles history center)]
    {:league-log (or log-multiplier center)
     :profiles   (or profiles [])
     :history    history}))

(defn rival-habits
  "A rival's profile, or a silent manager's when he has never bid."
  [team {:keys [profiles league-log history]}]
  (or (bid-history/team-profile team profiles)
      (bid-history/silent-profile history league-log)))

(defn bidders
  "The rivals able to bid at all, each with his habits, his claim rates under
  `weights`, his cap and his tie odds against me."
  [weights rivals me fas habits budget min-bid shared]
  (->> rivals
       (map (fn [r]
              (let [h (rival-habits r habits)]
                (assoc r
                       :style (:style h)
                       :habits h
                       :cap (long (min budget (or (:faab-left r) budget)))
                       :rates (claim-rates weights (:needs r) fas (:per-week h) shared)
                       :tie (tie-chance (:waiver-position me) (:waiver-position r))))))
       (filterv #(<= min-bid (:cap %)))))

(defn market
  "What pricing one manager's claims needs, built once for all his free agents:
  the league's budget and minimum, what he has left, every rival able to bid
  (`bidders`) and `:dist`, each rival's bid distribution by position and bidder
  count. `rivals` is `waiver/rival-needs`, `me` the manager's own team, `habits`
  `league-habits`, `week` the week claims are decided in, and `heat` is
  `heat-of` over the whole board, or none. `kind` picks the weights and the
  spread (`model-for`). `:mine` is my own claim rate on each free agent, my
  part of the league's interest in him."
  [fas {:keys [rivals me waiver habits week heat kind] :or {heat (constantly 0.0)}}]
  (let [budget  (long (or (:budget waiver) 0))
        min-bid (long (or (:min-bid waiver) 0))
        phase   (prior/phase week)
        {:keys [weights spread]} (model-for kind)
        shared  (when (seq rivals) (mapv #(shared-utility weights % week heat) fas))
        rivals  (bidders weights rivals me fas habits budget min-bid shared)]
    {:budget  budget
     :spread  spread
     :min-bid min-bid
     :left    (some-> (:faab-left me) long (min budget))
     :rivals  rivals
     ;; Mine, with my need read as `waiver/rival-needs` reads a rival's: part
     ;; of the league's interest in a player, which sets what a claim says.
     :mine    (when (seq rivals)
                (claim-rates weights (zipmap (map :player-id fas) (map #(or (:lineup-upgrade %) (:upgrade %)) fas))
                             fas (:per-week (rival-habits me habits)) shared))
     ;; One distribution per rival, position and bidder count, not per
     ;; player: a few dozen, where there are hundreds of free agents.
     :dist    (memoize
               (fn [i position bucket]
                 (let [{:keys [habits cap]} (rivals i)
                       pmf (bid-pmf (get prior/bid-share [bucket phase])
                                    (bid-scale bucket position budget (:log-multiplier habits))
                                    (:zero-share habits) budget min-bid cap)]
                   [pmf (cumulative pmf)])))}))

(defn active-rivals
  "The rivals who might bid for `p` once I have claimed him, each with `:ps`,
  his chance of bidding at each of the player's `hotness` points, `:p` their
  mean, and `:pmf`/`:cdf` over what he would bid. The bidder count that picks
  their distributions is the one predicted for this player."
  [p {:keys [rivals dist mine spread]}]
  (let [rates  (mapv #(double (get (:rates %) (:player-id p) 0.0)) rivals)
        mine   (double (get mine (:player-id p) 0.0))
        etas   (hotness spread (+ (reduce + 0.0 rates) mine) mine)
        pss    (mapv (fn [rate] (double-array (map #(bid-chance (* rate %)) etas))) rates)
        ps     (mapv (fn [^doubles a] (/ (areduce a i s 0.0 (+ s (aget a i))) (alength a))) pss)
        bucket (prior/bucket (max 1 (Math/round (+ 1.0 (reduce + 0.0 ps)))))]
    (into []
          (keep-indexed (fn [i r]
                          (let [pi (ps i)]
                            (when (pos? pi)
                              (let [[pmf cdf] (dist i (:position p) bucket)]
                                (assoc (select-keys r [:roster-id :name :faab-left :style :tie])
                                       :p pi :ps (pss i) :pmf pmf :cdf cdf))))))
          rivals)))

(defn competition
  "One free agent's `:bid`, `:win-prob`, `:bid-sure`, `:rivals` and
  `:competition` in `market` `m`, bidding up to `worth`."
  [p worth {:keys [min-bid left budget] :as m}]
  (let [active (active-rivals p m)
        bid    (value-bid active worth min-bid left)]
    {:bid         bid
     :win-prob    (when bid (round-to 3 (win-chance active bid)))
     :bid-sure    (sure-bid active min-bid left)
     :rivals      (round-to 2 (reduce + 0.0 (map :p active)))
     :competition {:top         (top-bid active budget)
                   :uncontested (round-to 3 (uncontested active))
                   :threats     (threats active)}}))

(defn with-market
  "Assoc `competition` onto every free agent with a `:walk-away`; the rest get a
  nil `:bid`, since a league that does not run FAAB, or a budget already spent,
  has nothing to bid. `ctx` is `market`'s."
  [fas ctx]
  (let [{:keys [left budget] :as m} (market fas ctx)]
    (mapv (fn [p]
            (let [worth (:walk-away p)]
              (if (and worth left (pos? budget))
                (merge p (competition p worth m))
                (assoc p :bid nil))))
          fas)))

(defn bidding
  "The envelope's account of what the bids were priced from: the league's own
  history or the Sleeper-wide backbone alone, how much history there is, the
  league's price level against Sleeper's, and the league minimum."
  [{:keys [league-log history]} waiver]
  (let [seasons  (:seasons history)
        auctions (reduce + 0 (map (comp count :auctions) seasons))]
    {:source            (if (pos? auctions) :league :sleeper-wide)
     :auctions          auctions
     :seasons           (mapv :season seasons)
     :league-multiplier (round-to 3 (Math/exp league-log))
     :min-bid           (or (:min-bid waiver) 0)}))
