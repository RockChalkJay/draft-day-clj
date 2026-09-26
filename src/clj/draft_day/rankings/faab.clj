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
  aims them by `interest-weight` — what a player adds to his own starting
  lineup, plus a CHOSEN fraction of value over replacement for the stash
  claimed with no hole to fill, plus the player's `heat` on Sleeper's trending
  list, which is news no projection has caught up with. Heat adds rather than
  multiplies, so a backup the whole site is adding the day his starter goes
  down draws claims while his projection still says he is nobody. The count is
  Poisson, so he bids on a player with chance 1 − e^(−rate); a fixed count,
  1 − (1 − share)^λ, would make a rival with one target certain to bid however
  rarely he bids.

  What he bids: `bid-prior/bid-share` for the predicted bidder count and the
  phase, so a player everybody wants is priced like one, scaled by position,
  budget and his `:log-multiplier`. His $0 share moves the cell's $0 rate on the
  log-odds scale, round numbers are heaped as `bid-prior/heaping` measured, and
  his bids are capped at his `:faab-left` and floored at the league minimum.

  Who wins: rivals bid independently, so a bid's chance is a product over them.
  Each sits it out, bids under it, or ties and loses on waiver order — the lower
  `:waiver-position` first, even odds when either is unknown.

  Every CHOSEN constant, and the estimate as a whole, stands until the replay
  backtest scores it."
  (:require [draft-day.bid-history :as bid-history]
            [draft-day.bid-prior :as prior]))

(def speculative-weight
  "The interest a point of rest-of-season value over replacement draws, against
  a point added to the rival's starting lineup. CHOSEN."
  0.5)

(def sure-win
  "The chance of winning that `:bid-sure` buys."
  0.9)

(def heat-weight
  "The interest the most-added player on Sleeper draws from heat alone, as a
  share of what the rival's best target draws on need: at 1.0 the top of the
  list competes with his best target even with no need at all. CHOSEN, and the
  replay backtest cannot rebuild past trending lists, so it waits on the
  snapshots `ingestion.sleeper-trending` keeps."
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

(defn interest-weight
  "How much of a rival's attention a free agent draws on football alone; see
  the ns docstring. Heat is added by `claim-rates`, which knows the scale."
  [need ros-vorp]
  (+ (max 0.0 (double (or need 0.0)))
     (* speculative-weight (max 0.0 (double (or ros-vorp 0.0))))))

(defn claim-rates
  "`{player-id rate}`: how many of a rival's `per-week` claims land on each free
  agent on average. The rates sum to `per-week` whenever anybody draws his
  interest. A player who draws none is absent. Heat is scaled by the most any
  free agent draws from this rival on football, so it is in his own points
  whatever the week and whatever his roster."
  [needs fas per-week heat]
  (let [base  (mapv #(interest-weight (get needs (:player-id %)) (:ros-vorp %)) fas)
        top   (reduce max 0.0 base)
        ws    (mapv #(+ %1 (* heat-weight top (heat %2))) base fas)
        total (reduce + 0.0 ws)]
    (if (pos? total)
      (into {}
            (keep (fn [[p w]] (when (pos? w) [(:player-id p) (* per-week (/ w total))])))
            (map vector fas ws))
      {})))

(defn bid-chance
  "The chance of at least one claim when claims arrive at `rate`."
  [rate]
  (- 1.0 (Math/exp (- (double rate)))))

(defn logit [p] (Math/log (/ p (- 1.0 p))))

(defn logistic [x] (/ 1.0 (+ 1.0 (Math/exp (- x)))))

(defn clamp-p [p] (min 0.99 (max 0.01 (double p))))

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

(defn tie-chance
  "The chance I win a tie with a rival: waiver order, the lower position first,
  and even odds when either side's is unknown."
  [mine theirs]
  (if (and (number? mine) (number? theirs))
    (if (< mine theirs) 1.0 0.0)
    0.5))

(defn win-chance
  "The chance a bid of `b` wins. Every rival either sits it out, bids under it,
  or ties it and loses the tie. Each rival carries `:p` (the chance he bids),
  `:pmf` and `:cdf` over his bid, and `:tie`."
  [rivals b]
  (reduce (fn [acc {:keys [p ^doubles pmf ^doubles cdf tie]}]
            (let [n     (alength pmf)
                  below (cond (<= b 0) 0.0 (<= b n) (aget cdf (dec b)) :else 1.0)
                  at    (if (< b n) (aget pmf b) 0.0)]
              (* acc (+ (- 1.0 p) (* p (+ below (* tie at)))))))
          1.0
          rivals))

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

(defn sure-bid
  "The cheapest bid that wins `sure-win` of the time, or nil when nothing up to
  what is left does."
  [rivals min-bid left]
  (first (filter #(>= (win-chance rivals %) sure-win) (range min-bid (inc left)))))

(defn top-bid
  "`[p50 p90]` of the highest rival bid, given that somebody bids, or nil when
  nobody is expected to."
  [rivals budget]
  (let [none (reduce * 1.0 (map #(- 1.0 (:p %)) rivals))]
    (when (< none 1.0)
      (let [upto (fn [b]
                   (/ (- (reduce * 1.0 (map (fn [{:keys [p ^doubles cdf]}]
                                              (+ (- 1.0 p) (* p (aget cdf (min b (dec (alength cdf)))))))
                                            rivals))
                         none)
                      (- 1.0 none)))
            at   (fn [q] (first (filter #(>= (upto %) (- q 1e-9)) (range 0 (inc budget)))))]
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
  "The rivals able to bid at all, each with his habits, his claim rates, his cap
  and his tie odds against me."
  [rivals me fas habits budget min-bid heat]
  (->> rivals
       (map (fn [r]
              (let [h (rival-habits r habits)]
                (assoc r
                       :style (:style h)
                       :habits h
                       :cap (long (min budget (or (:faab-left r) budget)))
                       :rates (claim-rates (:needs r) fas (:per-week h) heat)
                       :tie (tie-chance (:waiver-position me) (:waiver-position r))))))
       (filterv #(<= min-bid (:cap %)))))

(defn competition
  "One free agent's `:bid`, `:win-prob`, `:bid-sure`, `:rivals` and
  `:competition`, against the rivals who might bid for him."
  [p worth rivals dist min-bid left budget]
  (let [ps     (mapv #(bid-chance (get (:rates %) (:player-id p) 0.0)) rivals)
        bucket (prior/bucket (max 1 (Math/round (+ 1.0 (reduce + 0.0 ps)))))
        active (into []
                     (keep-indexed (fn [i r]
                                     (let [pi (ps i)]
                                       (when (pos? pi)
                                         (let [[pmf cdf] (dist i (:position p) bucket)]
                                           (assoc (select-keys r [:roster-id :name :faab-left :style :tie])
                                                  :p pi :pmf pmf :cdf cdf))))))
                     rivals)
        bid    (value-bid active worth min-bid left)]
    {:bid         bid
     :win-prob    (when bid (round-to 3 (win-chance active bid)))
     :bid-sure    (sure-bid active min-bid left)
     :rivals      (round-to 2 (reduce + 0.0 (map :p active)))
     :competition {:top         (top-bid active budget)
                   :uncontested (round-to 3 (reduce * 1.0 (map #(- 1.0 (:p %)) active)))
                   :threats     (threats active)}}))

(defn with-market
  "Assoc `competition` onto every free agent with a `:walk-away`; the rest get a
  nil `:bid`, since a league that does not run FAAB, or a budget already spent,
  has nothing to bid. `rivals` is `waiver/rival-needs`, `me` the manager's own
  team, `habits` `league-habits`, `week` the week claims are decided in, and
  `heat` is `heat-of` over the whole board, or none."
  [fas {:keys [rivals me waiver habits week heat] :or {heat (constantly 0.0)}}]
  (let [budget  (long (or (:budget waiver) 0))
        min-bid (long (or (:min-bid waiver) 0))
        left    (some-> (:faab-left me) long (min budget))
        phase   (prior/phase week)
        rivals  (bidders rivals me fas habits budget min-bid heat)
        ;; One distribution per rival, position and bidder count, not per
        ;; player: a few dozen, where there are hundreds of free agents.
        dist    (memoize
                 (fn [i position bucket]
                   (let [{:keys [habits cap]} (rivals i)
                         pmf (bid-pmf (get prior/bid-share [bucket phase])
                                      (bid-scale bucket position budget (:log-multiplier habits))
                                      (:zero-share habits) budget min-bid cap)]
                     [pmf (cumulative pmf)])))]
    (mapv (fn [p]
            (let [worth (:walk-away p)]
              (if (and worth left (pos? budget))
                (merge p (competition p worth rivals dist min-bid left budget))
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
