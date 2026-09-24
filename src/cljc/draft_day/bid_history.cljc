(ns draft-day.bid-history
  "What one league's bid history says about how its managers bid: a profile per
  manager, the league's own price level, and a bidding style to show for each.

  Every number here is the Sleeper-wide `draft-day.bid-prior` corrected by the
  league's own evidence, and only as far as that evidence goes: a figure is the
  league's weighted bids blended with a CHOSEN number of pseudo-bids at the
  prior's value, so a manager with three bids reads close to the typical Sleeper
  manager and one with sixty reads as himself. The measures are the harness's
  own, so the prior is on their scale: bids a week are over the weeks the league
  ran auctions, and a bid's log ratio is its share of the budget over the
  backbone's typical positive bid for the same bidders and phase.

  Evidence is weighted twice. The current season decays toward its latest week,
  so a manager who stopped bidding stops reading as active, and last season
  counts at a CHOSEN fraction, since habits carry across seasons well but not
  fully. A bid belongs to the manager who placed it (`:owner-id`), which is what
  follows him from one season to the next; a bid with no owner is kept under its
  roster.

  Price level has two tiers. The league's multiplier is its bids against the
  backbone, and a manager's aggression is his bids against the league's, so a
  manager with no history bids like his league and a league with no history like
  Sleeper. `:log-multiplier` is the two together, what the bid model scales a
  manager's bids by.

  Pure and universe-free: the server computes it from the history cache and the
  browser renders the profiles it is sent. Every CHOSEN constant stands until
  the replay backtest scores it."
  (:require [draft-day.bid-prior :as prior]))

(def previous-season-weight
  "What a bid from last season counts for against one from this season. CHOSEN,
  near `bid-prior/persistence`'s season-to-season correlations (0.43-0.62)."
  0.5)

(def half-life-weeks
  "How many weeks it takes a current-season bid to count half as much as one
  from the league's latest week. CHOSEN."
  4.0)

(def pseudo-weeks
  "Weeks at the Sleeper-wide median rate a manager's bids a week are blended
  with. CHOSEN."
  2.0)

(def pseudo-bids
  "Bids at the prior's value a manager's $0 share, or his aggression, is blended
  with. CHOSEN."
  8.0)

(def pseudo-league-bids
  "Positive bids at the backbone's price a league's multiplier is blended with.
  CHOSEN."
  30.0)

(defn typical-share
  "The backbone's median positive bid for an auction, as a share of the league's
  `budget`: its bidders and phase cell, moved by `bid-prior/budget-shift` when
  the league does not play for $100."
  [bidders week budget]
  (let [b (prior/bucket bidders)]
    (* (get-in prior/bid-share [[b (prior/phase week)] :positive 0.5])
       (if (== 100 budget) 1.0 (Math/exp (get-in prior/budget-shift [b :log-shift]))))))

(defn manager-key
  "Who a bid belongs to: its owner, or its roster where Sleeper names none."
  [bid]
  (or (some-> (:owner-id bid) str) (str "roster:" (:roster-id bid))))

(defn latest-week [season] (reduce max 0 (map :week (:auctions season))))

(defn weights
  "`[current-season-weight earlier-season-weight]` as functions of the week."
  [current]
  (let [latest (latest-week current)]
    [(fn [week] (Math/pow 0.5 (/ (- latest week) half-life-weeks)))
     (constantly previous-season-weight)]))

(defn season-bids
  "One season's bids, each with its weight and what a profile reads off it."
  [{:keys [budget auctions]} weight current?]
  (let [budget (double (or budget 0))]
    (when (pos? budget)
      (mapcat (fn [{:keys [week bids]}]
                (let [typical (typical-share (count bids) week budget)]
                  (map (fn [b]
                         (let [share (/ (double (:amount b)) budget)]
                           {:manager   (manager-key b)
                            :roster-id (:roster-id b)
                            :week      week
                            :current?  current?
                            :weight    (weight week)
                            :amount    (:amount b)
                            :log-ratio (when (pos? share) (Math/log (/ share typical)))}))
                       bids)))
              auctions))))

(defn weighted-bids
  "Every bid in a history's seasons, current first, weighted."
  [[current & earlier]]
  (let [[now before] (weights current)]
    (vec (concat (season-bids current now true)
                 (mapcat #(season-bids % before false) earlier)))))

(defn exposure
  "Weeks a manager could have bid in, weighted as his bids are: each week the
  league ran auctions, decayed this season and discounted before it."
  [[current & earlier]]
  (let [[now before] (weights current)
        weeks        #(distinct (map :week (:auctions %)))]
    (+ (reduce + 0.0 (map now (weeks current)))
       (reduce + 0.0 (mapcat #(map before (weeks %)) earlier)))))

(defn blend
  "`sum` of weighted evidence and `pseudo` observations at `center`, over their
  combined weight."
  [sum weight pseudo center]
  (/ (+ sum (* pseudo center)) (+ weight pseudo)))

(defn weighted-sum [f bs] (reduce + 0.0 (map #(* (:weight %) (f %)) bs)))

(defn league-log-multiplier
  "How far the league's positive bids sit from the backbone, as a natural log,
  shrunk toward none."
  [bids]
  (let [pos (filter :log-ratio bids)]
    (blend (weighted-sum :log-ratio pos) (weighted-sum (constantly 1.0) pos)
           pseudo-league-bids 0.0)))

(defn win-over-second
  "The median of what this manager's contested wins this season cost over the
  bid each beat, or nil. Display only."
  [auctions manager]
  (let [ratios (keep (fn [{:keys [bids]}]
                       (let [[w s] (sort-by (comp - :amount) bids)]
                         (when (and s (:won? w) (= manager (manager-key w)) (pos? (:amount s)))
                           (/ (double (:amount w)) (:amount s)))))
                     auctions)]
    (when (seq ratios)
      (nth (vec (sort ratios)) (quot (count ratios) 2)))))

(def quiet-weeks
  "Weeks without a bid, at the end of a season that has run at least that many,
  after which a manager reads as quiet. CHOSEN."
  3)

(defn style
  "A bidding style, from a profile and how many weeks of auctions the season has
  run. CHOSEN thresholds, against the Sleeper-wide spread in
  `bid-prior/managers`: a sniper bids rarely and big — big on average, or once
  for a quarter of the budget, which an average over his bids would dilute — a
  big spender bids big whatever the rate, and a $0 flyer bids often and rarely
  pays. A sniper is named before a quiet manager, since going quiet and then
  pouncing is what a sniper does."
  [{:keys [bids current-bids recent-bids per-week zero-share aggression top-share]} weeks-run]
  (cond
    (zero? (or bids 0))                                     :new
    (and (<= per-week 0.8)
         (or (>= aggression 0.4) (>= (or top-share 0) 0.25))) :sniper
    (and (>= weeks-run quiet-weeks) (zero? recent-bids))    :quiet
    (>= aggression 0.6)                                     :big-spender
    (and (>= zero-share 0.75) (>= per-week 1.2) (pos? current-bids)) :zero-flyer
    :else                                                   :typical))

(defn profile
  "One manager's bids, shrunk, with the raw counts beside the blended figures so
  a reader can see how much evidence stands behind each."
  [manager bs exposure league-log current]
  (let [pos        (filter :log-ratio bs)
        this-year  (filter :current? bs)
        latest     (latest-week current)
        aggression (blend (weighted-sum #(- (:log-ratio %) league-log) pos)
                          (weighted-sum (constantly 1.0) pos) pseudo-bids 0.0)]
    {:manager         manager
     :roster-id       (some :roster-id (filter :current? bs))
     :bids            (count bs)
     :current-bids    (count this-year)
     :recent-bids     (count (filter #(> (:week %) (- latest quiet-weeks)) this-year))
     :per-week        (blend (weighted-sum (constantly 1.0) bs) exposure pseudo-weeks
                             (get-in prior/managers [:per-week :p50]))
     :zero-share      (blend (weighted-sum #(if (:log-ratio %) 0.0 1.0) bs)
                             (weighted-sum (constantly 1.0) bs) pseudo-bids
                             (get-in prior/managers [:zero-share :p50]))
     :aggression      aggression
     :log-multiplier  (+ league-log aggression)
     :max-bid         (reduce max 0 (map :amount this-year))
     :top-share       (let [budget (:budget current)]
                        (when (and budget (pos? budget))
                          (/ (reduce max 0 (map :amount this-year)) (double budget))))
     :win-over-second (win-over-second (:auctions current) manager)}))

(defn profiles
  "A history's profiles: `{:log-multiplier league-log :weeks-run n :profiles
  [profile]}`, every manager who bid in it, each with his `:style`. A vector
  rather than a map keyed by manager, because the browser keywordizes the keys
  of what it is sent and a manager id is a number."
  [{:keys [seasons]}]
  (when (seq seasons)
    (let [bids      (weighted-bids seasons)
          league    (league-log-multiplier bids)
          exp-weeks (exposure seasons)
          current   (first seasons)
          weeks-run (count (distinct (map :week (:auctions current))))]
      {:log-multiplier league
       :weeks-run      weeks-run
       :profiles       (->> (group-by :manager bids)
                            (map (fn [[m bs]]
                                   (let [p (profile m bs exp-weeks league current)]
                                     (assoc p :style (style p weeks-run)))))
                            (sort-by :manager)
                            vec)})))

(defn team-profile
  "The profile of whoever manages `team` — its owner, else whatever bid under
  its roster — or nil when he has never bid. Ids compare as strings: the sync
  and the history reach the browser by different roads."
  [team profiles]
  (let [owner  (some-> (:owner-id team) str)
        roster (str "roster:" (:roster-id team))]
    (or (some #(when (= owner (:manager %)) %) profiles)
        (some #(when (= roster (:manager %)) %) profiles))))

(def style-labels
  {:new         "No bids yet"
   :quiet       "Quiet"
   :sniper      "Sniper"
   :big-spender "Big spender"
   :zero-flyer  "$0 flyer"
   :typical     "Typical"})

(defn one-decimal [x]
  #?(:clj (format "%.1f" (double x)) :cljs (.toFixed x 1)))

(defn style-line
  "One line for a team card: the style and the evidence for it, e.g.
  \"Sniper — 0.6 claims a week, 40% at $0, top bid $38\"."
  [{:keys [style per-week zero-share max-bid bids]}]
  (if (or (nil? style) (= :new style) (zero? (or bids 0)))
    (style-labels :new)
    (str (style-labels style "Typical") " — "
         (one-decimal per-week) " claims a week, "
         (Math/round (* 100.0 zero-share)) "% at $0"
         (when (pos? (or max-bid 0)) (str ", top bid $" max-bid)))))
