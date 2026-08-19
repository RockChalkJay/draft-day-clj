(ns draft-day.rankings.replacement
  "Piece 2: replacement level + VORP (static). Pure function of
  (board, num-teams, config). K/DST are intentionally absent from the levels map,
  so they carry no VORP and earn no share of the discretionary money — but they
  still fill roster slots, and `rankings.value` prices those at the league
  minimum.")

(def default-config
  "Starters per team; flex slots are RB/WR/TE-eligible."
  {:qb 1 :rb 2 :wr 2 :te 1 :flex 1})

(def flex-starter-keys
  "Positions a FLEX slot accepts, paired with the config key holding their
  dedicated starter count. Matches the roster's own rule (see `events/eligible?`).

  Named for the mapping rather than the membership because `pdm` and
  `benchmark.simulate` each carry a `flex-positions` **set** of the same three
  strings; `(flex-positions pos)` reading as a predicate there and as a lookup
  here is how the `priced-positions` drift started."
  {"RB" :rb "WR" :wr "TE" :te})

(defn- sorted-pools
  "Board grouped by position, each pool sorted descending on `score-key`. Sorting
  once here is what lets the flex pass and the level lookup share the work."
  [board score-key]
  (into {} (map (fn [[pos grp]] [pos (vec (sort-by score-key > grp))]))
        (group-by :position board)))

(defn- claims-from-pools
  [pools num-teams config score-key]
  (let [spots (* num-teams (long (or (:flex config) 0)))]
    (if-not (pos? spots)
      {}
      (let [leftovers (mapcat (fn [[pos starters-key]]
                                (drop (* num-teams (long (or (get config starters-key) 0)))
                                      (get pools pos)))
                              flex-starter-keys)]
        (frequencies (map :position (take spots (sort-by score-key > leftovers))))))))

(defn flex-claims
  "How many of the league's flex slots each flex-eligible position actually wins:
  `{\"RB\" n \"WR\" m \"TE\" k}`, summing to `num-teams * flex-spots`.

  Pool the players left after every team's dedicated starters are filled, rank
  that pool on its own merits, and take the best `num-teams * flex-spots` of
  them. Whoever is left standing is who the flex slots go to.

  This replaces a 50/50 RB/WR split (with TE getting none), which is the single
  largest positional bias the engine had, and it is format-dependent — so a
  standard league was roughly right while a PPR league was materially wrong.
  Measured on the bundled sample, the best twelve flex-eligible players actually
  available after base starters:

      standard   RB 7 / WR 5      (assumed RB 6 / WR 6)
      half-PPR   RB 3 / WR 9      (assumed RB 6 / WR 6)
      PPR        WR 12 / RB 0     (assumed RB 6 / WR 6)

  In PPR that put RB replacement six slots too deep — 160.2 points instead of
  174.8, so **every** running back carried +14.6 phantom points and about +$9.
  Against the vendor consensus the mean per-player error at RB was +$8.6 and is
  now +$2.4.

  `config` is merged with `default-config` here rather than assumed complete: a
  missing `:rb`/`:wr`/`:te` would otherwise read as *zero dedicated starters* and
  hand the flex slots to whole position pools competing from the top."
  [board num-teams config score-key]
  (claims-from-pools (sorted-pools board score-key) num-teams
                     (merge default-config config) score-key))

(defn replacement-levels
  "Return {\"QB\" pts \"RB\" pts \"WR\" pts \"TE\" pts}. The replacement index for
  a position is num-teams*starters plus the flex slots that position actually
  wins (`flex-claims`), clamped to (count pool)-1; the score of the player at that
  index is the level. Positions with an empty pool are omitted. `score-key`
  (default :points) selects the score field to compute levels on."
  ([board num-teams config] (replacement-levels board num-teams config :points))
  ([board num-teams config score-key]
   (let [config (merge default-config config)
         pools  (sorted-pools board score-key)
         claims (claims-from-pools pools num-teams config score-key)
         spec   [["QB" (:qb config) 0]
                 ["RB" (:rb config) (get claims "RB" 0)]
                 ["WR" (:wr config) (get claims "WR" 0)]
                 ["TE" (:te config) (get claims "TE" 0)]]]
     (reduce (fn [acc [pos starters flx]]
               (let [pool (get pools pos)]
                 (if (empty? pool)
                   acc
                   (let [idx (min (+ (* num-teams starters) flx) (dec (count pool)))]
                     (assoc acc pos (double (score-key (nth pool idx))))))))
             {} spec))))

(defn with-vorp
  "Assoc :vorp = score - level for QB/RB/WR/TE; nil for positions absent from
  levels (K/DST). `score-key` (default :points) matches replacement-levels.

  VORP is signed. It used to be max(0, ...), which collapsed every player below
  replacement to exactly 0.0 — 549 of 633 on the sample board — and left the
  back half of the draft with nothing to order it but raw :points, a scale that
  means something different at every position. The tail came out grouped by
  position with twelve straight quarterbacks at its head, and a receiver ranked
  49th by FantasyPros sat behind a quarterback ranked 247th. Signed, the same
  stretch reads one player per position in a sensible order.

  Negative VORP is not merely display. `rankings.value/priced-vorp?` gates the
  *discretionary* pool on VORP being positive, but `value/min-bid-ids` then draws
  the $1 minimum bids exclusively from the players it rejects — 96 of the 97 $1
  rows on the sample board are priced *because* their VORP is non-positive. Read
  `value/calculate-value` for the real rule before rescaling or re-flooring this.

  K and DST get **nil**, not a number. They take no replacement level of their
  own — at one starter each, the best defense on the sample board would carry +20
  real VORP and outrank seventy skill players, which is not what a $1 position is
  worth. But spelling 'no opinion' as 0.0 made it a value that compares: it read
  as *at replacement*, so every consumer that sorted on VORP floated all 76
  specialists above the whole below-replacement skill board, and each one had to
  be taught the exception separately. nil cannot be compared by accident — the
  board's sort already puts it last in both directions, and the VORP column
  renders it as the same dash the price does."
  ([board levels] (with-vorp board levels :points))
  ([board levels score-key]
   (mapv (fn [p]
           (let [lvl (get levels (:position p))]
             (assoc p :vorp (when lvl (- (double (score-key p)) lvl)))))
         board)))
