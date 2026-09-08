(ns draft-day.rankings.replacement
  "Piece 2: replacement level + VORP (static). Pure function of (board,
  num-teams, config). K/DST are intentionally absent from the levels map, so
  they carry no VORP and earn no share of the discretionary money — but they
  still fill roster slots, and `rankings.value` prices those at the league
  minimum.

  HOW THE FLEX SLOTS ARE ALLOCATED (`flex-claims`). Pool the players left after
  every team's dedicated starters are filled, rank that pool on its own merits,
  and take the best `num-teams * flex-spots`. Whoever is left standing is who
  the flex slots go to. This replaced a fixed 50/50 RB/WR split (TE getting
  none), the single largest positional bias the engine had — and a
  format-dependent one, so a standard league was roughly right while PPR was
  materially wrong. On the bundled sample the best twelve flex-eligible players
  after base starters are RB 7 / WR 5 standard, RB 3 / WR 9 half-PPR, WR 12 / RB
  0 PPR. In PPR the old assumption put RB replacement six slots too deep — 160.2
  points instead of 174.8 — so every running back carried +14.6 phantom points
  and about +$9. Mean per-player error at RB against vendor consensus was +$8.6
  and is now +$2.4.

  WHY VORP IS SIGNED (`with-vorp`). It used to be max(0, ...), which collapsed
  every below-replacement player to exactly 0.0 — 549 of 633 on the sample board
  — leaving the back half of the draft ordered by raw :points, a scale that
  means something different at every position. The tail came out grouped by
  position with twelve straight quarterbacks at its head, and a receiver ranked
  49th by FantasyPros sat behind a quarterback ranked 247th. Signed, the same
  stretch reads one player per position in a sensible order. Negative VORP is
  not merely display: `value/priced-vorp?` gates the discretionary pool on VORP
  being positive, and `value/min-bid-ids` then draws the $1 minimum bids
  exclusively from the players it rejects — 96 of the 97 $1 rows on the sample
  board are priced *because* their VORP is non-positive. Read
  `value/calculate-value` before rescaling or re-flooring this.

  WHY K/DST GET nil AND NOT 0.0. They take no replacement level of their own —
  at one starter each, the best defense on the sample board would carry +20 real
  VORP and outrank seventy skill players, which is not what a $1 position is
  worth. But spelling 'no opinion' as 0.0 made it a value that compares: it read
  as *at replacement*, so every consumer sorting on VORP floated all 76
  specialists above the whole below-replacement skill board, and each had to be
  taught the exception separately. nil cannot be compared by accident — the
  board's sort puts it last in both directions, and the VORP column renders it
  as the same dash the price does.")

(def default-config
  "Starters per team; flex slots are RB/WR/TE-eligible."
  {:qb 1 :rb 2 :wr 2 :te 1 :flex 1})

(def flex-starter-keys
  "Positions a FLEX slot accepts -> the config key holding their dedicated
  starter count. Matches the roster's own rule — see `events/eligible?`. Named
  for the mapping, not the membership: `pdm` and `benchmark.simulate` carry a
  `flex-positions` **set** of the same strings, and `(flex-positions pos)`
  reading as a predicate there and a lookup here is how the `priced-positions`
  drift started."
  {"RB" :rb "WR" :wr "TE" :te})

(defn- sorted-pools
  "Board grouped by position, each pool sorted descending on `score-key`.
  Sorting once here is what lets the flex pass and the level lookup share the
  work."
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
  "`{\"RB\" n \"WR\" m \"TE\" k}` summing to `num-teams * flex-spots`; the rule
  is in the ns docstring. `config` is merged with `default-config` rather than
  assumed complete — a missing key would read as zero starters."
  [board num-teams config score-key]
  (claims-from-pools (sorted-pools board score-key) num-teams
                     (merge default-config config) score-key))

(defn replacement-levels
  "Replacement index is num-teams*starters plus the flex slots the position wins
  (`flex-claims`), clamped to (count pool)-1; that player's score is the level.
  Empty pools are omitted. `score-key` defaults to :points."
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
  "Assoc :vorp = score - level for QB/RB/WR/TE; **nil** for positions absent
  from levels (K/DST). `score-key` (default :points) matches replacement-levels.
  The sign and the nil are both load-bearing — see this namespace's docstring."
  ([board levels] (with-vorp board levels :points))
  ([board levels score-key]
   (mapv (fn [p]
           (let [lvl (get levels (:position p))]
             (assoc p :vorp (when lvl (- (double (score-key p)) lvl)))))
         board)))
