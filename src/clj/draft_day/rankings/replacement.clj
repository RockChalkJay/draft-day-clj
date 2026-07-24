(ns draft-day.rankings.replacement
  "Piece 2: replacement level + VORP (static). Pure function of
  (board, num-teams, config). K/DST are intentionally absent from the levels map
  so they price at $0 (vorp 0).")

(def default-config
  "Starters per team; flex slots are RB/WR/TE-eligible."
  {:qb 1 :rb 2 :wr 2 :te 1 :flex 1})

(defn flex-share-each
  "Total league flex demand (num-teams * flex-spots) split evenly between RB and
  WR (TE gets none). At 12 teams / flex 1 this is floor(12/2)=6 each, matching the
  original hardcoded `+ floor(num_teams * 0.5)`."
  [num-teams flex-spots]
  (long (Math/floor (/ (* num-teams flex-spots) 2.0))))

(defn replacement-levels
  "Return {\"QB\" pts \"RB\" pts \"WR\" pts \"TE\" pts}. The replacement index for
  a position is num-teams*starters (+ flex share for RB/WR), clamped to
  (count pool)-1; the score of the player at that index is the level. Positions
  with an empty pool are omitted. `score-key` (default :points) selects the score
  field to compute levels on."
  ([board num-teams config] (replacement-levels board num-teams config :points))
  ([board num-teams config score-key]
   (let [config (merge default-config config)
         flex   (flex-share-each num-teams (:flex config))
         spec   [["QB" (:qb config) 0]
                 ["RB" (:rb config) flex]
                 ["WR" (:wr config) flex]
                 ["TE" (:te config) 0]]]
     (reduce (fn [acc [pos starters flx]]
               (let [pool (sort-by score-key > (filter #(= (:position %) pos) board))]
                 (if (empty? pool)
                   acc
                   (let [idx (min (+ (* num-teams starters) flx) (dec (count pool)))]
                     (assoc acc pos (double (score-key (nth pool idx))))))))
             {} spec))))

(defn with-vorp
  "Assoc :vorp = max(0, score - level) for QB/RB/WR/TE; 0 for positions absent
  from levels (K/DST). `score-key` (default :points) matches replacement-levels."
  ([board levels] (with-vorp board levels :points))
  ([board levels score-key]
   (mapv (fn [p]
           (let [lvl (get levels (:position p))]
             (assoc p :vorp (if (nil? lvl)
                              0.0
                              (max 0.0 (- (double (score-key p)) lvl))))))
         board)))
