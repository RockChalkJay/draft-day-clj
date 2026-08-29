(ns draft-day.rankings.pos-rank
  "The board's third display-only signal: where a player falls within his own
  position. `RB1` is the best running back on the board, `RB25` the 25th.

  It is the number a manager says out loud — every cheatsheet in the hobby is
  printed in it — and until now the board made you count rows to get it. The `#`
  column answers a different question: it is an *overall* live rank by Worth, so
  it mixes positions and renumbers on every pick.

  Static, and deliberately so. `engine/static-rankings` recomputes only when the
  scoring or roster config changes, so RB1 stays RB1 for the whole draft even
  after he is gone. That is the point: it is an identifier you can cross-
  reference against an outside cheatsheet, not a scarcity signal. Scarcity is
  what `tcm` and the tier columns are for.

  ORDERED ON :points, THOUGH THE QUESTION IT ANSWERS IS 'BY VALUE'. Within a
  single position these are the same ordering wherever Value discriminates at
  all: `replacement/with-vorp` is `points - level` with `level` a per-position
  constant, and `value/calculate-value` prices a priced player at
  `1 + (vorp/total-vorp) * disc`, which is strictly monotone in VORP. But :value
  is *flat* over most of a position — everything below replacement prices at the
  $1 minimum, everything past the last roster slot at $0, and `to-dollars`
  rounds the rest to whole dollars. Ranking on it directly would leave the whole
  RB/WR tail, which is the majority of each position, in whatever order the
  server happened to emit. That is the same failure `db/rank-key` exists to
  fix for the `#` column.

  :points also covers K and DST, which :vorp cannot: the engine gives those two
  no replacement level, so their :vorp is nil by design (see
  `replacement/with-vorp`). A VORP-ordered rank would have to special-case them;
  a points-ordered one does not.

  Display only. Nothing downstream reads :pos-rank — like `injury/:injury-risk`
  and `tcm`, and for the same reason: the market already prices what it knows,
  and a board signal that quietly re-enters the valuation charges a player twice.")

(defn- rank-key
  "Descending sort key within a position: points first, player-id to break ties.

  The tiebreak is not cosmetic. Identical projections are common on the tail —
  Sleeper rounds — and without a total order the ordinal would flicker between
  two recomputes of the same board, so a player's `RB47` would silently become
  `RB48` when nothing about him changed."
  [p]
  [(- (double (:points p))) (str (:player-id p))])

(defn- ranks-for-position
  "Seq of [player-id ordinal] for one position's players, best first. Players the
  model never scored are skipped rather than ranked last — see `with-pos-rank`."
  [players]
  (->> players
       (filter #(number? (:points %)))
       (sort-by rank-key)
       (map-indexed (fn [i p] [(:player-id p) (inc i)]))))

(defn with-pos-rank
  "Assoc :pos-rank — 1-based rank within the player's own position, best first.

  A player the model left without :points is untouched, so the board renders the
  bare position rather than inventing a rank for a row it could not score. Row
  order is preserved: this builds an id->ordinal index and maps it back over the
  board, exactly as `tiers/with-tiers` does, so nothing downstream of here sees
  the pool reordered."
  [board]
  (let [index (into {}
                    (mapcat (comp ranks-for-position val))
                    (group-by :position board))]
    (mapv (fn [p]
            (if-let [n (index (:player-id p))]
              (assoc p :pos-rank n)
              p))
          board)))

