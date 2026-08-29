(ns draft-day.stat-lines
  "The On-the-block tile's season trend table: three completed seasons of what a
  player actually produced, against what he is projected to produce.

  Pure, and in cljc rather than in the view, because every rule here is a
  judgment about data rather than about markup — which stats a position is
  described by, when a row says nothing worth a line, and which seasons the
  columns stand for. `lein test` reaches cljc; the cljs test build only covers
  what genuinely needs a browser.

  Two shapes arrive from the server and they are NOT symmetric, which is the
  thing to keep straight in here:

  - `:nflverse/history` is a vector of `{:season :stats}`, oldest first. It is a
    vector on purpose — see `ingestion.nflverse/history` — so that JSON cannot
    mangle it on the way to the browser.
  - `:nflverse/games-by-season` is a map keyed by season, and it *is* mangled:
    jsonista writes the integer key 2023 as the string \"2023\" and `fx.cljs`
    decodes with `:keywordize-keys true`, so the browser sees `{:2023 17.0}`.
    Every read of it here goes through `season-key`, and the tests exercise the
    keyword form deliberately — the JVM sees integers and would otherwise never
    touch the shape the browser actually gets.")

(def stat-positions
  "Positions the table is built for. K and DST render no table at all: nflverse
  publishes no DST rows, and a kicker's realized line is not ingested (his
  columns in `nflverse/line-columns` would all be structurally zero). Rather than
  show a table of dashes for them, the tile falls back to what it showed before."
  #{"QB" "RB" "WR" "TE"})

(def position-rows
  "position -> the ordered [label stat-key] pairs the table describes it by.

  Keyed by the same Sleeper stat keys a projected `:stats` line uses, which is
  why a realized season and a projected one can share a row without translation.

  Two rules, and they pull in opposite directions — which is the whole design.

  EVERY POSITION SPLITS ITS TOUCHDOWNS BY PHASE. A back's rushing and receiving
  scores used to share one `TD` row while a quarterback's were split, so the tile
  answered \"how did he score?\" for one position and refused to for the others.
  A back who scores fourteen on the ground is a different asset from one who
  scores six and catches eight, exactly as a running quarterback is a different
  asset from a pocket one, and the table now says so everywhere.

  BUT EACH POSITION IS STILL DESCRIBED BY ITS OWN PHASES. One shared row list for
  all four would be more consistent still, and it would be worse: `stat-table`
  drops a row only when it is dead across the *entire* window, so a quarterback
  who caught one trick-play touchdown in one season would grow permanent `Rec`,
  `Rec Yd` and `Rec TD` rows reading `– 1 –`. A stray catch is not a thing a
  quarterback is described by, and three rows of dashes to say so is worse than
  not asking.

  Within a position the phases run in `db/scoring-catalog` order — passing,
  rushing, receiving — with each phase's volume above its scores, so the table
  reads the same way whoever is on the block."
  (let [passing   [["Pass Yd" :pass_yd] ["Pass TD" :pass_td]]
        rushing   [["Rush Yd" :rush_yd] ["Rush TD" :rush_td]]
        receiving [["Rec"     :rec]     ["Rec Yd"  :rec_yd] ["Rec TD" :rec_td]]]
    {"QB" (into passing rushing)
     "RB" (into rushing receiving)
     "WR" (into receiving rushing)
     "TE" (into receiving rushing)}))

(defn season-key
  "A season as a number, whether it arrived as one or as the keyword JSON turned
  it into (`:2023`). Returns nil for anything that is neither.

  See the ns docstring: the same map is an integer-keyed map on the server and a
  keyword-keyed one in the browser, and a lookup in the wrong vocabulary fails
  silently rather than throwing."
  [k]
  (cond
    (number? k)  (long k)
    (keyword? k) (parse-long (name k))
    (string? k)  (parse-long k)
    :else        nil))

(defn by-season
  "Normalize a `{season value}` map to integer keys, dropping any key that is not
  a season at all."
  [m]
  (into {} (keep (fn [[k v]] (when-let [s (season-key k)] [s v]))) m))

(defn stat-value
  "The value a stat line carries for `k`, or nil when it carries none.

  A thin `get`, named because the nil it returns is load-bearing — this is the
  BLANK IS NOT ZERO rule reaching the view. A season the source has no row for
  must render as a dash, not as a season the player produced nothing in, while a
  line carrying a real 0.0 keeps it: a back with no receiving touchdowns
  genuinely scored none.

  It used to sum a vector of keys, because `TD` was rushing plus receiving. Every
  row names exactly one stat now (see `position-rows`), so there is nothing left
  to add up."
  [stats k]
  (get stats k))

(defn season-columns
  "Which seasons the table has columns for: the window that was *fetched*, oldest
  first — not the seasons this player happens to have a row in.

  The distinction is `ingestion.nflverse`'s A MISSING SEASON IS NOT A MISSED
  SEASON rule, seen from the other end. A player missing from a fetched season
  gets a dash in a column that exists; a season the network lost gets no column
  at all, rather than showing the whole league a blank year."
  [player]
  (vec (sort (keys (by-season (:nflverse/games-seasons player))))))

(defn stat-table
  "Player + the projection season -> the tile's table, or nil when there is
  nothing worth drawing.

  {:seasons [2023 2024 2025] :proj-season 2026 :rookie? false
   :rows [{:label \"Rush Yd\" :values [976.0 1456.0 1478.0] :proj 1372.0}]}

  `:values` is always one entry per `:seasons` column, so the view can zip them
  positionally without re-deriving which season a cell belongs to.

  A row whose every cell is absent or zero across the whole window is dropped:
  a quarterback carries receiving columns in the raw data and they are all
  zeroes, and dead rows would push the rows that matter off the tile.

  Note what that rule does *not* do — it drops a row that is dead everywhere, not
  a row that is dead in most places. One non-zero cell in one season keeps a row
  for the whole window. That is why `position-rows` still asks each position for
  its own phases rather than handing every position the same list."
  [player season]
  (when-let [rows (get position-rows (:position player))]
    (let [seasons (season-columns player)
          hist    (into {} (map (juxt :season :stats)) (:nflverse/history player))
          games   (by-season (:nflverse/games-by-season player))
          proj    (:stats player)
          built   (into []
                        (keep (fn [[label k]]
                                (let [values (mapv #(stat-value (get hist %) k) seasons)
                                      pv     (stat-value proj k)]
                                  (when-not (every? #(or (nil? %) (zero? %))
                                                    (conj values pv))
                                    {:label label :values values :proj pv}))))
                        rows)]
      (when (seq built)
        (let [game-counts (mapv #(get games %) seasons)]
          {:seasons     seasons
           :proj-season season
           ;; A skill player with no realized season at all — a rookie, or
           ;; someone the join missed. Said plainly in the tile rather than left
           ;; as a row of dashes the manager has to interpret.
           :rookie?     (empty? hist)
           ;; Games last, and only when some season actually has one. It is
           ;; context for the rows above rather than production of its own, and
           ;; it has no projection — nobody forecasts availability.
           :rows        (cond-> built
                          (some some? game-counts)
                          (conj {:label "Games" :values game-counts :proj nil}))})))))
