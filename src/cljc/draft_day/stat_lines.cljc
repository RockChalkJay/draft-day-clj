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
  "position -> ordered [label stat-keys] the table describes it by.

  Keyed by the same Sleeper stat keys a projected `:stats` line uses, which is
  why a realized season and a projected one can share a row without translation.

  A label may name more than one key: `TD` is rushing plus receiving, because a
  back who scores twelve does not care which way they came, and two rows of
  single digits reads worse than one honest total. Quarterbacks keep theirs
  split — a passing touchdown and a rushing touchdown are different skills, and
  a QB who runs for fourteen is a different asset from one who does not."
  (let [receiving [["Rec"     [:rec]]
                   ["Rec Yd"  [:rec_yd]]
                   ["Rush Yd" [:rush_yd]]
                   ["TD"      [:rush_td :rec_td]]]]
    {"QB" [["Pass Yd" [:pass_yd]]
           ["Pass TD" [:pass_td]]
           ["Rush Yd" [:rush_yd]]
           ["Rush TD" [:rush_td]]]
     "RB" [["Rush Yd" [:rush_yd]]
           ["Rec"     [:rec]]
           ["Rec Yd"  [:rec_yd]]
           ["TD"      [:rush_td :rec_td]]]
     "WR" receiving
     "TE" receiving}))

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

(defn combine
  "Sum the `ks` a stat line actually has, or nil when it has none of them.

  Nil rather than zero, and that is the BLANK IS NOT ZERO rule reaching the view:
  a season the source has no row for must render as a dash, not as a season the
  player produced nothing in. But a stat line that carries a real 0.0 keeps it —
  a back with no receiving touchdowns genuinely scored none."
  [stats ks]
  (let [vs (keep #(get stats %) ks)]
    (when (seq vs) (reduce + vs))))

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
  zeroes, and four dead rows would push the rows that matter off the tile."
  [player season]
  (when-let [rows (get position-rows (:position player))]
    (let [seasons (season-columns player)
          hist    (into {} (map (juxt :season :stats)) (:nflverse/history player))
          games   (by-season (:nflverse/games-by-season player))
          proj    (:stats player)
          built   (into []
                        (keep (fn [[label ks]]
                                (let [values (mapv #(combine (get hist %) ks) seasons)
                                      pv     (combine proj ks)]
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
