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

(def position-rows
  "position -> ordered [label stat-keys] the game log describes it by.

  Keyed by the same Sleeper stat keys a projected `:stats` line uses, which is
  why a realized season and a projected one can share a row without translation.

  QB/RB/WR/TE only, and the key set is the gate: K and DST miss and render no
  game log at all, since a weekly log of kicks and sacks is not what it is for.
  The Season table has its own, wider set: `season-rows`.

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

(def season-rows
  "position -> ordered `[label stat-keys kind]` the player card's Season table
  describes it by; `position-rows` is the game log's narrower set.

  A label may name several keys, summed: `FG 0-29` is two distance buckets.
  `:ratio` reads two keys as one cell, since completions without attempts say
  nothing about accuracy. A kicker's history is nflverse's kicking columns and a
  defense's is Sleeper's season totals (`ingestion.sleeper-defense`); both ride
  the same keys as everyone else."
  (let [catcher [["Targets"  [:rec_tgt]]
                 ["Rec"      [:rec]]
                 ["Rec Yd"   [:rec_yd]]
                 ["Rush Att" [:rush_att]]
                 ["Rush Yd"  [:rush_yd]]
                 ["Rush TD"  [:rush_td]]
                 ["Rec TD"   [:rec_td]]]]
    {"QB"  [["Comp/Att" [:pass_cmp :pass_att] :ratio]
            ["Pass Yd"  [:pass_yd]]
            ["Pass TD"  [:pass_td]]
            ["Rush Att" [:rush_att]]
            ["Rush Yd"  [:rush_yd]]
            ["Rush TD"  [:rush_td]]]
     "RB"  catcher
     "WR"  catcher
     "TE"  catcher
     "K"   [["FG 0-29"  [:fgm_0_19 :fgm_20_29]]
            ["FG 30-39" [:fgm_30_39]]
            ["FG 40-49" [:fgm_40_49]]
            ["FG 50+"   [:fgm_50p]]
            ["XP made"  [:xpm]]
            ["XP missed" [:xpmiss]]]
     "DST" [["Sacks"          [:sack]]
            ["INT"            [:int]]
            ["Blocked kicks"  [:blk_kick]]
            ["Forced fumbles" [:ff]]
            ["Fumble rec."    [:fum_rec]]
            ["Yds allowed"    [:yds_allow]]
            ["Pts allowed"    [:pts_allow]]]}))

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

(defn combine-ratio
  "`\"made/attempts\"` from two keys, or nil unless both are present. A season
  with neither is a dash; one with a half is not a ratio."
  [stats [made att]]
  (let [m (get stats made) a (get stats att)]
    (when (and m a) (str (Math/round (double m)) "/" (Math/round (double a))))))

(defn blank?
  "Whether a cell says nothing: absent, a zero, or an empty ratio."
  [v]
  (or (nil? v) (and (number? v) (zero? v)) (= "0/0" v)))

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
  zeroes, and four dead rows would push the rows that matter off the tile.

  In season (`:in-season?`) the last column is what he has done so far rather
  than what he was projected for — a projection is not what a manager reads a
  card for once games are played — and the table says so with `:so-far? true`."
  ([player season] (stat-table player season {}))
  ([player season {:keys [in-season?]}]
   (when-let [rows (get season-rows (:position player))]
     (let [seasons (season-columns player)
           hist    (into {} (map (juxt :season :stats)) (:nflverse/history player))
           games   (by-season (:nflverse/games-by-season player))
           nfl     (:nflverse/season-to-date player)
           ;; Sleeper is a defense's only source and omits a stat it did not
           ;; accrue, so for one a missing key after a played game is a zero.
           realized? (and in-season? (= "DST" (:position player))
                          (pos? (or (get-in player [:realized/season-to-date :games]) 0)))
           so-far  (if realized? (:realized/season-to-date player) nfl)
           proj    (if in-season? (:stats so-far) (:stats player))
           cell    (fn [stats [_ ks kind]]
                     (if (= :ratio kind) (combine-ratio stats ks) (combine stats ks)))
           built   (into []
                         (keep (fn [[label ks kind :as row]]
                                 (let [values (mapv #(cell (get hist %) row) seasons)
                                       pv     (cell proj row)
                                       pv     (if (and realized? (nil? pv)) (if (= :ratio kind) "0/0" 0.0) pv)]
                                   (when-not (every? blank? (conj values pv))
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
            :so-far?     (boolean in-season?)
            :rows        (cond-> built
                           (some some? game-counts)
                           (conj {:label "Games" :values game-counts
                                  :proj (when in-season? (:games so-far))}))}))))))
