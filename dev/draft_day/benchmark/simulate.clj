(ns draft-day.benchmark.simulate
  "Score a model by the team it drafts, not by how well-ordered its board was.

  Rank correlation treats every player equally: a model that nails RB1-12 and
  butchers RB40-60 scores much like the reverse, though only one of them wins
  leagues. It also cannot see positional scarcity, which is the entire purpose of
  VORP. So this runs the decision the board actually feeds.

  Twelve teams snake-draft. One seat drafts off the model's board; the other
  eleven draft off consensus ADP, which is what real opponents approximate. The
  simulation is repeated with the model in **every** seat, because pick 1 and
  pick 12 are different games and a single slot would measure draft position as
  much as model quality. The result is the realized points of the team the model
  built, against the field it played.

  What this deliberately does NOT model: in-season waivers, trades, weekly
  start/sit, and opponents reacting to runs. It is a season-long approximation —
  the roster is scored on its best lineup by realized points, which flatters
  every seat equally. Read it as a relative measure between models, never as a
  projection of league finish."
  (:require [draft-day.benchmark.metrics :as metrics]
            [draft-day.db :as db]
            [draft-day.rankings.replacement :as replacement]))

(def default-config
  {:teams    12
   :rounds   12
   ;; Starting lineup a 12-team league fields; FLEX takes RB/WR/TE.
   :starters {"QB" 1 "RB" 2 "WR" 2 "TE" 1}
   :flex     1
   :flex-positions #{"RB" "WR" "TE"}
   ;; Roster caps stop a greedy board drafting six quarterbacks.
   :caps     {"QB" 2 "RB" 6 "WR" 6 "TE" 2}})

(defn snake-order
  "Team index per overall pick, snaking each round: 0..n-1, then n-1..0."
  [teams rounds]
  (into []
        (mapcat (fn [r] (if (even? r) (range teams) (reverse (range teams)))))
        (range rounds)))

(defn vorp-board
  "Re-order a board by value over replacement rather than raw points.

  Raw projected points is NOT a draft board. Quarterbacks project the most points
  and have the highest replacement level, so a points-ordered board spends four
  of its first twelve picks on them while the field takes none — and drafts a
  team worth ~180 points less. VORP is the correction, and this measures how much
  of that gap it recovers.

  Reuses the live engine's `replacement-levels` / `with-vorp` so the board under
  test is the one the app would actually build, not a reimplementation.

  Two things about the ordering are worth knowing before comparing a run against
  an older number:

  - **VORP is signed now.** It used to be floored at 0, so every below-replacement
    player tied and the stable sort left that whole block in pool order. A 12x12
    simulation makes 144 picks against roughly 84 above-replacement players, so
    every pick from about round 8 on now comes off a differently ordered board.
    Any `--simulate` figure recorded before that change is not what this computes.
  - **K/DST are demoted explicitly.** They carry no VORP at all, and the `(or ...
    0.0)` below would read that absence as *at replacement* and float them above
    every below-replacement skill player. `vintage/scoring-positions` keeps them
    out of the benchmark pool today, so this guard is unreachable — but that is a
    property of the data source, and `default-config`'s `:caps` names no K/DST, so
    a pool that ever included them would let the model seat take unlimited
    kickers. Same rule the live board applies in `db/vorp-sort-key`."
  [players num-teams replacement-config]
  (let [levels (replacement/replacement-levels players num-teams (or replacement-config {}) :points)]
    (->> (replacement/with-vorp players levels :points)
         (sort-by (fn [p] [(if (db/priced-positions (:position p)) 0 1)
                           (- (double (or (:vorp p) 0.0)))])))))

(defn board-order
  "Players sorted by a seat's preference.

  The field ranks by draft-time consensus (ascending ADP, else ECR); a player it
  has no price for goes last rather than becoming undraftable.

  The model seat ranks by :points descending — but ONLY when :points is globally
  comparable. Ordinal models (`:adp`, `:ecr`) synthesize :points per position, so
  a quarterback's synthesized value and a running back's are on unrelated scales.
  Sorting them together produces a scrambled board and an edge that looks like a
  model result: `:adp` scored -257 against a field drafting the very same order,
  when the true answer is zero. Such models pass their native key as
  `ordinal-key` and are ordered by it globally."
  ([players model? ordinal-key] (board-order players model? ordinal-key nil))
  ([players model? ordinal-key vorp-opts]
  (cond
    (and model? vorp-opts)
    (vorp-board players (:teams vorp-opts 12) (:replacement-config vorp-opts))

    (and model? ordinal-key)
    (sort-by #(double (or (ordinal-key %) Double/MAX_VALUE)) players)

    model?
    (sort-by #(- (double (or (:points %) 0.0))) players)

    :else
    (sort-by #(double (or (:adp %) (:ecr %) Double/MAX_VALUE)) players))))

(defn pick
  "First player on `ordered` still available and not at the team's positional cap."
  [ordered taken counts caps]
  (some (fn [p]
          (let [pos (:position p)]
            (when (and (not (contains? taken (:player-id p)))
                       (< (get counts pos 0) (get caps pos 99)))
              p)))
        ordered))

(defn best-lineup-points
  "Realized points of the roster's best legal starting lineup.

  Scoring the whole roster would reward hoarding; scoring the slots as drafted
  would punish a manager for bench depth they would in practice have started.
  Best-lineup is the standard compromise and treats every seat identically."
  [roster {:keys [starters flex flex-positions]} truth-key]
  (let [pts   #(double (or (truth-key %) 0.0))
        by-pos (into {} (map (fn [[pos ps]] [pos (sort-by (comp - pts) ps)]))
                     (group-by :position roster))
        used   (into {} (map (fn [[pos n]] [pos (take n (get by-pos pos []))])) starters)
        left   (mapcat (fn [[pos ps]]
                         (when (flex-positions pos) (drop (get starters pos 0) ps)))
                       by-pos)
        flexed (take flex (sort-by (comp - pts) left))]
    (reduce + 0.0 (map pts (concat (mapcat val used) flexed)))))

(defn simulate-season
  "One season, model in one seat. Returns {:model-points :field-mean :edge}."
  [players model-seat config truth-key]
  (let [{:keys [teams rounds caps ordinal-key vorp?]} config
        model-board (board-order players true ordinal-key
                                 (when vorp? {:teams teams
                                              :replacement-config (:replacement-config config)}))
        field-board (board-order players false nil)
        order       (snake-order teams rounds)]
    (loop [[seat & more] order taken #{} rosters {} counts {}]
      (if (nil? seat)
        (let [pts (into {} (map (fn [[s r]] [s (best-lineup-points r config truth-key)])) rosters)
              model-pts (get pts model-seat 0.0)
              field     (map val (dissoc pts model-seat))]
          {:model-points model-pts
           :field-mean   (metrics/mean field)
           :edge         (- model-pts (metrics/mean field))})
        (let [board (if (= seat model-seat) model-board field-board)
              p     (pick board taken (get counts seat {}) caps)]
          (if (nil? p)
            (recur more taken rosters counts)
            (recur more
                   (conj taken (:player-id p))
                   (update rosters seat (fnil conj []) p)
                   (update-in counts [seat (:position p)] (fnil inc 0)))))))))

(defn simulate-all-seats
  "Average edge over every draft slot, so draft position cancels out."
  [players config truth-key]
  (let [seats (range (:teams config))
        runs  (mapv #(simulate-season players % config truth-key) seats)]
    {:edge         (metrics/mean (map :edge runs))
     :model-points (metrics/mean (map :model-points runs))
     :field-mean   (metrics/mean (map :field-mean runs))
     :by-seat      (mapv :edge runs)}))

(defn run
  "Per season, the model's drafted-team edge over the field.
  `results` is a `core/run` output. Returns [{:season :edge ...}]."
  ([results truth-key] (run results truth-key default-config))
  ([results truth-key config]
   (into []
         (comp (remove :skipped?)
               (map (fn [{:keys [season players]}]
                      (assoc (simulate-all-seats players config truth-key)
                             :season season))))
         results)))
