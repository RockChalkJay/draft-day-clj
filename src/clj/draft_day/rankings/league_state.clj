(ns draft-day.rankings.league-state
  "Minimal in-memory snapshot of a draft in progress, enough to drive the live
  valuation pieces (tcm/inflation/worth). Plain maps — the browser owns this
  state and re-sends it on every call.

  Shape:
    {:teams [{:team-id \"t0\" :bankroll 200.0
              :roster [{:pos \"RB\" :player-id nil} ...]} ...]
     :drafted-player-ids #{...}
     :starting-bankroll 200.0}")

(defn initial-cash
  "Total cash the room started with = the league's total Value pool."
  [ls]
  (* (count (:teams ls)) (double (:starting-bankroll ls 200.0))))

(defn total-remaining-cash [ls]
  (reduce + 0.0 (map #(double (:bankroll %)) (:teams ls))))

(defn slot-counts
  "Every roster slot in the league, filled or not, keyed by slot label
  (\"QB\"/\"FLEX\"/\"BENCH\"/\"K\"/...).

  `engine`, `inflation` and `value` each want a different cut of this — the
  total, the count a position can fill, the count K and DST will take — and each
  used to hand-roll the same reduce."
  [ls]
  (reduce (fn [acc team]
            (reduce (fn [a slot] (update a (:pos slot) (fnil inc 0)))
                    acc (:roster team)))
          {} (:teams ls)))

(defn total-slots
  "How many roster slots the league drafts in total."
  [ls]
  (reduce + 0 (vals (slot-counts ls))))

(defn streamed-slots
  "Roster slots reserved for the streamed positions, keyed by label — `{\"K\" 12
  \"DST\" 12}` at the 12-team default.

  These seats get filled and paid for like any other, but the engine never puts a
  VORP dollar on them (see `rankings.replacement/with-vorp`), so `value` prices
  exactly this many of each at the league minimum rather than letting skill
  players collect their share."
  [ls]
  (select-keys (slot-counts ls) ["K" "DST"]))

(defn empty-slots-by-pos
  "Empty (unfilled) slots aggregated across every team, keyed by slot label
  (incl. \"FLEX\"/\"BENCH\")."
  [ls]
  (reduce (fn [acc team]
            (reduce (fn [a slot]
                      (if (nil? (:player-id slot))
                        (update a (:pos slot) (fnil inc 0))
                        a))
                    acc (:roster team)))
          {} (:teams ls)))
