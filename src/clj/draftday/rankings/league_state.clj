(ns draftday.rankings.league-state
  "Minimal in-memory snapshot of a draft in progress, enough to drive the live
  valuation pieces (tcm/pdm/inflation/worth). Plain maps — the browser owns this
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
