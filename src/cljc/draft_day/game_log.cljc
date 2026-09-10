(ns draft-day.game-log
  "The player detail modal's week-by-week table: what he did in each week the
  season has reached, scored under the league's own rules.

  Pure and in cljc for `stat-lines`' reason — every rule here is a judgment
  about data rather than about markup, and `lein test` reaches cljc.

  A ROW PER WEEK, NOT A ROW PER APPEARANCE. Weeks the player has no entry for
  are still rows, marked `:played? false` with nil values. A back who missed
  weeks 3 to 5 has to read as a back who missed weeks 3 to 5, not as one whose
  season was three weeks shorter — the same rule `stat-lines/season-columns`
  applies to seasons, one scale down.

  POINTS ARE NIL FOR A WEEK HE DID NOT PLAY, and that distinction is the trap
  worth naming: `scoring/player-points` reads a missing stat as 0, so it scores
  an absent week as a cheerful 0.0. For a week he *played*, a zero is the truth
  — he was active and did nothing. For a week he did not, it is BLANK IS NOT
  ZERO rule broken in the one cell where the lie looks most like a result.

  THE LINE ARRIVES SPARSE. `nflverse-weekly` drops the zeros the file publishes
  in every column, so a stat missing from a week he appeared in means he did
  none of it and reads back as 0 here. A week he missed has no entry at all,
  which is what keeps the two apart.

  The columns are `stat-lines/position-rows`, unchanged. The season trend table
  sits directly above this one in the same modal, and two tables describing one
  player by two different stat vocabularies is exactly the drift the rest of the
  app keeps single lists to avoid."
  (:require [draft-day.scoring :as scoring]
            [draft-day.stat-lines :as sl]))

(defn- row [columns scoring entry week]
  (let [stats (:stats entry)
        value (fn [[_ ks]] (if entry (or (sl/combine stats ks) 0) nil))]
    {:week     week
     :opponent (:opponent entry)
     :played?  (some? entry)
     :values   (mapv value columns)
     :points   (when entry (scoring/player-points {:stats stats} scoring))}))

(defn table
  "nil when there is nothing to draw: a kicker, a defense, or a season that has
  not started."
  [player through-week scoring]
  (when-let [columns (get sl/position-rows (:position player))]
    (when (and through-week (pos? through-week))
      (let [by-week (into {} (map (juxt :week identity))
                          (:nflverse/game-log player))]
        {:columns columns
         :rows    (mapv #(row columns scoring (by-week %) %)
                        (range 1 (inc through-week)))}))))
