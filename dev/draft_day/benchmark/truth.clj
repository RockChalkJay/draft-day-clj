(ns draft-day.benchmark.truth
  "Realized outcomes, scored under the league's own rules.

  The answer key is not 'PPR points'. It is 'points *this* league would have
  awarded', which is why nflverse rows are mapped onto the Sleeper stat keys
  `rankings.scoring` speaks and then run through the very same
  `scoring/player-points` the live board uses. Scoring the truth with a fixed PPR
  constant would validate a league nobody in this app is playing in, and would
  quietly hide any bug in custom scoring — the exact thing worth testing.

  Season total vs per-game: both are kept. Total is what an auction buys, but
  it is heavily contaminated by injury luck (games played is close to
  unpredictable — prior-year to next-year rank correlation is about +0.07), so
  per-game is the fairer read on whether a model understood the player. Report
  both; do not average them into one number that means neither."
  (:require [draft-day.rankings.scoring :as scoring]))

(defn realized-points
  "Points the league's scoring config would have awarded for a realized season."
  [player scoring-config]
  (scoring/player-points {:stats (:actual/stats player)} scoring-config))

(defn with-realized
  "Assoc :actual/points and :actual/ppg. Players with no outcome row (never
  played) score zero — that is a real draft outcome, not missing data, so they
  stay in the pool and count against whichever model ranked them highly."
  [players scoring-config]
  (mapv (fn [p]
          (let [pts   (realized-points p scoring-config)
                games (double (or (:actual/games p) 0.0))]
            (assoc p
                   :actual/points pts
                   :actual/ppg    (if (pos? games) (/ pts games) 0.0))))
        players))
