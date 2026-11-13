(ns draft-day.rankings.opponent
  "What each defense has allowed: the fantasy points each position scored
  against it, a game, under the league's own weights. Display only, like
  `rankings.role`: nothing here feeds a projection.

  Every game-log row names the team the player faced (`:opponent`, Sleeper's
  abbreviations, which are the app's), so every row against a team is points it
  allowed. A team's games are the weeks anyone faced it, not its rows: five
  receivers in one game are one game. A defense's own row names the offense it
  faced, so what DST scored against a team says how good that offense is to
  stream a defense against.

  The rank runs from the most allowed down, the way defense-vs-position tables
  on the web run it, and it is never stated as a bare number: `#1 defense` reads
  as the best one in football talk and as the softest matchup in a DvP table.
  `rank-label` puts the direction in the words."
  (:require [draft-day.scoring :as scoring]))

(def recent-games
  "How many of a team's latest games `:recent-per-game` spans."
  3)

(defn allowed-rows
  "`{:team :position :week :points}` for every game-log row through
  `through-week`, `:team` being the opponent. Live, the log already holds
  Thursday's game of the week being played, so the cap keeps every team on the
  same weeks."
  [players scoring through-week]
  (mapcat (fn [{:keys [position] :as p}]
            (keep (fn [{:keys [week opponent stats]}]
                    (when (and opponent position (<= week through-week))
                      {:team     opponent
                       :position position
                       :week     week
                       :points   (scoring/resolved-points {:stats stats} scoring)}))
                  (:realized/game-log p)))
          players))

(defn per-game [total games]
  (when (pos? games) (/ total (double games))))

(defn team-lines
  "`{team {position {:games :points :per-game :recent-per-game}}}` off
  `allowed-rows`. A position nobody played against a team scored nothing there."
  [rows]
  (let [positions (distinct (map :position rows))]
    (update-vals
     (group-by :team rows)
     (fn [team-rows]
       (let [played (sort (distinct (map :week team-rows)))
             recent (set (take-last recent-games played))
             by-pos (group-by :position team-rows)
             sum    (fn [rows] (reduce + 0.0 (map :points rows)))]
         (into {}
               (map (fn [pos]
                      (let [total (sum (by-pos pos))]
                        [pos {:games           (count played)
                              :points          total
                              :per-game        (per-game total (count played))
                              :recent-per-game (per-game (sum (filter (comp recent :week) (by-pos pos)))
                                                         (count recent))}])))
               positions))))))

(defn with-ranks
  "`lines` with `:rank` and `:of` on each position's line: 1 is the team that
  allowed the most a game there, ties broken by team so the order is stable."
  [lines]
  (let [positions (distinct (mapcat (comp keys val) lines))]
    (reduce (fn [acc pos]
              (let [ranked (sort-by (juxt #(- (get-in lines [% pos :per-game] 0.0)) identity)
                                    (filter #(get-in lines [% pos]) (keys lines)))
                    n      (count ranked)]
                (reduce (fn [acc [i team]]
                          (update-in acc [team pos] assoc :rank (inc i) :of n))
                        acc
                        (map-indexed vector ranked))))
            lines
            positions)))

(defn points-allowed
  "`{team {position {:games :points :per-game :recent-per-game :rank :of}}}`:
  what each team has allowed each position through `through-week` under
  `scoring`. Empty before a week has been played."
  [players scoring through-week]
  (if-not (and (number? through-week) (pos? through-week))
    {}
    (-> (allowed-rows players (scoring/resolve-buckets scoring) through-week)
        team-lines
        with-ranks)))

(defn ordinal
  "1st, 2nd, 3rd, 4th, 11th, 12th, 13th, 21st."
  [n]
  (str n (if (<= 11 (mod n 100) 13)
           "th"
           (case (mod n 10) 1 "st" 2 "nd" 3 "rd" "th"))))

(defn rank-label
  "A rank with its direction in the words: `8th most` in the top half, `3rd
  fewest` in the bottom, counted from that end."
  [rank of]
  (if (<= rank (quot (inc of) 2))
    (str (ordinal rank) " most")
    (str (ordinal (- (inc of) rank)) " fewest")))

(defn with-opp-vs-pos
  "Assoc `:opp-vs-pos`, his opponent this week's line at his position from
  `table`, with `:team` naming the opponent. Run after the week's opponent is
  joined; a bye or an opponent with no games leaves the row as it was."
  [players table]
  (mapv (fn [p]
          (let [opp (or (:week/opponent p) (:kickoff/opponent p))]
            (if-let [line (get-in table [opp (:position p)])]
              (assoc p :opp-vs-pos (assoc line :team opp))
              p)))
        players))
