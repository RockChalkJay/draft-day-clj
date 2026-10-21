(ns draft-day.rankings.role
  "Who has taken over a role, and from whom. Display only: nothing here feeds a
  projection or a score, for the reason `rankings.ros` gives about recent games,
  and whether a promotion caused by a teammate's injury predicts better is a
  question for a backtest.

  A vacancy is a player at a position on a team who held at least half of its
  offensive snaps in some week, now carries a serious designation
  (`db/serious-injury?`), and has stayed under a quarter of them since the week
  after the last one he held. That week is the change point. A teammate at the
  position has taken the role when he has averaged at least a quarter of the
  snaps since then and more than he had before. Snap share is the player's snaps
  over his team's, off Sleeper's weekly counts (`sleeper-actual/game-log`), and a
  week he did not play counts as zero since the change point: it is what a
  vacancy is made of.

  The share a row shows is over the weeks he played, so a bye is not a zero."
  (:require [draft-day.db :as db]))

(def held-share
  "The share of snaps that makes a player the one whose exit is a vacancy."
  0.5)

(def gone-share
  "The share he stays under once he has left."
  0.25)

(def took-share
  "The share a teammate must average since the change point to have the role."
  0.25)

(def recent-weeks
  "How many of the most recent weeks the displayed recent share spans."
  3)

(def role-positions #{"QB" "RB" "WR" "TE"})

(defn snap-share
  "A game-log entry's share of his team's offensive snaps, or nil unsaid."
  [{:keys [snaps team-snaps]}]
  (when (and (number? snaps) (number? team-snaps) (pos? team-snaps))
    (/ (double snaps) team-snaps)))

(defn shares-by-week
  "`{week share}` over the weeks a log entry states one for."
  [player]
  (into {} (keep (fn [{:keys [week] :as e}]
                   (when-let [s (snap-share e)] [week s])))
        (:realized/game-log player)))

(defn mean [xs]
  (when (seq xs) (/ (reduce + 0.0 xs) (count xs))))

(defn change-point
  "The first week from which `player` stayed under `gone-share` through
  `through-week`, having held `held-share` before it; nil if he never did. A week
  with no entry is a week under."
  [player through-week]
  (let [shares (shares-by-week player)
        at     (fn [w] (get shares w 0.0))
        held?  (fn [w] (>= (at w) held-share))
        gone?  (fn [w] (< (at w) gone-share))
        weeks  (range 1 (inc (long through-week)))
        ;; The last week he held the role; the exit is the week after it.
        last-held (last (filter held? weeks))
        exit   (some-> last-held inc)]
    (when (and exit (<= exit through-week)
               (every? gone? (range exit (inc (long through-week)))))
      exit)))

(defn vacancies
  "`{[team position] {:since week :by player}}` for every team and position that
  lost the player who held it. A team with two such players keeps the earlier
  exit."
  [players through-week]
  (reduce (fn [acc p]
            (if-let [w (and (contains? role-positions (:position p))
                            (db/serious-injury? (:sleeper/injury-status p))
                            (change-point p through-week))]
              (update acc [(:team p) (:position p)]
                      (fn [old] (if (and old (<= (:since old) w)) old {:since w :by p})))
              acc))
          {}
          players))

(defn took-over?
  "Whether `player` has the role vacated in `since`: more snaps since than
  before, and at least `took-share` of them."
  [player since through-week]
  (let [shares (shares-by-week player)
        at     (fn [w] (get shares w 0.0))
        after  (mean (map at (range since (inc (long through-week)))))
        before (mean (map at (range 1 since)))]
    (boolean (and after (>= after took-share) (> after (or before 0.0))))))

(defn recent-share
  "His mean snap share over the weeks he played inside the last `recent-weeks`
  of the season so far, or nil."
  [player through-week]
  (mean (for [[w s] (shares-by-week player) :when (> w (- through-week recent-weeks))] s)))

(defn season-share
  "His mean snap share over the weeks he played, or nil."
  [player]
  (mean (vals (shares-by-week player))))

(defn with-role
  "Assoc `:snap-share` (recent), `:snap-share-season`, and for a player who took
  over a vacated role `:role/since` (the change point), `:role/vacated-by` (the
  player's name) and `:role/vacated-status` (his designation). Run before the
  game log is stripped. A board with no snap counts gains nothing."
  [players through-week]
  (if-not (and (number? through-week) (pos? through-week))
    players
    (let [vacant (vacancies players through-week)]
      (mapv (fn [p]
              (let [recent (recent-share p through-week)
                    season (season-share p)
                    {:keys [since by]} (get vacant [(:team p) (:position p)])
                    took?  (and since
                                (not= (:player-id p) (:player-id by))
                                (took-over? p since through-week))]
                (cond-> p
                  recent (assoc :snap-share recent)
                  season (assoc :snap-share-season season)
                  took?  (assoc :role/since since
                                :role/vacated-by (:player-name by)
                                :role/vacated-status (:sleeper/injury-status by)))))
            players))))
