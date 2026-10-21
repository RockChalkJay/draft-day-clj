(ns draft-day.rankings.ros
  "Blend projected and realized production into rest-of-season totals. The
  realized rate is shrunk toward the projected rate using a fixed prior, then
  prorated over the player's remaining games. A stat projected for no player
  takes no prior; a missing realized line therefore preserves the projection.

  The projection is Sleeper's outlook where it has one: the sum of its weekly
  lines for the weeks left (`sleeper/fetch-outlook`), regenerated against the
  current depth chart. August's season line is the fallback, for a player the
  outlook omits and for every board with no outlook (offline, a replayed week).
  The August line cannot see a promotion: a back who took over in week 3 is
  still projected at 1.5 a game by it and 13 a game by the outlook, and six
  prior games on the wrong rate priced him at a third of his production.

  A player with a serious designation (`db/serious-injury?`) has no games
  remaining in season, until Sleeper clears the tag.

  How strongly the preseason line is trusted was measured on 2021-25
  (`benchmark/ros_backtest.clj`): six games for every position but defenses,
  which take sixty. The same harness tested a player who has taken over a role a
  teammate vacated, predicted from his games since the change, and it did not
  beat the whole-season rate where it fires (backs -0.9 +- 0.7 squared points a
  game, receivers +0.2 +- 0.3), so the role is display-only (`rankings.role`).
  It scores the August prior; the outlook prior cannot be replayed, so a
  strength measured there is a floor for it.

  The rate is the whole season's, with no weight on the last few games.
  `benchmark/ros_recent.clj` replayed 2022-25: blending recent points, gating
  that on a jump in targets plus carries, and blending opportunity alone all
  predicted the rest of the season worse than the season rate did. A promotion
  caused by a teammate's injury is not identified there, so it is untested."
  (:require [draft-day.db :as db]
            [draft-day.scoring :as scoring]))

(def PRIOR-GAMES
  "Number of prior games represented by the preseason projection. Six holds for
  quarterbacks, backs, receivers, tight ends and kickers: `benchmark/ros_backtest.clj`
  over 2021-25 finds their error flat from about four to twelve, with no strength
  distinguishable from six, and the best one varying by season."
  6.0)

(def PRIOR-GAMES-BY-POSITION
  "Where six is measurably wrong. A defense's weekly points are mostly noise, and
  the backtest's error falls as the preseason line gets more weight: sixty is 0.6
  squared points a game better than six, 4.6 standard errors, better in four of
  five seasons; preseason alone was no better than sixty. The harness's prior is
  sparser than production's, which should favour the larger strength if
  anything, but the live fill cannot be replayed for past seasons, so that is
  expected and not shown."
  {"DST" 60.0})

(defn prior-games-for
  "The prior strength for a player at `position`."
  [position]
  (get PRIOR-GAMES-BY-POSITION position PRIOR-GAMES))

(defn games-remaining
  "Return games remaining after `through-week`, subtracting a future bye.
  Players without a recorded bye receive the expected fractional bye."
  [{:keys [through-week season-games bye]}]
  (let [season-weeks (inc (long season-games))
        weeks-left   (max 0 (- season-weeks (long (or through-week 0))))
        bye-left     (cond
                       (nil? bye)                  (/ (double weeks-left) season-weeks)
                       (> (long bye) (long (or through-week 0))) 1.0
                       :else                       0.0)]
    (max 0.0 (- weeks-left bye-left))))

(defn blend
  "Blend projected and realized totals by stat key. `played` is the player's
  game count, and keys outside `projected` are treated as having no prior.
  `pre` is a total over `pre-games` games, the season's unless given."
  [{:keys [pre realized played games-remaining season-games pre-games prior-games projected]}]
  (let [played      (double (or played 0))
        prior       (double (or prior-games PRIOR-GAMES))
        season-games (double (or pre-games season-games))]
    (when (and (pos? (+ prior played)) (pos? games-remaining))
      (into {}
            (keep (fn [k]
                    (let [prior-k (if (and projected (not (contains? projected k)))
                                    0.0
                                    prior)
                          denom  (+ prior-k played)
                          pre-pg (/ (double (get pre k 0.0)) season-games)
                          rate   (if (pos? denom)
                                   (/ (+ (* prior-k pre-pg) (double (get realized k 0.0)))
                                      denom)
                                   0.0)
                          total  (* rate games-remaining)]
                      (when-not (zero? total) [k total]))))
            (into (set (keys pre)) (keys realized))))))

(defn- ros-for
  "Return one player's rest-of-season columns, or nil when no games remain."
  [{:keys [stats] :as player} {:keys [through-week season-games prior-games projected]}]
  ;; Preseason tags (PUP, NFI) clear before the season, so week 0 ignores them.
  (let [out?   (and (pos? (long (or through-week 0)))
                    (db/serious-injury? (:sleeper/injury-status player)))
        ahead  (:outlook/games player)
        outlook? (and (number? ahead) (pos? ahead))
        left   (cond
                 out?      0.0
                 outlook?  (double ahead)
                 :else     (games-remaining
                            (merge player
                                   {:through-week through-week
                                    :season-games season-games})))
        realized (or (:realized/season-to-date player)
                     (:nflverse/season-to-date player))
        played (get realized :games 0)
        real   (get realized :stats {})
        line   (blend {:pre (if outlook? (:outlook/stats player) stats)
                       :pre-games (when outlook? ahead)
                       :realized real :played played
                       :games-remaining left :season-games season-games
                       :prior-games (or prior-games (prior-games-for (:position player)))
                       :projected projected})]
    (when line
      {:ros/stats           line
       :ros/games-remaining left
       :ros/games-played    played})))

(defn- projected-keys
  "Return all stat keys that have a preseason or outlook projection on the board."
  [board]
  (into #{}
        (mapcat (fn [player]
                  (concat (keys (:stats player))
                          (keys (:outlook/stats player)))))
        board))

(defn- enrich-player-with-ros
  "Add rest-of-season columns and ROS points for one player."
  [player ctx scoring]
  (let [cols        (ros-for player ctx)
        ros-points (if cols
                     (scoring/resolved-points {:stats (:ros/stats cols)} scoring)
                     0.0)]
    (assoc (merge player cols) :ros-points ros-points)))

(defn with-ros
  "Add rest-of-season columns and a sortable `:ros-points` score to every player.
  The full board is required so projected stat keys are inferred across players."
  [board scoring {:keys [season-games] :as ctx}]
  (when-not (and (number? season-games) (pos? season-games))
    (throw (ex-info "rest-of-season projection needs :season-games"
                    {:season-games season-games})))
  (let [scoring (scoring/resolve-buckets scoring)
        ctx     (assoc ctx :projected (projected-keys board))]
    (mapv #(enrich-player-with-ros % ctx scoring) board)))
