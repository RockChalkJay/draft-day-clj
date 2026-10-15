(ns draft-day.rankings.ros
  "Blend preseason and realized production into rest-of-season totals. The
  realized rate is shrunk toward the preseason rate using a fixed prior, then
  prorated over the player's remaining games. A stat projected for no player
  takes no prior; a missing realized line therefore preserves the projection.

  A player with a serious designation (`db/serious-injury?`) has no games
  remaining in season, until Sleeper clears the tag."
  (:require [draft-day.db :as db]
            [draft-day.scoring :as scoring]))

(def PRIOR-GAMES
  "Number of prior games represented by the preseason projection."
  6.0)

(def ^:const RECENT-WEIGHT
  "Share of the realized rate taken from the recent window rather than the whole
  season, once the window holds a full `recent-window` of games. A back promoted
  by an injury ahead of him is not the player his bench weeks describe. CHOSEN,
  NOT MEASURED — same standing as `PRIOR-GAMES`."
  0.5)

(def ^:const RECENT-WINDOW
  "Games in a full recent window; a shorter one earns proportionally less weight."
  3.0)

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
  game count, and keys outside `projected` are treated as having no prior."
  [{:keys [pre realized played games-remaining season-games prior-games projected
           recent recent-games recent-weight]}]
  (let [played      (double (or played 0))
        prior       (double (or prior-games PRIOR-GAMES))
        season-games (double season-games)
        recent-games (double (or recent-games 0))
        w            (if (pos? recent-games)
                       (* (double (or recent-weight RECENT-WEIGHT))
                          (min 1.0 (/ recent-games RECENT-WINDOW)))
                       0.0)]
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
                          recent-rate (if (pos? recent-games)
                                        (/ (double (get recent k 0.0)) recent-games)
                                        rate)
                          total  (* (+ (* (- 1.0 w) rate) (* w recent-rate))
                                    games-remaining)]
                      (when-not (zero? total) [k total]))))
            (into (set (keys pre)) (keys realized))))))

(defn- ros-for
  "Return one player's rest-of-season columns, or nil when no games remain."
  [{:keys [stats] :as player} {:keys [through-week season-games prior-games projected]}]
  ;; Preseason tags (PUP, NFI) clear before the season, so week 0 ignores them.
  (let [out?   (and (pos? (long (or through-week 0)))
                    (db/serious-injury? (:sleeper/injury-status player)))
        left   (if out?
                 0.0
                 (games-remaining (assoc player :through-week through-week
                                         :season-games season-games)))
        ;; One source per player: the window comes from the same feed as the season.
        sleeper? (some? (:realized/season-to-date player))
        realized (if sleeper?
                   (:realized/season-to-date player)
                   (:nflverse/season-to-date player))
        recent   (if sleeper? (:realized/recent player) (:nflverse/recent player))
        played (get realized :games 0)
        real   (get realized :stats {})
        line   (blend {:pre stats :realized real :played played
                       :games-remaining left :season-games season-games
                       :prior-games prior-games :projected projected
                       :recent (:stats recent) :recent-games (:games recent)})]
    (when line
      {:ros/stats           line
       :ros/games-remaining left
       :ros/games-played    played})))

(defn with-ros
  "Add rest-of-season columns and a sortable `:ros-points` score to every player.
  The full board is required so projected stat keys are inferred across players."
  [board scoring {:keys [season-games] :as ctx}]
  (when-not (and (number? season-games) (pos? season-games))
    (throw (ex-info "rest-of-season projection needs :season-games"
                    {:season-games season-games})))
  (let [scoring (scoring/resolve-buckets scoring)
        ctx     (assoc ctx :projected
                       (into #{} (mapcat (comp keys :stats)) board))]
    (mapv (fn [p]
            (let [cols (ros-for p ctx)]
              (assoc (merge p cols)
                     :ros-points (if cols
                                   (scoring/resolved-points {:stats (:ros/stats cols)} scoring)
                                   0.0))))
          board)))
