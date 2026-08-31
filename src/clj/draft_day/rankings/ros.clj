(ns draft-day.rankings.ros
  "Rest-of-season projection: the preseason line, corrected by what has actually
  happened, prorated over the games that are left.

  The board's `:stats` are a *preseason, full-season* projection. In September
  that is the best evidence there is. In November it is a claim about a season
  that has half happened, made by someone who has not looked — and a waiver
  board built on it ranks the manager's own draft, which is the one ranking he
  already has.

  THE BLEND. Per stat key, on a per-game basis, shrinking the realized rate
  toward the projected one:

      pre-pg = preseason-season-total / season-games
      ros-pg = (prior-games * pre-pg + realized-total) / (prior-games + played)
      ros    = ros-pg * games-remaining

  `realized-total` rather than `played * realized-per-game` is the same quantity
  with the division cancelled, and cancelling it is what makes the whole thing
  total: **a player with no games played needs no special case**. At `played` 0
  the expression collapses to `pre-pg`, i.e. the prorated preseason line, which
  is exactly the right answer for a rookie who has not debuted, for a defense
  (nflverse publishes no DST row at all), and for the entire board in August.

  WHAT `prior-games` MEANS. It is how many games of realized evidence the
  preseason projection is worth. At `PRIOR-GAMES` 6 a week-3 breakout is still
  two-thirds projection, and by week 12 the season in progress has the floor.
  It is a **chosen constant, not a measured one** — the same stance
  `nflverse/availability-lookback` takes about its window, and for the same
  reason: `dev/draft_day/benchmark/` is where a number earns its place, and this
  one has not been there yet.

  A PLAYER WITH NO PRESEASON LINE — the undrafted rookie who is now the lead
  back — is the player this whole feature exists for, and he is handled by the
  same expression: `pre-pg` is 0, so his rate is his realized total spread over
  `prior-games + played` instead of `played`. He is therefore *deliberately
  discounted early* and climbs as the games accumulate. One 140-yard game does
  not make him a starter; five of them do. Setting `PRIOR-GAMES` is precisely
  the choice of how loud one week is allowed to be.

  WEEK ZERO IS SAFE BY CONSTRUCTION. `:through-week` and the in-season columns
  come from the same fetch (`ingestion.nflverse-weekly/fetch`), so they cannot
  disagree: if the weekly file is missing — preseason, or a failed fetch in
  November — the week reads 0 *and* no player has a realized line, and the whole
  board collapses to its preseason projection. That is the honest degradation,
  and it is why the two facts must keep travelling together.

  WHAT THIS DOES NOT KNOW. A player who has been out since week 2 still carries
  a full share of the games remaining; nothing here reads a designation. That is
  `:injury-risk` and `:sleeper/injury-status`' job on the board, and merging
  them into the projection would be the double-charging `rankings.injury`'s
  docstring already argues against."
  (:require [draft-day.scoring :as scoring]))

(def PRIOR-GAMES
  "How many games of realized evidence the preseason projection is worth.

  Six — a bit over a third of a season. Below about four, one loud game
  reorders the board; much above eight and a role change in September is still
  being argued with in December. See the ns docstring: chosen, not measured."
  6.0)

(defn games-remaining
  "How many games this player has left after `through-week`.

  Weeks and games are not the same number and conflating them is the trap here.
  Since 2021 a team plays `season-games` games across `season-games + 1` weeks,
  because it sits exactly one of them out — so the games left are the weeks left
  minus the bye, when the bye is still ahead.

  A player with no bye on record (no team, so `sleeper/assoc-byes` had nothing
  to key on) gets the *expected* bye instead: the chance a uniformly-placed bye
  falls in the weeks that remain. It costs a fractional game and keeps a
  teamless player from quietly gaining one, which is the direction the error
  would otherwise run all season."
  [{:keys [through-week season-games bye]}]
  (let [season-weeks (inc (long season-games))
        weeks-left   (max 0 (- season-weeks (long (or through-week 0))))
        bye-left     (cond
                       (nil? bye)                  (/ (double weeks-left) season-weeks)
                       (> (long bye) (long (or through-week 0))) 1.0
                       :else                       0.0)]
    (max 0.0 (- weeks-left bye-left))))

(defn blend
  "Pure: preseason season totals + realized totals -> rest-of-season totals.

  `played` is the player's own game count, never weeks elapsed — see
  `nflverse-weekly/accumulate` on why dividing by weeks charges an absence
  twice. Keys are the union of both lines: a stat one side never mentions
  contributes 0 from that side, which for a projection is what silence means."
  [{:keys [pre realized played games-remaining season-games prior-games]}]
  (let [played      (double (or played 0))
        prior       (double (or prior-games PRIOR-GAMES))
        denom       (+ prior played)
        season-games (double season-games)]
    (when (and (pos? denom) (pos? games-remaining))
      (into {}
            (keep (fn [k]
                    (let [pre-pg (/ (double (get pre k 0.0)) season-games)
                          rate   (/ (+ (* prior pre-pg) (double (get realized k 0.0)))
                                    denom)
                          total  (* rate games-remaining)]
                      ;; A stat neither side has anything to say about stays out
                      ;; of the line entirely rather than landing as 0.0 — the
                      ;; same BLANK IS NOT ZERO rule the ingestion side keeps,
                      ;; so a consumer can still tell silence from futility.
                      (when-not (zero? total) [k total]))))
            (into (set (keys pre)) (keys realized))))))

(defn ros-for
  "Pure: one player + context -> his rest-of-season columns, or nil when there is
  nothing left to project.

  Returns `{:ros/stats :ros/games-remaining :ros/games-played}`. The stat line
  is in the same Sleeper vocabulary as `:stats`, which is the whole point: every
  scoring weight, every model and every later stage reads it without translating
  between two spellings of the same stat."
  [{:keys [stats] :as player} {:keys [through-week season-games prior-games]}]
  (let [left   (games-remaining (assoc player :through-week through-week
                                       :season-games season-games))
        played (get-in player [:nflverse/season-to-date :games] 0)
        real   (get-in player [:nflverse/season-to-date :stats] {})
        line   (blend {:pre stats :realized real :played played
                       :games-remaining left :season-games season-games
                       :prior-games prior-games})]
    (when line
      {:ros/stats           line
       :ros/games-remaining left
       :ros/games-played    played})))

(defn with-ros
  "Assoc the rest-of-season columns and `:ros-points` on every player.

  `:ros-points` is scored from `:ros/stats` under the league's own weights via
  the same `scoring/player-points` the preseason board uses, so a rest-of-season
  board and a draft board are the same board asked about different games.

  A player with nothing left to project — the season is over, or he has no line
  on either side — gets `:ros-points` 0 rather than nil. Downstream this is a
  score that sorts and subtracts (`replacement/with-vorp` reads it with
  `score-key`), and nil in a sort key throws where nil in a *display* column
  merely renders a dash. The columns that would explain the number are simply
  absent, which is what the board reads as 'nothing here'."
  [board scoring {:keys [season-games] :as ctx}]
  ;; `season-games` is passed in rather than known here, so `rankings` keeps no
  ;; NFL calendar of its own — `ingestion.nflverse/games-in-season` is the one
  ;; copy, and it already has to reach back past the 16-game era for the
  ;; benchmark harness. Missing, it would reach `games-remaining` as a nil and
  ;; NPE somewhere unhelpful, so say what is actually wrong.
  (when-not (and (number? season-games) (pos? season-games))
    (throw (ex-info "rest-of-season projection needs :season-games"
                    {:season-games season-games})))
  (mapv (fn [p]
          (let [cols (ros-for p ctx)]
            (assoc (merge p cols)
                   :ros-points (if cols
                                 (scoring/player-points {:stats (:ros/stats cols)} scoring)
                                 0.0))))
        board))
