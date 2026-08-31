(ns draft-day.rankings.waiver
  "The waiver board: who is free, how much better he is than what you would have
  to drop, and what share of your FAAB he is worth.

  Everything here is measured in **rest-of-season points** (`:ros-points`, from
  `rankings.ros`), never in auction dollars. The draft board's Value and Worth
  price a whole roster out of a fixed bankroll at a preseason auction; a waiver
  claim is one seat against a budget that is spent down over months. Reusing
  those dollars here would be the same category error as reading an overall
  expert tier as a positional one.

  So `value`, `inflation`, `tcm`, `worth`, `bargain` and `market` are all
  deliberately absent, and the reused pieces are the ones that are actually
  about football rather than about an auction: `replacement/replacement-levels`
  and `with-vorp`, which already accept a `score-key` and so run on
  `:ros-points` untouched.

  THE THREE ANSWERS.

  `:upgrade` is the real waiver question. A claim costs a *roster spot*, not a
  positional slot, so the thing you give up is your worst player, not your worst
  player at his position. With a spot already open you give up nothing and the
  upgrade is his whole rest-of-season line.

  `:bid` is a conserving share of your remaining budget, and what it conserves
  against is the part worth stating: not every free agent, but the best
  `claims-left` of them, where `claims-left` is how many waiver runs the season
  has left. That bound is read off the calendar rather than chosen, and it is
  what makes the number behave like FAAB actually behaves — many runs left means
  small bids, one run left means spend it. Over those top claims the bids sum to
  the budget, which is the property `waiver-test` pins.

  A `:bid` of $0 is a real bid, not a refusal. FAAB accepts one, and a player
  whose upgrade rounds to nothing is honestly worth the minimum — unlike the
  auction board, where $0 meant a player nobody should draft and the $1 floor
  existed to say so.

  `:rival-max` is not a formula at all, it is the largest budget anyone else
  still holds. It is the most useful number on the screen precisely because it
  is a fact.

  WHAT IS DISPLAY ONLY. `:trend` compares recent opportunity to the season's and
  feeds nothing — same shelf as `:injury-risk` and `:tcm`, and for the reason
  `rankings.injury` spells out: the repo has already shipped one signal that was
  computed on every pick and consumed by nothing."
  (:require [draft-day.db :as db]
            [draft-day.rankings.replacement :as replacement]))

;; ---- who is available ----

(defn held-ids
  "One team's roster ids in the *board's* id space.

  Roster ids arrive as the provider's (see `league-sync.sleeper`); the board is
  keyed by GSIS wherever one resolved. `xwalk` is `db/sleeper->player-id`, and an
  id it has no entry for maps to itself — team defenses carry their abbreviation
  in both id spaces, and an unmapped id is not evidence that it is wrong, exactly
  as `db/remap-draft-ids` argues.

  Every reader of a roster goes through this. It exists as a named function
  rather than inline in `rostered-index` because the *second* reader is what
  went wrong: the availability filter translated its ids and the drop candidate
  did not, so `by-id` resolved almost nothing, every roster looked empty of
  droppable players, and every upgrade on the board was measured against a floor
  of zero. The tests missed it because a fixture where player-id equals the
  Sleeper id makes the crosswalk a no-op — which no real league is.

  `k` selects which of the team's id lists to read: `:player-ids` for who is
  unavailable, `:active-ids` for who occupies a seat a claim would need."
  ([team xwalk] (held-ids team xwalk :player-ids))
  ([team xwalk k] (mapv (fn [id] (get xwalk id id)) (get team k))))

(defn rostered-index
  "`{canonical-player-id team-name}` over every team in the synced league."
  [teams xwalk]
  (into {}
        (mapcat (fn [{:keys [name] :as team}]
                  (map (fn [id] [id name]) (held-ids team xwalk))))
        teams))

(defn free-agents
  "The board minus everyone on a roster."
  [board rostered]
  (filterv #(not (contains? rostered (:player-id %))) board))

;; ---- what a claim actually costs ----

(defn drop-candidate
  "The player a claim would cost me: the lowest `:ros-points` player holding one
  of my active seats, or nil when a seat is already open.

  `held` is already in the board's id space and already excludes IR and taxi —
  see `held-ids` and `league-sync.sleeper/normalize-roster`. Both exclusions
  matter and in opposite directions: a player parked on IR occupies no active
  seat, so counting him fills a roster that is not full, while dropping him
  frees no seat for the claim being priced.

  Rostered ids the board cannot value are skipped rather than treated as
  worthless. A missing row usually *does* mean a player who has fallen off the
  board — genuinely the man to drop — but 'we have no projection for him' and
  'he is projected to score nothing' are different claims, and only one of them
  is evidence. Skipping keeps the named drop a player the manager can check.

  `roster-size` is how many seats the league gives each team. Absent, the roster
  is treated as full: naming a drop that was not needed costs a suggestion,
  while missing one that was needed costs a roster spot the manager did not
  know he was spending."
  [held by-id roster-size]
  (when-not (and roster-size (< (count held) roster-size))
    (->> held
         (keep #(get by-id %))
         (sort-by #(double (or (:ros-points %) 0.0)))
         first)))

(defn with-upgrade
  "Assoc `:upgrade` — rest-of-season points gained by making the claim — on every
  free agent, plus `:drop-candidate` naming the seat it costs.

  The floor is the drop's own rest-of-season points, or 0 when nothing has to be
  dropped. It is deliberately *not* his replacement level: replacement is the
  right baseline for a draft, where every team fills the same slots from the same
  pool, but the question here is what leaves this manager's roster, and that is a
  specific player."
  [fas drop]
  (let [floor (double (or (:ros-points drop) 0.0))]
    (mapv (fn [p]
            (assoc p
                   :upgrade (- (double (or (:ros-points p) 0.0)) floor)
                   :drop-candidate (when drop
                                     (select-keys drop [:player-id :player-name
                                                        :position :ros-points]))))
          fas)))

;; ---- what to bid ----

(defn claims-left
  "How many waiver runs the season has left — one a week.

  The bound the bid conserves against, and it is read off the calendar rather
  than chosen. `playoff-week-start` is the honest end when the league reports it:
  claims made once the fantasy playoffs are under way buy at most a game or two.
  Without it the NFL regular season is the fallback, which errs long and so errs
  toward bidding conservatively.

  At least 1 whenever any week remains, because a budget with one run left is
  still a budget; 0 once there is nothing left to claim for."
  [{:keys [through-week season-games playoff-week-start]}]
  (let [end  (or playoff-week-start (+ 2 (long season-games)))
        left (- (long end) 1 (long (or through-week 0)))]
    (max 0 left)))

(defn bid-pool
  "The total upgrade the bids are a share of: the best `n` upgrades available.

  Summing over *every* free agent instead would divide the budget among hundreds
  of players a manager will never claim, and every real target would round to
  nothing. `n` is the number of claims the season still allows, so the pool is
  the set of players he could actually still add."
  [fas n]
  (->> fas
       (map #(max 0.0 (double (or (:upgrade %) 0.0))))
       (sort >)
       (take n)
       (reduce + 0.0)))

(defn faab?
  "Does this league run FAAB? True for `:faab` and for its JSON spelling.

  The league reaches this namespace two ways: straight from `league-sync`, where
  the type is a keyword, and round-tripped through the browser, where it is a
  plain string — `read-json-body` keywordizes *keys*, not values. Testing the
  keyword alone therefore passed every server-side test and produced a nil bid
  for every real request. Same family as the drift `scoring/resolve-config`
  exists to stop, and handled the same way: one predicate that accepts both
  spellings, rather than a coercion repeated at each caller."
  [type]
  (= :faab (when type (keyword type))))

(defn with-bids
  "Assoc `:bid` on every free agent: his share of the remaining budget.

  nil rather than a number in the two cases where there is no bid to make — a
  league that does not run FAAB, and a manager with nothing left to spend. A
  zero would read as 'worth nothing' when the truth is 'there is nothing to
  bid', which is the same distinction `league-sync` keeps by reporting
  `:faab-left` nil outside FAAB."
  [fas {:keys [type]} budget-left n]
  (let [pool (bid-pool fas n)]
    (if-not (and (faab? type) (number? budget-left) (pos? budget-left) (pos? pool))
      (mapv #(assoc % :bid nil) fas)
      (mapv (fn [p]
              (let [up (max 0.0 (double (or (:upgrade p) 0.0)))]
                (assoc p :bid (-> (* (/ up pool) budget-left)
                                  (min budget-left)
                                  Math/rint
                                  long
                                  (max 0)))))
            fas))))

(defn rival-max
  "The largest budget anyone *else* still holds — what it would take to be sure.

  nil when no rival reports one, which outside FAAB is every rival."
  [teams my-roster-id]
  (let [others (remove #(= (:roster-id %) my-roster-id) teams)]
    (when-let [ls (seq (keep :faab-left others))]
      (apply max ls))))

;; ---- display ----

(defn trend
  "Recent opportunity per game over the season's, or nil.

  Volume, not points: a back who has taken over the carries is a buy before the
  touchdowns arrive, and a receiver whose targets have dried up is a sell while
  his season line still looks fine. Above 1.0 means the role is growing.

  DISPLAY ONLY — nothing reads this. See the ns docstring."
  [{:nflverse/keys [season-to-date recent]}]
  (let [per-game (fn [{:keys [games usage]}]
                   (when (and games (pos? games))
                     (/ (+ (double (get usage :targets 0.0))
                           (double (get usage :carries 0.0)))
                        games)))
        season   (per-game season-to-date)
        window   (per-game recent)]
    (when (and season window (pos? season))
      (/ window season))))

(defn with-trend [fas]
  (mapv (fn [p] (assoc p :trend (trend p))) fas))

;; ---- orchestration ----

(defn with-ros-vorp
  "Replacement level and VORP computed on `:ros-points`, landing on `:ros-vorp`.

  Over the **whole** board, not just the free agents: replacement level is a
  property of what the league's starting lineups demand, not of who happens to
  be unclaimed this week. Scoped to the free agents it would drift down every
  time a good player was added and make the remaining scraps look like starters.

  Renamed off `:vorp` because the client already reads that key as the draft
  board's preseason, full-season number, and two different scales under one name
  is the mistake `engine/static-rankings` documents about expert tiers."
  [board num-teams replacement-config]
  (let [levels (replacement/replacement-levels board num-teams
                                               (or replacement-config {}) :ros-points)]
    {:levels levels
     :players (mapv (fn [p] (-> p (assoc :ros-vorp (:vorp p)) (dissoc :vorp)))
                    (replacement/with-vorp board levels :ros-points))}))

(defn waiver-board
  "The whole answer: `{:players :rostered :faab :claims-left :replacement-levels}`.

  `:players` is the free agents only. Shipping the rostered ones too would be
  most of the universe re-sent on every refresh for rows the board does not
  render; `:rostered` is the compact `{player-id team-name}` index that answers
  'who has him' instead.

  A league with no synced rosters is not an error — it is a manager who has not
  connected one yet. Everyone is free, there is nothing to drop and no budget to
  bid, and the board is a rest-of-season ranking, which is a useful thing on its
  own."
  [board {:keys [league my-roster-id roster-size num-teams replacement-config] :as ctx}]
  (let [{:keys [teams waiver]} league
        xwalk    (db/sleeper->player-id board)
        rostered (rostered-index teams xwalk)
        {:keys [levels players]} (with-ros-vorp board num-teams replacement-config)
        by-id    (db/index-by-id players)
        my-team  (first (filter #(= (:roster-id %) my-roster-id) teams))
        ;; The synced league's own seat count wins over the request's. The
        ;; browser derives its fallback from the *draft* config, which a manager
        ;; who synced without importing has never set to match this league — and
        ;; a wrong seat count decides the one question `drop-candidate` asks.
        seats    (or (:roster-size league) roster-size)
        drop     (when my-team
                   (drop-candidate (held-ids my-team xwalk :active-ids) by-id seats))
        n        (claims-left ctx)]
    {:players            (-> (free-agents players rostered)
                             (with-upgrade drop)
                             (with-bids waiver (:faab-left my-team) n)
                             with-trend)
     :rostered           rostered
     :replacement-levels levels
     :claims-left        n
     :faab               {:type      (:type waiver)
                          :budget    (:budget waiver)
                          :left      (:faab-left my-team)
                          :rival-max (rival-max teams my-roster-id)}}))
