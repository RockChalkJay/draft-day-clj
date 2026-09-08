(ns draft-day.rankings.waiver
  "The waiver board: who is free, how much better he is than what you would have
  to drop, and what share of your FAAB he is worth.

  Everything here is measured in **points**, never in auction dollars:
  rest-of-season (`:ros-points`, from `rankings.ros`) for every ranking and
  every claim, with a single week (`:week-points`) as a display column beside
  it. The draft board's Value and Worth price a whole roster out of a fixed
  bankroll at a preseason auction; a waiver claim is one seat against a budget
  that is spent down over months. Reusing those dollars here would be the same
  category error as reading an overall expert tier as a positional one.

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
  `:form-points` scores what the last few weeks were actually worth; both feed
  nothing — same shelf as `:injury-risk` and `:tcm`, and for the reason
  `rankings.injury` spells out: the repo has already shipped one signal that was
  computed on every pick and consumed by nothing. `:week-pos-rank` is the one
  in-season signal that is *not* on that shelf, and it is added elsewhere — see
  `rankings.pos-rank`.

  ROSTER IDS GO THROUGH `held-ids`, ALWAYS. Roster ids arrive as the provider's
  (see `league-sync.sleeper`); the board is keyed by GSIS wherever one resolved,
  and `db/sleeper->player-id` bridges them. An id the crosswalk has no entry for
  maps to itself — team defenses carry their abbreviation in both spaces, and an
  unmapped id is not evidence of a bug: the universe may be a stale cache or the
  offline sample. It is a named function rather than inline because the *second*
  reader is what went wrong. The availability filter translated its ids and the
  drop candidate did not, so `by-id` resolved almost nothing, every roster
  looked empty of droppable players, and every upgrade on the board was measured
  against a floor of zero. Tests missed it because a fixture where player-id
  equals the Sleeper id makes the crosswalk a no-op — which no real league is.

  WHICH PLAYER A CLAIM COSTS (`drop-candidate`), AND WHY IT IS NOT SIMPLY THE
  LOWEST SCORER. With `slots` the drop is whoever costs the *starting lineup*
  least to lose, ties broken by lowest `:ros-points`. On a deep bench every
  bench player costs nothing, so the tiebreak decides and the answer is the one
  the old points-only rule gave. It diverges exactly where that rule was wrong:
  measured on a real 12-team league, the lowest-scoring active player was the
  manager's *only kicker* — a starter — so every claim was priced as costing his
  whole line and 442 of 457 free agents came out negative. That was not the
  board finding bad claims; it was the board charging every claim for a seat it
  did not have to empty. Without `slots` it keeps the points rule exactly, there
  being no lineup to cost anything against. Rostered ids the board cannot value
  are skipped rather than treated as worthless — 'we have no projection for him'
  and 'he is projected to score nothing' are different claims, and only one is
  evidence.

  IR AND TAXI ARE EXCLUDED, IN OPPOSITE DIRECTIONS. A player parked on IR
  occupies no active seat, so counting him fills a roster that is not full,
  while dropping him frees no seat for the claim being priced. Hence
  `:active-ids` for seats and drops, `:player-ids` for availability.

  `:lineup-upgrade` IS THE HEADLINE. `db/waiver-rank-key` leads with it and
  `with-bids` prices most of the budget on it, so it is no longer the
  display-only signal it shipped as. It earned that: the bench delta it replaces
  put ten quarterbacks on top of a real league's board, none of whom would ever
  start, each carrying an $8 bid. `:upgrade` stays alongside because most of a
  free-agent pool has no lineup effect at all, so it is what keeps that majority
  ordered. Both are left *off* — not set to 0 — when there is no lineup to
  measure against, which is the default state; a zero there is meaningless and
  numerically identical to `:upgrade` beside it, so nothing on screen would say
  it is not answering.

  BIDS COME FROM TWO POOLS (`with-bids`). A player who improves the starting
  lineup is worth real money; a bench stash is worth keeping ordered and cheap.
  `stash-share` splits the budget, each pool conserves its own share, and an
  empty pool hands its share to the other — without that a manager with a single
  lineup upgrade available would leave `stash-share` of his budget unallocated."
  (:require [draft-day.db :as db]
            [draft-day.rankings.lineup :as lineup]
            [draft-day.rankings.replacement :as replacement]
            [draft-day.scoring :as scoring]))

;; ---- who is available ----

(defn held-ids
  "One team's roster ids in the *board's* id space — see the ns docstring, which
  every roster reader is required to come through. `k` selects the list:
  `:player-ids` for who is unavailable, `:active-ids` for who occupies a seat."
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
  "The player a claim would cost me, or nil when a seat is open; absent
  `roster-size` the roster is full. `held` is board-spaced and IR/taxi-free — see
  `held-ids`, `league-sync.sleeper/normalize-roster`, and the ns docstring."
  [held by-id roster-size slots]
  (when-not (and roster-size (< (count held) roster-size))
    (let [players (vec (keep #(get by-id %) held))
          points  #(double (or (:ros-points %) 0.0))]
      (if-not (seq slots)
        (first (sort-by points players))
        (let [full (lineup/lineup-points players slots :ros-points)
              cost (fn [p]
                     (- full (lineup/lineup-points
                              (remove #(= (:player-id %) (:player-id p)) players)
                              slots :ros-points)))]
          ;; Keyed once per player and sorted on the pairs. `sort-by` calls its
          ;; keyfn inside the comparator, so keying in the sort would run a full
          ;; lineup fill O(n log n) times for a value that is fixed per player.
          (->> players
               (map (fn [p] [[(cost p) (points p)] p]))
               (sort-by first)
               first
               second))))))

(defn with-upgrade
  "Assoc `:upgrade` — rest-of-season points gained by the claim — plus
  `:drop-candidate` naming the seat it costs. The floor is the drop's own points,
  not his replacement level: what leaves this roster is a specific player."
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
  "Waiver runs the season has left, one a week — the bound `with-bids` conserves
  against, read off the calendar rather than chosen. `playoff-week-start` is the
  honest end; without it the NFL regular season errs long, and so errs cheap."
  [{:keys [through-week season-games playoff-week-start]}]
  (let [end  (or playoff-week-start (+ 2 (long season-games)))
        left (- (long end) 1 (long (or through-week 0)))]
    (max 0 left)))

(def stash-share
  "The slice of a FAAB budget reserved for players who would not crack the
  starting lineup. CHOSEN, NOT MEASURED — same standing as `ros/PRIOR-GAMES`.
  Only 0.2-4.8% of a real league's free agents have a positive lineup delta."
  0.15)

(defn weights
  "`[lineup-weight stash-weight]`; a player is in exactly one pool. With no
  `:lineup-upgrade` anywhere every lineup weight is 0 and the stash pool takes
  the whole budget, which is the pre-lineup rule exactly — no special case."
  [p]
  (let [lu (max 0.0 (double (or (:lineup-upgrade p) 0.0)))]
    (if (pos? lu)
      [lu 0.0]
      [0.0 (max 0.0 (double (or (:upgrade p) 0.0)))])))

(defn bid-pool
  "The total weight the bids are a share of: the best `n` of them. Summing over
  every free agent would divide the budget among hundreds a manager will never
  claim, and every real target would round to nothing."
  [ws n]
  (->> ws (sort >) (take n) (reduce + 0.0)))

(defn faab?
  "True for `:faab` and its JSON spelling: the league arrives as a keyword from
  `league-sync` and as a string through the browser, so testing only the keyword
  nil'd every real bid. Same family as `scoring/resolve-config`."
  [type]
  (= :faab (when type (keyword type))))

(defn with-lineup-upgrade
  "Assoc `:lineup-upgrade` — what the claim adds to the *starting* lineup, as
  against `:upgrade`'s bench delta. See `rankings.lineup`, and the ns docstring
  for why it is the headline and why it is absent rather than 0 with no lineup."
  [fas roster drop slots]
  (if-not (and (seq slots) (seq roster))
    fas
    (let [before (lineup/lineup-points roster slots :ros-points)]
      (mapv (fn [p]
              (assoc p :lineup-upgrade
                     (lineup/upgrade before roster p drop slots :ros-points)))
            fas))))

(defn with-bids
  "Assoc `:bid` — his share of the remaining budget, from the two pools the ns
  docstring describes. **nil**, not 0, for a league that does not run FAAB or a
  manager with nothing left to spend: 'worth nothing' is a different answer."
  [fas {:keys [type]} budget-left n]
  (let [ws    (mapv weights fas)
        lin   (bid-pool (map first ws) n)
        stash (bid-pool (map second ws) n)]
    (if-not (and (faab? type) (number? budget-left) (pos? budget-left)
                 (pos? (+ lin stash)))
      (mapv #(assoc % :bid nil) fas)
      (let [lin-budget   (if (pos? stash) (* (- 1.0 stash-share) budget-left) budget-left)
            stash-budget (if (pos? lin) (* stash-share budget-left) budget-left)]
        (mapv (fn [p [lu st]]
                (let [share (cond
                              (pos? lu) (* (/ lu lin) lin-budget)
                              (pos? st) (* (/ st stash) stash-budget)
                              :else     0.0)]
                  (assoc p :bid (-> share (min budget-left) Math/rint long (max 0)))))
              fas ws)))))

(defn rival-max
  "The largest budget anyone *else* still holds — what it would take to be sure.

  nil when no rival reports one, which outside FAAB is every rival."
  [teams my-roster-id]
  (let [others (remove #(= (:roster-id %) my-roster-id) teams)]
    (when-let [ls (seq (keep :faab-left others))]
      (apply max ls))))

;; ---- display ----

(defn trend
  "Recent opportunity per game over the season's, or nil; above 1.0 means the
  role is growing. Volume, not points — a back who has taken over the carries is
  a buy before the touchdowns arrive. DISPLAY ONLY, see the ns docstring."
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

;; The other reading of a weekly number, and the reason both are shown. A
;; projection is what a model expects of a player; form is what his current role
;; has actually been worth. They disagree on exactly the players a waiver board
;; exists for — a back who has just taken over a backfield is worth more than a
;; vendor has got round to saying — so this sits beside `:week-points` rather
;; than being blended into it. Blending was measured and bought +0.37%.

(defn form-points
  "Points per game over `nflverse-weekly/recent-window` under the league's own
  weights, or nil before he has played inside it. Scored from the window's own
  stats and per-game for the same reasons `ros.clj` is."
  [{:nflverse/keys [recent]} scoring]
  (let [{:keys [games stats]} recent]
    (when (and games (pos? games) (seq stats))
      (/ (scoring/player-points {:stats stats} scoring) (double games)))))

(defn with-form-points [players scoring]
  (mapv (fn [p]
          (if-let [v (form-points p scoring)] (assoc p :form-points v) p))
        players))

;; ---- this week ----
;; The other half of the question `:ros-points` answers. Rest-of-season says who
;; helps you from here; this says who helps you on Sunday, and the two routinely
;; disagree — which is the point of showing both rather than a blend of them.
;; The line itself is joined at request time by `pipeline/assoc-weekly`
;; (see that section's comment for why it is cached apart from the universe).

(defn with-week-points
  "Score the joined weekly line under the league's own weights. A player Sleeper
  did not project this week gets no key at all rather than a zero — not playing
  and projected to do nothing are different answers."
  [players scoring]
  (mapv (fn [p]
          (if-let [stats (:week/stats p)]
            (assoc p :week-points (scoring/player-points {:stats stats} scoring))
            p))
        players))

;; ---- orchestration ----

(defn with-ros-vorp
  "Replacement and VORP on `:ros-points`, landing on `:ros-vorp`. Over the
  **whole** board — scoped to free agents it drifts down with every add. Renamed
  off `:vorp`, which the client already reads as the draft board's number."
  [board num-teams replacement-config]
  (let [levels (replacement/replacement-levels board num-teams
                                               (or replacement-config {}) :ros-points)]
    {:levels levels
     :players (mapv (fn [p] (-> p (assoc :ros-vorp (:vorp p)) (dissoc :vorp)))
                    (replacement/with-vorp board levels :ros-points))}))

(defn roster-sort-key
  "Starters in the league's own lineup order (`slot-idx` is `{id slot}` off
  `:starter-ids`), so a WR starting at FLEX keeps that seat rather than sorting up
  beside the other receivers. Everyone else follows by position, then points."
  [slot-idx {:keys [starter? player-id position ros-points]}]
  [(if starter? 0 1)
   (get slot-idx player-id (count slot-idx))
   (db/position-rank position)
   (- (or ros-points 0.0))])

(defn my-roster
  "The manager's own roster for the panel beside the board, ordered; nil when no
  team is picked, since the panel says something different for 'pick your team'
  than for an empty one. Rows the board cannot value are kept as placeholders."
  [my-team xwalk by-id drop]
  (when my-team
    (let [lineup   (held-ids my-team xwalk :starter-ids)
          ;; An unfilled slot is "0" — an index matching nobody, which keeps
          ;; the seats below it in their real places.
          slot-idx (zipmap lineup (range))
          starters (set lineup)
          active   (set (held-ids my-team xwalk :active-ids))
          drop-id  (:player-id drop)]
      (->> (held-ids my-team xwalk :player-ids)
           (map (fn [id]
                  (let [flags {:starter? (contains? starters id)
                               ;; IR and taxi: rostered, holding no seat.
                               :parked?  (not (contains? active id))
                               :drop?    (= id drop-id)}]
                    (if-let [p (get by-id id)]
                      ;; Exactly what the panel draws — a key nobody reads is a
                      ;; claim that something uses it (see the PDM).
                      (merge (select-keys p [:player-id :player-name
                                             :position :ros-points])
                             flags)
                      (merge {:player-id id :unvalued? true} flags)))))
           (sort-by #(roster-sort-key slot-idx %))
           vec))))

(defn my-roster-players
  "The manager's roster as full board rows, so a held player compares against a
  free agent on the same columns and one renderer serves both. `:upgrade` and
  `:bid` are absent rather than zero — you cannot claim a man you already hold."
  [my-team xwalk by-id]
  (when my-team
    (with-trend (keep #(get by-id %) (held-ids my-team xwalk :player-ids)))))

(defn waiver-board
  "`:players` is the free agents only — `:rostered` is the compact `{player-id
  team-name}` index that answers 'who has him' without re-sending the universe.
  No synced rosters is not an error: everyone is free."
  [board {:keys [league my-roster-id roster-size num-teams replacement-config
                 starting-slots] :as ctx}]
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
        ;; One binding, read by the drop and by the lineup. `held-ids`' own
        ;; docstring is about what happened the one time two readers of a roster
        ;; disagreed, and two call sites that must stay in step is that shape.
        active   (held-ids my-team xwalk :active-ids)
        drop     (when my-team (drop-candidate active by-id seats starting-slots))
        n        (claims-left ctx)]
    {:players            (-> (free-agents players rostered)
                             (with-upgrade drop)
                             (with-lineup-upgrade
                               (vec (keep #(get by-id %) active))
                               drop starting-slots)
                             (with-bids waiver (:faab-left my-team) n)
                             with-trend)
     :my-roster          (my-roster my-team xwalk by-id drop)
     :my-roster-players  (my-roster-players my-team xwalk by-id)
     :rostered           rostered
     :replacement-levels levels
     :claims-left        n
     :faab               {:type      (:type waiver)
                          :budget    (:budget waiver)
                          :left      (:faab-left my-team)
                          :rival-max (rival-max teams my-roster-id)}}))
