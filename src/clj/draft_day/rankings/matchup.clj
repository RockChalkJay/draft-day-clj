(ns draft-day.rankings.matchup
  "Two teams, one week: what each side's lineup is projected to score, what it
  has actually scored, and what the best legal lineup would have been.

  MEASURED IN POINTS, never in dollars, for `rankings.waiver`'s reason exactly.
  A matchup is one week of one roster against another; the draft board's Value
  and Worth divide a bankroll among a whole roster on one night. Nothing here
  touches them.

  PREDICTED AND ACTUAL COME FROM DIFFERENT PLACES, deliberately. `:week-points`
  is Sleeper's weekly line scored under the league's own weights
  (`waiver/with-week-points`); `:actual` is the provider's own number, taken as
  given. They are not on the same basis in a league whose rules the flat model
  cannot express — FG distance buckets, DST points-allowed tiers, yardage
  bonuses, all of which `league-import` already reports as
  `:unsupported-scoring`. That is a real seam and the honest one to have: the
  provider's figure is what the league *records*, and recomputing it from a
  model that cannot score half its rules would put the board in confident
  disagreement with the standings.

  A ZERO BEFORE KICKOFF IS NOT A ZERO. Providers score every player 0.0 until
  his game starts, so `:actual` is nil for a player whose game has not kicked
  off — the same BLANK IS NOT ZERO rule `game-log` states one scale down, in the
  one cell where the lie looks most like a result. The evidence is
  `:kickoff/started?`, and **absent means unknown**: with no scoreboard the
  provider's number is shown rather than the whole week blanked.

  OPTIMAL IS DRAWN FROM ACTIVE PLAYERS, NOT ROSTERED ONES. A man on IR cannot be
  started, so an optimal lineup that seated him would be illegal and would
  report a gain no manager could have taken. Same `:active-ids` / `:player-ids`
  split `waiver` draws, for a different reason.

  BOTH BASES ARE COMPUTED, and they answer different questions. Optimal on
  `:week-points` is a decision — start him over him — and is the only one worth
  anything before kickoff. Optimal on `:actual` is regret: what the bench was
  worth once the games were played. `lineup/best-lineup` already takes a
  `score-key`, so this is two calls rather than two implementations.

  THE SWAPS ARE `:in` AND `:out` LISTS, NOT PAIRS. Optimal and current are sets
  of players; pairing a bench player to a specific seat would assert a
  correspondence the set difference does not carry, and a seat's occupant is not
  even well defined when two interchangeable seats swap men."
  (:require [draft-day.db :as db]
            [draft-day.rankings.lineup :as lineup]
            [draft-day.rankings.waiver :as waiver]))

;; ---- one player ----

(def row-keys
  "Exactly what a matchup row draws.

  Named rather than passing whole board rows through: the response carries every
  roster in the league, and the removed PDM is this repo's cautionary tale for
  shipping a key nobody reads."
  [:player-id :player-name :position :team :bye :week-points
   :week/opponent :week/home? :kickoff/at :kickoff/status :kickoff/neutral?
   :sleeper/injury-status])

(defn actual-points
  "What he has actually scored this week, or nil before his game kicks off.

  `points` is the provider's own figure and nil when it did not name him at all.
  The kickoff test is `false?` rather than `not`, because the three states are
  started / not started / **no scoreboard**, and only the middle one is evidence
  that a zero is not a result. See the ns docstring."
  [p points]
  (when-not (false? (:kickoff/started? p))
    points))

(defn row
  "One roster entry as the board draws it, seated in `slot` (nil on the bench).

  A player the universe cannot value keeps his seat and says so, rather than
  vanishing — the same distinction `waiver/my-roster` draws. His `:actual` still
  shows: the provider scored him whether or not we have a projection for him,
  and that is exactly the row a manager most wants explained."
  [slot p id points]
  (if p
    (assoc (select-keys p row-keys) :slot slot :actual (actual-points p points))
    {:player-id id :slot slot :unvalued? true :actual points}))

;; ---- one team ----

(defn points-by-player-id
  "A score's `:player-points` in the board's id space.

  Provider ids in, canonical ids out. An id the crosswalk has no entry for maps
  to itself, which is what carries team defenses through — their id is the team
  abbreviation in both spaces."
  [score xwalk]
  (update-keys (:player-points score {}) #(get xwalk % %)))

(defn week-lineup
  "The ids that started this week, in seat order, in the board's id space.

  Off the *matchup* document where there is one and the roster's own lineup
  otherwise. A team the provider gave no matchup entry still has a lineup, and
  drawing it empty would read as \"nobody started\" rather than \"no scoreboard\".
  `\"0\"` survives the translation unmapped, which is what keeps an unfilled seat
  in its real place."
  [team score xwalk]
  (waiver/held-ids (if (seq (:starter-ids score)) score team) xwalk :starter-ids))

(defn filled?
  "Is this lineup entry a player rather than an empty seat? Sleeper writes `\"0\"`
  for a seat nobody is starting in."
  [id]
  (and id (not= "0" id)))

(defn starter-rows
  "One row per *seat*, in the league's own lineup order, including the empty
  ones. A manager with an unfilled seat needs to see the seat, and a lineup that
  silently omitted it would read as a shorter roster."
  [slots lineup by-id points]
  (vec (map-indexed
        (fn [i slot]
          (let [id (nth lineup i nil)]
            (if-not (filled? id)
              {:slot slot :empty? true}
              (row slot (get by-id id) id (get points id)))))
        slots)))

(defn bench-rows
  "Everyone rostered who did not get a seat, by position then by projection.

  `seated` IS DERIVED FROM THE SEATS, NOT FROM THE LINEUP LIST, and the two come
  apart whenever there are fewer slots than the provider named starters — a
  league whose seats could not be resolved, or a roster config standing in for a
  synced league with a different shape. Subtracting the lineup list would then
  drop players who were never actually seated from *both* halves, and they would
  vanish off the board entirely rather than appearing on the bench.

  IR and taxi are included and flagged: they are on the roster and they are
  visibly not startable, which is the answer to \"why is he not in my optimal
  lineup\"."
  [held seated active by-id points]
  (->> held
       (remove seated)
       (map (fn [id]
              (assoc (row nil (get by-id id) id (get points id))
                     :parked? (not (contains? active id)))))
       (sort-by (juxt #(db/position-rank (:position %))
                      #(- (double (or (:week-points %) 0.0)))))
       vec))

(defn total
  "Sum of `k` over rows, skipping the ones that carry no number.

  Skipping rather than defaulting to 0.0 is the same rule the cells follow: a
  seat nobody has projected and a seat projected at nothing are different
  claims, and only one of them belongs in a total."
  [rows k]
  (reduce (fn [t r] (if (number? (k r)) (+ t (double (k r))) t)) 0.0 rows))

(defn brief
  "A player as the swap lists name him — enough to recognise him, no more."
  [k p]
  {:player-id (:player-id p) :player-name (:player-name p)
   :position (:position p) :points (k p)})

(defn optimal
  "The best legal lineup on `score-key`, and what it would have changed.

  `:gain` is measured against the *current* lineup on the same key, so both
  sides are sums of the same thing. `:in` and `:out` are set differences; see
  the ns docstring on why they are not paired."
  [candidates slots score-key current-rows]
  (let [best    (lineup/best-lineup candidates slots score-key)
        seated  (mapv second best)
        ids     (set (map :player-id seated))
        current (filterv #(number? (score-key %)) current-rows)
        now     (set (map :player-id current))
        best'   (total seated score-key)]
    {:total best'
     :gain  (- best' (total current score-key))
     :in    (mapv #(brief score-key %) (remove #(now (:player-id %)) seated))
     :out   (mapv #(brief score-key %) (remove #(ids (:player-id %)) current))}))

(defn team-board
  "One team's whole side of a matchup."
  [team score xwalk by-id slots]
  (let [points   (points-by-player-id score xwalk)
        held     (waiver/held-ids team xwalk :player-ids)
        active   (set (waiver/held-ids team xwalk :active-ids))
        lineup   (week-lineup team score xwalk)
        starters (starter-rows slots lineup by-id points)
        ;; Off the seats rather than off `lineup` — see `bench-rows`.
        seated   (set (keep :player-id starters))
        bench    (bench-rows held seated active by-id points)
        ;; A row with no position cannot be *reasoned about* positionally, so it
        ;; is out of BOTH halves of the optimizer rather than one. `best-lineup`
        ;; can never seat it — `db/slot-accepts?` matches a nil position against
        ;; nothing but BENCH — but it does carry an `:actual`, so counting it as
        ;; current put a man who had scored well into `:out`, reported as
        ;; somebody to bench out of a lineup that was in fact perfect, and
        ;; understated the gain by his whole line. That is not a rare shape: the
        ;; provider scores everyone on the roster, including players missing
        ;; from a stale universe cache or the offline sample.
        placeable (fn [rows] (filterv :position rows))
        current   (placeable (remove :empty? starters))
        ;; Only players who could legally start. A man on IR seated in an
        ;; optimal lineup would report a gain nobody could have taken.
        startable (placeable (filter #(contains? active (:player-id %))
                                     (concat (remove :empty? starters) bench)))]
    {:roster-id (:roster-id team)
     :name      (:name team)
     :wins      (:wins team)
     :losses    (:losses team)
     :starters  starters
     :bench     bench
     :projected (total starters :week-points)
     :actual    (total starters :actual)
     ;; The provider's own team total, which is the league's score of record.
     ;; `:actual` beside it is the same figure summed, and the two are what the
     ;; optimal gain is measured between — see `optimal`.
     :official  (:official score)
     :optimal   {:projected (optimal startable slots :week-points current)
                 :actual    (optimal startable slots :actual current)}}))

;; ---- the league ----

(defn matchup-board
  "Every team in the league valued for this week, plus the pairing.

  All of them rather than the one pair asked for: twelve rosters is a few
  hundred rows against the waiver board's six hundred, so switching matchups
  costs no round trip — the same argument `tiers/with-tiers` makes for shipping
  both tier scales.

  `:teams` is a **vector**, not a map keyed by roster id. An integer map key
  round-trips through JSON as a string and comes back keywordized, so the
  browser would be indexing on `:1`."
  [board {:keys [league matchups scores slots provider]}]
  (let [xwalk (db/provider->player-id board (or provider :sleeper))
        by-id (db/index-by-id board)]
    {:matchups (vec matchups)
     :teams    (mapv #(team-board % (get scores (:roster-id %)) xwalk by-id slots)
                     (:teams league))}))
