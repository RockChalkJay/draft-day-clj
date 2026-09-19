(ns draft-day.rankings.matchup
  "Two teams, one week: what each side's lineup is projected to score, what it
  has actually scored, and what the best legal lineup would have been.

  Measured in points, never in dollars, for `rankings.waiver`'s reason: a
  matchup is one week of one roster against another, not a bankroll divided
  among a whole roster on one night.

  Predicted and actual come from different places, deliberately. `:week-points`
  is Sleeper's weekly line scored under the league's own weights; `:actual` is
  the provider's own number, taken as given. They are not on the same basis in a
  league whose rules the flat model cannot express (`:unsupported-scoring`), and
  that is the honest seam to have — the provider's figure is what the league
  records, and recomputing it would put the board in confident disagreement with
  the standings.

  A zero before kickoff is not a zero. Providers score every player 0.0 until
  his game starts, so `:actual` is nil until `:kickoff/started?` says otherwise.
  Absent means unknown: with no scoreboard the provider's number is shown rather
  than the whole week blanked.

  Optimal is drawn from `:active-ids`, not from everyone rostered — a man on IR
  seated in an optimal lineup would report a gain nobody could have taken. It is
  computed on both bases: `:week-points` is a decision (start him over him),
  `:actual` is regret. The swaps are `:in`/`:out` lists rather than paired
  seats, since pairing would assert a correspondence a set difference does not
  carry."
  (:require [clojure.set :as set]
            [draft-day.db :as db]
            [draft-day.rankings.lineup :as lineup]
            [draft-day.rankings.waiver :as waiver]))

(def row-keys
  "Exactly what a matchup row draws.

  Named rather than passing whole board rows through: the response carries every
  roster in the league."
  [:player-id :player-name :position :team :bye :week-points
   :week/opponent :week/home? :kickoff/at :kickoff/status
   :kickoff/opponent :kickoff/home? :kickoff/neutral?
   :sleeper/injury-status])

(defn actual-points
  "What he has actually scored this week, or nil before his game kicks off.

  `false?` rather than `not`: the three states are started, not started and no
  scoreboard, and only the middle one is evidence that a zero is not a result."
  [p points]
  (when-not (false? (:kickoff/started? p))
    points))

(defn row
  "One roster entry as the board draws it, seated in `slot` (nil on the bench).

  A player the universe cannot value keeps his seat and says so rather than
  vanishing, and his `:actual` still shows — that is the row a manager most
  wants explained."
  [slot p id points]
  (if p
    (assoc (select-keys p row-keys) :slot slot :actual (actual-points p points))
    {:player-id id :slot slot :unvalued? true :actual points}))

(defn points-by-player-id
  "A score's `:player-points` in the board's id space.

  An id the crosswalk has no entry for maps to itself, which carries team
  defenses through: their id is the abbreviation in both spaces."
  [score xwalk]
  (update-keys (:player-points score {}) #(get xwalk % %)))

(defn week-lineup
  "The ids that started this week, in seat order, in the board's id space.

  Off the matchup document where there is one, the roster otherwise: drawing a
  team the provider skipped as empty would read as \"nobody started\" rather than
  \"no scoreboard\". `\"0\"` survives untranslated, keeping an unfilled seat in
  place."
  [team score xwalk]
  (waiver/held-ids (if (seq (:starter-ids score)) score team) xwalk :starter-ids))

(defn filled?
  "Is this lineup entry a player rather than an empty seat? Sleeper writes `\"0\"`
  for a seat nobody is starting in."
  [id]
  (and id (not= "0" id)))

(defn starter-rows
  "One row per seat, in the league's own lineup order, including the empty ones —
  omitting an unfilled seat would read as a shorter roster."
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

  `seated` comes from the seats, not from the lineup list: with fewer slots than
  the provider named starters the two come apart, and subtracting the list would
  drop a player off both halves. IR and taxi are included and flagged — the
  answer to \"why is he not in my optimal lineup\"."
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

  A seat nobody has projected and a seat projected at nothing are different
  claims, and only one of them belongs in a total. Always a number, including
  0.0 for an empty sum — the optimal arithmetic subtracts these and cannot take
  a nil. `reported` is what a score goes through."
  [rows k]
  (reduce (fn [t r] (if (number? (k r)) (+ t (double (k r))) t)) 0.0 rows))

(defn reported
  "A total as a score to put on screen: nil when nothing contributed to it.

  `total` is 0.0 for an empty sum, right for arithmetic and wrong as a score:
  before kickoff a team would announce a bold 0.0 over nine rows each correctly
  showing a dash, and two such teams would read as a tie."
  [rows k]
  (when (some #(number? (k %)) rows)
    (total rows k)))

(defn brief
  "A player as the swap lists name him — enough to recognise him, no more."
  [k p]
  {:player-id (:player-id p) :player-name (:player-name p)
   :position (:position p) :points (k p)})

(defn without-each
  "`coll` with one occurrence of each item in `xs` removed. A league with two WR
  seats and one pending WR gives up one seat, not both."
  [coll xs]
  (reduce (fn [acc x]
            (let [[before after] (split-with #(not= x %) acc)]
              (vec (concat before (rest after)))))
          (vec coll) xs))

(defn settled-slots
  "The seats whose result is in: `slots` less the seat of every starter who has
  not played.

  Regret is only measurable over a finished seat. Left in, the seat of a man who
  plays Sunday is empty on the actual basis, so a bench player who played
  Thursday is \"left on the bench\" in a seat that was never his to take."
  [slots current-rows]
  (without-each slots (keep #(when-not (number? (:actual %)) (:slot %)) current-rows)))

(defn optimal
  "The best legal lineup on `score-key`, and what it would have changed.

  `:gain` is measured against the current lineup on the same key, so both sides
  sum the same thing. `:in`/`:out` are set differences, not pairs."
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
  "One team's whole side of a matchup.

  Who is on the roster comes off the matchup document where there is one, as the
  lineup does, and only IR and taxi from the sync. The sync goes stale on anyone's
  claim; drawing the roster from it put a man picked up midweek in the lineup and
  nowhere among those who could start."
  [team score xwalk by-id slots]
  (let [points   (points-by-player-id score xwalk)
        synced   (waiver/held-ids team xwalk :player-ids)
        parked   (set/difference (set synced)
                                 (set (waiver/held-ids team xwalk :active-ids)))
        held     (if (seq (:player-ids score))
                   (waiver/held-ids score xwalk :player-ids)
                   synced)
        lineup   (week-lineup team score xwalk)
        starters (starter-rows slots lineup by-id points)
        ;; Off the seats rather than off `lineup` — see `bench-rows`.
        seated   (set (keep :player-id starters))
        ;; A starter is startable whatever the sync says: the provider seated him.
        active   (into (set/difference (set held) parked) seated)
        bench    (bench-rows held seated active by-id points)
        ;; A row with no position is out of *both* halves: `best-lineup` can
        ;; never seat it, so counting it as current would put a man who scored
        ;; well into `:out` and understate the gain by his whole line.
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
     :ties      (:ties team)
     :starters  starters
     :bench     bench
     :projected (reported starters :week-points)
     :actual    (reported starters :actual)
     ;; The league's score of record, nil'd alongside `:actual` when nothing has
     ;; kicked off: a provider publishes 0.0 for a team that has not played.
     :official  (when (some #(number? (:actual %)) starters) (:official score))
     :optimal   {:projected (optimal startable slots :week-points current)
                 :actual    (optimal startable (settled-slots slots current)
                                     :actual current)}}))

(defn matchup-board
  "Every team in the league valued for this week, plus the pairing.

  All of them rather than the pair asked for, so switching matchups costs no
  round trip. `:teams` is a vector, not a map keyed by roster id: an integer key
  round-trips through JSON as a string and comes back keywordized."
  [board {:keys [league matchups scores slots provider]}]
  (let [xwalk (db/provider->player-id board (or provider :sleeper))
        by-id (db/index-by-id board)]
    {:matchups (vec matchups)
     :teams    (mapv #(team-board % (get scores (:roster-id %)) xwalk by-id slots)
                     (:teams league))}))
