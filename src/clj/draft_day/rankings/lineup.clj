(ns draft-day.rankings.lineup
  "What a roster's best legal starting lineup is worth.

  The waiver board's `:upgrade` measures a claim against the player it would cost
  — the one a manager can lose most cheaply — which answers whether his roster
  improved. It is not the question a claim is really asking. (`drop-candidate`
  picks that player by marginal cost *to this lineup* wherever the league's seats
  are known, falling back to plain worst-points only when they are not; both
  answer the bench question, which is the one being improved on here.)

  A quarterback projected 180 behind a starter projected 260 never plays, and his
  contribution to points scored is zero however far he clears the last man on the
  bench.

  Greedy is optimal for nested seats, and the argument is worth stating so
  nobody replaces this with a matching algorithm. A dedicated seat accepts one
  position, FLEX a superset of three, SUPER_FLEX a superset of FLEX plus the
  quarterback — so each seat's eligible set contains every narrower seat's, and
  filling them narrowest-first (the order is read off `db/flex-slots`, not
  hardcoded) selects the same *set* an optimal assignment would. Only the set
  determines the total.

  `WRRB_FLEX` (RB/WR) and `REC_FLEX` (WR/TE) are the exception: neither contains
  the other, so a league running both breaks the nesting and greedy can leave a
  point or two behind. A deliberate trade — the exact answer is a weighted
  matching, and it buys nothing until a real league runs that pair.

  Scored on whatever `score-key` the caller passes, the way
  `replacement/replacement-levels` and `with-vorp` already are, so the same code
  answers the rest-of-season question and a weekly one."
  (:require [draft-day.db :as db]))

(defn slot-breadth
  "How many positions a seat accepts; 1 for a dedicated one.

  The sort key `best-lineup` fills by. Branching rather than defaulting to
  `#{slot}` because `sort-by` calls its keyfn inside the comparator, and this
  runs once per free agent."
  [slot]
  (if-let [accepts (get db/flex-slots slot)] (count accepts) 1))

(defn best-lineup
  "`[[slot player] ...]` for the best legal lineup, in fill order.

  Players the score left blank are skipped rather than seated at zero — a row
  the board could not value is not evidence that he would score nothing, the
  same distinction `waiver/drop-candidate` draws. A slot nothing can fill is
  simply absent from the result, so a short roster yields a short lineup instead
  of throwing."
  [players slots score-key]
  (let [scored  (filterv #(number? (score-key %)) players)
        ;; Narrowest seat first: the ns docstring's correctness argument, not
        ;; a cosmetic ordering. `sort-by` is stable, so ties keep league order.
        ordered (sort-by slot-breadth slots)]
    (first
     (reduce (fn [[acc used] slot]
               (if-let [p (->> scored
                               (remove #(contains? used (:player-id %)))
                               (filter #(db/slot-accepts? slot (:position %)))
                               (sort-by score-key >)
                               first)]
                 [(conj acc [slot p]) (conj used (:player-id p))]
                 [acc used]))
             [[] #{}]
             ordered))))

(defn lineup-points
  "What the best legal lineup scores. 0.0 for an empty roster or no slots."
  [players slots score-key]
  (reduce (fn [t [_ p]] (+ t (double (score-key p)))) 0.0
          (best-lineup players slots score-key)))

(defn upgrade
  "Points a claim adds to the *starting lineup*, after the drop.

  `before` is the roster's current lineup total, passed in rather than derived,
  because it is constant across a whole free-agent pool and the caller scores
  several hundred rows on a response the board re-POSTs on every refresh.

  Signed, and the negative case is real rather than a guard: **the drop can be a
  starter**. `waiver/drop-candidate` prefers a drop that costs the lineup
  nothing, but a roster can fail to offer one — every bench seat empty, or a
  bench thinner than the claim — and then the cheapest available drop is still
  a starter, so claiming a free agent who would not start costs what he was
  contributing. A board that clamped this at 0 would hide the one thing the
  manager most needs to know, which is that the claim makes his lineup worse.

  0 means he would not start and the drop was not starting either. That is the
  common case for a bench stash, and it is the answer `:upgrade` cannot give."
  [before roster candidate drop slots score-key]
  (let [kept  (if drop
                (remove #(= (:player-id %) (:player-id drop)) roster)
                roster)
        after (lineup-points (conj (vec kept) candidate) slots score-key)]
    (- after before)))

(defn seat-floor
  "The score a player of `position` must beat to change `seated`, a
  `best-lineup`: the least among those seated where he could sit. nil when one
  of those seats is empty, since any score takes it, and infinite when the
  lineup has no seat for him at all."
  [slots seated score-key position]
  (let [seats (count (filter #(db/slot-accepts? % position) slots))
        taken (filter (fn [[slot _]] (db/slot-accepts? slot position)) seated)]
    (when (= seats (count taken))
      (reduce min ##Inf (map #(double (score-key (second %))) taken)))))

(defn upgrades
  "`upgrade` for each of `candidates` against one roster and one drop: the same
  numbers for a fraction of the fills. A candidate scoring no more than his
  `seat-floor` cannot change a greedy fill — it reaches each seat he could take
  with the players it had without him, and a tie keeps the man already there,
  since the candidate is appended after them — so he adds exactly what losing
  the drop costs. Only the few above their floor are seated one by one, which
  is what makes asking this of every team in a league affordable."
  [roster candidates drop slots score-key]
  (let [before (lineup-points roster slots score-key)
        kept   (vec (if drop (remove #(= (:player-id %) (:player-id drop)) roster) roster))
        seated (best-lineup kept slots score-key)
        base   (- (reduce (fn [t [_ p]] (+ t (double (score-key p)))) 0.0 seated) before)
        floor  (memoize #(seat-floor slots seated score-key %))]
    (mapv (fn [c]
            (let [s (score-key c)
                  f (floor (:position c))]
              (if (or (not (number? s)) (and f (<= s f)))
                base
                (- (lineup-points (conj kept c) slots score-key) before))))
          candidates)))
