(ns draft-day.rankings.lineup
  "What a roster's best legal starting lineup is worth.

  The waiver board's `:upgrade` measures a claim against the manager's worst
  *rostered* player, which answers whether his bench improved. It is not the
  question a claim is really asking. A quarterback projected 180 behind a
  starter projected 260 never plays, and his contribution to points scored is
  zero however far he clears the last man on the bench.

  GREEDY IS OPTIMAL FOR NESTED SEATS, and the reason is worth stating so nobody
  replaces this with a matching algorithm. Dedicated seats accept exactly one
  position; FLEX accepts a superset of three of them; SUPER_FLEX a superset of
  FLEX plus the quarterback. Each seat's eligible set therefore contains every
  narrower seat's, and filling them narrowest-first selects the same *set* an
  optimal assignment would. Only the set determines the total — which of two
  interchangeable seats a player sits in cannot change the sum — so that order
  is the whole of the correctness argument.

  THE ORDER IS READ OFF `db/flex-slots` RATHER THAN HARDCODED. It used to be the
  single rule `FLEX last`, which is the same thing in a league whose only wide
  seat is FLEX and silently wrong in a superflex one: SUPER_FLEX filled first
  takes the back FLEX needed, which is precisely the mistake the rule exists to
  prevent, one seat over.

  WHERE IT IS ONLY NEAR-OPTIMAL. `WRRB_FLEX` (RB/WR) and `REC_FLEX` (WR/TE)
  accept the same number of positions and neither contains the other, so a
  league running both breaks the nesting this argument depends on and greedy can
  leave a point or two behind. That is a deliberate trade rather than an
  oversight: the exact answer is a weighted matching, and it buys nothing until
  a real league runs that pair.

  Scored on whatever `score-key` the caller passes, the way
  `replacement/replacement-levels` and `with-vorp` already are, so the same code
  answers the rest-of-season question and a weekly one."
  (:require [draft-day.db :as db]))

(defn slot-breadth
  "How many positions a seat accepts; 1 for a dedicated one.

  The sort key `best-lineup` fills by, and the reason `db/flex-slots` is a map
  of sets rather than a list of names."
  [slot]
  (count (get db/flex-slots slot #{slot})))

(defn best-lineup
  "`[[slot player] ...]` for the best legal lineup, in fill order.

  Players the score left blank are skipped rather than seated at zero — a row
  the board could not value is not evidence that he would score nothing, the
  same distinction `waiver/drop-candidate` draws. A slot nothing can fill is
  simply absent from the result, so a short roster yields a short lineup instead
  of throwing."
  [players slots score-key]
  (let [scored  (filterv #(number? (score-key %)) players)
        ;; Narrowest seat first. See the ns docstring — this is the whole
        ;; correctness argument, not a cosmetic ordering. `sort-by` is stable,
        ;; so seats of equal breadth keep the league's own order.
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
