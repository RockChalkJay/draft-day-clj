(ns draft-day.rankings.lineup
  "What a roster's best legal starting lineup is worth.

  The waiver board's `:upgrade` measures a claim against the manager's worst
  *rostered* player, which answers whether his bench improved. It is not the
  question a claim is really asking. A quarterback projected 180 behind a
  starter projected 260 never plays, and his contribution to points scored is
  zero however far he clears the last man on the bench.

  GREEDY IS OPTIMAL HERE, and the reason is worth stating so nobody replaces
  this with a matching algorithm. Dedicated seats accept exactly one position;
  FLEX accepts a superset of three of them; QB, K and DST are disjoint from
  FLEX. Filling dedicated seats with the best available at each position and
  FLEX with the best remaining eligible therefore selects the same *set* an
  optimal assignment would, and only the set determines the total — which of two
  interchangeable seats a player sits in cannot change the sum. The one thing
  that does matter is order: FLEX must be filled last, or it takes a running
  back the RB seat needed.

  Scored on whatever `score-key` the caller passes, the way
  `replacement/replacement-levels` and `with-vorp` already are, so the same code
  answers the rest-of-season question and a weekly one."
  (:require [draft-day.db :as db]))

(defn best-lineup
  "`[[slot player] ...]` for the best legal lineup, in fill order.

  Players the score left blank are skipped rather than seated at zero — a row
  the board could not value is not evidence that he would score nothing, the
  same distinction `waiver/drop-candidate` draws. A slot nothing can fill is
  simply absent from the result, so a short roster yields a short lineup instead
  of throwing."
  [players slots score-key]
  (let [scored  (filterv #(number? (score-key %)) players)
        ;; FLEX last. See the ns docstring — this is the whole correctness
        ;; argument, not a cosmetic ordering.
        ordered (concat (remove #{"FLEX"} slots) (filter #{"FLEX"} slots))]
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

(defn upgrade-from
  "`upgrade` with the before-total already computed.

  It is the same value for every candidate against one roster, and the caller
  scores a whole free-agent pool — several hundred rows on a response the board
  re-POSTs on every refresh — so recomputing it per candidate is that many
  redundant sorts."
  [before roster candidate drop slots score-key]
  (let [kept  (if drop
                (remove #(= (:player-id %) (:player-id drop)) roster)
                roster)
        after (lineup-points (conj (vec kept) candidate) slots score-key)]
    (- after before)))

(defn upgrade
  "Points a claim adds to the *starting lineup*, after the drop.

  Signed, and the negative case is real rather than a guard: **the drop can be a
  starter**. `waiver/drop-candidate` names the lowest-scoring player holding an
  active seat, and on a roster whose bench is thin that man is in the lineup —
  so claiming a free agent who would not start costs exactly what the drop was
  contributing. A board that clamped this at 0 would hide the one thing the
  manager most needs to know, which is that the claim makes his lineup worse.

  0 means he would not start and the drop was not starting either. That is the
  common case for a bench stash, and it is the answer `:upgrade` cannot give."
  [roster candidate drop slots score-key]
  (upgrade-from (lineup-points roster slots score-key)
                roster candidate drop slots score-key))
