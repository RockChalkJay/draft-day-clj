(ns draft-day.rankings.matchup-test
  "Two rosters, one week. What is pinned here is mostly the set of distinctions
  the board exists to keep apart: a pre-kickoff zero from a real one, a seat
  nobody filled from a seat that does not exist, a man on IR from a man on the
  bench, and a provider's id space from the board's."
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.rankings.matchup :as matchup]))

(defn sleeper-id
  "The Sleeper spelling of a board id.

  Deliberately *different* from `:player-id` for every player here, for
  `waiver-test`'s reason: a fixture where the two are equal makes the crosswalk
  an identity map, and an identity crosswalk hides every bug in which a caller
  forgets to translate. The one exception is the team defense, whose id really
  is the abbreviation in both spaces."
  [id]
  (if (= id "SF") id (str "s-" id)))

(defn- p [id pos wk & {:as over}]
  (merge {:player-id id :player-name (str "P" id) :position pos
          :week-points wk :ids {:sleeper (sleeper-id id)}
          ;; Every game has kicked off unless a test says otherwise; the
          ;; not-started case is the interesting one and is set locally.
          :kickoff/started? true}
         over))

(def ^:private slots ["QB" "RB" "RB" "WR" "WR" "TE" "FLEX" "K" "DST"])

;; Projections are what a manager set his lineup by, so the current lineup is
;; already optimal on `:week-points`. Actuals are what happened, and there the
;; bench receiver outscored a starting back four to one — the whole case the
;; board exists to show.
(def ^:private board
  [(p "qb1" "QB" 22.0) (p "rb1" "RB" 16.0) (p "rb2" "RB" 12.0) (p "rb3" "RB" 9.0)
   (p "wr1" "WR" 15.0) (p "wr2" "WR" 11.0) (p "wr3" "WR" 4.0)
   (p "te1" "TE" 8.0)  (p "k1" "K" 7.0)    (p "SF" "DST" 6.0)
   ;; Projected better than anyone on the bench, and unstartable.
   (p "ir1" "WR" 20.0)])

(def ^:private lineup ["qb1" "rb1" "rb2" "wr1" "wr2" "te1" "rb3" "k1" "SF"])
(def ^:private roster (conj lineup "wr3" "ir1"))

(def ^:private team
  {:roster-id 1 :name "Mine" :wins 5 :losses 3
   :player-ids  (mapv sleeper-id roster)
   :active-ids  (mapv sleeper-id (remove #{"ir1"} roster))
   :starter-ids (mapv sleeper-id lineup)})

(def ^:private actuals
  {"qb1" 20.0 "rb1" 5.0 "rb2" 12.0 "wr1" 18.0 "wr2" 11.0
   "te1" 8.0 "rb3" 9.0 "k1" 7.0 "SF" 6.0
   "wr3" 25.0 "ir1" 30.0})

(def ^:private score
  {:official 96.0
   :starter-ids (mapv sleeper-id lineup)
   :player-points (into {} (map (fn [[id v]] [(sleeper-id id) v])) actuals)})

(defn- run [& {:keys [team score board slots]
               :or   {team team score score board board slots slots}}]
  (matchup/matchup-board board {:league {:teams [team]}
                                :matchups [{:matchup-id 1 :roster-ids [1 2]}]
                                :scores {1 score}
                                :provider :sleeper
                                :slots slots}))

(defn- side [& args] (first (:teams (apply run args))))
(defn- by-slot [t] (into {} (map (juxt :slot :player-id)) (:starters t)))

;; ---- seating ----

(deftest the-lineup-is-seated-in-the-leagues-own-order
  ;; Sleeper's starters array is positional against `roster_positions`, so the
  ;; nth id sits in the nth scoring seat. Nothing else can name a seat.
  (let [t (side)]
    (is (= ["QB" "RB" "RB" "WR" "WR" "TE" "FLEX" "K" "DST"]
           (mapv :slot (:starters t))))
    (is (= "rb3" (get (by-slot t) "FLEX")) "the back starting at FLEX keeps that seat")
    (is (= "SF" (get (by-slot t) "DST")))))

(deftest an-unfilled-seat-keeps-its-place-rather-than-shifting-the-rest
  ;; Sleeper writes "0" for a seat nobody is starting in. Dropping it would
  ;; slide every seat below it up one and mislabel the whole lineup.
        ;; The "0" is written by Sleeper as a literal, not as an id, so it is
        ;; placed after the translation rather than run through it.
  (let [gapped (assoc score :starter-ids (assoc (mapv sleeper-id lineup) 3 "0"))
        t      (side :score gapped)]
    (is (= 9 (count (:starters t))) "the seat is still drawn")
    (is (:empty? (nth (:starters t) 3)))
    (is (nil? (:player-id (nth (:starters t) 3))) "an empty seat is not a player")
    (is (= "WR" (:slot (nth (:starters t) 3))))
    (is (= "wr2" (:player-id (nth (:starters t) 4))) "the seats below it do not shift")))

(deftest the-lineup-comes-off-the-matchup-not-the-roster
  ;; They agree until somebody edits his lineup after the games lock, which is
  ;; exactly when the board must not follow him.
  (let [edited (assoc team :starter-ids (mapv sleeper-id (repeat 9 "wr3")))
        t      (side :team edited)]
    (is (= "qb1" (get (by-slot t) "QB"))
        "the matchup document's lineup wins over the roster's")))

(deftest a-team-with-no-matchup-entry-still-shows-a-lineup
  ;; A provider that gave this roster no entry is not a manager who started
  ;; nobody, and drawing it empty would say so.
  (let [t (side :score {})]
    (is (= "qb1" (get (by-slot t) "QB")))
    (is (every? nil? (map :actual (:starters t))) "but nothing has a score")))

;; ---- the id spaces ----

(deftest roster-ids-go-through-the-crosswalk
  ;; The bug `waiver/held-ids` memorialises, one board over: with the fixture's
  ;; sleeper ids deliberately unequal to its player ids, a reader that forgot to
  ;; translate resolves nobody at all.
  (let [t (side)]
    (is (every? :player-name (remove :empty? (:starters t)))
        "every seat resolved to a real player")
    (is (not-any? :unvalued? (:starters t)))))

(deftest player-points-are-translated-too
  ;; `:player-points` arrives keyed by the provider's ids, the same as the
  ;; roster. Translating one and not the other is how the last one broke.
  (is (= 20.0 (:actual (first (:starters (side)))))))

(deftest a-team-defense-survives-the-crosswalk
  ;; Its id is the team abbreviation in both spaces, so it resolves by falling
  ;; through unmapped rather than by being found.
  (let [dst (last (:starters (side)))]
    (is (= "SF" (:player-id dst)))
    (is (= 6.0 (:actual dst)))))

;; ---- blank is not zero ----

(deftest a-zero-before-kickoff-is-not-a-zero
  ;; Providers score everyone 0.0 until his game starts. Drawing that as a
  ;; result is the `game-log` trap one scale up, in the cell where the lie looks
  ;; most like a score.
  (let [pending (mapv #(assoc % :kickoff/started? false) board)
        t       (side :board pending)]
    (is (every? nil? (map :actual (:starters t))) "no seat claims a score")
    (is (zero? (:actual t)) "and the team total has nothing to add")
    (testing "while the projection is unaffected — it never needed a kickoff"
      (is (= 106.0 (:projected t))))))

(deftest a-real-zero-after-kickoff-is-kept
  ;; He was active and did nothing. That is a result, and blanking it would hide
  ;; the worst start of the week.
  (let [t (side :score (assoc-in score [:player-points (sleeper-id "qb1")] 0.0))]
    (is (= 0.0 (:actual (first (:starters t)))))))

(deftest an-unknown-kickoff-shows-what-the-provider-said
  ;; The scoreboard degrades to nil whole, so a missing status is not evidence
  ;; that nothing has started. Reading it as "not started" would blank a
  ;; completed week.
  (let [unknown (mapv #(dissoc % :kickoff/started?) board)]
    (is (= 20.0 (:actual (first (:starters (side :board unknown))))))))

(deftest a-player-the-provider-did-not-score-has-no-actual
  (let [t (side :score (update score :player-points dissoc (sleeper-id "qb1")))]
    (is (nil? (:actual (first (:starters t)))))))

;; ---- rows the board cannot value ----

(deftest an-unvalued-player-keeps-his-seat-and-his-score
  ;; The provider scored him whether or not the universe knows him, and that is
  ;; exactly the row a manager most wants explained.
  (let [thin (remove #(= "qb1" (:player-id %)) board)
        t    (side :board thin)
        qb   (first (:starters t))]
    (is (:unvalued? qb))
    (is (= "QB" (:slot qb)) "the seat is still named")
    (is (= 20.0 (:actual qb)) "and his score still shows")))

(deftest totals-skip-what-they-cannot-add
  ;; A seat nobody projected and a seat projected at nothing are different
  ;; claims; only one belongs in a total.
  (let [t (side :board (mapv #(if (= "qb1" (:player-id %)) (dissoc % :week-points) %) board))]
    (is (= 84.0 (:projected t)) "106 less the 22 nobody projected")))

;; ---- bench ----

(deftest the-bench-is-everyone-not-in-the-lineup
  (let [t (side)]
    (is (= #{"wr3" "ir1"} (set (map :player-id (:bench t)))))
    (is (every? nil? (map :slot (:bench t))) "a bench seat has no slot")))

(deftest a-parked-player-is-on-the-bench-and-says-so
  ;; It is the answer to "why is he not in my optimal lineup", and without the
  ;; flag the omission looks like a bug.
  (let [b (into {} (map (juxt :player-id :parked?)) (:bench (side)))]
    (is (true? (get b "ir1")))
    (is (false? (get b "wr3")))))

;; ---- optimal ----

(deftest a-lineup-set-by-projection-is-already-optimal-on-projection
  ;; Which is the point: the projected basis is a decision, and a manager who
  ;; made it well has nothing to change.
  (let [o (get-in (side) [:optimal :projected])]
    (is (= 106.0 (:total o)))
    (is (zero? (:gain o)))
    (is (= [] (:in o)))
    (is (= [] (:out o)))))

(deftest the-actual-basis-finds-the-points-left-on-the-bench
  ;; The same lineup, measured after the games: wr3 scored 25 on the bench while
  ;; rb1 scored 5 in the lineup.
  (let [o (get-in (side) [:optimal :actual])]
    (is (= 116.0 (:total o)))
    (is (= 20.0 (:gain o)) "116 against the 96 he actually started")
    (is (= ["wr3"] (mapv :player-id (:in o))))
    (is (= ["rb1"] (mapv :player-id (:out o))))
    (is (= 25.0 (:points (first (:in o)))) "named in the unit it was measured in")))

(deftest the-two-bases-disagree-and-both-are-reported
  ;; The toggle exists because they answer different questions, so neither is
  ;; allowed to stand in for the other.
  (let [{:keys [projected actual]} (:optimal (side))]
    (is (not= (:gain projected) (:gain actual)))))

(deftest an-injured-reserve-player-is-never-in-an-optimal-lineup
  ;; He is projected better than anyone on the bench and scored more than
  ;; anyone on the roster. Seating him would report a gain no manager could have
  ;; taken.
  (doseq [basis [:projected :actual]]
    (is (not-any? #{"ir1"} (map :player-id (get-in (side) [:optimal basis :in])))
        (name basis))))

(deftest an-unvalued-starter-is-not-judged-by-an-optimizer-that-cannot-place-him
  ;; He carries no position, so `best-lineup` can never seat him — but he does
  ;; carry an `:actual`. Counted as current and necessarily absent from optimal,
  ;; he came out in `:out` as a man to bench out of a lineup that was in fact
  ;; perfect, with the gain understated by his whole line.
  (let [thin (remove #(= "wr1" (:player-id %)) board)
        o    (get-in (side :board thin) [:optimal :actual])]
    (is (not-any? #{"wr1"} (map :player-id (:out o)))
        "the board cannot place him, so it does not judge him")
    (is (= [] (:out o)) "and nobody else was wrong either")
    (is (= ["wr3"] (mapv :player-id (:in o))))
    (is (= 25.0 (:gain o))
        "measured over the eight seats it can place, on both sides")))

(deftest an-unvalued-starter-still-shows-his-seat-and-his-score
  ;; Excluded from the optimizer is not excluded from the board — the row a
  ;; manager most wants explained is still drawn.
  (let [thin (remove #(= "wr1" (:player-id %)) board)
        wr   (nth (:starters (side :board thin)) 3)]
    (is (:unvalued? wr))
    (is (= "WR" (:slot wr)))
    (is (= 18.0 (:actual wr)))))

(deftest an-empty-seat-is-not-a-player-the-optimizer-can-drop
  ;; `:out` is drawn from the current lineup, and an unfilled seat has nobody in
  ;; it to name.
  (let [gapped (assoc score :starter-ids (assoc (mapv sleeper-id lineup) 1 "0"))
        o      (get-in (side :score gapped) [:optimal :actual])]
    (is (not-any? nil? (map :player-id (:out o))))
    (is (not-any? :empty? (:out o)))))

;; ---- the envelope ----

(deftest the-teams-come-back-as-a-vector
  ;; Not a map keyed by roster id: an integer key round-trips through JSON as a
  ;; string and comes back keywordized, so the browser would index on `:1`.
  (is (vector? (:teams (run))))
  (is (= 1 (:roster-id (first (:teams (run)))))))

(deftest the-teams-record-and-the-providers-own-total-ride-along
  (let [t (side)]
    (is (= 5 (:wins t)))
    (is (= 3 (:losses t)))
    (is (= 96.0 (:official t)) "the league's score of record, beside the sum")))

(deftest the-pairing-is-passed-through
  (is (= [{:matchup-id 1 :roster-ids [1 2]}] (:matchups (run)))))

(deftest a-roster-with-no-seats-yields-no-lineup-rather-than-throwing
  ;; No synced positions and no roster config — the fallback of the fallback.
  (let [t (side :slots nil)]
    (is (= [] (:starters t)))
    (is (zero? (:projected t)))
    (is (= 11 (count (:bench t))) "everybody is on the bench")))
