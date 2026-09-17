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
          ;; Every game is over unless a test sets otherwise locally.
          :kickoff/started? true
          :kickoff/status "STATUS_FINAL"}
         over))

(def ^:private slots ["QB" "RB" "RB" "WR" "WR" "TE" "FLEX" "K" "DST"])

;; The current lineup is already optimal on `:week-points`; on actuals the
;; bench receiver outscored a starting back four to one.
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


(deftest the-lineup-is-seated-in-the-leagues-own-order
  ;; Sleeper's starters array is positional against `roster_positions`.
  (let [t (side)]
    (is (= ["QB" "RB" "RB" "WR" "WR" "TE" "FLEX" "K" "DST"]
           (mapv :slot (:starters t))))
    (is (= "rb3" (get (by-slot t) "FLEX")) "the back starting at FLEX keeps that seat")
    (is (= "SF" (get (by-slot t) "DST")))))

(deftest an-unfilled-seat-keeps-its-place-rather-than-shifting-the-rest
  ;; Dropping Sleeper's "0" would slide every seat below it up one.
        ;; A literal, not an id, so it is placed after the translation.
  (let [gapped (assoc score :starter-ids (assoc (mapv sleeper-id lineup) 3 "0"))
        t      (side :score gapped)]
    (is (= 9 (count (:starters t))) "the seat is still drawn")
    (is (:empty? (nth (:starters t) 3)))
    (is (nil? (:player-id (nth (:starters t) 3))) "an empty seat is not a player")
    (is (= "WR" (:slot (nth (:starters t) 3))))
    (is (= "wr2" (:player-id (nth (:starters t) 4))) "the seats below it do not shift")))

(deftest the-lineup-comes-off-the-matchup-not-the-roster
  ;; They agree until somebody edits his lineup after the games lock.
  (let [edited (assoc team :starter-ids (mapv sleeper-id (repeat 9 "wr3")))
        t      (side :team edited)]
    (is (= "qb1" (get (by-slot t) "QB"))
        "the matchup document's lineup wins over the roster's")))

(deftest a-team-with-no-matchup-entry-still-shows-a-lineup
  (let [t (side :score {})]
    (is (= "qb1" (get (by-slot t) "QB")))
    (is (every? nil? (map :actual (:starters t))) "but nothing has a score")))


(deftest roster-ids-go-through-the-crosswalk
  ;; The fixture's sleeper ids are deliberately unequal to its player ids, so a
  ;; reader that forgot to translate resolves nobody — `db/held-ids`' bug.
  (let [t (side)]
    (is (every? :player-name (remove :empty? (:starters t)))
        "every seat resolved to a real player")
    (is (not-any? :unvalued? (:starters t)))))

(deftest player-points-are-translated-too
  ;; Keyed by the provider's ids, like the roster; translating one and not the
  ;; other is how the last one broke.
  (is (= 20.0 (:actual (first (:starters (side)))))))

(deftest a-team-defense-survives-the-crosswalk
  ;; Its id is the abbreviation in both spaces: it falls through unmapped.
  (let [dst (last (:starters (side)))]
    (is (= "SF" (:player-id dst)))
    (is (= 6.0 (:actual dst)))))


(deftest a-zero-before-kickoff-is-not-a-zero
  ;; Providers score everyone 0.0 until his game starts.
  (let [pending (mapv #(assoc % :kickoff/started? false) board)
        t       (side :board pending)]
    (is (every? nil? (map :actual (:starters t))) "no seat claims a score")
    (is (nil? (:actual t)) "and the team total has nothing to report either")
    (testing "while the projection is unaffected — it never needed a kickoff"
      (is (= 106.0 (:projected t))))))

(deftest a-real-zero-after-kickoff-is-kept
  (let [t (side :score (assoc-in score [:player-points (sleeper-id "qb1")] 0.0))]
    (is (= 0.0 (:actual (first (:starters t)))))))

(deftest an-unknown-kickoff-shows-what-the-provider-said
  ;; The scoreboard degrades to nil whole, so a missing status is not evidence
  ;; that nothing has started.
  (let [unknown (mapv #(dissoc % :kickoff/started?) board)]
    (is (= 20.0 (:actual (first (:starters (side :board unknown))))))))

(deftest a-player-the-provider-did-not-score-has-no-actual
  (let [t (side :score (update score :player-points dissoc (sleeper-id "qb1")))]
    (is (nil? (:actual (first (:starters t)))))))


(deftest an-unvalued-player-keeps-his-seat-and-his-score
  (let [thin (remove #(= "qb1" (:player-id %)) board)
        t    (side :board thin)
        qb   (first (:starters t))]
    (is (:unvalued? qb))
    (is (= "QB" (:slot qb)) "the seat is still named")
    (is (= 20.0 (:actual qb)) "and his score still shows")))

(deftest totals-skip-what-they-cannot-add
  ;; A seat nobody projected and a seat projected at nothing differ.
  (let [t (side :board (mapv #(if (= "qb1" (:player-id %)) (dissoc % :week-points) %) board))]
    (is (= 84.0 (:projected t)) "106 less the 22 nobody projected")))


(deftest the-bench-is-everyone-not-in-the-lineup
  (let [t (side)]
    (is (= #{"wr3" "ir1"} (set (map :player-id (:bench t)))))
    (is (every? nil? (map :slot (:bench t))) "a bench seat has no slot")))

(deftest a-parked-player-is-on-the-bench-and-says-so
  ;; The answer to "why is he not in my optimal lineup".
  (let [b (into {} (map (juxt :player-id :parked?)) (:bench (side)))]
    (is (true? (get b "ir1")))
    (is (false? (get b "wr3")))))


(deftest a-lineup-set-by-projection-is-already-optimal-on-projection
  (let [o (get-in (side) [:optimal :projected])]
    (is (= 106.0 (:total o)))
    (is (zero? (:gain o)))
    (is (= [] (:in o)))
    (is (= [] (:out o)))))

(deftest the-actual-basis-finds-the-points-left-on-the-bench
  ;; wr3 scored 25 on the bench while rb1 scored 5 in the lineup.
  (let [o (get-in (side) [:optimal :actual])]
    (is (= 116.0 (:total o)))
    (is (= 20.0 (:gain o)) "116 against the 96 he actually started")
    (is (= ["wr3"] (mapv :player-id (:in o))))
    (is (= ["rb1"] (mapv :player-id (:out o))))
    (is (= 25.0 (:points (first (:in o)))) "named in the unit it was measured in")))

(deftest the-actual-basis-waits-until-every-game-is-final
  ;; rb1 plays Sunday. Before then wr3's 25 is "left on the bench" in a seat
  ;; whose starter has not played, which is not regret but a guess.
  (let [sunday (mapv #(if (= "rb1" (:player-id %))
                        (assoc % :kickoff/started? false :kickoff/status "STATUS_SCHEDULED")
                        %)
                     board)]
    (is (nil? (get-in (side :board sunday) [:optimal :actual])))
    (is (some? (get-in (side :board sunday) [:optimal :projected]))
        "while the projection still has advice to give"))
  (testing "a side with no scoreboard at all is unknown, not final"
    (let [blind (mapv #(dissoc % :kickoff/status) board)]
      (is (nil? (get-in (side :board blind) [:optimal :actual]))))))

(deftest the-projected-basis-only-moves-players-whose-games-have-not-started
  ;; wr3 is projected to outscore rb3 at FLEX, but only if rb3's game has not
  ;; kicked off and wr3's has not either.
  (let [open  (fn [ids] (mapv #(if (ids (:player-id %))
                                 (assoc % :kickoff/started? false :kickoff/status "STATUS_SCHEDULED")
                                 %)
                              board))
        wr3up (fn [b] (mapv #(if (= "wr3" (:player-id %)) (assoc % :week-points 12.0) %) b))]
    (testing "both unlocked: the swap is offered"
      (let [o (get-in (side :board (wr3up (open #{"rb3" "wr3"}))) [:optimal :projected])]
        (is (= ["wr3"] (mapv :player-id (:in o))))
        (is (= ["rb3"] (mapv :player-id (:out o))))
        (is (= 3.0 (:gain o)))
        (is (false? (:locked? o)))))
    (testing "the starter already playing keeps his seat"
      (let [o (get-in (side :board (wr3up (open #{"wr3"}))) [:optimal :projected])]
        (is (= [] (:in o)))
        (is (true? (:locked? o)) "every seat's starter has kicked off")))
    (testing "a bench player already playing cannot come in"
      (let [o (get-in (side :board (wr3up (open #{"rb3"}))) [:optimal :projected])]
        (is (= [] (:in o)))
        (is (zero? (:gain o)))))))

(deftest the-best-lineup-comes-back-as-seats-and-a-bench
  (let [open (mapv #(cond-> %
                      (#{"rb3" "wr3"} (:player-id %))
                      (assoc :kickoff/started? false :kickoff/status "STATUS_SCHEDULED")
                      (= "wr3" (:player-id %)) (assoc :week-points 12.0))
                   board)
        {:keys [starters bench]} (get-in (side :board open) [:optimal :projected])
        flex (nth starters 6)]
    (is (= slots (mapv :slot starters)) "every seat, in the league's order")
    (is (= "wr3" (:player-id flex)))
    (is (:moved-in? flex))
    (is (not-any? :moved-in? (remove #(= "wr3" (:player-id %)) starters)))
    (is (= "rb3" (:player-id (first bench))) "the benched starter leads the bench")
    (is (:moved-out? (first bench)))
    (is (not-any? #{"wr3"} (map :player-id bench)))))

(deftest a-pickup-since-the-last-sync-is-startable
  ;; wr9 was claimed and started after the sync. The lineup is live and the sync
  ;; is not, so reading the roster off the sync made him a starter nobody could
  ;; start: "sit wr9", and a negative gain off a perfect lineup.
  (let [live    (-> (vec (remove #{"wr2"} roster)) (conj "wr9"))
        started (assoc lineup 4 "wr9")
        board'  (conj board (p "wr9" "WR" 11.0))
        score'  (-> score
                    (assoc :player-ids (mapv sleeper-id live)
                           :starter-ids (mapv sleeper-id started))
                    (assoc-in [:player-points (sleeper-id "wr9")] 11.0))
        t       (side :board board' :score score')
        o       (get-in t [:optimal :projected])]
    (is (zero? (:gain o)))
    (is (= [] (:out o)))
    (is (not-any? #{"wr2"} (map :player-id (:bench t))) "and the man he replaced is gone")
    (is (true? (:parked? (first (filter #(= "ir1" (:player-id %)) (:bench t)))))
        "IR still comes from the sync, which is the only thing that knows it")))

(deftest the-two-bases-disagree-and-both-are-reported
  (let [{:keys [projected actual]} (:optimal (side))]
    (is (not= (:gain projected) (:gain actual)))))

(deftest an-injured-reserve-player-is-never-in-an-optimal-lineup
  ;; Projected and scoring better than anyone on the roster, and unstartable.
  (doseq [basis [:projected :actual]]
    (is (not-any? #{"ir1"} (map :player-id (get-in (side) [:optimal basis :in])))
        (name basis))))

(deftest an-unvalued-starter-is-not-judged-by-an-optimizer-that-cannot-place-him
  ;; No position, so `best-lineup` can never seat him — but he carries an
  ;; `:actual`, and as current he came out in `:out` off a perfect lineup.
  (let [thin (remove #(= "wr1" (:player-id %)) board)
        o    (get-in (side :board thin) [:optimal :actual])]
    (is (not-any? #{"wr1"} (map :player-id (:out o)))
        "the board cannot place him, so it does not judge him")
    (is (= [] (:out o)) "and nobody else was wrong either")
    (is (= ["wr3"] (mapv :player-id (:in o))))
    (is (= 25.0 (:gain o))
        "measured over the eight seats it can place, on both sides")))

(deftest an-unvalued-starter-still-shows-his-seat-and-his-score
  ;; Excluded from the optimizer is not excluded from the board.
  (let [thin (remove #(= "wr1" (:player-id %)) board)
        wr   (nth (:starters (side :board thin)) 3)]
    (is (:unvalued? wr))
    (is (= "WR" (:slot wr)))
    (is (= 18.0 (:actual wr)))))

(deftest an-empty-seat-is-not-a-player-the-optimizer-can-drop
  (let [gapped (assoc score :starter-ids (assoc (mapv sleeper-id lineup) 1 "0"))
        o      (get-in (side :score gapped) [:optimal :actual])]
    (is (not-any? nil? (map :player-id (:out o))))
    (is (not-any? :empty? (:out o)))))


(deftest a-row-carries-the-scoreboards-opponent
  ;; `waivers/week-matchup` falls back to these when Sleeper names nobody.
  (let [b (conj (vec (remove #(= "qb1" (:player-id %)) board))
                (p "qb1" "QB" 22.0 :kickoff/opponent "NE" :kickoff/home? true))
        qb (first (:starters (side :board b)))]
    (is (= "NE" (:kickoff/opponent qb)))
    (is (true? (:kickoff/home? qb)))))

(deftest the-teams-come-back-as-a-vector
  ;; An integer map key round-trips through JSON as a keywordized string.
  (is (vector? (:teams (run))))
  (is (= 1 (:roster-id (first (:teams (run)))))))

(deftest a-team-that-has-not-kicked-off-reports-no-score-at-all
  ;; `total` is 0.0 for an empty sum: before kickoff the header would announce
  ;; a bold 0.0 over nine rows each showing a dash, and two such teams a tie.
  (let [pending (mapv #(assoc % :kickoff/started? false) board)
        t       (side :board pending)]
    (is (nil? (:actual t)) "no starter carries a number, so there is no score")
    (is (nil? (:official t))
        "and the provider's own 0.0 goes with it — that is the figure most read as a result")
    (is (= 106.0 (:projected t)) "the projection never needed a kickoff"))
  (testing "one player having played is enough to report a score"
    (let [one (mapv #(if (= "qb1" (:player-id %)) % (assoc % :kickoff/started? false)) board)
          t   (side :board one)]
      (is (= 20.0 (:actual t))))))

(deftest a-team-with-nothing-projected-reports-no-projection
  ;; So a board with no weekly lines does not claim every team is projected
  ;; to score nothing.
  (let [blank (mapv #(dissoc % :week-points) board)]
    (is (nil? (:projected (side :board blank))))))

(deftest the-teams-record-and-the-providers-own-total-ride-along
  (let [t (side)]
    (is (= 5 (:wins t)))
    (is (= 3 (:losses t)))
    (is (= 96.0 (:official t))
        "the league's score of record, beside the sum — the fixture has kicked off")))

(deftest the-pairing-is-passed-through
  (is (= [{:matchup-id 1 :roster-ids [1 2]}] (:matchups (run)))))

(deftest a-roster-with-no-seats-yields-no-lineup-rather-than-throwing
  ;; No synced positions and no roster config.
  (let [t (side :slots nil)]
    (is (= [] (:starters t)))
    (is (nil? (:projected t)) "no seats means no projection, not a projection of nothing")
    (is (= 11 (count (:bench t))) "everybody is on the bench")))
