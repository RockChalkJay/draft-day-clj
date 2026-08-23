(ns draft-day.benchmark.auction-test
  (:require [clojure.test :refer [deftest is]]
            [draft-day.benchmark.auction :as a]
            [draft-day.rankings.engine :as engine]
            [draft-day.replay.core :as replay]))

(def cfg (assoc a/default-config :teams 2 :rounds 7 :budget 20
                :caps {"QB" 2 "RB" 3 "WR" 3 "TE" 2}))

(defn- mk [id pos pts adp]
  {:player-id id :position pos :points (double pts) :adp (double adp) :actual (double pts)})

(def pool
  (vec (concat (map #(mk (str "qb" %) "QB" (- 300 (* 10 %)) (+ 4 (* 4 %))) (range 4))
               (map #(mk (str "rb" %) "RB" (- 260 (* 8 %)) (+ 1 (* 3 %))) (range 8))
               (map #(mk (str "wr" %) "WR" (- 250 (* 7 %)) (+ 2 (* 3 %))) (range 8))
               (map #(mk (str "te" %) "TE" (- 200 (* 9 %)) (+ 6 (* 5 %))) (range 4)))))

(def by-id (into {} (map (juxt :player-id identity)) pool))

;; ---- roster arithmetic ------------------------------------------------------

(deftest roster-fills-the-rounds
  ;; The auction has to draft the same number of seats the snake sim does, or the
  ;; two edges are measured on differently sized teams and cannot be compared.
  (let [slots (replay/roster->slots (a/roster-slots a/default-config))]
    (is (= (:rounds a/default-config) (count slots))))
  (is (= 5 (:bench (a/roster-slots a/default-config)))
      "12 rounds less a 7-slot starting lineup"))

(deftest starter-holes-count-flex-from-the-spare-bodies
  (let [c a/default-config]
    (is (= 7 (a/open-starter-slots {} c))
        "an empty roster owes six fixed slots and a flex")
    (is (= 0 (a/open-starter-slots {"QB" 1 "RB" 2 "WR" 3 "TE" 1} c))
        "the third receiver covers flex")
    (is (= 1 (a/open-starter-slots {"QB" 1 "RB" 2 "WR" 2 "TE" 1} c))
        "exactly the starters leaves flex open")
    (is (= 1 (a/open-starter-slots {"QB" 3 "RB" 2 "WR" 2 "TE" 1} c))
        "spare quarterbacks are not flex-eligible")))

;; ---- eligibility ------------------------------------------------------------

(deftest a-full-roster-cannot-bid
  (let [t {:team-id "1" :bankroll 50.0 :roster [{:pos "RB" :player-id "rb0"}]}]
    (is (not (a/can-take? t (mk "rb1" "RB" 100 1) by-id {"RB" 6} a/default-config)))))

(deftest a-position-at-its-cap-cannot-bid
  (let [t {:team-id "1" :bankroll 50.0
           :roster (conj (mapv #(hash-map :pos "BENCH" :player-id (str "rb" %)) (range 3))
                         {:pos "BENCH" :player-id nil})}]
    (is (not (a/can-take? t (mk "rb7" "RB" 10 40) by-id (:caps cfg) cfg))
        "three running backs is the cap in this fixture")))

(deftest a-purchase-that-orphans-a-starting-slot-is-refused
  ;; The rule the snake simulator never needed. Worth is position-blind — dollars
  ;; per VORP — so without this the model seat buys six backs, no receiver, and
  ;; the metric reports roster construction instead of the dollar math.
  (let [t {:team-id "1" :bankroll 5.0
           :roster [{:pos "QB" :player-id "qb0"} {:pos "RB" :player-id "rb0"}
                    {:pos "RB" :player-id "rb1"} {:pos "WR" :player-id "wr0"}
                    {:pos "TE" :player-id "te0"} {:pos "FLEX" :player-id "rb2"}
                    {:pos "BENCH" :player-id nil}]}]
    (is (not (a/can-take? t (mk "rb7" "RB" 10 40) by-id {"RB" 6 "WR" 6} cfg))
        "a fourth back would leave the second receiver slot unfillable")
    (is (a/can-take? t (mk "wr7" "WR" 10 40) by-id {"RB" 6 "WR" 6} cfg)
        "the receiver that fills it is still allowed")))

(deftest an-obligated-seat-is-recognized-as-obligated
  (let [t {:team-id "1" :bankroll 3.0
           :roster [{:pos "QB" :player-id "qb0"} {:pos "RB" :player-id "rb0"}
                    {:pos "RB" :player-id "rb1"} {:pos "WR" :player-id "wr0"}
                    {:pos "TE" :player-id "te0"} {:pos "FLEX" :player-id "rb2"}
                    {:pos "BENCH" :player-id nil}]}]
    (is (a/must-bid? t (mk "wr7" "WR" 10 40) by-id cfg)
        "the one seat left is the second receiver slot")
    (is (not (a/must-bid? t (mk "qb3" "QB" 10 40) by-id cfg))
        "a quarterback does not fill it")))

(deftest a-spare-bench-seat-does-not-excuse-an-empty-starting-slot
  ;; The rule used to require that EVERY remaining seat be a starting hole, so a
  ;; roster holding bench room alongside an unfilled RB2 declined the whole tail
  ;; of the board and went to kickoff starting nobody there. Four seats open, one
  ;; of them owed.
  (let [t {:team-id "1" :bankroll 8.0
           :roster [{:pos "QB" :player-id "qb0"} {:pos "RB" :player-id "rb0"}
                    {:pos "WR" :player-id "wr0"} {:pos "WR" :player-id "wr1"}
                    {:pos "BENCH" :player-id nil} {:pos "BENCH" :player-id nil}
                    {:pos "BENCH" :player-id nil}]}]
    (is (a/must-bid? t (mk "rb7" "RB" 10 40) by-id cfg)
        "the second back is still owed, bench room or not")
    (is (not (a/must-bid? t (mk "qb3" "QB" 10 40) by-id cfg))
        "a second quarterback fills no hole")))

;; ---- the budget guard -------------------------------------------------------

(deftest a-bid-always-leaves-a-dollar-on-every-remaining-seat
  (is (= 8 (a/max-bid {:bankroll 10.0 :roster [{:player-id nil} {:player-id nil}
                                               {:player-id nil}]}))
      "$10 over three seats bids $8 and keeps $2")
  (is (= 10 (a/max-bid {:bankroll 10.0 :roster [{:player-id nil}]}))
      "the last seat may spend it all")
  (is (zero? (a/max-bid {:bankroll 10.0 :roster [{:player-id "x"}]}))
      "a full roster bids nothing"))

;; ---- the auction as a whole -------------------------------------------------

(defn- run-one
  "`simulate-season`'s loop, kept open so the final state can be asserted on."
  [clearing]
  (let [st (a/static-board pool cfg)]
    (loop [state (replay/base-state 2 20 (a/roster-slots cfg))
           [p & more] (a/nomination-order pool) i 0]
      (if (or (nil? p) (every? #(zero? (a/open-slots %)) (:teams state)))
        state
        (let [live (engine/live-valuation st state)
              w    (or (:worth (first (filter #(= (:player-id %) (:player-id p))
                                              (:players live)))) 0)
              m    (nth clearing (min i (dec (count clearing))))]
          (if-let [[tid price] (#'a/winner state "1" m w by-id cfg p)]
            (recur (replay/apply-pick state {:player-id (:player-id p)
                                             :position  (:position p)
                                             :price     (double price)
                                             :team-id   tid})
                   more (inc i))
            (recur state more (inc i))))))))

(deftest nobody-ever-ends-up-in-debt
  ;; `replay/apply-pick` subtracts whatever price it is handed and will happily
  ;; drive a bankroll negative — it was written for real drafts, where the prices
  ;; are facts. Here they are decisions, so the guard has to be on this side.
  (doseq [clearing [(vec (repeat 14 3)) (vec (repeat 14 500)) (vec (repeat 14 1))]]
    (doseq [t (:teams (run-one clearing))]
      (is (>= (:bankroll t) 0.0) (str "team " (:team-id t) " went into debt")))))

(deftest every-seat-gets-filled
  ;; Worth is 0 for anyone the board valued under a dollar, so a seat reading only
  ;; Worth declines the tail outright and finishes short. Nobody drafts an
  ;; incomplete roster.
  (let [state (run-one (vec (repeat 14 3)))]
    (doseq [t (:teams state)]
      (is (zero? (a/open-slots t)) (str "team " (:team-id t) " left a seat empty")))
    (is (= 14 (count (:picks state))) "two teams, seven seats each")))

(deftest every-seat-fields-a-legal-starting-lineup
  ;; The invariant `every-seat-gets-filled` could not see: a roster can be full of
  ;; bodies and still start nobody at a position. Scoring an incomplete lineup
  ;; reports a valuation failure that is really a bookkeeping one.
  (doseq [clearing [(vec (repeat 14 3)) (vec (repeat 14 500)) (vec (repeat 14 1))]]
    (doseq [t (:teams (run-one clearing))]
      (let [counts (frequencies (keep #(some-> (:player-id %) by-id :position) (:roster t)))]
        (is (zero? (a/open-starter-slots counts cfg))
            (str "team " (:team-id t) " starts nobody somewhere: " counts))))))

(deftest a-market-nobody-can-afford-still-clears
  ;; Prices far above what the room holds: every seat must still fill, at the
  ;; dollar the reserve leaves.
  (let [state (run-one (vec (repeat 14 500)))]
    (doseq [t (:teams state)]
      (is (zero? (a/open-slots t)))
      (is (>= (:bankroll t) 0.0)))
    (is (every? #(<= (:price %) 20.0) (:picks state))
        "no seat pays more than the whole bankroll")))

(deftest the-model-seat-never-pays-the-asking-price-it-declined
  ;; The point of the seat: Worth is a maximum bid, not a mandate. At $500 a head
  ;; nothing is worth it, so the model only ever pays the reserve dollar.
  (let [state (run-one (vec (repeat 14 500)))]
    (is (every? #(< (:price %) 500) (:picks state)))))

(deftest a-bargain-is-taken-and-paid-for-at-the-market
  ;; An auction charges what it took to win, not what the winner would have gone
  ;; to — so a seat that values a player above the room still pays the room's
  ;; price plus a dollar, and the surplus stays in its pocket.
  (let [state (run-one (vec (repeat 14 1)))
        model (first (filter #(= "1" (:team-id %)) (:teams state)))]
    (is (some? model))
    (is (every? #(<= (:price %) 2.0) (:picks state))
        "a $1 market clears at $1, or $2 when someone outbids it")))

(deftest nomination-runs-best-first
  (is (= "rb0" (first (map :player-id (a/nomination-order pool))))
      "lowest ADP goes up first")
  (is (= ["a" "b"] (map :player-id (a/nomination-order
                                    [{:player-id "b" :adp 9} {:player-id "a" :adp 2}])))))

(deftest a-player-nobody-has-a-price-for-is-nominated-last-not-dropped
  (is (= ["ranked" "none"]
         (map :player-id (a/nomination-order
                          [{:player-id "none"} {:player-id "ranked" :adp 50}])))))

;; ---- the top-level metric ---------------------------------------------------

(deftest a-season-scores-both-sides
  (let [r (a/simulate-season pool 0 cfg :actual (vec (repeat 14 3)))]
    (is (pos? (:model-points r)))
    (is (pos? (:field-mean r)))
    (is (= (:edge r) (- (:model-points r) (:field-mean r))))))

(deftest every-seat-is-simulated
  (is (= 2 (count (:by-seat (a/simulate-all-seats pool cfg :actual (vec (repeat 14 3))))))
      "one run per seat, so nomination luck cancels"))

(deftest a-skipped-season-is-not-auctioned
  ;; Clearing prices passed in rather than measured: `market-prices` reads the
  ;; replay cache, which is gitignored and may not exist on the machine running
  ;; this.
  (is (= [] (a/run [{:season 2020 :skipped? true :players pool}] :actual cfg
                   (vec (repeat 14 3)))))
  (is (= [2019] (map :season (a/run [{:season 2019 :players pool}] :actual cfg
                                    (vec (repeat 14 3)))))))
