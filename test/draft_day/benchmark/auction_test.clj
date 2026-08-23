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
    (is (not (a/can-take? t (mk "rb1" "RB" 100 1) by-id a/default-config)))))

(deftest a-position-at-its-cap-cannot-bid
  (let [t {:team-id "1" :bankroll 50.0
           :roster (conj (mapv #(hash-map :pos "BENCH" :player-id (str "rb" %)) (range 3))
                         {:pos "BENCH" :player-id nil})}]
    (is (not (a/can-take? t (mk "rb7" "RB" 10 40) by-id cfg))
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
    (is (not (a/can-take? t (mk "rb7" "RB" 10 40) by-id (assoc cfg :caps {"RB" 6 "WR" 6})))
        "a fourth back would leave the second receiver slot unfillable")
    (is (a/can-take? t (mk "wr7" "WR" 10 40) by-id (assoc cfg :caps {"RB" 6 "WR" 6}))
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

(defn- run-one
  "`simulate-season`'s loop, kept open so the final state can be asserted on.
  Adds a `:log` of who bought what, which `apply-pick` does not record."
  ([clearing] (run-one clearing (vec (repeat (count clearing) 0.0)) cfg))
  ([clearing spread] (run-one clearing spread cfg))
  ([clearing spread config]
   (let [st (a/static-board pool config)]
     (loop [state (assoc (replay/base-state 2 (:budget config) (a/roster-slots config)) :log [])
            [p & more] (a/nomination-order pool) i 0]
       (if (or (nil? p) (every? #(zero? (a/open-slots %)) (:teams state)))
         state
         (let [live (engine/live-valuation st state)
               w    (or (:worth (first (filter #(= (:player-id %) (:player-id p))
                                               (:players live)))) 0)
               j    (min i (dec (count clearing)))]
           (if-let [[tid price] (#'a/winner state "1" (nth clearing j) (nth spread j)
                                            j w by-id config p)]
             (recur (-> (replay/apply-pick state {:player-id (:player-id p)
                                                  :position  (:position p)
                                                  :price     (double price)
                                                  :team-id   tid})
                        (update :log conj {:team-id tid :price price :rank j}))
                    more (inc i))
             (recur state more (inc i)))))))))

;; ---- the field has to disagree ----------------------------------------------

(deftest a-seat-a-dollar-over-the-market-does-not-sweep-the-board
  ;; The flaw this whole mechanism exists to fix. With every field seat naming
  ;; the same number to the dollar, the marginal bidder sits exactly at the mean:
  ;; a seat bidding $1 more wins every contest it enters and a seat bidding $1
  ;; less wins none. Measured, that step function cost a market+$1 seat 13 points
  ;; a game — more than bidding Worth cost — which made the headline finding an
  ;; artifact of being the only seat that was different.
  ;; Every seat fills its roster either way, so the tell is not how many players
  ;; the aggressive seat got but WHICH: how many of the early, expensive
  ;; nominations it took.
  (let [rich  (assoc cfg :budget 100 :model-bid (fn [m _] (inc m)))
        early (fn [spread]
                (->> (:log (run-one (vec (repeat 14 5)) spread rich))
                     (filter #(and (= "1" (:team-id %)) (< (:rank %) 7)))
                     count))]
    (is (= 7 (early (vec (repeat 14 0.0))))
        "with no disagreement a dollar takes every one of the first seven")
    (is (< (early (vec (repeat 14 0.35))) 7)
        "with real disagreement it takes only some of them")))

(deftest jitter-is-deterministic-and-scales-with-the-spread
  (is (= (a/jitter "3" 7 0.2 11) (a/jitter "3" 7 0.2 11))
      "the benchmark has to reproduce, so the draw cannot come from a fresh RNG")
  (is (not= (a/jitter "3" 7 0.2 11) (a/jitter "4" 7 0.2 11))
      "two seats hold different opinions of the same player")
  (is (not= (a/jitter "3" 7 0.2 11) (a/jitter "3" 8 0.2 11))
      "one seat holds different opinions of two players")
  (is (= 1.0 (a/jitter "3" 7 0.0 11)) "no measured spread, no disagreement")
  (is (every? #(<= 0.0 % 2.0) (for [t (range 40)] (a/jitter (str t) 3 0.3 11)))
      "no seat is ever willing to pay a negative or wildly unbounded price"))

(deftest the-field-is-centred-so-the-winning-bid-lands-on-the-curve
  ;; The measured spread is the scatter of what rooms actually PAID, which is
  ;; already a winning bid. Centring eleven bidders on it and taking the top of
  ;; them would price every player above what any real room paid.
  (let [n 11
        runner-up (fn [rank cv]
                    (second (sort > (map #(a/jitter (str %) rank cv n) (range n)))))
        mean (fn [cv] (let [xs (map #(runner-up % cv) (range 400))]
                        (/ (reduce + 0.0 xs) (count xs))))]
    (doseq [cv [0.15 0.25 0.4]]
      (is (< 0.9 (mean cv) 1.1)
          (str "at cv " cv " the runner-up among eleven opinions lands on the curve, "
               "not above it")))))

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
  (let [r (a/simulate-season pool 0 cfg :actual (vec (repeat 14 3)) (vec (repeat 14 0.0)))]
    (is (pos? (:model-points r)))
    (is (pos? (:field-mean r)))
    (is (= (:edge r) (- (:model-points r) (:field-mean r))))))

(deftest every-seat-is-simulated
  (is (= 2 (count (:by-seat (a/simulate-all-seats pool cfg :actual (vec (repeat 14 3)) (vec (repeat 14 0.0))))))
      "one run per seat, so nomination luck cancels"))

(deftest a-skipped-season-is-not-auctioned
  ;; Clearing prices passed in rather than measured: `market-prices` reads the
  ;; replay cache, which is gitignored and may not exist on the machine running
  ;; this.
  (let [market {:clearing (vec (repeat 14 3)) :spread (vec (repeat 14 0.0))}]
    (is (= [] (a/run [{:season 2020 :skipped? true :players pool}] :actual cfg market)))
    (is (= [2019] (map :season (a/run [{:season 2019 :players pool}] :actual cfg market))))))
