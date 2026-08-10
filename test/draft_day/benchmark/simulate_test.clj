(ns draft-day.benchmark.simulate-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.benchmark.simulate :as sim]))

(defn p [id pos adp points actual]
  {:player-id id :gsis-id id :position pos :adp adp :points points :actual/points actual})

(deftest snake-order-reverses-every-other-round
  (is (= [0 1 2  2 1 0  0 1 2] (sim/snake-order 3 3)))
  (testing "each team picks exactly once per round"
    (is (= {0 4 1 4 2 4} (frequencies (sim/snake-order 3 4))))))

(deftest board-order-differs-by-seat
  (let [ps [(p "a" "RB" 30 100 0) (p "b" "RB" 5 90 0) (p "c" "RB" 60 80 0)]]
    (testing "the model seat ranks by its own points"
      (is (= ["a" "b" "c"] (mapv :player-id (sim/board-order ps true nil)))))
    (testing "the field ranks by consensus ADP"
      (is (= ["b" "a" "c"] (mapv :player-id (sim/board-order ps false nil)))))))

(deftest players-with-no-consensus-price-go-last-not-missing
  ;; A player the field has no ADP for must still be draftable, or the pool
  ;; silently shrinks and late rounds run dry.
  (let [ps [(p "a" "RB" nil 100 0) (p "b" "RB" 50 90 0)]]
    (is (= ["b" "a"] (mapv :player-id (sim/board-order ps false nil))))))

(deftest pick-respects-positional-caps
  (let [ordered [(p "q1" "QB" 1 100 0) (p "q2" "QB" 2 99 0) (p "r1" "RB" 3 98 0)]]
    (testing "at the QB cap the next pick moves on rather than hoarding"
      (is (= "r1" (:player-id (sim/pick ordered #{} {"QB" 2} {"QB" 2 "RB" 6})))))
    (testing "already-taken players are skipped"
      (is (= "q2" (:player-id (sim/pick ordered #{"q1"} {} {"QB" 2 "RB" 6})))))))

(deftest best-lineup-uses-the-best-legal-starters-and-one-flex
  ;; Roster: 1 QB, 3 RB, 2 WR, 1 TE. Starters QB1/RB2/WR2/TE1 + 1 FLEX.
  ;; Realized: QB 300; RB 200,150,100; WR 180,120; TE 90
  ;; Starters: 300 + (200+150) + (180+120) + 90 = 1040
  ;; FLEX: best leftover among RB3=100 / TE none left / WR none left -> 100
  ;; Total 1140
  (let [roster [(p "q" "QB" 1 0 300)
                (p "r1" "RB" 2 0 200) (p "r2" "RB" 3 0 150) (p "r3" "RB" 4 0 100)
                (p "w1" "WR" 5 0 180) (p "w2" "WR" 6 0 120)
                (p "t" "TE" 7 0 90)]
        cfg   sim/default-config]
    (is (= 1140.0 (sim/best-lineup-points roster cfg :actual/points)))))

(deftest best-lineup-ignores-bench-depth-beyond-the-flex
  (let [thin [(p "q" "QB" 1 0 300) (p "r1" "RB" 2 0 200) (p "r2" "RB" 3 0 150)
              (p "w1" "WR" 4 0 180) (p "w2" "WR" 5 0 120) (p "t" "TE" 6 0 90)]
        deep (concat thin [(p "r9" "RB" 90 0 1) (p "w9" "WR" 91 0 1)])
        cfg  sim/default-config]
    (testing "extra scrubs add only the single best flex slot, not the whole bench"
      (is (= (+ (sim/best-lineup-points thin cfg :actual/points) 1.0)
             (sim/best-lineup-points deep cfg :actual/points))))))

(deftest a-perfect-board-beats-the-field
  ;; 24 players. The model's :points equal realized points (perfect foresight),
  ;; so it drafts best-first. The field drafts by ADP ascending, and ADP here is
  ;; (inc i) while realized points are also (inc i) — so the field takes the
  ;; WORST player first. Model should win handily.
  (let [ps (for [i (range 24)]
             (let [pos (nth ["QB" "RB" "WR" "TE"] (mod i 4))]
               (p (str "p" i) pos (inc i) (double (inc i)) (double (inc i)))))
        cfg (assoc sim/default-config :teams 4 :rounds 4
                   :caps {"QB" 2 "RB" 4 "WR" 4 "TE" 2})
        out (sim/simulate-season (vec ps) 0 cfg :actual/points)]
    (is (pos? (:edge out)) "a board with perfect foresight must out-draft the field")))

(deftest simulating-every-seat-cancels-draft-position
  ;; Here ADP is (24 - i) while points are (inc i), so ADP-ascending and
  ;; points-descending produce the SAME order: the model's board is the
  ;; consensus. Averaged over every seat its edge must be exactly zero.
  (let [ps (for [i (range 24)]
             (let [pos (nth ["QB" "RB" "WR" "TE"] (mod i 4))]
               (p (str "p" i) pos (- 24 i) (double (inc i)) (double (inc i)))))
        cfg (assoc sim/default-config :teams 4 :rounds 4
                   :caps {"QB" 2 "RB" 4 "WR" 4 "TE" 2})
        out (sim/simulate-all-seats (vec ps) cfg :actual/points)]
    (is (= 4 (count (:by-seat out))))
    (testing "a model whose board IS the consensus gains nothing anywhere"
      (is (< (Math/abs (:edge out)) 1e-9)))))

(deftest run-skips-unscored-seasons
  (let [ps [(p "a" "RB" 1 10 10)]
        rs [{:season 2021 :players ps} {:season 2022 :skipped? true :reason "no capture"}]]
    (is (= [2021] (mapv :season (sim/run rs :actual/points
                                         (assoc sim/default-config :teams 2 :rounds 1)))))))
