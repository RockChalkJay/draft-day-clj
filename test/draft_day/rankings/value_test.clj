(ns draft-day.rankings.value-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.rankings.value :as value]
            [draft-day.scoring :as scoring]
            [draft-day.rankings.league-state :as ls]
            [draft-day.rankings.engine :as engine]))

;; ---- Value (stable salary-cap dollars) --------------------------------------

(deftest value-conserves-to-budget
  ;; Discretionary pool (budget - slots = 185) split by VORP share, +$1 each.
  (let [board [{:player-id "a" :position "RB" :vorp 100.0}
               {:player-id "b" :position "WR" :vorp 60.0}
               {:player-id "c" :position "TE" :vorp 40.0}]
        v     (mapv :value (value/calculate-value board 200 15))]
    (is (<= (Math/abs (long (- (reduce + v) (+ 3 185)))) 2))  ; conserves
    (is (> (nth v 0) (nth v 1) (nth v 2)))))                  ; more VORP -> more value

(deftest k-dst-and-replacement-get-zero-value
  (let [board [{:player-id "k" :position "K"   :vorp 50.0}
               {:player-id "d" :position "DST" :vorp 50.0}
               {:player-id "r" :position "RB"  :vorp 0.0}    ; replacement level
               {:player-id "a" :position "RB"  :vorp 80.0}]
        vv    (into {} (map (juxt :player-id :value) (value/calculate-value board 200 15)))]
    (is (= 0 (vv "k"))) (is (= 0 (vv "d"))) (is (= 0 (vv "r"))) (is (pos? (vv "a")))))

(deftest missing-vorp-gives-zero-value
  (is (= 0 (:value (first (value/calculate-value [{:player-id "p0" :position "RB"}] 200 15))))))

;; ---- Price (Value scaled by live inflation) ---------------------------------

(defn- pf [v & [pos]] [{:player-id "p0" :position (or pos "RB") :value v}])
(defn- worth1 [board infl drafted] (:worth (first (value/calculate-price board infl drafted))))

(deftest price-equals-value-at-inflation-one
  (is (= 40 (worth1 (pf 40) 1.0 #{}))))

(deftest price-deflates-below-one
  (is (= 20 (worth1 (pf 40) 0.5 #{}))))          ; 1 + (40-1)*0.5 = 20.5 -> 20 (round half even)

(deftest dollar-value-stays-a-dollar-regardless-of-inflation
  (is (= 1 (worth1 (pf 1) 1.8 #{}))))

(deftest price-k-dst-zero
  (is (= 0 (worth1 (pf 30 "K") 1.0 #{})))
  (is (= 0 (worth1 (pf 30 "DST") 1.0 #{}))))

(deftest price-drafted-zero
  (is (= 0 (worth1 (pf 40) 1.0 #{"p0"}))))

(deftest price-missing-value-zero
  (is (= 0 (worth1 [{:player-id "p0" :position "RB"}] 1.0 #{}))))

;; ---- End to end -------------------------------------------------------------

(defn- rb [i]
  {:player-id (str "rb" i) :player-name (str "RB" i) :position "RB"
   :stats {:rush_yd (- 1600 (* i 70)) :rush_td (- 14 (* i 0.5))
           :rec (- 50 i) :rec_yd (- 400 (* i 12)) :rec_td 3}})

(defn- wr [i]
  {:player-id (str "wr" i) :player-name (str "WR" i) :position "WR"
   :stats {:rec (- 110 (* i 4)) :rec_yd (- 1500 (* i 70)) :rec_td (- 11 (* i 0.4))}})

(defn- qb [i]
  {:player-id (str "qb" i) :player-name (str "QB" i) :position "QB"
   :stats {:pass_yd (- 4800 (* i 160)) :pass_td (- 38 i) :pass_int 9}})

(defn- synthetic-board []
  (vec (concat (map rb (range 20)) (map wr (range 18)) (map qb (range 14)))))

(defn- team [id cash roster-pos]
  {:team-id id :bankroll cash :roster (mapv (fn [p] {:pos p :player-id nil}) roster-pos)})

(def ^:private std-roster (into ["RB" "RB" "WR" "WR" "QB" "TE" "FLEX"] (repeat 8 "BENCH")))

(deftest end-to-end-static-then-live
  (let [static (engine/static-rankings (synthetic-board) (:ppr scoring/presets) 12 {:num-tiers 5})]
    (is (every? #(and (contains? % :points) (contains? % :tier) (contains? % :vorp))
                (:players static)))
    (let [state  {:teams (mapv #(team (str "t" %) 200.0 std-roster) (range 12))
                  :drafted-player-ids #{} :starting-bankroll 200.0}
          live   (engine/live-valuation static state)
          players (:players live)
          priced  (filter #(pos? (:value %)) players)]
      (is (every? #(and (contains? % :value) (contains? % :worth)
                        (contains? % :bargain) (contains? % :tcm)) players))
      (is (some? (:inflation live)))
      (testing "at draft start inflation ~1.0, so Price == Value and Bargain == 0"
        (is (< (Math/abs (double (- (:inflation live) 1.0))) 0.05))
        (is (every? #(= (:worth %) (:value %)) priced))
        (is (every? #(zero? (:bargain %)) priced)))
      (testing "Value is budget-conserving (never exceeds cash in room)"
        (is (<= (reduce + (map :value players)) (ls/total-remaining-cash state))))
      (testing "the top-VORP RB is the priciest RB"
        (let [rbs (sort-by :vorp > (filter #(= "RB" (:position %)) players))]
          (is (>= (:worth (first rbs)) (:worth (last rbs)))))))))

(deftest overpay-deflates-and-opens-bargains
  (let [static  (engine/static-rankings (synthetic-board) (:ppr scoring/presets) 12)
        live-fn (fn [t0-cash drafted]
                  (let [teams (into [(team "t0" t0-cash std-roster)]
                                    (mapv #(team (str "t" %) 200.0 std-roster) (range 1 12)))]
                    (engine/live-valuation static {:teams teams
                                                   :drafted-player-ids (set drafted)
                                                   :starting-bankroll 200.0})))
        start   (live-fn 200.0 [])
        top     (first (sort-by :value > (:players start)))
        after   (live-fn (- 200.0 (+ (:value top) 60)) [(:player-id top)])]
    (is (< (:inflation after) (:inflation start)))
    (let [ap (filter #(pos? (:worth %)) (:players after))]
      (is (every? #(>= (:bargain %) 0) ap))
      (is (some #(pos? (:bargain %)) ap)))))

(deftest expert-tiers-adopted-and-reanchored-per-position
  ;; FantasyPros' tier is *overall*; adopted expert tiers are dense-ranked within
  ;; each position so the best cluster -> tier 1. QBs qb0..qb13 are in descending
  ;; points order, so index < 7 == top-7 by points.
  (let [board  (mapv (fn [p]
                       (if (= "QB" (:position p))
                         (let [i (Integer/parseInt (subs (:player-id p) 2))]
                           (assoc p :fantasypros/ecr-tier (if (< i 7) 4 5)))
                         p))
                     (synthetic-board))
        static (engine/static-rankings board (:ppr scoring/presets) 12 {:num-tiers 5})
        out-qbs (filter #(= "QB" (:position %)) (:players static))]
    (is (= #{1 2} (set (map :tier out-qbs))))       ; re-anchored, 4/5 split kept
    (let [top7 (set (map :player-id (take 7 (sort-by :points > out-qbs))))]
      (is (= top7 (set (map :player-id (filter #(= 1 (:tier %)) out-qbs))))))
    (let [rbs (filter #(= "RB" (:position %)) (:players static))]
      (is (= 1 (reduce min (map :tier rbs))))       ; positions w/o expert tier keep cliffs
      (is (<= (reduce max (map :tier rbs)) 5)))))

(deftest worth-sags-as-rosters-fill-even-at-par
  ;; 4 teams x 6 slots = 24; draft the top 18 by value at par, round-robin, so
  ;; only phase decay (not over/underpay) pulls remaining Worth below open level.
  (let [static      (engine/static-rankings (synthetic-board) (:ppr scoring/presets) 4)
        roster      ["RB" "RB" "WR" "WR" "QB" "TE"]
        fresh-teams #(mapv (fn [i] (team (str "t" i) 200.0 roster)) (range 4))
        at-open     (engine/live-valuation static {:teams (fresh-teams)
                                                   :drafted-player-ids #{} :starting-bankroll 200.0})
        board       (take 18 (sort-by :value > (:players at-open)))
        teams       (loop [ts (fresh-teams) i 0 ps board]
                      (if (empty? ps)
                        ts
                        (let [p        (first ps)
                              ti       (mod i 4)
                              t        (ts ti)
                              slot-idx (first (keep-indexed (fn [idx s] (when (nil? (:player-id s)) idx))
                                                            (:roster t)))
                              t'       (-> t
                                           (update :bankroll - (:value p))
                                           (assoc-in [:roster slot-idx :player-id] (:player-id p)))]
                          (recur (assoc ts ti t') (inc i) (rest ps)))))
        mid         (engine/live-valuation static {:teams teams
                                                   :drafted-player-ids (set (map :player-id board))
                                                   :starting-bankroll 200.0})]
    (is (< (Math/abs (double (- (:market-heat mid) (- 1.0 (* 0.2 0.75 0.75))))) 1e-9))  ; t=18/24
    (let [survivor (first (sort-by :value > (filter #(pos? (:worth %)) (:players mid))))
          open-row (first (filter #(= (:player-id survivor) (:player-id %)) (:players at-open)))]
      (is (< (:worth survivor) (:worth open-row))))))

(deftest static-result-reusable-across-picks
  (let [rows   (mapv (fn [i] {:player-id (str "rb" i) :player-name (str "RB" i) :position "RB"
                              :stats {:rush_yd (- 1500 (* i 100)) :rush_td (- 12 i)
                                      :rec 40 :rec_yd 300 :rec_td 2}})
                     (range 15))
        static (engine/static-rankings rows (:ppr scoring/presets) 12)
        live1  (engine/live-valuation static {:teams [(team "t0" 200.0 ["RB"])] :drafted-player-ids #{}})
        live2  (engine/live-valuation static {:teams [(team "t0" 100.0 ["RB"])] :drafted-player-ids #{"rb0"}})]
    (is (not (contains? (first (:players static)) :worth)))       ; static frame untouched
    (is (= (count (:players live1)) (count (:players live2))))
    (testing "static-derived fields carry through unchanged regardless of live
              draft state — the static frame is reused, not recomputed"
      (let [by-id #(into {} (map (juxt :player-id identity)) (:players %))
            l1    (by-id live1)
            l2    (by-id live2)]
        (is (every? (fn [id] (= (select-keys (l1 id) [:points :vorp :tier])
                                (select-keys (l2 id) [:points :vorp :tier])))
                    (keys l1)))))
    (testing "live-valuation is idempotent for a fixed league-state"
      (let [again (engine/live-valuation
                   static {:teams [(team "t0" 200.0 ["RB"])] :drafted-player-ids #{}})]
        (is (= (:players live1) (:players again)))))))
