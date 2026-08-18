(ns draft-day.rankings.value-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.rankings.value :as value]
            [draft-day.scoring :as scoring]
            [draft-day.rankings.league-state :as ls]
            [draft-day.rankings.engine :as engine]))

;; ---- Value (stable salary-cap dollars) --------------------------------------

(deftest value-conserves-to-budget
  ;; Discretionary pool (budget - slots = 185) split by VORP share, +$1 each.
  ;; Only 3 players and 15 slots, so there is nobody left to take the other 12
  ;; minimum bids — a real board has 96 of them and reaches the full $200.
  (let [board [{:player-id "a" :position "RB" :vorp 100.0}
               {:player-id "b" :position "WR" :vorp 60.0}
               {:player-id "c" :position "TE" :vorp 40.0}]
        v     (mapv :value (value/calculate-value board 200 15))]
    (is (<= (Math/abs (long (- (reduce + v) (+ 3 185)))) 2))  ; conserves
    (is (> (nth v 0) (nth v 1) (nth v 2)))))                  ; more VORP -> more value

(deftest the-below-replacement-tail-conserves-the-rest-of-the-budget
  ;; Three players above replacement rather than one, so no single row can absorb
  ;; the whole rounding residue and make the equality true by construction.
  ;; `to-dollars` rounds each priced player independently, so the claim is "to
  ;; within rounding", which is what `calculate-value` now says.
  (let [board (into [{:player-id "a" :position "RB" :vorp 80.0}
                     {:player-id "b" :position "WR" :vorp 55.0}
                     {:player-id "c" :position "TE" :vorp 33.0}]
                    (map (fn [i] {:player-id (str "t" i) :position "WR"
                                  :vorp (- (double (inc i)))}))
                    (range 30))
        vs    (value/calculate-value board 200 20)
        vv    (into {} (map (juxt :player-id :value)) vs)]
    (is (<= (Math/abs (- (reduce + (map :value vs)) 200)) 2)
        "sums to the budget within rounding, not to priced + discretionary")
    (is (= 20 (count (filter #(pos? (:value %)) vs)))
        "exactly one priced player per roster slot")
    (is (= 17 (count (filter #(= 1 (:value %)) vs))) "the rest of the slots at the minimum")
    (is (= 1 (vv "t0")) "the best of the tail is drafted, so it costs a dollar")
    (is (= 0 (vv "t25")) "past the last roster slot, nothing")))

(deftest a-board-cannot-price-more-players-than-the-league-has-slots
  ;; `:teams` and `:replacement-config` come from different snapshots — editing
  ;; roster or team count mid-draft keeps the old `:teams` — so more players can
  ;; clear replacement than there are seats. Left alone the board over-summed the
  ;; room and rows past the last slot read a real price.
  (let [board (mapv (fn [i] {:player-id (str "p" i) :position "RB"
                             :vorp (double (- 50 i))})
                    (range 40))
        vs    (value/calculate-value board 200 10)]
    (is (= 10 (count (filter #(pos? (:value %)) vs))) "never more priced rows than slots")
    (is (<= (reduce + (map :value vs)) 200) "and never more dollars than the room holds")))

(deftest a-room-too-poor-to-pay-a-dollar-a-slot-never-over-sums
  ;; `routes` rejects this league outright; the pure function still must not claim
  ;; 150% of the room if one reaches it another way.
  (let [board (mapv (fn [i] {:player-id (str "p" i) :position "RB"
                             :vorp (double (- 20 i))})
                    (range 40))
        vs    (value/calculate-value board 12 40)]
    (is (<= (reduce + (map :value vs)) 12))))

(deftest missing-vorp-gives-zero-value
  ;; No :vorp is no opinion, which is not the same as a minimum bid.
  (is (= 0 (:value (first (value/calculate-value [{:player-id "p0" :position "RB"}] 200 15))))))

(deftest min-bid-tail-takes-the-best-of-what-is-left
  (let [board [{:player-id "hi"  :position "WR" :vorp -1.0}
               {:player-id "lo"  :position "WR" :vorp -99.0}
               {:player-id "k"   :position "K"  :vorp 0.0}
               {:player-id "nil" :position "WR"}]]
    (is (= #{"hi"} (value/min-bid-ids board 1)) "one slot goes to the least-worst")
    (is (= #{"hi" "lo"} (value/min-bid-ids board 2)))
    (is (= #{} (value/min-bid-ids board 0)))))

(deftest k-and-dst-are-excluded-from-the-tail-by-name-not-by-accident
  ;; `priced-vorp?` is a `group-by` key, and `(and (a-set x) ...)` yields nil
  ;; rather than false on a miss — so K/DST used to land in a third group that
  ;; the `{priced true tail false}` destructuring dropped, and the explicit
  ;; position filter that really excluded them read as dead defensive code.
  (is (false? (value/priced-vorp? {:position "K" :vorp 50.0})))
  (is (false? (value/priced-vorp? {:position "RB" :vorp -5.0})))
  (is (true?  (value/priced-vorp? {:position "RB" :vorp 5.0})))
  (let [board [{:player-id "k1" :position "K"   :vorp 0.0}
               {:player-id "d1" :position "DST" :vorp 0.0}
               {:player-id "w1" :position "WR"  :vorp -3.0}]]
    (is (= #{"w1"} (value/min-bid-ids board 3))
        "three slots but only one player a dollar can attach to")))

(deftest minimum-bids-follow-each-position-s-share-of-the-roster
  ;; Below replacement every position's points curve has its own slope, so
  ;; ranking the tail globally by VORP bought the flattest curves: on the real
  ;; board it gave TE 27 of 96 minimums against 12 TE starters, pricing a TE at
  ;; ADP 251 while an RB at ADP 147 read as undraftable.
  (let [priced (concat (map (fn [i] {:player-id (str "rb" i) :position "RB" :vorp (double (- 60 i))})
                            (range 30))
                       (map (fn [i] {:player-id (str "te" i) :position "TE" :vorp (double (- 20 i))})
                            (range 10)))
        ;; TE's tail falls away gently, RB's steeply — the shape that used to
        ;; hand every minimum bid to TE.
        tail   (concat (map (fn [i] {:player-id (str "rbt" i) :position "RB"
                                     :vorp (double (* -5 (inc i)))}) (range 30))
                       (map (fn [i] {:player-id (str "tet" i) :position "TE"
                                     :vorp (double (- (inc i)))}) (range 30)))
        ids    (value/min-bid-ids (vec (concat priced tail)) 60)
        by-pos (frequencies (map #(if (.startsWith ^String % "rb") "RB" "TE") ids))]
    (is (= 20 (count ids)) "60 slots less the 40 already above replacement")
    (is (= 15 (get by-pos "RB")) "RB starts 30 of 40, so it takes 30/40 of the minimums")
    (is (= 5  (get by-pos "TE")) "and TE starts 10 of 40, so it takes a quarter")))

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
  (let [static (engine/static-rankings (synthetic-board) (:ppr scoring/presets) 12)]
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

(deftest the-expert-tier-does-not-override-the-computed-one
  ;; It used to. :fantasypros/ecr-tier covers most of the board but not all of
  ;; it, so adopting it left the rest untiered — and it is an *overall* tier, so
  ;; dense-ranking it alongside per-position cliff tiers merged two different
  ;; scales into one number. (It is format-scoped now, so "it ignores your
  ;; scoring" is no longer one of the reasons.) The expert tier still rides along
  ;; for the :fp-tier column.
  (let [board   (mapv (fn [p]
                        (if (= "QB" (:position p))
                          (assoc p :fantasypros/ecr-tier 9)
                          p))
                      (synthetic-board))
        static  (engine/static-rankings board (:ppr scoring/presets) 12)
        out-qbs (filter #(= "QB" (:position %)) (:players static))]
    (is (= 1 (reduce min (map :tier out-qbs)))
        "every position starts at tier 1, not at whatever FantasyPros said")
    (is (every? #(= 9 (:fantasypros/ecr-tier %)) out-qbs)
        "the expert tier is carried through untouched")
    (is (apply <= (map :tier (sort-by :points > out-qbs)))
        "tier only ever worsens as points fall")))

(deftest tiers-are-cut-from-points-so-they-follow-scoring
  (let [board (synthetic-board)
        tiers-under (fn [s]
                      (into {} (map (juxt :player-id :tier))
                            (:players (engine/static-rankings board s 12))))]
    (is (not= (tiers-under (:standard scoring/presets))
              (tiers-under (:ppr scoring/presets)))
        "reception scoring reshapes where the cliffs fall")))

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
