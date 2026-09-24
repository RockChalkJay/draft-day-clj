(ns draft-day.faab.planted-test
  "The report run on a synthetic corpus whose answers are planted, so each
  measurement is checked against a known truth rather than against itself.

  Prices depend on the number of bidders, the position and the manager, and on
  nothing else. Dynasty and superflex leagues draw more bidders but pay the
  same within a bidder count, so a shift there is the report failing to
  condition on bidders. Half the leagues run a $300 budget and bid the same
  dollars, which is a third of the share."
  (:require [clojure.test :refer [deftest is]]
            [draft-day.faab.corpus :as corpus]
            [draft-day.faab.report :as report])
  (:import (java.util ArrayList Collections Random)))

(def base-dollars
  "The planted median positive bid by bidder bucket, in dollars."
  {1 10 2 20 3 30 4 40})

(def zero-rate {1 0.6 2 0.4 3 0.3 4 0.15})

(def pos-mult {"QB" 1.5 "K" 0.4 "RB" 1.0 "WR" 1.0 "TE" 1.0})

(def positions
  (into {} (map (fn [i] [(str "p" i) (["QB" "K" "RB" "RB" "RB" "WR" "WR" "WR" "TE" "TE"] (mod i 10))]))
        (range 1000)))

(def rho "Planted correlation of a manager's aggression across seasons." 0.6)
(def aggression-sd 0.4)
(def noise-sd 0.3)

(def light [[1 0.55] [2 0.2] [3 0.1] [4 0.1] [5 0.03] [6 0.02]])
(def heavy [[1 0.35] [2 0.2] [3 0.15] [4 0.2] [5 0.05] [6 0.05]])

(defn draw [^Random rng weighted]
  (let [u (.nextDouble rng)]
    (loop [[[v w] & more] weighted acc 0.0]
      (if (or (nil? more) (< u (+ acc w))) v (recur more (+ acc w))))))

(defn shuffled [^Random rng xs]
  (let [l (ArrayList. ^java.util.Collection xs)]
    (Collections/shuffle l rng)
    (vec l)))

(defn bid-amount [^Random rng budget bucket pos a]
  (if (< (.nextDouble rng) (zero-rate bucket))
    0
    (-> (* (base-dollars bucket) (pos-mult pos) (Math/exp (+ a (* noise-sd (.nextGaussian rng)))))
        Math/round
        (max 1)
        (min budget))))

(defn gen-season
  "One league-season of `n-auctions`, bid by `aggression` `{owner a}`."
  [^Random rng {:keys [league-id previous budget kind superflex? mix aggression season]} n-auctions]
  (let [owners (vec (sort (keys aggression)))]
    {:meta   {:league-id league-id :season season :budget budget :num-teams (count owners)
              :kind kind :superflex? superflex? :previous-league-id previous}
     :season {:auctions
              (vec (repeatedly
                    n-auctions
                    (fn []
                      (let [week    (inc (.nextInt rng 17))
                            player  (str "p" (.nextInt rng 1000))
                            n       (draw rng mix)
                            bidders (take n (shuffled rng (range (count owners))))
                            bucket  (corpus/bucket n)
                            bids    (mapv (fn [i]
                                            (let [o (owners i)]
                                              {:owner-id o :roster-id (inc i) :tie (.nextDouble rng)
                                               :amount (bid-amount rng budget bucket (positions player)
                                                                   (aggression o))}))
                                          bidders)
                            winner  (apply max-key #(+ (:amount %) (:tie %)) bids)]
                        {:week week :at week :player-id player
                         :bids (mapv #(-> % (assoc :won? (= % winner)) (dissoc :tie)) bids)}))))}}))

(defn gen-corpus
  "`chains` leagues of two linked seasons each, with the planted aggression of
  every manager-season as `{[league-id owner] a}`."
  [seed chains n-auctions]
  (let [rng (Random. seed)
        g   #(* aggression-sd (.nextGaussian rng))]
    (reduce
     (fn [acc i]
       (let [dynasty?   (even? i)
             superflex? (even? (quot i 2))
             budget     (if (even? (quot i 4)) 100 300)
             owners     (map #(str "c" i "-m" %) (range 12))
             a1         (zipmap owners (repeatedly 12 g))
             a2         (update-vals a1 #(+ (* rho %) (* (Math/sqrt (- 1 (* rho rho))) (g))))
             spec       {:budget budget :kind (if dynasty? :dynasty :redraft) :superflex? superflex?
                         :mix (if (or dynasty? superflex?) heavy light)}
             id1        (str "L" i "-2023")
             id2        (str "L" i "-2024")
             s1         (gen-season rng (assoc spec :league-id id1 :season "2023" :aggression a1) n-auctions)
             s2         (gen-season rng (assoc spec :league-id id2 :season "2024" :previous id1
                                               :aggression a2) n-auctions)]
         (-> acc
             (update :seasons conj s1 s2)
             (update :planted merge
                     (update-keys a1 #(vector id1 %))
                     (update-keys a2 #(vector id2 %)))
             (update :budget-of assoc id1 budget id2 budget))))
     {:seasons [] :planted {} :budget-of {}}
     (range chains))))

(def measured
  (delay
    (let [{:keys [seasons] :as c} (gen-corpus 42 32 400)
          rows     (corpus/corpus-rows seasons positions)
          bids     (corpus/bid-rows rows)
          managers (report/manager-habits rows bids)]
      (assoc c :managers (filter #(>= (:bids %) 10) managers)
             :backbone (report/backbone bids managers)))))

(defn near? [tol expected actual] (and actual (<= (Math/abs (- expected actual)) tol)))

(deftest each-cell-recovers-its-planted-zero-rate-and-median
  ;; the QB and K mix pulls a cell's median just under its base, so a dollar
  ;; either side of $10 — ln(0.9) — is the grain rather than a bias
  (doseq [[[b ph] {:keys [p-zero positive]}] (:bid-share (:backbone @measured))]
    (is (near? 0.04 (zero-rate b) p-zero) (str "$0 rate, " b " bidders " ph))
    (is (near? 0.11 0.0 (Math/log (/ (positive 0.5) (/ (base-dollars b) 100.0))))
        (str "median positive share, " b " bidders " ph))))

(deftest position-shifts-recover-the-planted-multipliers
  (let [s (:position (:backbone @measured))]
    (is (near? 0.05 0.0 (get-in s ["WR" :log-shift])))
    (is (near? 0.05 (Math/log 1.5) (get-in s ["QB" :log-shift])))
    (is (near? 0.05 (Math/log 0.4) (get-in s ["K" :log-shift])))))

(deftest a-league-type-that-only-draws-more-bidders-shifts-nothing
  (doseq [k [:kind :superflex]
          [v {:keys [log-shift]}] (k (:backbone @measured))]
    (is (near? 0.05 0.0 log-shift) (str k " " v))))

(deftest the-budget-shift-recovers-the-planted-third
  (doseq [[b {:keys [log-shift]}] (:budget-shift (:backbone @measured))]
    (is (near? 0.05 (Math/log (/ 1 3.0)) log-shift) (str b " bidders"))))

(deftest managers-are-measured-in-100-dollar-leagues-against-their-own-scale
  (let [{:keys [managers budget-of]} @measured]
    (is (= #{100} (set (map #(budget-of (:league-id %)) managers)))
        "a $300 league's managers would carry its budget as a habit")
    (is (near? 0.1 0.0 (report/quantile (keep :aggression managers) 0.5)))))

(deftest aggression-ranks-managers-as-planted
  (let [{:keys [managers planted]} @measured]
    (is (< 0.9 (report/spearman (keep #(when (:aggression %)
                                         [(:aggression %) (planted [(:league-id %) (:owner-id %)])])
                                      managers))))))

(deftest season-persistence-recovers-the-planted-correlation
  (let [{:keys [managers planted backbone]} @measured
        truth (report/spearman (map (fn [[a b]] [(planted [(:league-id a) (:owner-id a)])
                                                 (planted [(:league-id b) (:owner-id b)])])
                                    (report/season-pairs managers)))]
    (is (near? 0.1 truth (get-in backbone [:persistence :seasons :aggression])))))
