(ns draft-day.benchmark.ros-backtest-test
  (:require [clojure.test :refer [deftest is]]
            [draft-day.benchmark.ros-backtest :as bt]))

(defn- row [id week team pos & {:keys [rec snaps]}]
  {:id id :week week :team team :position pos
   :stats (cond-> {} rec (assoc :rec rec))
   :snaps (double (or snaps 0)) :team-snaps 60.0})

(defn- weeks [id team pos from to & {:as m}]
  (mapv #(row id % team pos :rec (:rec m 4.0) :snaps (:snaps m 30)) (range from (inc to))))

(def ppr (:ppr bt/configs))

(deftest a-prediction-is-the-blends-own
  (let [p (bt/predict {:pre {:rec 170.0} :real {:games 4 :stats {:rec 40.0}}
                       :projected #{:rec} :k 6})]
    (is (< 9.99 (get p :ppr) 10.01) "(6 * 10 + 40) / 10 receptions a game at a point each")))

(deftest a-stronger-prior-moves-the-prediction-toward-the-preseason-line
  (let [at (fn [k] (get (bt/predict {:pre {:rec 170.0} :real {:games 4 :stats {:rec 80.0}}
                                     :projected #{:rec} :k k}) :ppr))]
    (is (< (at 1000) (at 6) (at 1)) "the line says 10 a game, the games say 20")))

(deftest takers-are-found-off-snaps-alone
  (let [rows (-> {}
                 (assoc "starter" (into (weeks "starter" "MIA" "RB" 1 2 :snaps 50) (weeks "starter" "MIA" "RB" 3 3 :snaps 3)))
                 (assoc "backup" (into (weeks "backup" "MIA" "RB" 1 2 :snaps 4) (weeks "backup" "MIA" "RB" 3 6 :snaps 40)))
                 (assoc "sub" (weeks "sub" "MIA" "RB" 1 6 :snaps 6)))
        rows (update-vals rows (fn [rs] (filterv #(<= (:week %) 6) rs)))
        t    (bt/takers rows 6)]
    (is (= {"backup" 3} t) "the man who gained the snaps, from the week it changed")))

(deftest a-vacancy-needs-two-weeks-not-one
  (let [rows {"starter" (into (weeks "starter" "MIA" "RB" 1 2 :snaps 50) (weeks "starter" "MIA" "RB" 3 3 :snaps 3))
              "backup"  (into (weeks "backup" "MIA" "RB" 1 2 :snaps 4) (weeks "backup" "MIA" "RB" 3 3 :snaps 50))}]
    (is (empty? (bt/takers rows 3)) "the change is the as-of week itself: one game is not a role")))

(deftest the-bootstrap-is-reproducible-and-centred-on-the-observed-change
  (let [asks (for [i (range 40)] {:season 2023 :id i :games 2})
        f    (fn [_] -1.0)
        a    (bt/bootstrap asks f 80.0 200)
        b    (bt/bootstrap asks f 80.0 200)]
    (is (= a b))
    (is (< -0.501 (:delta a) -0.499) "forty asks each 1 better, spread over 80 games")))

(defn- qb-asks [n]
  (for [i (range n)]
    {:season 2023 :as-of 5 :id i :position "QB"
     :actual {:ppr (double i)} :pred {6 {:ppr (double i)}} :games 3}))

(deftest hit-rate-counts-the-overlap-of-the-two-top-groups
  (let [asks (qb-asks 24)
        n    {[2023 5] {"QB" 12}}]
    (is (= 1.0 (bt/hit-rate asks :ppr 6 n)) "a perfect ranking")
    (is (= 0.0 (bt/hit-rate (map #(update-in % [:pred 6 :ppr] -) asks) :ppr 6 n)) "an inverted one")))

(deftest the-top-n-follows-the-league-size
  (let [asks (map #(cond-> % (= 12 (:id %)) (assoc-in [:pred 6 :ppr] 10.5)) (qb-asks 24))
        at   (fn [teams] (bt/hit-rate asks :ppr 6 {[2023 5] {"QB" teams}}))]
    (is (< (at 12) 1.0) "one player just inside the actual twelve is predicted just outside")
    (is (= 1.0 (at 6)) "but the top six are")
    (is (nil? (at 13)) "a group smaller than twice the starters is not asked")))

(deftest starters-are-the-dedicated-seats-times-the-teams-plus-flex
  (let [group (concat (for [i (range 30)] {:position "RB" :actual {:ppr (+ 10.0 i)}})
                      (for [i (range 30)] {:position "WR" :actual {:ppr (+ 10.0 i)}})
                      (for [i (range 12)] {:position "QB" :actual {:ppr (+ 10.0 i)}}))
        s12  (bt/starters group :ppr 12 {:qb 1 :rb 2 :wr 2 :te 1 :flex 1 :k 1 :dst 1})
        s10  (bt/starters group :ppr 10 {:qb 1 :rb 2 :wr 2 :te 1 :flex 1 :k 1 :dst 1})]
    (is (= 12 (get s12 "QB")))
    (is (= 10 (get s10 "QB")))
    (is (= 12 (get s12 "K")))
    (is (= 10 (get s10 "K")))
    (is (= 12 (+ (- (get s12 "RB") 24) (- (get s12 "WR") 24))) "twelve flex seats, split between RB and WR here")
    (is (= 10 (+ (- (get s10 "RB") 20) (- (get s10 "WR") 20))))))
