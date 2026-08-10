(ns draft-day.rankings.model-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.rankings.model :as model]
            [draft-day.rankings.model.blend]      ; register the candidate models
            [draft-day.rankings.scoring :as scoring]))

(defn wr [id yards adp]
  {:player-id id :position "WR" :stats {:rec_yd yards} :adp adp})

(def scoring-cfg {:rec_yd 0.1})

(deftest points-model-is-exactly-the-previous-behaviour
  ;; The seam must be a no-op for the shipped default, or every existing test and
  ;; every persisted board silently changes meaning.
  (let [board [(wr "a" 1400 90) (wr "b" 1200 3)]]
    (is (= (scoring/with-points board scoring-cfg)
           (model/score-board :points {:scoring scoring-cfg} board)))))

(deftest unknown-model-throws-with-the-known-set
  (let [e (try (model/score-board :nope {:scoring scoring-cfg} [])
               (catch clojure.lang.ExceptionInfo e e))]
    (is (some? e))
    (is (= :nope (:model (ex-data e))))
    (is (contains? (set (:known (ex-data e))) :points))))

(deftest registered-lists-models-without-default
  (let [r (set (model/registered))]
    (is (contains? r :points))
    (is (contains? r :points+adp))
    (is (not (contains? r :default)))))

(deftest zscores-of-a-constant-column-are-zero-not-nan
  ;; A signal that is flat across the pool must contribute nothing to a blend
  ;; rather than dividing by a zero standard deviation.
  (is (= [0.0 0.0 0.0] (model/zscores [5 5 5])))
  (is (= [0.0] (model/zscores [7]))))

(deftest rank-transfer-preserves-the-points-multiset
  ;; This is what keeps a blended model safe downstream: floor/ceiling multiplies
  ;; points by (1 +/- band) and the dollar split divides by total VORP, neither of
  ;; which tolerates an arbitrary z-scale or negatives. Re-ordering must move
  ;; players between existing point values, never invent new ones.
  (let [group [{:player-id "a" :points 100.0} {:player-id "b" :points 90.0}
               {:player-id "c" :points 80.0}]
        ;; score by reverse of points -> exact reversal
        out   (model/rank-transfer group #(- (:points %)))]
    (is (= [100.0 90.0 80.0] (mapv :points out)))
    (is (= ["c" "b" "a"] (mapv :player-id out)))
    (is (= (sort (map :points group)) (sort (map :points out))))))

(deftest adp-blend-reorders-when-consensus-disagrees
  ;; WR weights are consensus-heavy (0.4 points / 0.6 adp), so an ADP of 3
  ;; against 90 should overturn a 200-yard projection gap.
  (let [board [(wr "a" 1400 90) (wr "b" 1200 3) (wr "c" 600 40)]
        out   (model/score-board :points+adp {:scoring scoring-cfg} board)
        by-id (into {} (map (juxt :player-id :points)) out)]
    (is (= 140.0 (by-id "b")) "b takes the top points slot")
    (is (= 120.0 (by-id "a")))
    (is (= 60.0  (by-id "c")))))

(deftest adp-blend-falls-back-to-points-order-without-adp
  ;; Live boards can lack ADP entirely; the model must degrade to the baseline
  ;; rather than ranking on noise.
  (let [board (mapv #(dissoc % :adp) [(wr "a" 1400 90) (wr "b" 1200 3) (wr "c" 600 40)])
        out   (model/score-board :points+adp {:scoring scoring-cfg} board)]
    (is (= [140.0 120.0 60.0] (mapv :points (sort-by #(- (:points %)) out))))
    (is (= ["a" "b" "c"] (mapv :player-id (sort-by #(- (:points %)) out))))))

(deftest blend-reads-sleeper-adp-when-canonical-adp-absent
  ;; The live pipeline populates :sleeper/adp; the benchmark supplies :adp from a
  ;; deeper vintage source. Both must work.
  (let [board [{:player-id "a" :position "WR" :stats {:rec_yd 1400} :sleeper/adp 90}
               {:player-id "b" :position "WR" :stats {:rec_yd 1200} :sleeper/adp 3}
               {:player-id "c" :position "WR" :stats {:rec_yd 600}  :sleeper/adp 40}]
        by-id (into {} (map (juxt :player-id :points))
                    (model/score-board :points+adp {:scoring scoring-cfg} board))]
    (is (= 140.0 (by-id "b")))))

(deftest weights-can-be-overridden-per-call
  ;; The harness sweeps weights; pinning them in the model would make that
  ;; impossible. All-points weights must reproduce the baseline ordering.
  (let [board [(wr "a" 1400 90) (wr "b" 1200 3)]
        out   (model/score-board :points+adp
                                 {:scoring scoring-cfg
                                  :weights {:default {:points 1.0 :adp 0.0}}}
                                 board)]
    (is (= ["a" "b"] (mapv :player-id (sort-by #(- (:points %)) out))))))

(deftest blend-preserves-every-player
  (testing "no player is dropped or duplicated across positions"
    (let [board [(wr "a" 1400 90) (wr "b" 1200 3)
                 {:player-id "r" :position "RB" :stats {:rush_yd 900} :adp 10}
                 {:player-id "q" :position "QB" :stats {:pass_yd 4000} :adp 50}]
          out   (model/score-board :points+adp {:scoring {:rec_yd 0.1 :rush_yd 0.1 :pass_yd 0.04}} board)]
      (is (= 4 (count out)))
      (is (= #{"a" "b" "r" "q"} (set (map :player-id out)))))))
