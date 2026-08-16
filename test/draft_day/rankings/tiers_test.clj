(ns draft-day.rankings.tiers-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.db]
            [draft-day.rankings.tcm :as tcm]
            [draft-day.rankings.tiers :as tiers]
            ;; registers the :ecr strategy
            [draft-day.rankings.tiers.ecr]))

(defn- frame [scores]
  (mapv (fn [i s] {:player-id (str "p" i) :points s}) (range (count scores)) scores))

(defn- tier-seq
  "tiers-by-cliffs already returns players sorted descending by points."
  ([scores] (tier-seq scores nil))
  ([scores level] (tier-seq scores level {}))
  ([scores level opts] (mapv :tier (tiers/tiers-by-cliffs (frame scores) level opts))))

(deftest empty-input
  (is (empty? (tiers/tiers-by-cliffs (frame []) nil))))

;; ---- cut-points: the selection rule on its own ----

(deftest cuts-land-on-the-biggest-gaps
  ;; gaps: 10, 2, 30, 2, 1. Two cuts requested -> the 30 first, then the 10.
  (is (= #{1 3} (set (tiers/cut-points [100 90 88 58 56 55] 3 1)))))

(deftest a-requested-count-is-produced-exactly
  (doseq [n [2 3 4 5]]
    (is (= (dec n) (count (tiers/cut-points (vec (range 100 0 -5)) n 2)))
        (str "asked for " n " tiers"))))

(deftest no-tier-is-smaller-than-the-minimum
  ;; The biggest gap here is 100 -> 40 at index 1, which would orphan the leader.
  ;; With a minimum of 2 that cut is refused and the next-biggest (39 -> 5, at
  ;; index 3) is taken instead, leaving tiers of 3 and 2.
  (is (= #{3} (set (tiers/cut-points [100 40 39 5 4] 2 2))))
  (testing "a pool too small to hold two full tiers is never cut"
    (is (empty? (tiers/cut-points [100 10 9] 8 2)))))

(deftest a-pool-too-small-for-the-count-yields-what-fits
  ;; Six players at a minimum of 2 cannot hold eight tiers; three is the ceiling,
  ;; and the result is three real tiers rather than singletons plus a remainder.
  (let [ts (tier-seq [100 90 60 55 20 18] nil {:tier-count 8})]
    (is (= 3 (reduce max ts)))
    (is (every? #(>= % tiers/MIN-TIER-SIZE) (vals (frequencies ts))))))

(deftest identical-scores-are-never-split
  ;; A zero gap is not a cliff, so it is never a candidate however badly the
  ;; requested count wants one.
  (is (= [1 1 1 2 2] (tier-seq [100 100 100 50 50] nil {:tier-count 4})))
  (is (= #{1} (set (tier-seq [100 100 100 100] nil {:tier-count 4})))))

(deftest the-count-is-a-target-not-a-consequence-of-the-shape
  ;; The deliberate reversal. Under the old relative-drop threshold a smooth
  ;; decline was "a slope, not tiers" and came back as a single tier. A configured
  ;; count cuts it anyway, which is what makes a tier number mean the same thing
  ;; at every position and in every scoring format.
  (testing "a smooth 2%-per-slot decline still gets the requested tiers"
    (is (= 3 (reduce max (tier-seq [100 98 96.04 94.12 92.24 90.4]
                                   nil {:tier-count 3})))))

  (testing "and the cuts still land on such cliffs as there are"
    (is (= [1 1 2 2 3 3] (tier-seq [100 99 80 79 60 59] nil {:tier-count 3})))))

(deftest gaps-are-ranked-absolutely-not-relatively
  ;; Reversed from the rule this replaced. A relative drop is measured against the
  ;; falling player, so it grows without bound as the metric decays toward zero
  ;; and drags nearly every cut into the tail. Here 100 -> 60 is the fall that
  ;; changes a draft, even though 8 -> 4 is by far the larger share.
  (is (= [1 1 2 2 2 2] (tier-seq [100 100 60 20 8 4] nil {:tier-count 2}))))

(deftest everything-at-or-below-replacement-shares-the-final-tier
  ;; Below replacement the only true statement is "worse than the player you can
  ;; have for $1", so the tail is one tier however lumpy it is. Only the four
  ;; above 50 are cut; 10 and 1 land together despite the gap between them.
  (let [tiers (tier-seq [100 99 60 55 10 1] 50 {:tier-count 2})]
    (is (= [1 1 2 2 3 3] tiers))
    (is (= 3 (reduce max tiers)) "the tail does not keep splitting")))

(deftest a-nil-replacement-level-tiers-the-whole-pool
  ;; K and DST are never priced, so they have no replacement level.
  (is (= [1 1 2 2] (tier-seq [100 95 50 20] nil {:tier-count 2}))))

(deftest nobody-above-replacement-is-a-single-tier
  (is (= #{1} (set (tier-seq [10 9 8] 100)))))

(deftest tiers-and-tcm-share-one-definition-of-a-fall
  (testing "relative-drop is still the board's shared notion, and tcm's input"
    (is (= 0.25 (tiers/relative-drop 100 75)))
    (is (= 0.0 (tiers/relative-drop 0 0)) "a zero-point player has nothing to fall from"))

  (testing "tcm keeps a threshold; tiering no longer has one to compare against"
    (is (pos? tcm/DROP-THRESHOLD))
    (is (not (contains? (ns-publics 'draft-day.rankings.tiers) 'DROP-THRESHOLD))
        "the paired constant is gone — tiering cuts a count, not a threshold")))

;; ---- the strategy seam ----

(defn- board
  "A small board with :vorp, :points, both expert-tier columns and a position."
  []
  [{:player-id "rb1" :position "RB" :points 300.0 :vorp 120.0
    :fantasypros/ecr-tier 1 :fantasypros/ecr-pos-tier 1}
   {:player-id "rb2" :position "RB" :points 290.0 :vorp 110.0
    :fantasypros/ecr-tier 3 :fantasypros/ecr-pos-tier 2}
   {:player-id "rb3" :position "RB" :points 200.0 :vorp 20.0
    :fantasypros/ecr-tier 3 :fantasypros/ecr-pos-tier 2}
   {:player-id "wr1" :position "WR" :points 280.0 :vorp 108.0
    :fantasypros/ecr-tier 1 :fantasypros/ecr-pos-tier 1}
   {:player-id "wr2" :position "WR" :points 180.0 :vorp 5.0
    :fantasypros/ecr-tier 7 :fantasypros/ecr-pos-tier 3}
   ;; below replacement, and unranked by FantasyPros on both scales
   {:player-id "wr3" :position "WR" :points 100.0 :vorp -20.0}])

(def ^:private ctx {:replacement-levels {"RB" 180.0 "WR" 175.0} :num-teams 12})

(deftest an-unknown-strategy-throws-with-the-registered-set
  (let [e (try (tiers/tier-board :nope ctx (board))
               (catch clojure.lang.ExceptionInfo e e))]
    (is (instance? clojure.lang.ExceptionInfo e))
    (is (re-find #"registered strategies are" (ex-message e))
        "the known set goes in the message, not only in ex-data")
    (is (= :nope (:strategy (ex-data e))))
    (is (= (tiers/registered) (:known (ex-data e))))))

(deftest registered-lists-every-strategy-and-never-default
  (require 'draft-day.rankings.tiers.ecr)
  (is (= [:cliffs :ecr] (tiers/registered)))
  (is (not-any? #{:default} (tiers/registered))))

(deftest the-seam-does-not-change-what-the-positional-tier-was
  ;; The pin that this whole change is additive: :cliffs/:position must equal
  ;; what `tiers-by-cliffs` produces per position, which is what the pipeline
  ;; computed before the seam existed.
  (let [b        (board)
        expected (into {}
                       (mapcat (fn [[pos grp]]
                                 (let [sorted (vec (sort-by :points > grp))]
                                   (map (juxt :player-id :tier)
                                        (tiers/tiers-by-cliffs
                                         sorted
                                         (tiers/tier-floor
                                          sorted (get (:replacement-levels ctx) pos) 12))))))
                       (group-by :position b))]
    (is (= expected (:position (tiers/tier-board :cliffs ctx b))))))

(deftest the-overall-scale-is-cut-on-vorp-not-points
  ;; wr1 scores 10 fewer points than rb2 but is worth 2 more VORP, because WR
  ;; replacement is lower. On points it would sit a rank below rb2; on VORP the
  ;; two are near-substitutes, which is the question an overall tier answers.
  (let [{:keys [overall]} (tiers/tier-board :cliffs ctx (board))]
    (is (= (get overall "wr1") (get overall "rb2"))
        "similar VORP lands in the same overall tier across positions")
    (is (< (get overall "rb1") (get overall "rb3"))
        "a far worse VORP lands in a worse overall tier")))

(deftest everything-at-or-below-replacement-shares-the-final-overall-tier
  (let [b   (into (board)
                  (map (fn [i] {:player-id (str "z" i) :position "WR"
                                :points 50.0 :vorp (- (double i))}))
                  (range 5))
        {:keys [overall]} (tiers/tier-board :cliffs ctx b)
        worst (apply max (vals overall))]
    (is (every? #(= worst (get overall (str "z" %))) (range 5))
        "negative VORP is one undifferentiated tail, whatever its spread")
    (is (= worst (get overall "wr3")))))

(deftest a-deep-tail-drop-is-not-a-cliff-but-the-same-fall-up-top-is
  ;; What keeps the overall scale from shattering. Relative drop alone calls
  ;; 1.0 -> 0.4 a 60% cliff, which this close to zero means nothing; ranking gaps
  ;; absolutely puts the cut where the VORP actually goes.
  (let [mk   (fn [id v] {:player-id id :position "RB" :points (* 2 v) :vorp v})
        deep (tiers/tier-board :cliffs ctx [(mk "a" 100.0) (mk "b" 99.0)
                                            (mk "c" 1.0) (mk "d" 0.4)])]
    (is (= (get-in deep [:overall "c"]) (get-in deep [:overall "d"]))
        "a 60% fall from 1.0 to 0.4 is not where the board breaks")
    (is (< (get-in deep [:overall "a"]) (get-in deep [:overall "c"]))
        "the 98-point fall above it is")))

(deftest the-overall-and-positional-scales-use-their-configured-counts
  ;; A board wide enough that neither scale is limited by MIN-TIER-SIZE, and with
  ;; every VORP above replacement so no tail tier is appended — the counts that
  ;; come back are the configured ones, not a feasibility cap.
  (let [mk    (fn [i] (let [v (/ 400.0 (inc (* i 0.05)))]
                        {:player-id (str "p" i) :position "RB" :points v :vorp v}))
        board (mapv mk (range 200))
        ctx*  {:replacement-levels {"RB" 0.0} :num-teams 12}
        {:keys [overall position]} (tiers/tier-board :cliffs ctx* board)]
    (is (= (:overall tiers/tier-counts) (reduce max (vals overall))))
    (is (= (:position tiers/tier-counts) (reduce max (vals position))))
    (testing "and no tier anywhere is a single player"
      (doseq [scale [overall position]]
        (is (every? #(>= % tiers/MIN-TIER-SIZE) (vals (frequencies (vals scale)))))))))

(deftest with-tiers-ships-every-strategy-and-aliases-tier
  (require 'draft-day.rankings.tiers.ecr)
  (let [out (tiers/with-tiers (board) ctx)]
    (doseq [p out]
      (is (= (:tier p) (get-in p [:tiers :cliffs :position]))
          ":tier is the default strategy's positional tier, always")
      (is (contains? (:tiers p) :cliffs))
      (is (contains? (:tiers p) :ecr)))))

(deftest the-catalog-and-the-registry-agree
  ;; db.cljc is on the JVM classpath, so lein test can hold the two halves of the
  ;; feature together. A catalog entry with no defmethod does not error — it
  ;; renders every player unranked, which is why this is asserted rather than
  ;; trusted.
  (require 'draft-day.rankings.tiers.ecr)
  (is (= (set (map :key draft-day.db/tier-strategy-catalog))
         (set (tiers/registered))))
  (is (= tiers/DEFAULT-STRATEGY draft-day.db/default-tier-strategy))
  (is (contains? draft-day.db/tier-strategies-by-key tiers/DEFAULT-STRATEGY)))
