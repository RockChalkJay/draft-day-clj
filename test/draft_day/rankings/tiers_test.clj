(ns draft-day.rankings.tiers-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.rankings.tcm :as tcm]
            [draft-day.rankings.tiers :as tiers]))

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

(deftest identical-scores-are-never-split
  ;; A zero gap is not a cliff, so it is never a candidate however badly the
  ;; requested count wants one.
  (is (= [1 1 1 2 2] (tier-seq [100 100 100 50 50] nil {:target-size 2})))
  (is (= #{1} (set (tier-seq [100 100 100 100] nil {:target-size 1})))))

(deftest gaps-are-ranked-absolutely-not-relatively
  ;; Reversed from the rule this replaced. A relative drop is measured against the
  ;; falling player, so it grows without bound as the metric decays toward zero
  ;; and drags nearly every cut into the tail. Here 100 -> 60 is the fall that
  ;; changes a draft, even though 8 -> 4 is by far the larger share.
  (is (= [1 1 2 2 2 2] (tier-seq [100 100 60 20 8 4] nil {:target-size 3}))))

;; ---- tier-count: the count follows the pool's depth ----

(deftest the-count-follows-pool-depth-at-a-fixed-tier-size
  (testing "a target size of 4 asks for one tier per four players"
    (is (= 3 (tiers/tier-count 12 4)))
    (is (= 8 (tiers/tier-count 30 4)))
    (is (= 7 (tiers/tier-count 84 12))))

  (testing "measured on the sample at 12 teams: RB/WR are 30 deep and QB/TE 12,
            so the deeper positions get more tiers off the same constant"
    (is (> (tiers/tier-count 30 (:position tiers/TARGET-TIER-SIZE))
           (tiers/tier-count 12 (:position tiers/TARGET-TIER-SIZE)))))

  (testing "clamped at both ends"
    (is (= 2 (tiers/tier-count 1 4)) "a pool worth tiering has a top and a bottom")
    (is (= tiers/MAX-TIERS (tiers/tier-count 10000 4)))))

(deftest a-deeper-pool-really-does-come-back-with-more-tiers
  ;; End to end through tiers-by-cliffs, not just the arithmetic: same shape of
  ;; decline, different depth.
  (let [decline (fn [n] (mapv #(- 400.0 (* 3.0 %)) (range n)))
        tiers-of (fn [n] (reduce max (tier-seq (decline n))))]
    (is (< (tiers-of 12) (tiers-of 30)))
    (is (= 3 (tiers-of 12)))
    (is (= 8 (tiers-of 30)))))

(deftest no-tier-anywhere-is-a-single-player
  (doseq [n (range 4 40)]
    (let [sizes (vals (frequencies (tier-seq (mapv #(- 400.0 (* % %)) (range n)))))]
      (is (every? #(>= % tiers/MIN-TIER-SIZE) sizes)
          (str "pool of " n " produced sizes " (vec sizes))))))

(deftest the-count-is-a-target-not-a-consequence-of-the-shape
  ;; The deliberate reversal. Under the old relative-drop threshold a smooth
  ;; decline was "a slope, not tiers" and came back as a single tier. A pool-sized
  ;; count cuts it anyway, which is what makes a tier number mean the same thing
  ;; at every position and in every scoring format.
  (testing "a smooth 2%-per-slot decline still gets its tiers"
    (is (= 2 (reduce max (tier-seq [100 98 96.04 94.12 92.24 90.4]
                                   nil {:target-size 3})))))

  (testing "and the cuts still land on such cliffs as there are"
    (is (= [1 1 2 2 3 3] (tier-seq [100 99 80 79 60 59] nil {:target-size 2})))))

;; ---- the replacement tail ----

(deftest everything-at-or-below-replacement-shares-the-final-tier
  ;; Below replacement the only true statement is "worse than the player you can
  ;; have for $1", so the tail is one tier however lumpy it is. Only the four
  ;; above 50 are cut; 10 and 1 land together despite the gap between them.
  (let [tiers (tier-seq [100 99 60 55 10 1] 50 {:target-size 2})]
    (is (= [1 1 2 2 3 3] tiers))
    (is (= 3 (reduce max tiers)) "the tail does not keep splitting")))

(deftest a-nil-replacement-level-tiers-the-whole-pool
  (is (= [1 1 2 2] (tier-seq [100 95 50 20] nil {:target-size 2}))))

(deftest nobody-above-replacement-is-a-single-tier
  (is (= #{1} (set (tier-seq [10 9 8] 100)))))

(deftest unpriced-positions-still-get-a-tier-floor
  ;; K and DST are absent from the replacement-levels map on purpose (it is what
  ;; makes them price at $0), but tiering their whole pool spends every tier on
  ;; 44 kickers nobody drafts. One of each starts, so the num-teams-th best is
  ;; the floor.
  ;; k0..k29 at 300, 291, 282, … (300 - 9i) — descending, which is the order
  ;; `tier-floor` requires now that the caller sorts the group once for both it
  ;; and `tiers-by-cliffs`.
  (let [ks (mapv (fn [i] {:player-id (str "k" i) :position "K" :points (- 300.0 (* i 9))})
                 (range 30))]
    (is (= 255.0 (tiers/tier-floor ks nil 5)) "the 6th best, matching a 1-starter position")
    (is (= 39.0 (tiers/tier-floor ks nil 100)) "clamps to the worst when the pool is short")
    (is (= 42.0 (tiers/tier-floor ks 42.0 5)) "a real replacement level always wins")))

;; ---- the two scales ----

(defn- board
  "A small board with :vorp, :points and a position."
  []
  [{:player-id "rb1" :position "RB" :points 300.0 :vorp 120.0}
   {:player-id "rb2" :position "RB" :points 290.0 :vorp 110.0}
   {:player-id "rb3" :position "RB" :points 200.0 :vorp 20.0}
   {:player-id "rb4" :position "RB" :points 195.0 :vorp 15.0}
   {:player-id "wr1" :position "WR" :points 280.0 :vorp 108.0}
   {:player-id "wr2" :position "WR" :points 180.0 :vorp 5.0}
   ;; below replacement at both scales
   {:player-id "wr3" :position "WR" :points 100.0 :vorp -20.0}])

(def ^:private ctx {:replacement-levels {"RB" 180.0 "WR" 175.0} :num-teams 12})

(deftest tier-is-still-the-positional-tier
  ;; The pin that this change is additive where it has to be: :tier must equal
  ;; what `tiers-by-cliffs` produces per position, which is what the pipeline
  ;; wrote before the overall scale existed.
  (let [out      (tiers/with-tiers (board) ctx)
        expected (into {}
                       (mapcat (fn [[pos grp]]
                                 (let [sorted (vec (sort-by :points > grp))]
                                   (map (juxt :player-id :tier)
                                        (tiers/tiers-by-cliffs
                                         sorted
                                         (tiers/tier-floor
                                          sorted (get (:replacement-levels ctx) pos) 12))))))
                       (group-by :position (board)))]
    (doseq [p out]
      (is (= (get expected (:player-id p)) (:tier p)))
      (is (= (:tier p) (get-in p [:tiers :position]))
          ":tier is the flat alias for the positional scale, always")
      (is (some? (get-in p [:tiers :overall]))
          "every player has an overall tier — there is no untiered bucket"))))

(deftest the-overall-scale-is-cut-on-vorp-not-points
  ;; wr1 scores 10 fewer points than rb2 but is worth 2 less VORP, because WR
  ;; replacement is lower. On points the two are a rank apart; on VORP they are
  ;; near-substitutes, which is the question an overall tier answers.
  (let [by-id (into {} (map (juxt :player-id identity)) (tiers/with-tiers (board) ctx))
        ov    #(get-in by-id [% :tiers :overall])]
    (is (= (ov "wr1") (ov "rb2"))
        "similar VORP lands in the same overall tier across positions")
    (is (< (ov "rb1") (ov "rb3"))
        "a far worse VORP lands in a worse overall tier")))

(deftest everything-at-or-below-replacement-shares-the-final-overall-tier
  (let [b     (into (board)
                    (map (fn [i] {:player-id (str "z" i) :position "WR"
                                  :points 50.0 :vorp (- (double i))}))
                    (range 5))
        by-id (into {} (map (juxt :player-id identity)) (tiers/with-tiers b ctx))
        ov    #(get-in by-id [% :tiers :overall])
        worst (apply max (map #(get-in % [:tiers :overall]) (vals by-id)))]
    (is (every? #(= worst (ov (str "z" %))) (range 5))
        "negative VORP is one undifferentiated tail, whatever its spread")
    (is (= worst (ov "wr3")))))

(deftest a-deep-tail-drop-is-not-a-cliff-but-the-same-fall-up-top-is
  ;; What keeps the overall scale from shattering. Relative drop alone calls
  ;; 1.0 -> 0.4 a 60% cliff, which this close to zero means nothing; ranking gaps
  ;; absolutely puts the cut where the VORP actually goes.
  (let [mk    (fn [id v] {:player-id id :position "RB" :points (* 2 v) :vorp v})
        by-id (into {} (map (juxt :player-id identity))
                    (tiers/with-tiers [(mk "a" 100.0) (mk "b" 99.0)
                                       (mk "c" 1.0) (mk "d" 0.4)]
                                      ctx))
        ov    #(get-in by-id [% :tiers :overall])]
    (is (= (ov "c") (ov "d"))
        "a 60% fall from 1.0 to 0.4 is not where the board breaks")
    (is (< (ov "a") (ov "c")) "the 98-point fall above it is")))

(deftest tiers-and-tcm-share-one-definition-of-a-fall
  (testing "relative-drop is still the board's shared notion, and tcm's input"
    (is (= 0.25 (tiers/relative-drop 100 75)))
    (is (= 0.0 (tiers/relative-drop 0 0)) "a zero-point player has nothing to fall from"))

  (testing "tcm keeps a threshold; tiering no longer has one to compare against"
    (is (pos? tcm/DROP-THRESHOLD))
    (is (not (contains? (ns-publics 'draft-day.rankings.tiers) 'DROP-THRESHOLD))
        "the paired constant is gone — tiering cuts a count, not a threshold")))
