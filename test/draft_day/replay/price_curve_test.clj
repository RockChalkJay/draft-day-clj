(ns draft-day.replay.price-curve-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.replay.price-curve :as pc]))

(defn- draft
  ([prices] (draft prices {}))
  ([prices overrides]
   (merge {:num-teams 2 :budget 100
           :picks (mapv (fn [p] {:price (double p)}) prices)}
          overrides)))

(defn- close? [a b] (< (Math/abs (- (double a) (double b))) 1e-9))
(defn- total [xs] (reduce + 0.0 xs))

;; ---- normalization ----------------------------------------------------------

(deftest prices-become-shares-of-the-room
  (let [s (pc/shares (draft [50 25]))]
    (is (= [0.25 0.125] s) "a $50 buy in a $200 room is a quarter of the money")))

(deftest shares-are-sorted-by-price-not-nomination-order
  (is (= [0.25 0.125 0.05] (pc/shares (draft [10 50 25])))
      "the curve answers what the most expensive player costs"))

(deftest rooms-of-different-sizes-become-comparable
  ;; The corpus is deliberately heterogeneous — 8 to 14 teams, $100 to $400
  ;; budgets — and a curve that could not compare them could only be built from
  ;; the subset that happened to match.
  (let [small (draft [50 25]   {:num-teams 2  :budget 100})
        big   (draft [700 350] {:num-teams 14 :budget 200})]
    (is (= (pc/shares small) (pc/shares big))
        "same shape, different money: identical shares")))

(deftest a-room-with-no-money-yields-no-curve
  (is (= [] (pc/shares (draft [10] {:budget 0}))))
  (is (= [] (pc/shares (draft [] {})))))

;; ---- resampling -------------------------------------------------------------

(deftest resampling-conserves-the-money
  ;; The property the whole namespace rests on. Interpolating shares directly
  ;; would report a plausible-looking curve while silently dropping half the
  ;; pool whenever the grid is coarser than the draft — 280 real picks onto a
  ;; 144-pick simulation. Interpolating the cumulative and differencing cannot.
  (let [s [0.4 0.3 0.2 0.1]]
    (doseq [n [1 2 3 4 7 8 40]]
      (is (close? 1.0 (total (pc/resample s n)))
          (str "total preserved onto a grid of " n)))))

(deftest resampling-onto-its-own-length-changes-nothing
  (let [s [0.4 0.3 0.2 0.1]]
    (is (every? true? (map close? s (pc/resample s 4))))))

(deftest resampling-keeps-the-curve-descending
  ;; A bidder reads this as "rank 1 costs more than rank 2"; an interpolation
  ;; that inverted anywhere would price a worse player higher.
  (let [out (pc/resample [0.4 0.3 0.2 0.1] 9)]
    (is (apply >= out))))

(deftest a-coarser-grid-pools-adjacent-ranks
  ;; Two output buckets over four equal ranks: each takes exactly half.
  (is (every? true? (map close? [0.5 0.5] (pc/resample [0.25 0.25 0.25 0.25] 2)))))

(deftest resampling-degenerate-input-is-not-an-error
  (is (= [0.0 0.0] (pc/resample [] 2)))
  (is (= [] (pc/resample [0.5] 0))))

;; ---- the curve --------------------------------------------------------------

(deftest the-curve-averages-across-rooms
  (let [c (pc/curve [(draft [100 100]) (draft [200 0])] 2)]
    (is (= 2 (:n-drafts c)))
    (is (close? 0.75 (first (:shares c)))  "mean of 0.5 and 1.0")
    (is (close? 0.25 (second (:shares c))) "mean of 0.5 and 0.0")))

(deftest the-curve-keeps-what-the-room-did-not-spend
  ;; Deliberately not normalized to 1. Real rooms finish with money on the table,
  ;; and a simulated field that spent every dollar would clear prices no real
  ;; auction clears.
  (let [c (pc/curve [(draft [100 60])] 2)]      ; $160 of a $200 room
    (is (close? 0.8 (:spend-share c)))
    (is (close? 0.8 (total (:shares c))) "the shares carry the shortfall too")))

(deftest an-empty-corpus-yields-a-flat-zero-curve-not-a-crash
  (let [c (pc/curve [] 3)]
    (is (= [0.0 0.0 0.0] (:shares c)))
    (is (zero? (:n-drafts c)))))

;; ---- dollars ----------------------------------------------------------------

(deftest clearing-prices-are-whole-dollars-floored-at-one
  (let [c {:shares [0.5 0.25 0.0000001]}]
    (is (= [1000 500 1] (pc/clearing-prices c 2000))
        "the deep tail floors at $1 rather than handing the model free players")))

;; ---- corpus slicing ---------------------------------------------------------

(deftest superflex-rooms-are-separable
  ;; The benchmark simulates a single-quarterback lineup, and a superflex room
  ;; spends a different share of its pool on quarterbacks. Only possible to
  ;; separate because the corpus records league type per draft.
  (let [ds [(draft [10] {:superflex? true}) (draft [10]) (draft [10] {:superflex? false})]]
    (is (= 2 (count (pc/standard-drafts ds))))))

;; ---- the pairing a consumer should use --------------------------------------

(deftest for-picks-builds-the-grid-at-the-callers-own-pick-count
  ;; So rank fraction and absolute rank coincide, which is the thing a caller
  ;; would otherwise have to remember and would eventually get wrong.
  (let [c (pc/for-picks [(draft [100 60 40])] 3)]
    (is (= 3 (count (:shares c))))
    (is (= 1 (:n-drafts c)))))

(deftest an-empty-corpus-is-loud-rather-than-free-players
  ;; A curve of zeros floors every clearing price at $1, so the simulated field
  ;; bids nothing and the model seat wins the draft for pocket change — reported
  ;; as a spectacular edge rather than as a missing cache.
  (is (thrown? clojure.lang.ExceptionInfo (pc/for-picks [] 12)))
  (is (thrown? clojure.lang.ExceptionInfo
               (pc/for-picks [(draft [10] {:budget 0})] 12))
      "drafts that yield no shares count as empty too"))
