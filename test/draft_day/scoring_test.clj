(ns draft-day.scoring-test
  (:require [clojure.test :refer [deftest is]]
            [draft-day.scoring :as scoring]))

(deftest presets-has-exactly-three-keys
  (is (= #{:standard :half-ppr :ppr} (set (keys scoring/presets)))))

(deftest player-points-defaults-missing-stats-to-zero
  (is (= 0.0 (scoring/player-points {:stats {}} (:ppr scoring/presets)))))

(deftest player-points-ignores-unknown-stat-keys
  (let [player {:stats {:rec 10.0 :some-unknown-stat 999.0}}]
    (is (= 10.0 (scoring/player-points player (:ppr scoring/presets))))))

(deftest every-stat-key-actually-scores
  ;; 13 of the 21 keys had no test that they move :points at all — every kicking
  ;; and every team-defense key among them. A key that drifted from Sleeper's
  ;; spelling would score silently as zero, which is indistinguishable on the
  ;; board from a player who simply does not accumulate that stat.
  (let [all-ones (zipmap scoring/stat-keys (repeat 1.0))]
    (doseq [k scoring/stat-keys]
      (is (= 3.0 (scoring/player-points {:stats {k 3.0}} all-ones))
          (str k " does not reach :points")))))

(deftest every-preset-weight-is-applied-as-written
  (doseq [[fmt preset] scoring/presets
          [k w] preset
          :when (not (zero? w))]
    (is (= (* 2.0 w) (scoring/player-points {:stats {k 2.0}} preset))
        (str fmt " " k))))

(deftest only-the-reception-weight-separates-the-presets
  (doseq [k (remove #{:rec} scoring/stat-keys)]
    (is (apply = (map #(get % k) (vals scoring/presets)))
        (str k " differs between presets"))))

(deftest stat-keys-covers-every-preset-key
  (is (= (set scoring/stat-keys) (set (keys (:ppr scoring/presets)))))
  (is (every? (set scoring/stat-keys) [:pass_2pt :rush_2pt :rec_2pt :blk_kick])))

;; ---- format classification (which vendor variant a league reads) ----

(deftest presets-map-to-their-own-published-format
  (is (= :standard (scoring/format-of (:standard scoring/presets))))
  (is (= :half-ppr (scoring/format-of (:half-ppr scoring/presets))))
  (is (= :ppr (scoring/format-of (:ppr scoring/presets)))))

(deftest a-custom-config-lands-on-the-nearest-published-format
  ;; Vendors publish ECR, auction values and ADP for exactly three formats, and a
  ;; custom config names none of them. Receptions are what separate the three.
  (let [with-rec (fn [r] (scoring/format-of (assoc (:ppr scoring/presets) :rec r)))]
    (is (= :standard (with-rec 0.0)))
    (is (= :standard (with-rec 0.2)))
    (is (= :half-ppr (with-rec 0.4)))
    (is (= :half-ppr (with-rec 0.5)))
    (is (= :ppr (with-rec 1.0)))
    (is (= :ppr (with-rec 1.5)) "a TE-premium league is still PPR-shaped")))

(deftest a-config-with-no-reception-weight-reads-as-standard
  (is (= :standard (scoring/format-of (dissoc (:ppr scoring/presets) :rec))))
  (is (= :standard (scoring/format-of {})))
  (is (= :standard (scoring/format-of (assoc (:ppr scoring/presets) :rec nil)))
      "an unusable weight is 0, not a crash"))

;; ---- malformed weights ----

(deftest an-unusable-weight-costs-one-stat-not-the-whole-board
  ;; A cleared input box in the custom editor sends NaN, which JSON.stringify
  ;; writes as null. That used to throw on (zero? nil), 400 the rankings call and
  ;; blank the board.
  (let [p {:stats {:rec 10.0 :rec_yd 100.0}}]
    (is (= 10.0 (scoring/player-points p {:rec 1.0 :rec_yd nil})))
    (is (= 10.0 (scoring/player-points p {:rec 1.0 :rec_yd "0.1"})))
    (is (= 10.0 (scoring/player-points p {:rec 1.0 :rec_yd ##NaN})))
    (is (= 10.0 (scoring/player-points p {:rec 1.0 :rec_yd ##Inf})))))

(deftest scores-anything?-rejects-a-config-that-cannot-move-a-player
  (is (scoring/scores-anything? (:standard scoring/presets)))
  (is (not (scoring/scores-anything? {})))
  (is (not (scoring/scores-anything? (zipmap scoring/stat-keys (repeat 0))))
      "an all-zero config prices the whole board at $0, which reads as a valuation")
  (is (not (scoring/scores-anything? {:rec nil :rec_yd ##NaN}))))
