(ns draft-day.confidence-test
  "The calibration's boundaries and its three refusals.

  In cljc so `lein test` reaches it: the table is a measured claim about the
  world, and the tile that consumes it is the only cljs-only part."
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.confidence :as confidence]))

(defn- wr [n] {:position "WR" :week-pos-rank n})

;; ---- the bands ----

(deftest a-gap-lands-in-the-band-its-measured-win-rate-earns
  (testing "WR crosses 55% at a gap of 12 and 65% at 30"
    (is (= :coin-flip (confidence/level "WR" 11)))
    (is (= :slight    (confidence/level "WR" 12)))
    (is (= :slight    (confidence/level "WR" 29)))
    (is (= :clear     (confidence/level "WR" 30))))
  (testing "RB and TE separate earlier — 8 rather than 12"
    (is (= :slight (confidence/level "RB" 8)))
    (is (= :slight (confidence/level "TE" 8)))
    (is (= :coin-flip (confidence/level "RB" 7)))))

(deftest adjacent-ranks-are-a-coin-flip-at-every-position
  ;; The headline reading, and the reason the tile changed at all.
  (doseq [pos ["QB" "RB" "WR" "TE" "K"]]
    (is (= :coin-flip (confidence/level pos 1)) pos)
    (is (= :coin-flip (confidence/level pos 3)) pos)))

(deftest kickers-never-separate-however-large-the-gap
  ;; 47-51% at every measured gap. A tile that claimed one kicker beats another
  ;; this week would be inventing a difference the data does not contain.
  (doseq [gap [1 3 8 12 30 100]]
    (is (= :coin-flip (confidence/level "K" gap)) (str "gap " gap))))

(deftest a-quarterback-gap-never-reaches-clear
  ;; The table tops out at 59%, so :slight is as far as it goes. Rounding that
  ;; up to :clear would be asserting a measurement nobody made.
  (is (= :slight (confidence/level "QB" 8)))
  (is (= :slight (confidence/level "QB" 500))))

(deftest the-thresholds-match-the-table-they-were-read-off
  ;; Guards the pair from drifting apart, and in both directions. Iterating only
  ;; `separating-gaps` would miss the likelier drift: `win-rates` is the table
  ;; somebody re-deriving the calibration edits first, and a position added
  ;; there but forgotten here reports as never-measured though it was.
  (doseq [pos (into (set (keys confidence/win-rates))
                    (keys confidence/separating-gaps))]
    (let [rates (get confidence/win-rates pos)
          {:keys [slight clear]} (get confidence/separating-gaps pos)]
      (is (some? rates) (str pos " has a threshold but no measured rates"))
      (is (contains? confidence/separating-gaps pos)
          (str pos " was measured but has no threshold, so `level` reports it
               as never measured"))
      (when slight
        (is (>= (get rates slight) confidence/slight-threshold)
            (str pos " :slight at " slight)))
      (when clear
        (is (>= (get rates clear) confidence/clear-threshold)
            (str pos " :clear at " clear))))))

;; ---- the three refusals ----

(deftest a-pair-with-a-verdict-reports-its-gap
  (is (= {:level :slight :gap 12} (confidence/separation (wr 3) (wr 15))))
  (is (= {:level :coin-flip :gap 2} (confidence/separation (wr 8) (wr 6))))
  (testing "the gap is unsigned — which player leads is the bar's question"
    (is (= (confidence/separation (wr 3) (wr 15))
           (confidence/separation (wr 15) (wr 3))))))

(deftest no-weekly-rank-is-not-a-verdict
  ;; A bye or an unprojected player. The tile draws no track at all here, and
  ;; must not confuse that with a measured tie.
  (is (nil? (confidence/separation (wr 4) {:position "WR"})))
  (is (nil? (confidence/separation {:position "WR"} (wr 4))))
  (is (nil? (confidence/separation {} {}))))

(deftest two-positions-are-two-scales-and-get-no-verdict
  ;; WR8 against RB19 is not a gap of 11 — the calibration is same-position
  ;; only, and a flex decision is exactly where that would mislead.
  (is (nil? (confidence/separation (wr 8) {:position "RB" :week-pos-rank 19}))))

(deftest an-unmeasured-position-gets-no-verdict-rather-than-a-guess
  ;; DST is absent from the table. Reporting :coin-flip would be the same
  ;; overclaim as reporting an edge, just in the other direction.
  (is (nil? (confidence/level "DST" 1)))
  (is (nil? (confidence/separation {:position "DST" :week-pos-rank 2}
                                   {:position "DST" :week-pos-rank 9}))))
