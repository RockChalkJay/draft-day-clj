(ns draft-day.views.board-test
  "The tier palette, which is browser-side only and so out of `lein test`'s
  reach. What is worth pinning here is the property the old smooth ramp did not
  have: adjacent tiers stay visibly apart however many tiers a position has.
  Run with `npx shadow-cljs compile test && node out/node-tests.js`."
  (:require [cljs.test :refer [deftest is testing]]
            [draft-day.views.board :as board]))

(deftest tier-hue-ramps-green-to-red
  (testing "tier 1 is the green end and the last tier the red end"
    (is (= board/tier-hue-best (board/tier-hue 1 6)))
    (is (= board/tier-hue-worst (board/tier-hue 6 6))))
  (testing "the ramp is monotone, so hue alone still reads as an ordering"
    (let [hues (map #(board/tier-hue % 6) (range 1 7))]
      (is (apply > hues))))
  (testing "a single-tier position does not divide by zero"
    (is (= board/tier-hue-best (board/tier-hue 1 1))))
  (testing "a tier below 1 clamps rather than running off the green end"
    (is (= board/tier-hue-best (board/tier-hue 0 6)))))

(deftest tiers-alternate-bands
  (testing "consecutive tiers never share a band — this is what keeps
            neighbours apart once the hue steps get small"
    (let [bands (map board/tier-band (range 1 9))]
      (is (every? (fn [[a b]] (not= a b)) (partition 2 1 bands)))))
  (testing "the best tier is the bright one"
    (is (= :pale (board/tier-band 1)))))

(deftest tier-colors-are-well-formed-oklch
  (let [color (board/tier-color 3 6)
        fill  (board/tier-fill 3 6)]
    (is (re-matches #"oklch\(0\.\d+ 0\.\d+ \d+\.\d\)" color) color)
    (testing "the fill is the same color carrying an alpha"
      (is (re-matches #"oklch\(0\.\d+ 0\.\d+ \d+\.\d / 0\.\d+\)" fill) fill))))

(deftest tier-starts-marks-boundaries
  (testing "a row opens a tier when the row above it sits in another one"
    (is (= [false false true false true]
           (board/tier-starts [{:tier 1} {:tier 1} {:tier 2} {:tier 2} {:tier 3}]))))
  (testing "the first row never opens a tier — the header closes the top"
    (is (= [false] (board/tier-starts [{:tier 4}]))))
  (testing "boundaries follow the rendered order, not the tier numbers: sorting
            by a column that interleaves tiers still brackets each run"
    (is (= [false true true true]
           (board/tier-starts [{:tier 3} {:tier 1} {:tier 3} {:tier 2}]))))
  (is (= [] (board/tier-starts []))))
