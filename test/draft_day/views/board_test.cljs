(ns draft-day.views.board-test
  "The tier palette, which is browser-side only and so out of `lein test`'s
  reach. What is worth pinning here is the property the old smooth ramp did not
  have: adjacent tiers stay visibly apart however many tiers a position has.
  Run with `npx shadow-cljs compile test && node out/node-tests.js`."
  (:require [cljs.test :refer [deftest is testing]]
            [draft-day.views.board :as board]
            [draft-day.views.util :as util]))

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

(deftest dragleave-ignores-a-move-into-a-child
  ;; A header is `<th>` wrapping a `[:span.sort-ind]` arrow. dragleave fires on
  ;; the th when the pointer crosses into that span, so treating every dragleave
  ;; as "left the header" blinks the insertion line off mid-drag.
  ;; The node runner has no DOM, so `contains` is stubbed with the contract the
  ;; real Node.contains has: true for a descendant, false for anything else
  ;; including null.
  (let [arrow #js {}
        other #js {}
        th    #js {:contains (fn [n] (identical? n arrow))}
        event (fn [related] #js {:currentTarget th :relatedTarget related})]

    (testing "moving onto the sort arrow has not left the header"
      (is (not (util/left-element? (event arrow)))))

    (testing "moving onto a different header has"
      (is (util/left-element? (event other))))

    (testing "leaving for nothing at all counts as leaving"
      (is (util/left-element? (event nil))))))

(deftest column-drags-do-not-carry-droppable-text
  ;; text/plain would make every header a payload any text input will accept,
  ;; and the board's search box sits directly above the header row.
  (is (not= "text/plain" util/column-mime))
  (is (re-find #"^application/" util/column-mime)))

(deftest drop-side-follows-the-direction-of-travel
  ;; The insertion line has to name the gap the column will actually land in.
  ;; db/move-column-onto drops a rightward drag past the target and a leftward
  ;; one before it, so a line that always drew on the same edge would be a lie
  ;; half the time.
  (let [ks [:rank :ecr :name :worth :market]]
    (testing "dragging rightwards, the line sits on the target's far edge"
      (is (= "drop-after" (board/drop-side ks :rank :worth))))

    (testing "dragging leftwards, it sits on the near edge"
      (is (= "drop-before" (board/drop-side ks :market :ecr))))

    (testing "no line over the column being dragged — there is no gap there"
      (is (nil? (board/drop-side ks :name :name))))

    (testing "a key that is not on screen draws nothing rather than guessing"
      (is (nil? (board/drop-side ks :floor :name)))
      (is (nil? (board/drop-side ks :name :floor)))
      (is (nil? (board/drop-side ks nil :name))))))

(deftest every-row-is-striped-whatever-the-view
  ;; Striping used to be switched off unless the board was filtered to one
  ;; position, because :tier was per-position and a tier 2 RB beside a tier 2 WR
  ;; meant nothing. The sub now hands down whichever scale the filter implies, so
  ;; there is always a coherent number to colour by.
  (let [[_ attrs] (board/player-row {:player-id "p1" :tier 3} [] nil 6 true)]
    (is (some #{"tier-row"} (:class attrs)))
    (is (some #{"tier-start"} (:class attrs)))
    (is (contains? (:style attrs) "--tier-bg"))
    (is (contains? (:style attrs) "--tier-line")))

  (testing "a row the server never tiered still renders rather than throwing"
    (let [[_ attrs] (board/player-row {:player-id "p2"} [] nil 6 false)]
      (is (some #{"tier-row"} (:class attrs)))
      (is (not-any? #{"tier-start"} (:class attrs))))))
