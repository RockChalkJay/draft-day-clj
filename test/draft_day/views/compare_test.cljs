(ns draft-day.views.compare-test
  "The comparison tile's arithmetic and its sentence.

  Not reachable from `lein test` — run with
  `npx shadow-cljs compile test && node out/node-tests.js`."
  (:require [cljs.test :refer [deftest is testing]]
            [draft-day.views.compare :as cmp]))

;; ---- which way a row leans ----

(deftest lean-points-at-the-better-side
  (is (= :l (:side (cmp/lean 13.8 10.2 nil))))
  (is (= :r (:side (cmp/lean 96.0 119.0 nil))))
  ;; Injury risk inverts: 2 beats 4.
  (is (= :l (:side (cmp/lean 2 4 :lower))))
  (is (= :r (:side (cmp/lean 4 2 :lower)))))

(deftest lean-is-absent-rather-than-zero
  ;; No bar at all, so an empty track never has to mean two different things —
  ;; the row itself is only drawn when both sides are numbers.
  (is (nil? (cmp/lean 10.0 10.0 nil)) "a tie leans nowhere")
  (is (nil? (cmp/lean nil 10.0 nil)))
  (is (nil? (cmp/lean 10.0 nil nil)))
  (is (nil? (cmp/lean 0 0 nil))))

(deftest lean-scales-the-gap-against-the-larger-side
  ;; Doubled on purpose: the interesting comparisons are close ones, and an
  ;; honest 5% gap drawn at 5% of half a track is a row nobody can read.
  (is (< (abs (- 0.5 (:frac (cmp/lean 100.0 75.0 nil)))) 1e-9))   ; 25/100 * 2
  (is (= 1.0 (:frac (cmp/lean 100.0 10.0 nil))) "capped at the full half")
  ;; Signed values compare on magnitude, so a negative upgrade still leans.
  (is (= :r (:side (cmp/lean -5.0 3.0 nil)))))

;; ---- the sentence ----

(def ^:private odunze
  {:player-name "Rome Odunze" :week-points 13.8 :ros-points 96.0 :bye 7})
(def ^:private jennings
  {:player-name "Jauan Jennings" :week-points 10.2 :ros-points 119.0 :bye 9})

(defn- text [hiccup]
  (letfn [(walk [x] (cond (string? x) x
                          (vector? x) (apply str (map walk x))
                          :else ""))]
    (walk hiccup)))

(deftest reading-line-names-the-split
  ;; The whole reason the tile exists: the two horizons disagree, and a table of
  ;; numbers hides that behind arithmetic.
  (let [s (text (cmp/reading-line odunze jennings 5))]
    (is (re-find #"Rome Odunze projects higher this week" s))
    (is (re-find #"Jauan Jennings is the better rest-of-season hold" s))))

(deftest reading-line-says-so-when-they-agree
  (let [better (assoc jennings :week-points 18.0 :ros-points 140.0)
        s      (text (cmp/reading-line odunze better 5))]
    (is (= "Jauan Jennings is ahead on both." s))))

(deftest reading-line-never-picks-for-you
  ;; When the horizons split there is no answer without knowing whether the
  ;; manager is buying this Sunday or the rest of the year.
  (let [s (text (cmp/reading-line odunze jennings 5))]
    (is (not (re-find #"(?i)should|take |better claim|pick " s)))))

(deftest reading-line-leads-with-a-bye
  ;; A bye outranks every other reading: the weekly number is not low, it does
  ;; not exist.
  (let [s (text (cmp/reading-line (dissoc odunze :week-points) jennings 7))]
    (is (= "Rome Odunze is on bye this week." s))))

(deftest reading-line-falls-back-to-rest-of-season
  (let [a (dissoc odunze :week-points)
        b (dissoc jennings :week-points)
        s (text (cmp/reading-line a b nil))]
    (is (re-find #"Jauan Jennings is ahead rest-of-season" s))))

(deftest reading-line-is-absent-with-nothing-to-say
  (is (nil? (cmp/reading-line {:player-name "A"} {:player-name "B"} nil))))

;; ---- evidence ----

(deftest opportunity-pools-targets-and-carries
  ;; Volume, and position-agnostic: the two sides of a comparison need not be
  ;; the same position.
  (is (= 3.0 (cmp/opportunity-per-game
              {:nflverse/season-to-date {:games 4 :usage {:targets 8 :carries 4}}})))
  ;; A back with no targets still has a rate.
  (is (= 12.0 (cmp/opportunity-per-game
               {:nflverse/season-to-date {:games 2 :usage {:carries 24}}})))
  (is (nil? (cmp/opportunity-per-game {:nflverse/season-to-date {:games 0}})))
  (is (nil? (cmp/opportunity-per-game {}))))

(deftest on-bye-needs-the-week-and-a-missing-line
  (is (true?  (boolean (cmp/on-bye? {:bye 7} 7))))
  (is (false? (boolean (cmp/on-bye? {:bye 7} 6))))
  (is (false? (boolean (cmp/on-bye? {:bye 7} nil)) ) "no week: a bye cannot be claimed")
  ;; Projected in his bye week is a data disagreement, not a bye — believe the
  ;; projection, which is the thing the column actually renders.
  (is (false? (boolean (cmp/on-bye? {:bye 7 :week-points 9.1} 7)))))

;; ---- what the tile compares ----

(deftest bars-only-where-a-side-can-be-better
  ;; Games played is sample size and Bid is a price; a lean bar on either would
  ;; assert a verdict the number does not carry.
  (let [by-label (into {} (map (juxt :label identity)) cmp/rows)]
    (is (false? (:bar? (by-label "Games played"))))
    (is (false? (:bar? (by-label "Bid"))))
    (is (nil? (:bar? (by-label "This week"))) "defaults to drawn")
    (is (= :lower (:better (by-label "Injury risk"))))
    (is (= 2 (count (filter :big? cmp/rows))) "the two horizons are the question")))
