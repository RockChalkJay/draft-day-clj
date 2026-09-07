(ns draft-day.views.compare-test
  "The comparison tile's arithmetic and its sentence.

  Not reachable from `lein test` — run with
  `npx shadow-cljs compile test && node out/node-tests.js`."
  (:require [cljs.test :refer [deftest is testing]]
            [draft-day.confidence :as confidence]
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

;; ---- how much of the gap to believe ----

(defn- wk [pos rank] {:position pos :week-pos-rank rank :player-name (str pos rank)})

(deftest the-calibration-sentence-names-the-gap-and-what-it-is-worth
  (let [say (fn [a b] (text (cmp/separation-line a (confidence/separation a b))))]
    (is (re-find #"Too close to call — 2 WRs apart" (say (wk "WR" 8) (wk "WR" 10))))
    (is (re-find #"about half the time" (say (wk "WR" 8) (wk "WR" 10))))
    (is (re-find #"A slight edge — 12 WRs apart" (say (wk "WR" 3) (wk "WR" 15))))
    (is (re-find #"A clear gap — 30 WRs apart" (say (wk "WR" 1) (wk "WR" 31))))))

(deftest the-sentence-is-singular-for-a-gap-of-one
  (is (re-find #"1 WR apart"
               (text (cmp/separation-line (wk "WR" 8)
                                          {:level :coin-flip :gap 1})))))

(deftest nothing-is-said-where-nothing-was-measured
  ;; A cross-position pair, a DST, or a player with no weekly line. Silence is
  ;; the honest output — see `draft-day.confidence`.
  (is (nil? (cmp/separation-line (wk "WR" 8) nil))))

;; ---- the three track states ----
;; The substance of the calibration, and what the sentence above is only
;; commentary on. All three have to stay mutually distinguishable: an absent
;; track means "no weekly line", a centred fill means "measured tie", and a
;; directional fill means "the board separates them". Collapsing the first two
;; is the bug #52 shipped once, where missing data rendered as a tie.

(defn- row-by-label [label]
  (first (filter #(= label (:label %)) cmp/rows)))

(defn- track
  "The `<i>` inside a rendered row's bar, or `:no-track` when no bar was drawn
  at all. Walks the hiccup rather than pattern-matching a fixed shape, so a
  layout change does not silently turn every assertion vacuous."
  [row a b sep]
  (let [hit (atom :no-track)]
    (letfn [(walk [x]
              (when (vector? x)
                (when (= :div.cmp-bar (first x))
                  (reset! hit (or (second x) :empty-track)))
                (doseq [c x] (walk c))))]
      (walk (cmp/metric-row row a b sep)))
    @hit))

(def ^:private wr8  {:position "WR" :week-pos-rank 8  :week-points 13.8 :ros-points 96.0})
(def ^:private wr10 {:position "WR" :week-pos-rank 10 :week-points 12.9 :ros-points 119.0})
(def ^:private wr41 {:position "WR" :week-pos-rank 41 :week-points 8.1  :ros-points 119.0})

(deftest a-clear-gap-still-draws-a-directional-bar
  ;; The calibration removes a claim; it must not remove the tile's whole point.
  (let [i (track (row-by-label "This week") wr8 wr41
                 (confidence/separation wr8 wr41))]
    (is (= :i (first i)))
    (is (= "l" (:class (second i))) "and it leans toward the better player")
    (is (pos? (js/parseFloat (:width (:style (second i))))))))

(deftest a-coin-flip-gap-draws-a-centred-fill-instead
  (let [i (track (row-by-label "This week") wr8 wr10
                 (confidence/separation wr8 wr10))]
    (is (= [:i.even] i) "no direction, no width — a needle resting at zero")))

(deftest no-weekly-line-draws-no-track-at-all
  ;; The state that must never be confused with a measured tie.
  (is (= :no-track (track (row-by-label "This week")
                          wr8 (dissoc wr10 :week-points :week-pos-rank) nil))))

(deftest rest-of-season-is-unaffected-by-the-weekly-verdict
  ;; `sep` reaches every row, but only `:calibrated?` ones may consult it. A
  ;; coin flip this week says nothing about a dozen games from here, and
  ;; suppressing this bar too would be the regression that is easiest to ship.
  (let [coin (confidence/separation wr8 wr10)]
    (is (= :coin-flip (:level coin)) "precondition: the weekly row is a tie")
    (let [i (track (row-by-label "Rest of season") wr8 wr10 coin)]
      (is (= :i (first i)))
      (is (= "r" (:class (second i))) "and still leans to the better hold"))))

(deftest the-weekly-row-carries-a-rank-subline
  ;; What makes the row legible on exactly the comparisons where the bar says
  ;; nothing on purpose.
  (is (re-find #"WR8" (text (cmp/metric-row (row-by-label "This week")
                                            wr8 wr10 nil))))
  (is (re-find #"WR10" (text (cmp/metric-row (row-by-label "This week")
                                             wr8 wr10 nil)))))

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
