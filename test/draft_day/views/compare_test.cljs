(ns draft-day.views.compare-test
  "The comparison tile's arithmetic and its sentence.

  Not reachable from `lein test` — run with
  `npx shadow-cljs compile test && node out/node-tests.js`."
  (:require [cljs.test :refer [deftest is testing]]
            [draft-day.confidence :as confidence]
            [draft-day.db :as db]
            [draft-day.views.compare :as cmp]
            [draft-day.views.metrics :as metrics]))

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
  {:player-name "Rome Odunze" :week-points 13.8 :bye 7})
(def ^:private jennings
  {:player-name "Jauan Jennings" :week-points 10.2 :bye 9})

(defn- text [hiccup]
  (letfn [(walk [x] (cond (string? x) x
                          (vector? x) (apply str (map walk x))
                          :else ""))]
    (walk hiccup)))

(deftest reading-line-names-the-weekly-lead
  (let [s (text (cmp/reading-line odunze jennings 5 nil))]
    (is (= "Rome Odunze projects higher this week." s))))

(deftest a-split-between-the-week-and-replacement-is-named
  ;; Over replacement subtracts a per-position level, so the weekly leader and
  ;; the one further above replacement can be different players.
  (let [a (assoc odunze  :ros-vorp 5.0)
        b (assoc jennings :ros-vorp 70.0)
        s (text (cmp/reading-line a b 5 nil))]
    (is (re-find #"Rome Odunze projects higher this week" s))
    (is (re-find #"Jauan Jennings is further above replacement" s))))

(deftest agreeing-vorp-leaves-the-sentence-alone
  (let [a (assoc odunze :ros-vorp 40.0)
        b (assoc jennings :ros-vorp 5.0)]
    (is (= "Rome Odunze projects higher this week." (text (cmp/reading-line a b 5 nil))))))

(deftest a-vorp-nobody-has-is-not-a-disagreement
  ;; nil for K and DST, and for the whole board before a sync.
  (is (= "Rome Odunze projects higher this week."
         (text (cmp/reading-line odunze (assoc jennings :ros-vorp 40.0) 5 nil)))))

(deftest vorp-speaks-when-the-week-does-not
  (let [a (assoc odunze :ros-vorp 5.0)
        b (assoc jennings :ros-vorp 70.0)
        s (text (cmp/reading-line a b 5 {:level :coin-flip :gap 0.4}))]
    (is (not (re-find #"this week" s)) "nothing weekly to say about a coin flip")))

(deftest reading-line-never-picks-for-you
  (let [s (text (cmp/reading-line odunze jennings 5 nil))]
    (is (not (re-find #"(?i)should|take |better claim|pick " s)))))

(deftest reading-line-leads-with-a-bye
  ;; A bye outranks every other reading: the weekly number is not low, it does
  ;; not exist.
  (let [s (text (cmp/reading-line (dissoc odunze :week-points) jennings 7 nil))]
    (is (= "Rome Odunze is on bye this week." s))))

(deftest reading-line-is-absent-with-no-weekly-line
  (let [a (dissoc odunze :week-points)
        b (dissoc jennings :week-points)]
    (is (nil? (cmp/reading-line a b nil nil)))))

(deftest reading-line-is-absent-with-nothing-to-say
  (is (nil? (cmp/reading-line {:player-name "A"} {:player-name "B"} nil nil))))

;; ---- evidence ----


(deftest on-bye-needs-the-week-and-a-missing-line
  (is (true?  (boolean (cmp/on-bye? {:bye 7} 7))))
  (is (false? (boolean (cmp/on-bye? {:bye 7} 6))))
  (is (false? (boolean (cmp/on-bye? {:bye 7} nil)) ) "no week: a bye cannot be claimed")
  ;; Projected in his bye week is a data disagreement, not a bye — believe the
  ;; projection, which is the thing the column actually renders.
  (is (false? (boolean (cmp/on-bye? {:bye 7 :week-points 9.1} 7)))))

;; ---- the sentence must not outrun the measurement ----
;; The tile prints the reading line directly above the calibration line, so a
;; weekly claim here that `separation-line` disclaims underneath is two
;; sentences disagreeing about one number. Found in the live app, not by these
;; tests: `reading-line` predates the calibration and read `:week-points` raw.

(def ^:private coin-flip {:level :coin-flip :gap 5})
(def ^:private clear-gap {:level :clear :gap 30})

(deftest a-coin-flip-week-is-not-a-weekly-lead
  ;; Nabers vs McConkey, the live reproduction: ahead on the raw number, so the
  ;; tile claimed the week directly above "Too close to call".
  (is (nil? (cmp/reading-line odunze jennings 5 coin-flip))))

(deftest a-measured-gap-still-gets-its-sentence
  ;; The fix removes a claim; it must not silence the line generally.
  (let [s (text (cmp/reading-line odunze jennings 5 clear-gap))]
    (is (= "Rome Odunze projects higher this week." s))))

(deftest a-bye-still-outranks-a-coin-flip
  (let [s (text (cmp/reading-line (dissoc odunze :week-points) jennings 7 coin-flip))]
    (is (= "Rome Odunze is on bye this week." s))))

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
  (first (filter #(= label (:label %)) metrics/rows)))

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

(def ^:private wr8  {:position "WR" :week-pos-rank 8  :week-points 13.8})
(def ^:private wr10 {:position "WR" :week-pos-rank 10 :week-points 12.9})
(def ^:private wr41 {:position "WR" :week-pos-rank 41 :week-points 8.1 })

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

(deftest the-weekly-row-carries-a-rank-subline
  ;; What makes the row legible on exactly the comparisons where the bar says
  ;; nothing on purpose.
  (is (re-find #"WR8" (text (cmp/metric-row (row-by-label "This week")
                                            wr8 wr10 nil))))
  (is (re-find #"WR10" (text (cmp/metric-row (row-by-label "This week")
                                             wr8 wr10 nil)))))

;; ---- rows with nothing to compare ----

(deftest a-row-survives-on-one-value
  ;; The asymmetry is the answer: a free agent beside a man you already hold has
  ;; an upgrade and a bid, and the rostered side has none. Hiding that row would
  ;; hide the reason you are comparing them.
  (let [upg (first (filter #(= "Upgrade" (:label %)) metrics/rows))]
    (is (true? (cmp/row-has-value? upg {:upgrade 12.0} {})))
    (is (true? (cmp/row-has-value? upg {} {:upgrade 12.0})))
    (is (false? (cmp/row-has-value? upg {} {})))))

(deftest an-empty-band-is-nil-rather-than-empty
  ;; nil is what lets the caller say "you hold both of these players" instead of
  ;; drawing three dashes under a border.
  (is (nil? (cmp/band :claim {} {} nil)))
  (is (some? (cmp/band :claim {:lineup-upgrade 4} {} nil))))

(deftest a-band-drops-only-the-rows-with-nothing-in-them
  (let [rows (cmp/band :claim {:upgrade 12.0 :lineup-upgrade 4} {:upgrade 3.0} nil)]
    (is (= 2 (count rows)))
    (is (= #{"Upgrade" "Lineup gain"}
           (set (map #(-> % second :label) rows))))))

;; ---- the injury designation ----

(deftest the-designation-is-abbreviated-to-fit
  (is (= "Q" (cmp/status-label "Questionable")))
  (is (= "D" (cmp/status-label "Doubtful")))
  (is (= "IR" (cmp/status-label "IR")) "already short enough to stand")
  (is (= "Out" (cmp/status-label "Out")))
  (is (nil? (cmp/status-chip {}))))

(deftest a-reader-gets-the-word-the-eye-gets-abbreviated
  ;; "Q" announced aloud is nothing, and a `title` on an element nobody can
  ;; focus is not guaranteed to be read — the case `.sr-only` exists for.
  (let [chip (cmp/status-chip {:sleeper/injury-status "Questionable"})]
    (is (= [:span.sr-only "Questionable"] (last chip)))
    (is (= "true" (:aria-hidden (second (nth chip 2))))
        "and the abbreviation is hidden from it, so the word is not said twice")))

(deftest only-a-serious-designation-takes-the-warn-colour
  (let [class-of #(:class (second (cmp/status-chip {:sleeper/injury-status %})))]
    (is (= "cmp-status serious" (class-of "IR")))
    (is (= "cmp-status" (class-of "Questionable"))
        "a Questionable that shouts like an IR trains you to stop reading it")))

(deftest the-full-word-survives-on-the-hover
  (is (= "Questionable"
         (:title (second (cmp/status-chip {:sleeper/injury-status "Questionable"}))))))

;; ---- band order ----

(deftest the-answer-sits-above-the-evidence
  ;; Question, then what the claim costs and gains, then why. At nine rows the
  ;; claim band could sit last; at twelve it fell below the fold on a laptop,
  ;; under the band it is a conclusion of.
  ;;
  ;; Asserted off what `tile-bands` draws, not off `metrics/bands`. The vector used
  ;; to be a constant nothing read, and a test comparing it to itself would have
  ;; passed just as happily with the three bands emitted in any order at all.
  ;; One surviving row per band, so each band's position in the fragment is
  ;; readable. The rows are `[metric-row row …]` component references — reagent
  ;; expands them, a test reads the row map straight out of them.
  (let [a (assoc odunze  :upgrade 12.0 :injury-risk 2)
        b (assoc jennings :upgrade 3.0 :injury-risk 4)
        labels (fn [b*] (keep #(:label (second %)) (nth b* 1)))
        [_ horizon claim evidence] (cmp/tile-bands a b nil nil)]
    (is (= ["This week"] (labels horizon)))
    (is (= ["Upgrade"] (labels claim)))
    (is (= ["Injury risk"] (labels evidence))))
  (is (= (cond-> #{"Lineup gain" "Upgrade"}
           db/bid-predictions? (into ["Typical winning bid" "Suggested bid" "Rivals"]))
         (set (map :label (metrics/rows-by-band :claim))))
      "bid predictions are drawn only while they are not held out"))

(deftest an-evidence-band-with-nothing-in-it-is-not-drawn
  ;; The only band that can vanish. A bordered empty box below the claim reads
  ;; as a section that failed to load.
  (is (nil? (cmp/band-content :evidence {:week-points 1.0} {:week-points 2.0} nil nil)))
  (is (some? (cmp/band-content :evidence {:injury-risk 3} {} nil nil))))

(deftest every-band-in-bands-can-draw-itself
  ;; `tile-bands` keeps over `bands`, so a keyword added there with no `case`
  ;; branch would silently drop out instead of failing.
  (doseq [k metrics/bands]
    (is (some? (cmp/band-content k {:player-name "A" :week-points 10.0
                                    :upgrade 1.0 :injury-risk 2}
                                 {:player-name "B" :week-points 8.0
                                  :upgrade 2.0 :injury-risk 3} nil nil))
        (str k " draws nothing"))))
