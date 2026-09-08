(ns draft-day.views.compare-test
  "The comparison tile's arithmetic and its sentence.

  Not reachable from `lein test` — run with
  `npx shadow-cljs compile test && node out/node-tests.js`."
  (:require [cljs.test :refer [deftest is testing]]
            [draft-day.confidence :as confidence]
            [draft-day.db :as db]
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
  (let [s (text (cmp/reading-line odunze jennings 5 nil))]
    (is (re-find #"Rome Odunze projects higher this week" s))
    (is (re-find #"Jauan Jennings is the better rest-of-season hold" s))))

(deftest reading-line-says-so-when-they-agree
  ;; Named rather than counted. It read "ahead on both" when the band held two
  ;; rows; Over replacement made three, and the next one would have made four.
  (let [better (assoc jennings :week-points 18.0 :ros-points 140.0)
        s      (text (cmp/reading-line odunze better 5 nil))]
    (is (= "Jauan Jennings is ahead this week and rest-of-season." s))))

(deftest a-cross-position-pair-gets-its-own-sentence
  ;; The row that made this necessary. VORP subtracts a *per-position*
  ;; replacement level, so more points and further above replacement are
  ;; different players as soon as the two are not the same position — and Over
  ;; replacement sits in the band this sentence summarises, leaning visibly the
  ;; other way while it claimed they agreed.
  (let [qb (assoc odunze  :player-name "A QB" :ros-points 285.0 :ros-vorp 5.0)
        wr (assoc jennings :player-name "A WR" :ros-points 200.0 :ros-vorp 70.0
                  :week-points 1.0)
        s  (text (cmp/reading-line qb wr 5 nil))]
    (is (re-find #"A QB projects more points" s))
    (is (re-find #"A WR is further above replacement" s))
    (is (not (re-find #"ahead this week and rest-of-season" s))
        "the agreement sentence is exactly what must not print here")))

(deftest agreeing-vorp-leaves-the-sentence-alone
  ;; Same position, so replacement cancels and the third row cannot disagree.
  ;; The branch must not fire on every pair that happens to carry a VORP.
  (let [better (assoc jennings :week-points 18.0 :ros-points 140.0 :ros-vorp 40.0)
        s      (text (cmp/reading-line (assoc odunze :ros-vorp -4.0) better 5 nil))]
    (is (= "Jauan Jennings is ahead this week and rest-of-season." s))))

(deftest a-vorp-nobody-has-is-not-a-disagreement
  ;; nil for K and DST, and for the whole board before a sync.
  (let [better (assoc jennings :week-points 18.0 :ros-points 140.0)
        s      (text (cmp/reading-line odunze (assoc better :ros-vorp 40.0) 5 nil))]
    (is (= "Jauan Jennings is ahead this week and rest-of-season." s))))

(deftest reading-line-never-picks-for-you
  ;; When the horizons split there is no answer without knowing whether the
  ;; manager is buying this Sunday or the rest of the year.
  (let [s (text (cmp/reading-line odunze jennings 5 nil))]
    (is (not (re-find #"(?i)should|take |better claim|pick " s)))))

(deftest reading-line-leads-with-a-bye
  ;; A bye outranks every other reading: the weekly number is not low, it does
  ;; not exist.
  (let [s (text (cmp/reading-line (dissoc odunze :week-points) jennings 7 nil))]
    (is (= "Rome Odunze is on bye this week." s))))

(deftest reading-line-falls-back-to-rest-of-season
  (let [a (dissoc odunze :week-points)
        b (dissoc jennings :week-points)
        s (text (cmp/reading-line a b nil nil))]
    (is (re-find #"Jauan Jennings is ahead rest-of-season" s))))

(deftest reading-line-is-absent-with-nothing-to-say
  (is (nil? (cmp/reading-line {:player-name "A"} {:player-name "B"} nil nil))))

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

;; ---- the sentence must not outrun the measurement ----
;; The tile prints the reading line directly above the calibration line, so a
;; weekly claim here that `separation-line` disclaims underneath is two
;; sentences disagreeing about one number. Found in the live app, not by these
;; tests: `reading-line` predates the calibration and read `:week-points` raw.

(def ^:private coin-flip {:level :coin-flip :gap 5})
(def ^:private clear-gap {:level :clear :gap 30})

(deftest a-coin-flip-week-is-not-a-weekly-lead
  ;; Nabers vs McConkey, the live reproduction: ahead on *both* raw numbers, so
  ;; the tile claimed the week directly above "Too close to call".
  (let [weaker (assoc jennings :ros-points 80.0)
        s      (text (cmp/reading-line odunze weaker 5 coin-flip))]
    (is (not (re-find #"this week" s)))
    (is (not (re-find #"ahead this week and rest-of-season" s)))
    (is (re-find #"Rome Odunze is ahead rest-of-season" s))))

(deftest a-coin-flip-does-not-manufacture-a-split
  ;; The worse half of the bug. A split sentence frames a real decision — buy
  ;; for Sunday, or hold for the season — and doing that on a weekly difference
  ;; the measurement says is absent is worse than saying nothing.
  (let [s (text (cmp/reading-line odunze jennings 5 coin-flip))]
    (is (not (re-find #"projects higher this week" s))
        "odunze leads the week and jennings the season, but the week is noise")
    (is (re-find #"Jauan Jennings is ahead rest-of-season" s))))

(deftest a-measured-gap-still-gets-its-sentence
  ;; The fix removes a claim; it must not silence the line generally.
  (let [s (text (cmp/reading-line odunze jennings 5 clear-gap))]
    (is (re-find #"Rome Odunze projects higher this week" s))
    (is (re-find #"Jauan Jennings is the better rest-of-season hold" s))))

(deftest the-coin-flip-sentence-does-not-repeat-the-calibration
  ;; Two branches end at rest-of-season and must stay distinct. "no weekly
  ;; projection separates them" is about *absent data*; a coin flip has data,
  ;; and `separation-line` explains it underneath in its own terms.
  (let [flip (text (cmp/reading-line odunze jennings 5 coin-flip))
        gone (text (cmp/reading-line (dissoc odunze :week-points)
                                     (dissoc jennings :week-points) 5 nil))]
    (is (not (re-find #"no weekly" flip)))
    (is (re-find #"no weekly" gone) "the missing-data wording is still reachable")))

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

;; ---- the tile must not fall behind the board ----
;; It did: `:lineup-upgrade` became the column `db/waiver-rank-key` sorts by and
;; the tile never mentioned it, along with VORP, the preseason line and ECR. The
;; two lists are written separately because tile rows carry comparison semantics
;; (`:bar?`, `:better`, `:big?`) the board has no use for — so the guard is a
;; test rather than a shared definition.

(def ^:private board-metric->row
  "Every `db/waiver-column-catalog` key that states a fact about the player,
  against the row that shows it."
  {:ros       "Rest of season"
   :week      "This week"
   :week-rank "This week"          ; the weekly row's `:sub`
   :ros-vorp  "Over replacement"
   :lineup    "Lineup gain"
   :upgrade   "Upgrade"
   :bid       "Bid"
   :trend     "Trend"
   :form      "Form / game"
   :gp        "Games played"
   :preseason "Preseason"
   :ecr       "Expert rank"
   :risk      "Injury risk"})

(def ^:private not-a-row
  "Board columns deliberately absent from the tile, each for a stated reason:
  identity (the head says it), schedule (no winner, and Opp is in the head),
  a raw count the tile shows as a rate, and the designation, which is a chip on
  the name rather than a word among tabular numbers."
  #{:rank :name :team :position :bye :opp :tgt :car :inj})

(deftest every-board-metric-reaches-the-tile
  (let [labels (set (map :label cmp/rows))]
    (doseq [[k label] board-metric->row]
      (is (contains? labels label)
          (str "board column " k " has no row labelled " (pr-str label))))))

(deftest a-new-board-column-must-decide-about-the-tile
  ;; The half that actually catches drift: adding to `waiver-column-catalog`
  ;; fails here until the column is either given a row or listed as absent.
  (let [decided (into not-a-row (keys board-metric->row))]
    (doseq [{k :key} db/waiver-column-catalog]
      (is (contains? decided k)
          (str k " is a board column with no decision recorded about the tile")))))

(deftest the-tile-shows-nothing-the-board-cannot
  ;; The other direction: a row reading a key no board column carries is a
  ;; number the manager can only see here, which is drift of its own. One is
  ;; allowed, because it is derived rather than shipped — the board has Tgt and
  ;; Car, and the tile divides them by games played.
  (is (= ["Opportunity / game"]
         (let [from-board (set (vals board-metric->row))]
           (remove from-board (map :label cmp/rows))))))

;; ---- rows with nothing to compare ----

(deftest a-row-survives-on-one-value
  ;; The asymmetry is the answer: a free agent beside a man you already hold has
  ;; an upgrade and a bid, and the rostered side has none. Hiding that row would
  ;; hide the reason you are comparing them.
  (let [upg (first (filter #(= "Upgrade" (:label %)) cmp/rows))]
    (is (true? (cmp/row-has-value? upg {:upgrade 12.0} {})))
    (is (true? (cmp/row-has-value? upg {} {:upgrade 12.0})))
    (is (false? (cmp/row-has-value? upg {} {})))))

(deftest an-empty-band-is-nil-rather-than-empty
  ;; nil is what lets the caller say "you hold both of these players" instead of
  ;; drawing three dashes under a border.
  (is (nil? (cmp/band :claim {} {} nil)))
  (is (some? (cmp/band :claim {:bid 4} {} nil))))

(deftest a-band-drops-only-the-rows-with-nothing-in-them
  (let [rows (cmp/band :claim {:upgrade 12.0 :bid 4} {:upgrade 3.0} nil)]
    (is (= 2 (count rows)) "lineup gain is absent on both sides and goes")
    (is (= #{"Upgrade" "Bid"}
           (set (map #(-> % second :label) rows))))))

;; ---- zero is an answer in the claim band ----

(deftest a-claim-of-zero-prints-zero-not-a-dash
  ;; `util/signed` dashes zero because a board column has no bar beside it to
  ;; disagree with. This one does, and a dash next to a drawn bar reads as
  ;; missing data — which is the state that must stay distinguishable.
  (is (= "0" (cmp/claim-points 0)))
  (is (= "0" (cmp/claim-points 0.4)) "rounded once, and the digits follow it")
  (is (= "+8" (cmp/claim-points 8.2)))
  (is (= "-8" (cmp/claim-points -8.2)))
  (is (= "–" (cmp/claim-points nil)) "absent is still a dash"))

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
  ;; Asserted off what `tile-bands` draws, not off `cmp/bands`. The vector used
  ;; to be a constant nothing read, and a test comparing it to itself would have
  ;; passed just as happily with the three bands emitted in any order at all.
  ;; One surviving row per band, so each band's position in the fragment is
  ;; readable. The rows are `[metric-row row …]` component references — reagent
  ;; expands them, a test reads the row map straight out of them.
  (let [a (assoc odunze  :upgrade 12.0 :points 90.0)
        b (assoc jennings :upgrade 3.0 :points 70.0)
        labels (fn [b*] (keep #(:label (second %)) (nth b* 1)))
        [_ horizon claim evidence] (cmp/tile-bands (dissoc a :week-points)
                                                   (dissoc b :week-points)
                                                   nil nil)]
    (is (= ["Rest of season"] (labels horizon)))
    (is (= ["Upgrade"] (labels claim)))
    (is (= ["Preseason"] (labels evidence))))
  (is (= #{"Lineup gain" "Upgrade" "Bid"}
         (set (map :label (cmp/rows-by-band :claim))))))

(deftest an-evidence-band-with-nothing-in-it-is-not-drawn
  ;; The only band that can vanish. A bordered empty box below the claim reads
  ;; as a section that failed to load.
  (is (nil? (cmp/band-content :evidence {:ros-points 1.0} {:ros-points 2.0} nil nil)))
  (is (some? (cmp/band-content :evidence {:points 90.0} {} nil nil))))

(deftest every-band-in-bands-can-draw-itself
  ;; `tile-bands` keeps over `bands`, so a keyword added there with no `case`
  ;; branch would silently drop out instead of failing.
  (doseq [k cmp/bands]
    (is (some? (cmp/band-content k {:player-name "A" :ros-points 100.0
                                    :upgrade 1.0 :points 9.0}
                                 {:player-name "B" :ros-points 80.0
                                  :upgrade 2.0 :points 8.0} nil nil))
        (str k " draws nothing"))))
