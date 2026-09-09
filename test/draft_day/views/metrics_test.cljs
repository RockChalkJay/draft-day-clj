(ns draft-day.views.metrics-test
  "The metric list itself: what it contains, how its own formatters read, and
  the guard that keeps it level with the board.

  Split from `compare-test` when the list moved out of `views.compare`, because
  two renderers now draw it and neither owns it. What stays in `compare-test` is
  everything that needs two players.

  Not reachable from `lein test` — run with
  `npx shadow-cljs compile test && node out/node-tests.js`."
  (:require [cljs.test :refer [deftest is]]
            [draft-day.db :as db]
            [draft-day.views.metrics :as metrics]))

;; ---- derived metrics ----

(deftest opportunity-pools-targets-and-carries
  ;; Volume, and position-agnostic: the two sides of a comparison need not be
  ;; the same position.
  (is (= 3.0 (metrics/opportunity-per-game
              {:nflverse/season-to-date {:games 4 :usage {:targets 8 :carries 4}}})))
  ;; A back with no targets still has a rate.
  (is (= 12.0 (metrics/opportunity-per-game
               {:nflverse/season-to-date {:games 2 :usage {:carries 24}}})))
  (is (nil? (metrics/opportunity-per-game {:nflverse/season-to-date {:games 0}})))
  (is (nil? (metrics/opportunity-per-game {}))))

;; ---- the comparison semantics carried on a shared row ----
;; `:bar?`, `:better` and `:big?` are read only by the tile; the detail modal
;; ignores them. They are pinned here anyway, because they live on this list and
;; a list this file guards is a list this file describes.

(deftest bars-only-where-a-side-can-be-better
  ;; Games played is sample size and Bid is a price; a lean bar on either would
  ;; assert a verdict the number does not carry.
  (let [by-label (into {} (map (juxt :label identity)) metrics/rows)]
    (is (false? (:bar? (by-label "Games played"))))
    (is (false? (:bar? (by-label "Bid"))))
    (is (nil? (:bar? (by-label "This week"))) "defaults to drawn")
    (is (= :lower (:better (by-label "Injury risk"))))
    (is (= 2 (count (filter :big? metrics/rows))) "the two horizons are the question")))

;; ---- the list must not fall behind the board ----
;; It did: `:lineup-upgrade` became the column `db/waiver-rank-key` sorts by and
;; the tile never mentioned it, along with VORP, the preseason line and ECR.
;; `metrics/rows` and `db/waiver-column-catalog` are still written separately —
;; the rows carry comparison semantics the board has no use for, and the board
;; carries identity and schedule columns no readout wants — so the guard is a
;; test rather than a shared definition. It now protects two renderers, not one.

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

(deftest every-board-metric-reaches-the-list
  (let [labels (set (map :label metrics/rows))]
    (doseq [[k label] board-metric->row]
      (is (contains? labels label)
          (str "board column " k " has no row labelled " (pr-str label))))))

(deftest a-new-board-column-must-decide-about-the-list
  ;; The half that actually catches drift: adding to `waiver-column-catalog`
  ;; fails here until the column is either given a row or listed as absent.
  (let [decided (into not-a-row (keys board-metric->row))]
    (doseq [{k :key} db/waiver-column-catalog]
      (is (contains? decided k)
          (str k " is a board column with no decision recorded about the tile")))))

(deftest the-list-shows-nothing-the-board-cannot
  ;; The other direction: a row reading a key no board column carries is a
  ;; number the manager can only see here, which is drift of its own. One is
  ;; allowed, because it is derived rather than shipped — the board has Tgt and
  ;; Car, and the tile divides them by games played.
  (is (= ["Opportunity / game"]
         (let [from-board (set (vals board-metric->row))]
           (remove from-board (map :label metrics/rows))))))

;; ---- zero is an answer in the claim band ----

(deftest a-claim-of-zero-prints-zero-not-a-dash
  ;; `util/signed` dashes zero because a board column has no bar beside it to
  ;; disagree with. This one does, and a dash next to a drawn bar reads as
  ;; missing data — which is the state that must stay distinguishable.
  (is (= "0" (metrics/claim-points 0)))
  (is (= "0" (metrics/claim-points 0.4)) "rounded once, and the digits follow it")
  (is (= "+8" (metrics/claim-points 8.2)))
  (is (= "-8" (metrics/claim-points -8.2)))
  (is (= "–" (metrics/claim-points nil)) "absent is still a dash"))

