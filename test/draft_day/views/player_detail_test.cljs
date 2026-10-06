(ns draft-day.views.player-detail-test
  "The one-column readout: which bands get headings, and what a band does with
  a player the board can say nothing about.

  Not reachable from `lein test` — run with
  `npx shadow-cljs compile test && node out/node-tests.js`."
  (:require [cljs.test :refer [deftest is]]
            [draft-day.views.metrics :as metrics]
            [draft-day.views.player-detail :as pd]))

;; ---- headings ----

(deftest every-band-has-a-heading
  ;; The tile needs none — its two heads are the legend. Read down one column and
  ;; three unlabelled groups are just thirteen numbers, so a band added to
  ;; `metrics/bands` without a heading here would ship as an unlabelled section.
  (doseq [k metrics/bands]
    (is (string? (get pd/band-labels k))
        (str k " is a band with no heading"))))

(deftest no-heading-names-a-band-that-does-not-exist
  ;; The other direction: a heading left behind after a band was removed is a
  ;; label nothing can ever draw.
  (is (= (set metrics/bands) (set (keys pd/band-labels)))))

;; ---- a band with nothing in it ----

(deftest a-band-drops-the-metrics-the-board-cannot-say
  ;; The one-column form of the tile's restraint. A dash reads as "the board
  ;; cannot say" once; a dozen stacked is punctuation.
  (let [drawn (fn [b] (keep #(:label (second %)) (nth b 2)))]
    (is (= ["This week"]
           (drawn (pd/band :horizon {:week-points 14.0}))))))

(deftest a-band-with-no-surviving-rows-is-nil
  ;; nil rather than an empty bordered box, which reads as a section that failed
  ;; to load. In preseason most of the evidence band is genuinely absent.
  (is (nil? (pd/band :evidence {:week-points 14.0})))
  (is (nil? (pd/band :claim {})))
  (is (some? (pd/band :claim {:upgrade 12.0}))))

;; ---- a metric row reads the shared list ----

(deftest a-row-formats-through-the-lists-own-formatter
  ;; The point of sharing `metrics/rows`: the modal cannot print a number in a
  ;; different shape from the tile, because it does not own the formatter.
  (let [row  (first (filter #(= "Suggested bid" (:label %)) metrics/rows))
        cell (fn [p] (nth (nth (pd/metric-row row p) 2) 1))]
    (is (= "$23" (cell {:bid 23})))
    (is (= "–" (cell {})) "absent is a dash, not a zero")))

(deftest zero-bid-and-no-bid-do-not-render-the-same
  ;; `$0` is a legal FAAB bid; nil means the league does not run FAAB. The board
  ;; keeps these apart and so must the modal.
  (let [row  (first (filter #(= "Suggested bid" (:label %)) metrics/rows))
        cell (fn [p] (nth (nth (pd/metric-row row p) 2) 1))]
    (is (= "$0" (cell {:bid 0})))
    (is (= "–" (cell {:bid nil})))))

;; ---- the head's context line ----

(deftest a-schedule-that-does-not-exist-yet-is-omitted-not-dashed
  ;; Preseason is the common case, not an edge one: there is no week and no
  ;; opponent, and `waivers/week-matchup` answers with a dash. Between the team
  ;; and the bye that dash reads as a value that failed to load.
  (is (= ["WR7" "LAR" "Bye 8"]
         (pd/meta-segments {:position "WR" :pos-rank 7 :team "LAR" :bye 8} nil))))

(deftest a-real-matchup-keeps-its-place
  (is (= ["WR7" "LAR" "@ SEA" "Bye 8"]
         (pd/meta-segments {:position "WR" :pos-rank 7 :team "LAR" :bye 8
                            :week/opponent "SEA" :week/home? false}
                           3))))

(deftest a-player-with-no-team-still-reads
  ;; A free agent has no NFL team; the line must not collapse to one segment.
  (is (= ["TE11" "FA"] (pd/meta-segments {:position "TE" :pos-rank 11} nil))))

;; ---- the kickoff ----

(deftest the-kickoff-sits-beside-the-matchup
  (let [segs (pd/meta-segments {:position "WR" :pos-rank 7 :team "LAR" :bye 8
                                :week/opponent "SEA" :week/home? false
                                :kickoff/at "2026-09-13T17:00Z"
                                :kickoff/status "STATUS_SCHEDULED"}
                               3)]
    (is (= ["WR7" "LAR" "@ SEA"] (take 3 segs)))
    (is (re-find #"\d:\d\d" (nth segs 3)) "a wall-clock time follows the matchup")
    (is (= "Bye 8" (last segs)))))

(deftest a-game-already-played-says-so
  ;; "Sun 1:00 PM" over a game that finished two hours ago is the lie
  ;; `fetched-at-label` exists to prevent.
  (is (= "Final/OT"
         (last (pd/meta-segments {:position "WR" :pos-rank 7 :team "LAR"
                                  :week/opponent "SEA" :week/home? false
                                  :kickoff/at "2026-09-13T17:00Z"
                                  :kickoff/status "STATUS_FINAL"
                                  :kickoff/detail "Final/OT"}
                                 3)))))

(deftest no-scoreboard-drops-the-segment-rather-than-dashing-it
  ;; A fetch that failed is not evidence that there is no game.
  (is (= ["WR7" "LAR" "@ SEA" "Bye 8"]
         (pd/meta-segments {:position "WR" :pos-rank 7 :team "LAR" :bye 8
                            :week/opponent "SEA" :week/home? false}
                           3))))

;; ---- the venue ----

(deftest a-neutral-site-names-the-ground
  ;; "vs SF" says nothing about a game in Melbourne.
  (is (= ["WR7" "LAR" "vs SF" "Melbourne Cricket Ground"]
         (pd/meta-segments {:position "WR" :pos-rank 7 :team "LAR"
                            :kickoff/opponent "SF" :kickoff/home? true
                            :kickoff/neutral? true
                            :kickoff/venue "Melbourne Cricket Ground"}
                           1))))

(deftest an-ordinary-game-does-not-name-its-stadium
  (is (= ["WR7" "LAR" "@ SEA"]
         (pd/meta-segments {:position "WR" :pos-rank 7 :team "LAR"
                            :kickoff/opponent "SEA" :kickoff/home? false
                            :kickoff/neutral? false
                            :kickoff/venue "Lumen Field"}
                           1))))

(deftest the-card-leads-with-the-bid
  (let [p {:typical-bid 7 :typical-win 0.62 :bid 0 :win-prob 0.48 :bid-sure 28 :walk-away 3
           :rivals 1.3 :competition {:threats [{:name "Show me your TDs" :p 0.31}]}}]
    (is (= [["Typical winning bid" "$7 · 62%"]
            ["Suggested bid" "$0 · 48%"]
            ["90% sure" "$28 · worth $3 to you"]
            ["Rivals likely to bid" "1.3 — Show me your TDs 31%"]]
           (pd/bidding-rows p)))
    (is (= [] (pd/bidding-rows {})) "a league that does not bid has no Bidding section")))

;; ---- the strip, the latest line and the tabs ----

(def ^:private nacua
  {:week-points 14.2 :week-pos-rank 8 :position "WR" :season-ppg 17.74
   :season-pos-rank 6 :season-gp 5 :trending/adds 12400})

(deftest the-strip-is-week-average-and-adds
  (is (= [{:label "Wk 5" :value "14.20" :sub "WR8"}
          {:label "Avg pts" :value "17.7" :sub "WR6 · 5 GP"}
          {:label "Adds" :value "12.4k" :sub "48 hrs"}]
         (pd/tiles nacua 5))))

(deftest a-tile-without-a-number-is-dropped-not-dashed
  (is (= ["Wk 5" "Avg pts"] (mapv :label (pd/tiles (dissoc nacua :trending/adds) 5))))
  (is (= [] (pd/tiles {} 5)))
  (is (= "Week" (:label (first (pd/tiles nacua nil))))))

(deftest a-designation-leads-the-latest-line
  (let [injured {:sleeper/injury-status "Questionable" :sleeper/injury-body-part "Hip"
                 :sleeper/injury-notes "Soreness" :sleeper/injury-updated 1790812230509}
        news    {:state :loaded :reply {:news [{:headline "Limited Wednesday" :published "2026-09-30T23:47:41Z"}]}}]
    (is (= {:kind :injury :chip "Questionable" :text "Hip · Soreness" :at 1790812230509}
           (pd/latest-line injured news)))
    (is (= "Limited Wednesday" (:text (pd/latest-line (dissoc injured :sleeper/injury-notes :sleeper/injury-body-part) news)))
        "with no note from Sleeper, the newest blurb fills the line")))

(deftest a-healthy-player-shows-his-newest-blurb
  (is (= {:kind :news :text "Nix throws 3 TDs" :at "2026-10-01T10:00:00Z"}
         (pd/latest-line {} {:state :loaded
                             :reply {:news [{:headline "Nix throws 3 TDs" :published "2026-10-01T10:00:00Z"}
                                            {:headline "older"}]}}))))

(deftest nothing-to-say-is-no-line-but-loading-and-failure-are-stated
  (is (nil? (pd/latest-line {} {:state :loaded :reply {:news []}})))
  (is (nil? (pd/latest-line {} nil)) "never asked: no line, no skeleton")
  (is (= :loading (:kind (pd/latest-line {} {:state :loading}))))
  (is (= "News unavailable" (:text (pd/latest-line {} {:state :failed})))))

(deftest tabs-run-game-log-news-season
  (is (= [[:game-log "Game log"] [:news "News"] [:season "Season"]]
         (pd/tabs-for {:log? true :season? true})))
  (is (= [[:news "News"]] (pd/tabs-for {:log? false :season? false}))
      "News is always there: its empty state says why it is empty")
  (is (= [:news "News"] (second (pd/tabs-for {:log? true :season? false})))))

(deftest a-tab-that-is-not-offered-falls-back-to-the-first
  (let [tabs (pd/tabs-for {:log? false :season? true})]
    (is (= :season (pd/shown-tab tabs :season)))
    (is (= :news (pd/shown-tab tabs :game-log)) "no game log to show, so the first offered")))
