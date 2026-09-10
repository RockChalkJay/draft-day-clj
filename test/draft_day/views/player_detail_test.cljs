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
    (is (= ["Rest of season"]
           (drawn (pd/band :horizon {:ros-points 140.0}))))))

(deftest a-band-with-no-surviving-rows-is-nil
  ;; nil rather than an empty bordered box, which reads as a section that failed
  ;; to load. In preseason most of the evidence band is genuinely absent.
  (is (nil? (pd/band :evidence {:ros-points 140.0})))
  (is (nil? (pd/band :claim {})))
  (is (some? (pd/band :claim {:upgrade 12.0}))))

;; ---- a metric row reads the shared list ----

(deftest a-row-formats-through-the-lists-own-formatter
  ;; The point of sharing `metrics/rows`: the modal cannot print a number in a
  ;; different shape from the tile, because it does not own the formatter.
  (let [row  (first (filter #(= "Bid" (:label %)) metrics/rows))
        cell (fn [p] (nth (nth (pd/metric-row row p) 2) 1))]
    (is (= "$23" (cell {:bid 23})))
    (is (= "–" (cell {})) "absent is a dash, not a zero")))

(deftest zero-bid-and-no-bid-do-not-render-the-same
  ;; `$0` is a legal FAAB bid; nil means the league does not run FAAB. The board
  ;; keeps these apart and so must the modal.
  (let [row  (first (filter #(= "Bid" (:label %)) metrics/rows))
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
