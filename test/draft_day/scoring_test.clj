(ns draft-day.scoring-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.scoring :as scoring]))

(deftest presets-has-exactly-three-keys
  (is (= #{:standard :half-ppr :ppr} (set (keys scoring/presets)))))

(deftest player-points-defaults-missing-stats-to-zero
  (is (= 0.0 (scoring/player-points {:stats {}} (:ppr scoring/presets)))))

(deftest player-points-ignores-unknown-stat-keys
  (let [player {:stats {:rec 10.0 :some-unknown-stat 999.0}}]
    (is (= 10.0 (scoring/player-points player (:ppr scoring/presets))))))

(deftest every-stat-key-actually-scores
  ;; A key that drifted from Sleeper's spelling scores silently as zero, which
  ;; on the board is indistinguishable from a player who does not accumulate
  ;; that stat. Over the whole vocabulary rather than a list, so a key added
  ;; without a test cannot exist.
  ;; `:fgm` is the exception and has its own test below: a config stating its
  ;; field goals by distance drops the flat weight rather than counting the
  ;; same kick twice.
  (let [all-ones (zipmap scoring/stat-keys (repeat 1.0))]
    (doseq [k (remove #{:fgm} scoring/stat-keys)]
      (is (= 3.0 (scoring/player-points {:stats {k 3.0}} all-ones))
          (str k " does not reach :points")))))

(deftest every-preset-weight-is-applied-as-written
  (doseq [[fmt preset] scoring/presets
          [k w] preset
          ;; Every preset states its field goals by distance, so the flat
          ;; weight is superseded — `a-flat-field-goal-weight-yields-to-the-buckets`.
          :when (and (not (zero? w)) (not= :fgm k))]
    (is (= (* 2.0 w) (scoring/player-points {:stats {k 2.0}} preset))
        (str fmt " " k))))

(deftest only-the-reception-weight-separates-the-presets
  (doseq [k (remove #{:rec} scoring/stat-keys)]
    (is (apply = (map #(get % k) (vals scoring/presets)))
        (str k " differs between presets"))))

(deftest stat-keys-covers-every-preset-key
  (is (= (set scoring/stat-keys) (set (keys (:ppr scoring/presets)))))
  (is (every? (set scoring/stat-keys) [:pass_2pt :rush_2pt :rec_2pt :blk_kick])))

;; ---- format classification (which vendor variant a league reads) ----

(deftest presets-map-to-their-own-published-format
  (is (= :standard (scoring/format-of (:standard scoring/presets))))
  (is (= :half-ppr (scoring/format-of (:half-ppr scoring/presets))))
  (is (= :ppr (scoring/format-of (:ppr scoring/presets)))))

(deftest a-custom-config-lands-on-the-nearest-published-format
  ;; Vendors publish ECR, auction values and ADP for exactly three formats, and a
  ;; custom config names none of them. Receptions are what separate the three.
  (let [with-rec (fn [r] (scoring/format-of (assoc (:ppr scoring/presets) :rec r)))]
    (is (= :standard (with-rec 0.0)))
    (is (= :standard (with-rec 0.2)))
    (is (= :half-ppr (with-rec 0.4)))
    (is (= :half-ppr (with-rec 0.5)))
    (is (= :ppr (with-rec 1.0)))
    (is (= :ppr (with-rec 1.5)) "a TE-premium league is still PPR-shaped")))

(deftest a-config-with-no-reception-weight-reads-as-standard
  (is (= :standard (scoring/format-of (dissoc (:ppr scoring/presets) :rec))))
  (is (= :standard (scoring/format-of {})))
  (is (= :standard (scoring/format-of (assoc (:ppr scoring/presets) :rec nil)))
      "an unusable weight is 0, not a crash"))

(deftest a-partial-scoring-map-scores-like-its-zero-filled-twin
  ;; League import writes only the stat keys the league actually defines
  ;; (`league_import.sleeper/normalize-league` is a select-keys), so a config
  ;; holding a hole rather than a zero is the normal shape of an imported
  ;; league — nothing fills it in at boot any more.
  (let [partial-cfg {:rec 1.0 :rec_yd 0.1}
        filled      (merge (zipmap scoring/stat-keys (repeat 0.0)) partial-cfg)
        player      {:stats {:rec 80.0 :rec_yd 1200.0 :rush_yd 100.0 :pass_td 3.0}}]
    (is (= (scoring/player-points player filled)
           (scoring/player-points player partial-cfg))
        "a missing weight and a zero weight both contribute nothing")
    (is (= (scoring/format-of filled) (scoring/format-of partial-cfg)))
    (is (= (scoring/scores-anything? filled) (scoring/scores-anything? partial-cfg)))))

;; ---- malformed weights ----

(deftest an-unusable-weight-costs-one-stat-not-the-whole-board
  ;; A cleared input box in the custom editor sends NaN, which JSON.stringify
  ;; writes as null. That used to throw on (zero? nil), 400 the rankings call and
  ;; blank the board.
  (let [p {:stats {:rec 10.0 :rec_yd 100.0}}]
    (is (= 10.0 (scoring/player-points p {:rec 1.0 :rec_yd nil})))
    (is (= 10.0 (scoring/player-points p {:rec 1.0 :rec_yd "0.1"})))
    (is (= 10.0 (scoring/player-points p {:rec 1.0 :rec_yd ##NaN})))
    (is (= 10.0 (scoring/player-points p {:rec 1.0 :rec_yd ##Inf})))))

(deftest scores-anything?-rejects-a-config-that-cannot-move-a-player
  (is (scoring/scores-anything? (:standard scoring/presets)))
  (is (not (scoring/scores-anything? {})))
  (is (not (scoring/scores-anything? (zipmap scoring/stat-keys (repeat 0))))
      "an all-zero config prices the whole board at $0, which reads as a valuation")
  (is (not (scoring/scores-anything? {:rec nil :rec_yd ##NaN})))

  (testing "weights on stats nothing is projected for are not evidence of a league"
    ;; What the editor leaves behind when every weight a manager can reach is
    ;; zeroed: the unprojected ones are rendered disabled, so they keep whatever
    ;; the preset set. They cannot move a single player's points. `:fgm` left
    ;; this set when kickers got a projected total.
    ;; Weighted positively on purpose rather than read off a preset: most of
    ;; these sit at 0.0 there, and a set of zeroes would pass this for the
    ;; wrong reason.
    (let [only-unprojected (zipmap scoring/unprojected-stats (repeat 6.0))]
      (is (= (count scoring/unprojected-stats) (count only-unprojected)))
      (is (every? pos? (vals only-unprojected)))
      (is (not (scoring/scores-anything? only-unprojected)))
      (is (not (scoring/scores-anything?
                (merge (zipmap scoring/stat-keys (repeat 0)) only-unprojected))))
      (is (scoring/scores-anything? (assoc only-unprojected :rec_yd 0.1))
          "one projected weight is enough"))))

(deftest unprojected-stats-are-real-stat-keys
  (is (every? (set scoring/stat-keys) scoring/unprojected-stats)))

(deftest field-goals-are-projected-now
  ;; Kickers carry a :fgm summed from the buckets Sleeper publishes, so the
  ;; scoring editor must let a league price it and `scores-anything?` must count
  ;; it. Leaving it in the set locked FG Made for the one position the fill
  ;; exists for.
  (is (not (contains? scoring/unprojected-stats :fgm)))
  (is (scoring/scores-anything? {:fgm 3.0})
      "a config that can only move kickers is still a config")
  (testing "every distance bucket is projected on one line or the other"
    ;; The weekly line carries every make under fifty, the season line the two
    ;; over forty. A bucket listed as unprojected would be locked in the editor
    ;; for a league that really does score it.
    (is (not-any? scoring/unprojected-stats
                  [:fgm_0_19 :fgm_20_29 :fgm_30_39 :fgm_40_49 :fgm_50p
                   :fgmiss_40_49 :fgmiss_50p :xpmiss])))
  ;; A key wrongly in this set locks a rule a league really scores out of the
  ;; editor, and `scores-anything?` stops counting it, which can 400 a valid
  ;; config. Too wide to pin as a literal now, so pinned from both ends.
  (is (not-any? scoring/unprojected-stats
                (for [[k w] (:ppr scoring/presets)
                      :when (and (not (zero? w)) (not (#{:ff :def_td :safe} k)))]
                  k))
      "nothing a preset prices for a skill position is locked")
  (is (not-any? scoring/unprojected-stats [:ff :def_td :safe])
      "the weekly line projects all three, and the fill carries them to a season"))

(deftest a-projected-rule-is-not-locked-out-of-the-editor
  ;; The set is derived by removing these, so getting it wrong either way shows
  ;; up here. A key listed as unprojected is greyed out in the editor and
  ;; discounted by `scores-anything?`, which can 400 a config the board scores.
  (doseq [k [;; on Sleeper's season line directly
             :pass_cmp :pass_fd :pass_int_td :rush_fd
             :rec_fd :rec_20_29 :rec_30_39 :rec_40p
             ;; on its weekly line, reaching a season through the fill
             :pass_cmp_40p :rush_40p :fum :fgmiss_30_39 :st_td
             :pts_allow_14_20 :pts_allow_21_27 :pts_allow_28_34
             :yds_allow_200_299 :yds_allow_300_349 :yds_allow_350_399
             :ff :def_td :safe]]
    (is (not (contains? scoring/unprojected-stats k)) (str k " is projected"))
    (is (scoring/scores-anything? {k 1.0}) (str k " can move a board"))))

(deftest a-flat-field-goal-weight-yields-to-the-buckets
  ;; `sleeper/scored-stats` publishes :fgm *and* the buckets it is summed from,
  ;; so a config holding both weights would pay for the same kick twice.
  (let [line {:stats {:fgm 5.0 :fgm_40_49 3.0 :fgm_50p 2.0}}]
    (is (= 22.0 (scoring/player-points line {:fgm 3.0 :fgm_40_49 4.0 :fgm_50p 5.0}))
        "3x4 + 2x5, with the flat weight dropped rather than added to it")
    (is (= 15.0 (scoring/player-points line {:fgm 3.0}))
        "a league that scores every kick the same still uses the summed total")
    (is (= 15.0 (scoring/player-points line {:fgm 3.0 :fgm_40_49 0.0}))
        "a bucket set to zero is not a league stating its distances")))
