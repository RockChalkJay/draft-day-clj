(ns draft-day.scoring-golden-test
  "Sleeper's own scored numbers, reproduced from Sleeper's own stat lines under
  a real league's own rules.

  This is the claim the scoring vocabulary rests on, and it is not an
  approximation: Sleeper states a tier or a bonus as a *stat* — `pts_allow_1_6`
  arrives as 1.0 in the week it happened, `bonus_rec_yd_100` as 1.0 — so
  Σ(stat × weight) is not a model of how Sleeper scores but the thing itself.
  Measured over a full week of one league, the flat model reproduced all 166
  rostered players to the cent once the vocabulary covered every rule; over the
  twenty-nine keys it covered before, 48 of them were wrong by up to 14.5.

  The fixture is the 2026 week 2 line for `RaiderNation`, league
  1380540443179118592, paired with Sleeper's own `players_points` for that week
  off `/v1/league/{id}/matchups/2`. Rows were chosen for the rule families they
  exercise rather than sampled, plus four that state nothing exotic, so a
  regression names the family it broke. Offline and fixed on purpose — the
  live-data twin belongs in `draft-day.integration`."
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.ingestion.league-import.sleeper :as sleeper-import]
            [draft-day.scoring :as scoring]))

(def ^:private league-scoring
  "Every rule the league states, as Sleeper sends it."
  {:blk_kick 3.0 :bonus_pass_cmp_25 0.0 :bonus_pass_yd_300 2.0 :bonus_pass_yd_400 3.0
   :bonus_rec_yd_100 2.0 :bonus_rec_yd_200 3.0 :bonus_rush_att_20 0.0
   :bonus_rush_rec_yd_100 1.0 :bonus_rush_rec_yd_200 2.0 :bonus_rush_yd_100 2.0
   :bonus_rush_yd_200 3.0 :def_2pt 2.0 :def_3_and_out 0.3 :def_4_and_stop 0.1
   :def_kr_yd 0.0 :def_pr_yd 0.0 :def_st_ff 1.0 :def_st_fum_rec 1.0 :def_st_td 6.0
   :def_td 6.0 :ff 1.0 :fgm_0_19 3.0 :fgm_20_29 3.0 :fgm_30_39 3.0 :fgm_40_49 4.0
   :fgm_50p 5.0 :fgmiss -1.0 :fgmiss_0_19 -3.0 :fgmiss_20_29 -3.0 :fgmiss_30_39 -2.0
   :fgmiss_40_49 -1.0 :fum 0.0 :fum_lost -2.0 :fum_rec 2.0 :fum_rec_td 6.0 :int 2.0
   :pass_2pt 2.0 :pass_cmp 0.0 :pass_cmp_40p 1.0 :pass_fd 0.0 :pass_int -2.0
   :pass_int_td -4.0 :pass_td 4.0 :pass_td_40p 2.0 :pass_td_50p 3.0 :pass_yd 0.04
   :pts_allow_0 10.0 :pts_allow_14_20 1.0 :pts_allow_1_6 7.0 :pts_allow_21_27 0.0
   :pts_allow_28_34 -1.0 :pts_allow_35p -4.0 :pts_allow_7_13 4.0 :rec 1.0
   :rec_20_29 0.0 :rec_2pt 2.0 :rec_30_39 1.0 :rec_40p 2.0 :rec_fd 0.0 :rec_td 6.0
   :rec_td_40p 2.0 :rec_td_50p 3.0 :rec_yd 0.1 :rush_2pt 2.0 :rush_40p 1.0
   :rush_fd 0.0 :rush_td 6.0 :rush_td_40p 2.0 :rush_td_50p 3.0 :rush_yd 0.1
   :sack 1.0 :safe 2.0 :st_ff 1.0 :st_fum_rec 1.0 :st_td 6.0 :xpm 1.0 :xpmiss -2.0
   :yds_allow_0_100 10.0 :yds_allow_100_199 8.0 :yds_allow_200_299 6.0
   :yds_allow_300_349 4.0 :yds_allow_350_399 2.0 :yds_allow_450_499 -2.0
   :yds_allow_500_549 -4.0 :yds_allow_550p -6.0})

(def ^:private golden
  "`:sleeper` is what the league's own scoreboard paid the player for `:line`."
  [;; Points and yards allowed, a 3-and-out rate, a defensive touchdown.
   {:who "New England Patriots (DST)" :sleeper 30.5
    :line {:def_3_and_out 5.0 :def_kr_yd 29.0 :def_pr_yd 40.0 :def_td 1.0 :ff 2.0
           :fum_rec 1.0 :int 1.0 :pts_allow_1_6 1.0 :sack 4.0 :yds_allow_200_299 1.0}}
   {:who "Seattle Seahawks (DST)" :sleeper 17.2
    :line {:def_3_and_out 4.0 :def_kr_yd 47.0 :def_pr_yd 29.0 :int 1.0
           :pts_allow_7_13 1.0 :sack 2.0 :yds_allow_100_199 1.0}}
   ;; A fourth-down stop, weighted at a tenth of a point, is the smallest rule
   ;; the league states and the one a rounding error would hide.
   {:who "San Francisco 49ers (DST)" :sleeper 15.7
    :line {:def_3_and_out 2.0 :def_4_and_stop 1.0 :def_kr_yd 54.0 :def_pr_yd 1.0
           :ff 1.0 :pts_allow_7_13 1.0 :sack 4.0 :yds_allow_200_299 1.0}}
   {:who "Las Vegas Raiders (DST)" :sleeper 20.6
    :line {:def_3_and_out 2.0 :def_kr_yd 35.0 :def_pr_yd 16.0 :ff 4.0 :fum_rec 1.0
           :int 2.0 :pts_allow_14_20 1.0 :sack 3.0 :safe 1.0 :yds_allow_300_349 1.0}}
   ;; A bucket the league prices at zero still has to not be paid for.
   {:who "Tampa Bay Buccaneers (DST)" :sleeper 9.0
    :line {:def_3_and_out 3.0 :def_4_and_stop 1.0 :def_kr_yd 155.0 :def_pr_yd 14.0
           :pts_allow_21_27 1.0 :sack 2.0 :yds_allow_200_299 1.0}}
   ;; Long touchdowns stack: a 50-yard score pays rec_td, rec_td_40p and
   ;; rec_td_50p together, which is what makes the model additive rather than
   ;; a grid to be resolved.
   {:who "Jaxon Smith-Njigba (WR)" :sleeper 52.5
    :line {:bonus_rec_yd_100 1.0 :bonus_rush_rec_yd_100 1.0 :rec 9.0 :rec_40p 1.0
           :rec_fd 5.0 :rec_td 3.0 :rec_td_40p 1.0 :rec_td_50p 1.0 :rec_yd 155.0}}
   {:who "Tre Tucker (WR)" :sleeper 31.9
    :line {:bonus_rec_yd_100 1.0 :bonus_rush_rec_yd_100 1.0 :rec 5.0 :rec_40p 2.0
           :rec_fd 3.0 :rec_td 1.0 :rec_td_40p 1.0 :rec_yd 119.0}}
   ;; Reception-length buckets, and a hundred-yard bonus paid twice — once for
   ;; receiving and once from scrimmage.
   {:who "CeeDee Lamb (WR)" :sleeper 40.3
    :line {:bonus_rec_yd_100 1.0 :bonus_rush_rec_yd_100 1.0 :rec 8.0 :rec_20_29 2.0
           :rec_30_39 2.0 :rec_fd 5.0 :rec_td 2.0 :rec_yd 153.0}}
   {:who "Amon-Ra St. Brown (WR)" :sleeper 39.2
    :line {:bonus_rec_yd_100 1.0 :bonus_rush_rec_yd_100 1.0 :rec 9.0 :rec_20_29 3.0
           :rec_30_39 1.0 :rec_fd 5.0 :rec_td 2.0 :rec_yd 142.0}}
   {:who "Dalton Schultz (TE)" :sleeper 30.0
    :line {:bonus_rec_yd_100 1.0 :bonus_rush_rec_yd_100 1.0 :rec 12.0 :rec_20_29 1.0
           :rec_30_39 1.0 :rec_fd 6.0 :rec_yd 140.0}}
   ;; Passing bonuses, and a fumble the league prices at zero beside a lost one
   ;; it does not.
   {:who "Jared Goff (QB)" :sleeper 31.78
    :line {:bonus_pass_cmp_25 1.0 :bonus_pass_yd_300 1.0 :fum 2.0 :pass_cmp 26.0
           :pass_fd 15.0 :pass_td 4.0 :pass_yd 327.0 :rush_yd 7.0}}
   {:who "Patrick Mahomes (QB)" :sleeper 31.98
    :line {:bonus_pass_cmp_25 1.0 :bonus_pass_yd_300 1.0 :pass_cmp 32.0
           :pass_cmp_40p 1.0 :pass_fd 16.0 :pass_td 3.0 :pass_yd 382.0 :rush_fd 1.0
           :rush_yd 17.0}}
   {:who "Josh Allen (QB)" :sleeper 43.82
    :line {:fum 1.0 :pass_cmp 20.0 :pass_cmp_40p 1.0 :pass_fd 11.0 :pass_td 3.0
           :pass_td_40p 1.0 :pass_yd 248.0 :rush_fd 6.0 :rush_td 2.0 :rush_yd 69.0}}
   ;; A flat miss with no bucket beside it: the kick was blocked from 52 and the
   ;; league states no `fgmiss_50p`, so the only charge is the flat one. Paying a
   ;; bucket here, or suppressing the flat weight the way a *made* kick
   ;; suppresses it, both read 17.0.
   {:who "Harrison Butker (K)" :sleeper 16.0
    :line {:fgm_20_29 1.0 :fgm_30_39 1.0 :fgm_40_49 2.0 :fgmiss 1.0 :xpm 3.0}}
   ;; Nothing exotic, and negative receiving yards, which a bonus threshold
   ;; must not read as a hundred.
   {:who "Bhayshul Tuten (RB)" :sleeper 14.2 :plain? true
    :line {:rec 2.0 :rec_yd -3.0 :rush_fd 3.0 :rush_td 1.0 :rush_yd 65.0}}
   {:who "Ladd McConkey (WR)" :sleeper 6.5 :plain? true
    :line {:rec 3.0 :rec_fd 2.0 :rec_yd 35.0}}])

(def ^:private cent 0.005)

(deftest the-league-loses-no-rule-to-the-import
  ;; The premise of every assertion below: if the vocabulary dropped a rule this
  ;; league states, the totals could only agree by luck.
  (is (= [] (sleeper-import/unsupported-scoring league-scoring))
      "a rule with no key is a rule that scores differently and silently"))

(deftest every-rule-the-league-states-survives-select-keys
  (let [kept (select-keys league-scoring scoring/stat-keys)]
    (is (= (count league-scoring) (count kept)))
    (is (= league-scoring kept) "the import keeps the weights unchanged, too")))

(deftest the-flat-model-reproduces-sleepers-own-numbers
  (let [config (select-keys league-scoring scoring/stat-keys)]
    (doseq [{:keys [who sleeper line]} golden]
      (let [scored (scoring/player-points {:stats line} config)]
        (is (< (abs (- sleeper scored)) cent)
            (str who ": Sleeper paid " sleeper ", we score " scored))))))

(deftest a-weight-the-league-sets-to-zero-pays-nothing
  ;; `:rec_fd` and `:pass_fd` are stated at 0.0 by this league and are real
  ;; rules elsewhere. A vocabulary that quietly priced them would break every
  ;; total above rather than just these.
  (let [config (select-keys league-scoring scoring/stat-keys)]
    (is (zero? (scoring/player-points {:stats {:rec_fd 6.0 :pass_fd 15.0
                                               :pass_cmp 26.0 :rec_20_29 3.0}}
                                      config)))))

(deftest the-fixture-would-catch-a-narrower-vocabulary
  ;; Teeth: scored over the keys that were modelled before this vocabulary
  ;; widened, most of these rows are wrong. Without this the suite would pass
  ;; just as happily on the bug it was written for.
  (let [narrow (select-keys league-scoring
                            [:pass_yd :pass_td :pass_int :pass_2pt :rush_yd :rush_td
                             :rush_2pt :rec :rec_yd :rec_td :rec_2pt :fum_lost :fgm
                             :fgm_0_19 :fgm_20_29 :fgm_30_39 :fgm_40_49 :fgm_50p
                             :fgmiss_40_49 :fgmiss_50p :xpmiss :xpm :blk_kick :sack
                             :int :fum_rec :ff :def_td :safe])
        off?  (fn [{:keys [sleeper line]}]
                (>= (abs (- sleeper (scoring/player-points {:stats line} narrow)))
                    cent))]
    (is (every? off? (remove :plain? golden))
        "every row that states a widened rule was wrong before it was modelled")
    (is (not-any? off? (filter :plain? golden))
        "and the two that state nothing exotic were right all along")
    (testing "and the widened one gets every row"
      (is (every? (fn [{:keys [sleeper line]}]
                    (< (abs (- sleeper (scoring/player-points
                                        {:stats line}
                                        (select-keys league-scoring scoring/stat-keys))))
                       cent))
                  golden)))))
