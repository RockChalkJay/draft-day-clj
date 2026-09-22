(ns draft-day.ingestion.sleeper-test
  (:require [clojure.test :refer [deftest is]]
            [draft-day.ingestion.sleeper :as sleeper]
            [draft-day.scoring :as scoring]))

;; Fixtures shaped exactly like live Sleeper /projections entries.
(def ^:private sample-entries
  [{:player_id "9509" :team "ATL"
    :player {:first_name "Bijan" :last_name "Robinson" :position "RB" :years_exp 3}
    :stats {:rush_yd 1372.0 :rush_td 9.0 :rec 64.0 :rec_yd 537.0 :rec_td 3.0 :fum_lost 2.0
            :pts_ppr 324.9 :pts_half_ppr 292.9 :pts_std 260.9
            :adp_ppr 1.4 :adp_half_ppr 2.6 :adp_std 4.1}}
   {:player_id "10211" :team nil
    :player {:first_name "Camerun" :last_name "Peoples" :position "RB" :years_exp 1}
    :stats {:adp_ppr 999.0 :gp 0.0}}                       ; no pts_ppr -> excluded
   {:player_id "ARI" :team "ARI"
    :player {:first_name "Arizona" :last_name "Cardinals" :position "DEF"}
    :stats {:sack 40.0 :int 12.0 :pts_ppr 120.0 :pts_std 120.0 :adp_ppr 150.0}}
   {:player_id "99" :team "X"
    :player {:first_name "Some" :last_name "Corner" :position "CB"}
    :stats {:pts_ppr 50.0}}])                              ; non-fantasy pos -> excluded

(deftest universe-filters-and-normalizes
  (let [u     (sleeper/universe-from-entries sample-entries)
        by-id (into {} (map (juxt :player-id identity) u))]
    (is (= 2 (count u)))                                   ; Bijan + ARI only
    (let [bijan (by-id "9509")]
      (is (= "RB" (:position bijan)))
      (is (= "Bijan Robinson" (:player-name bijan)))
      (is (= 1372.0 (get-in bijan [:stats :rush_yd])))
      ;; ADP is carried per format rather than collapsed to a PPR-preferred
      ;; number; the three diverge enough to matter (Amon-Ra St. Brown was 8.1
      ;; PPR against 16.8 standard for 2026).
      (is (= {:ppr       {:sleeper/adp 1.4}
              :half-ppr  {:sleeper/adp 2.6}
              :standard  {:sleeper/adp 4.1}}
             (:vendor/by-format bijan))))
    (is (= "DST" (:position (by-id "ARI"))))))             ; DEF -> DST

(deftest the-sparse-season-line-is-completed-from-the-weekly-one
  ;; A defense is the case that matters: the season line projects it no
  ;; points-allowed bucket, no safety and no forced fumble at all, while the
  ;; weekly line projects all three. Left sparse, widening the vocabulary lifts
  ;; receivers — whose buckets the season line does carry — and leaves defenses
  ;; and quarterbacks flat, which replacement and VORP read as a real gap.
  (let [season {:player_id "ARI" :team "ARI"
                :player {:first_name "Arizona" :last_name "Cardinals" :position "DEF"}
                :stats {:sack 40.0 :int 12.0 :pts_ppr 120.0}}
        weekly {:sack 2.5 :int 0.75 :ff 0.8 :safe 0.1 :pts_allow_21_27 1.0
                :pts_ppr 8.0}
        p (sleeper/normalize-entry season weekly)
        games (/ 120.0 8.0)]
    (is (= 15.0 games))
    (is (= 40.0 (get-in p [:stats :sack]))
        "a key the season line states is never overwritten")
    (is (= 12.0 (get-in p [:stats :int])))
    (is (< 11.9 (get-in p [:stats :ff]) 12.1) "0.8 a week over fifteen games")
    (is (< 14.9 (get-in p [:stats :pts_allow_21_27]) 15.1)
        "a bucket Sleeper one-hots reads as the weeks it expects to land there")
    (is (nil? (get-in p [:stats :pts_ppr])) "still not a scoring key")))

(deftest a-made-field-goal-is-never-filled-from-the-weekly-line
  ;; The two horizons disagree about this grid rather than covering different
  ;; parts of it: Aubrey's season line carries nine makes from 40-49, eight from
  ;; 50+ and nothing under forty, and his weekly line carries every band under
  ;; fifty and no 50+ at all. Filled, he read as thirty-one makes against the
  ;; seventeen the season line states and scored 160 against Sleeper's own 116 —
  ;; where the sparse line scores 118. `summed-fgm` already reconciles the grid.
  (let [season {:player_id "K1" :team "DAL"
                :player {:first_name "Brandon" :last_name "Aubrey" :position "K"}
                :stats {:fgm_40_49 9.0 :fgm_50p 8.0 :xpm 42.0 :pts_ppr 116.0}}
        weekly {:fgm 2.09 :fgm_20_29 0.36 :fgm_30_39 0.46 :fgm_40_49 0.46
                :fgmiss_30_39 0.1 :pts_ppr 6.8}
        st (:stats (sleeper/normalize-entry season weekly))]
    (is (= 9.0 (:fgm_40_49 st)) "the season line's own band is untouched")
    (is (= 8.0 (:fgm_50p st)))
    (is (nil? (:fgm_20_29 st)) "and no band is invented from the weekly line")
    (is (nil? (:fgm_30_39 st)))
    (is (= 17.0 (:fgm st)) "still the summed floor, not the weekly rate")
    (is (some? (:fgmiss_30_39 st))
        "a miss is not exempt — the season line simply does not project it")))

(deftest a-player-with-no-weekly-line-keeps-the-season-line-alone
  ;; Sleeper projects a fraction of the universe in any one week, so this is
  ;; most of the board and it must not change.
  (let [with    (sleeper/normalize-entry (first sample-entries) nil)
        without (sleeper/normalize-entry (first sample-entries))]
    (is (= (:stats with) (:stats without)))
    (is (= 1372.0 (get-in with [:stats :rush_yd])))))

(deftest one-week-is-never-stretched-past-a-season
  ;; The two horizons disagree hard about a marginal player, and an unclamped
  ;; ratio reached thirty on the live line — which would have tripled a bonus
  ;; key rather than filling it.
  (is (= 17.0 (sleeper/implied-games {:pts_ppr 300.0} {:pts_ppr 1.0})))
  (is (= 15.0 (sleeper/implied-games {:pts_ppr 120.0} {:pts_ppr 8.0})))
  (is (nil? (sleeper/implied-games {:pts_ppr 120.0} {:pts_ppr 0.0}))
      "a week projected at nothing says nothing about a season")
  (is (nil? (sleeper/implied-games {:pts_ppr 120.0} nil)))
  (is (nil? (sleeper/implied-games {} {:pts_ppr 8.0}))))

(deftest a-completed-line-cannot-lose-a-stat-it-had
  ;; The merge runs the other way round from the obvious one: the season line
  ;; wins every key it states, and the weekly line only fills silence.
  (let [season {:rush_yd 1000.0 :rec 50.0}
        weekly {:rush_yd 99.0 :rec 9.0 :rush_40p 0.5}]
    (is (= {:rush_yd 1000.0 :rec 50.0 :rush_40p 5.0}
           (sleeper/complete-season-line season weekly 10.0))))
  (is (= {:rush_yd 1000.0}
         (sleeper/complete-season-line {:rush_yd 1000.0} {:rush_40p 0.5} nil))
      "no games, no fill"))

(deftest adp-sentinel-999-becomes-nil
  (let [entry {:player_id "p" :team "X"
               :player {:first_name "A" :last_name "B" :position "WR"}
               :stats {:pts_ppr 100.0 :adp_ppr 999.0 :adp_half_ppr 999.0 :adp_std 999.0
                       :rec 50.0 :rec_yd 700.0}}]
    (is (= {} (:vendor/by-format (sleeper/normalize-entry entry))))))

(deftest a-format-missing-its-adp-simply-has-none
  (let [entry {:player_id "p" :team "X"
               :player {:first_name "A" :last_name "B" :position "WR"}
               :stats {:pts_ppr 100.0 :adp_ppr 12.0 :adp_std 999.0 :rec 50.0}}]
    (is (= {:ppr {:sleeper/adp 12.0}}
           (:vendor/by-format (sleeper/normalize-entry entry))))))

(deftest scoring-engine-matches-sleeper-precomputed
  ;; Cross-check: our scoring on Sleeper :stats lands near Sleeper's own pts_ppr,
  ;; validating the stat-key alignment (we don't model every scoring bonus).
  (let [bijan (sleeper/normalize-entry (first sample-entries))
        pts   (scoring/player-points bijan (:ppr scoring/presets))]
    (is (< (Math/abs (- pts 324.9)) 25.0))))

;; ---- bye derivation from the schedule ----

;; Three teams over a 3-week season, each missing exactly one week (its bye):
;; ATL byes wk1, TB byes wk2, GB byes wk3.
(def ^:private sample-games
  [{:home "TB"  :away "GB"  :week 1}                       ; ATL idle -> bye 1
   {:home "GB"  :away "ATL" :week 2}                       ; TB idle  -> bye 2
   {:home "ATL" :away "TB"  :week 3}])                     ; GB idle  -> bye 3

(deftest schedule-derives-bye-per-team
  (let [byes (sleeper/schedule->byes sample-games)]
    (is (= {"ATL" 1 "TB" 2 "GB" 3} byes))))

(deftest schedule-omits-teams-without-a-single-bye
  ;; A team that plays every week (no missing week) gets no entry.
  (let [byes (sleeper/schedule->byes
              (conj sample-games {:home "ATL" :away "GB" :week 1}))] ; ATL now plays wk1 too
    (is (not (contains? byes "ATL")))                       ; 0 missing weeks -> omitted
    (is (= 2 (byes "TB")))))

(deftest assoc-byes-keys-on-team
  (let [universe [{:player-id "1" :team "ATL" :bye nil}
                  {:player-id "2" :team "GB"  :bye nil}
                  {:player-id "3" :team nil   :bye nil}]   ; free agent -> stays nil
        result   (into {} (map (juxt :player-id :bye))
                       (sleeper/assoc-byes universe {"ATL" 1 "GB" 3}))]
    (is (= {"1" 1 "2" 3 "3" nil} result))))

;; ---- weekly projections ----
;; Shaped like live /projections/nfl/{season}/{week} entries: same envelope as
;; the season line plus :opponent, :week and :updated_at.
(def ^:private sample-weekly
  [{:player_id "9509" :team "ATL" :opponent "TB" :week 1 :updated_at 1788755441199
    :player {:first_name "Bijan" :last_name "Robinson" :position "RB"}
    :stats {:rush_yd 81.0 :rush_td 0.6 :rec 3.8 :rec_yd 32.0 :pts_ppr 18.4
            :rec_fd 2.1                                    ; scored
            :rec_tgt 5.2}}                                 ; a usage column, not a rule
   {:player_id "4034" :team "GB" :opponent nil :week 1
    :player {:first_name "Bye" :last_name "Guy" :position "RB"}
    :stats {:gp 0.0}}                                      ; no pts_ppr -> excluded
   {:player_id "ARI" :team "ARI" :opponent "LAC" :week 1 :updated_at 1788755441000
    :player {:first_name "Arizona" :last_name "Cardinals" :position "DEF"}
    :stats {:sack 2.4 :int 0.8 :pts_ppr 7.1}}])

(deftest weekly-keeps-only-projected-players
  (let [by-id (sleeper/weekly-by-id sample-weekly #{"ATL"})]
    (is (= #{"9509" "ARI"} (set (keys by-id))))            ; the bye entry drops out
    ;; pts_ppr gates but is never carried — points come from :stats under the
    ;; league's own weights, exactly as the season line does.
    (is (nil? (get-in by-id ["9509" :stats :pts_ppr])))
    (is (= 2.1 (get-in by-id ["9509" :stats :rec_fd]))
        "a rule the league can state is carried, whether or not it prices it")
    (is (nil? (get-in by-id ["9509" :stats :rec_tgt]))
        "a column no scoring rule names stays out of the line")))

(deftest weekly-carries-opponent-and-side
  (let [by-id (sleeper/weekly-by-id sample-weekly #{"ATL"})]
    (is (= "TB" (get-in by-id ["9509" :opponent])))
    (is (true? (get-in by-id ["9509" :home?])))            ; ATL in the home set
    (is (= "LAC" (get-in by-id ["ARI" :opponent])))
    (is (false? (get-in by-id ["ARI" :home?])))))

(deftest weekly-line-scores-under-league-weights
  ;; The line is scored the same way the season line is, so it is the *league's*
  ;; number rather than the vendor's: half-PPR here is
  ;; 81*.1 + .6*6 + 3.8*.5 + 32*.1 = 8.1 + 3.6 + 1.9 + 3.2.
  (let [line (get (sleeper/weekly-by-id sample-weekly #{}) "9509")
        pts  #(scoring/player-points line (scoring/resolve-config %))]
    (is (< (abs (- 16.8 (pts :half-ppr))) 1e-9))
    (is (< (abs (- 18.7 (pts :ppr))) 1e-9))                ; +1.9 for full PPR
    (is (< (abs (- 14.9 (pts :standard))) 1e-9))))         ; -1.9 for no PPR

(deftest weekly-carries-its-own-timestamp
  ;; These revise through the week; a consumer that cannot date the number will
  ;; imply it is current.
  (let [by-id (sleeper/weekly-by-id sample-weekly #{})]
    (is (= 1788755441199 (get-in by-id ["9509" :updated-at])))))

(deftest home-teams-scopes-to-the-week
  (let [games [{:home "ATL" :away "TB" :week 1}
               {:home "GB"  :away "CHI" :week 2}]]
    (is (= #{"ATL"} (sleeper/home-teams games 1)))
    (is (= #{"GB"}  (sleeper/home-teams games 2)))
    (is (= #{}      (sleeper/home-teams games 3)))))

(deftest weekly-side-is-unknown-rather-than-away-without-a-schedule
  ;; The schedule only supplies vs/@, and it degrades on its own so a failure
  ;; there cannot cost the whole projection. An empty home set would read as
  ;; "everyone is away"; nil says the side is not known.
  (let [by-id (sleeper/weekly-by-id sample-weekly nil)]
    (is (nil? (get-in by-id ["9509" :home?])))
    (is (= "TB" (get-in by-id ["9509" :opponent])))        ; still known
    (is (some? (get-in by-id ["9509" :stats])))))          ; still projected

;; ---- kickers get their field goals back ----

(deftest a-kicker-without-a-published-total-gets-one-from-the-buckets
  ;; Sleeper's season line carries no :fgm at all — 0 of 45 kickers — so a flat
  ;; weight multiplied nothing and every kicker scored on extra points alone.
  ;; The buckets are carried through as well, for a league that prices them.
  (let [entry {:player_id "K1" :team "DAL"
               :player {:first_name "Brandon" :last_name "Aubrey" :position "K"}
               :stats {:fgm_40_49 9.0 :fgm_50p 8.0 :fgm_yds 841.0 :xpm 42.0
                       :pts_ppr 116.0}}
        st    (:stats (sleeper/normalize-entry entry))]
    (is (= 17.0 (:fgm st)) "the two published buckets, summed")
    (is (= 9.0 (:fgm_40_49 st)))
    (is (= 8.0 (:fgm_50p st)))
    (is (= 42.0 (:xpm st)))
    ;; 9*4 + 8*5 + 42 = 118, against 42 before any of this and Sleeper's own
    ;; 116. The distance grid is what closes that gap: a flat 3.0 underpaid
    ;; every kick these two buckets hold.
    (is (< (abs (- 118.0 (scoring/player-points {:stats st}
                                                (scoring/resolve-config :half-ppr))))
           1e-9))))

(deftest every-season-bucket-is-a-scoring-bucket
  ;; `summed-fgm` sums the season line's buckets into a flat `:fgm`. They differ
  ;; from `scoring/fg-buckets` only because that line has no sub-forty column —
  ;; a bucket outside the scoring set would sum a kick nothing prices, and a
  ;; whole grid drifting apart is how Aubrey came out at 42 instead of 116.
  (is (every? (set scoring/fg-buckets) @#'sleeper/fgm-buckets))
  (is (not= (set scoring/fg-buckets) (set @#'sleeper/fgm-buckets))
      "the day the season line gains a sub-forty bucket, this sum has to grow"))

(deftest a-published-total-is-never-overruled
  ;; The weekly endpoint does send :fgm; this only ever fills a gap.
  (let [entry {:player_id "K1" :team "DAL"
               :player {:first_name "B" :last_name "A" :position "K"}
               :stats {:fgm 1.68 :fgm_40_49 0.41 :fgm_50p 0.2 :xpm 2.46
                       :pts_ppr 6.26}}]
    (is (= 1.68 (get-in (sleeper/normalize-entry entry) [:stats :fgm])))))

(deftest a-kicker-with-no-buckets-at-all-gets-no-invented-total
  ;; A floor, not a guess: with nothing published there is nothing to sum.
  (let [entry {:player_id "K2" :team "X"
               :player {:first_name "No" :last_name "Legs" :position "K"}
               :stats {:xpm 12.0 :pts_ppr 12.0}}]
    (is (nil? (get-in (sleeper/normalize-entry entry) [:stats :fgm])))))

(deftest nobody-but-a-kicker-is-touched
  (let [rb (sleeper/normalize-entry (first sample-entries))]
    (is (nil? (get-in rb [:stats :fgm])))))
