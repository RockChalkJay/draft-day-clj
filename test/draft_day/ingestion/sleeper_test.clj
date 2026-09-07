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
            :rec_fd 2.1}}                                  ; rec_fd is not scored
   {:player_id "4034" :team "GB" :opponent nil :week 1
    :player {:first_name "Bye" :last_name "Guy" :position "RB"}
    :stats {:gp 0.0}}                                      ; no pts_ppr -> excluded
   {:player_id "ARI" :team "ARI" :opponent "LAC" :week 1 :updated_at 1788755441000
    :player {:first_name "Arizona" :last_name "Cardinals" :position "DEF"}
    :stats {:sack 2.4 :int 0.8 :pts_ppr 7.1}}])

(deftest weekly-keeps-only-projected-players
  (let [by-id (sleeper/weekly-by-id sample-weekly #{"ATL"})]
    (is (= #{"9509" "ARI"} (set (keys by-id))))            ; the bye entry drops out
    (is (= 18.4 (get-in sample-weekly [0 :stats :pts_ppr])))
    ;; pts_ppr gates but is never carried — points come from :stats under the
    ;; league's own weights, exactly as the season line does.
    (is (nil? (get-in by-id ["9509" :stats :pts_ppr])))
    (is (nil? (get-in by-id ["9509" :stats :rec_fd])))))

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
