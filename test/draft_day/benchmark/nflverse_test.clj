(ns draft-day.benchmark.nflverse-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.benchmark.sources.nflverse :as nflverse]
            [draft-day.rankings.scoring :as scoring]))

(defn row
  "An nflverse stats_player_reg row. Only the columns under test are set; the
  rest default to \"\" the way a real sparse row does."
  [overrides]
  (merge {"player_id" "00-0000001" "position" "WR" "recent_team" "SEA" "games" "10"}
         overrides))

(deftest stat-columns-map-onto-the-scoring-engines-keys
  ;; The point of the mapping is that realized outcomes can go through the very
  ;; same scoring/player-points the live board uses. Hand-computed under PPR:
  ;;   pass_yd  0.04 * 300 =  12
  ;;   pass_td  4.00 *   2 =   8
  ;;   rush_yd  0.10 *  20 =   2
  ;;   rush_td  6.00 *   1 =   6
  ;;   rec      1.00 *   5 =   5
  ;;   rec_yd   0.10 *  50 =   5
  ;;   fum_lost -2.00 *  1 =  -2
  ;;                        ----
  ;;                          36
  (let [r (row {"passing_yards" "300" "passing_tds" "2"
                "rushing_yards" "20"  "rushing_tds" "1"
                "receptions"    "5"   "receiving_yards" "50"
                "fumbles_lost_total" "1"})
        stats (nflverse/row->stats r)]
    (is (= 300.0 (:pass_yd stats)))
    (is (= 2.0   (:pass_td stats)))
    (is (= 5.0   (:rec stats)))
    (is (= 1.0   (:fum_lost stats)))
    (is (= 36.0 (scoring/player-points {:stats stats} (:ppr scoring/presets))))))

(deftest missing-cells-score-zero-not-crash
  ;; nflverse writes both "" and "NA" for missing; a QB row has no receiving
  ;; columns at all. None of that may throw.
  (let [stats (nflverse/row->stats (row {"receptions" "NA" "receiving_yards" ""}))]
    (is (= 0.0 (:rec stats)))
    (is (= 0.0 (:rec_yd stats)))
    (is (= 0.0 (scoring/player-points {:stats stats} (:ppr scoring/presets))))))

(deftest shares-are-rates-and-counts-are-per-game
  ;; REGRESSION TEST for a real bug made while exploring this data.
  ;; target_share / air_yards_share / wopr are already SEASON RATES in
  ;; stats_player_reg (Ja'Marr Chase 2024: target_share 0.279). Dividing them by
  ;; games — the natural-looking move, since they sit beside the count columns —
  ;; inflates the usage of anyone who missed time and corrupts cross-player
  ;; comparison. Counts are the opposite and must be normalized.
  (let [u (nflverse/row->usage (row {"games" "10"
                                     "wopr" "0.6" "target_share" "0.25"
                                     "air_yards_share" "0.30"
                                     "targets" "50" "carries" "20"
                                     "fantasy_points_ppr" "150"}))]
    (testing "rates pass through untouched"
      (is (= 0.6  (:wopr u)))
      (is (= 0.25 (:target-share u)))
      (is (= 0.30 (:air-yards-share u))))
    (testing "counts are divided by games"
      (is (= 5.0  (:targets-per-game u)))   ; 50 / 10
      (is (= 2.0  (:carries-per-game u)))   ; 20 / 10
      (is (= 15.0 (:ppg u))))))             ; 150 / 10

(deftest zero-games-does-not-divide-by-zero
  (let [u (nflverse/row->usage (row {"games" "0" "targets" "5" "fantasy_points_ppr" "10"}))]
    (is (= 0.0 (:targets-per-game u)))
    (is (= 0.0 (:ppg u)))))

(deftest outcome-carries-games-for-the-per-game-truth
  (let [o (nflverse/row->outcome (row {"games" "17" "receptions" "100"}))]
    (is (= "00-0000001" (:gsis-id o)))
    (is (= "WR" (:position o)))
    (is (= 17.0 (:games o)))
    (is (= 100.0 (get-in o [:stats :rec])))))
