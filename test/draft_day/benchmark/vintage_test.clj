(ns draft-day.benchmark.vintage-test
  "The gate is the difference between a benchmark and theatre, so it is tested
  against both a clean and a contaminated snapshot, with numbers taken from the
  real seasons it was calibrated on."
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.benchmark.sources.sleeper :as sleeper]
            [draft-day.benchmark.vintage :as vintage]))

(deftest gp-flatness-separates-real-seasons
  (testing "a preseason freeze projects a full season for nearly everyone"
    ;; Shape of Sleeper 2024: 652 of 684 at gp 18, the other 32 being team
    ;; defenses at gp 1. Measured flatness 0.953.
    (let [v {:n 684 :gp-freq {18.0 652 1.0 32}}]
      (is (< (Math/abs (- (vintage/gp-flatness v) (/ 652.0 684.0))) 1e-9))
      (is (vintage/preseason-snapshot? v))))
  (testing "a contaminated snapshot reflects games actually played and spreads"
    ;; Shape of Sleeper 2019, whose measured flatness is 0.215.
    (let [v {:n 1079 :gp-freq {16.0 244 15.0 199 14.0 120 13.0 100 12.0 90}}]
      (is (not (vintage/preseason-snapshot? v))))))

(deftest gate-threshold-sits-in-empty-space
  ;; Real seasons land at 0.95+ (pass) or 0.29 and below (fail); nothing observed
  ;; falls near the 0.85 line, so this is a separation, not a tuned knob.
  (is (vintage/preseason-snapshot? {:n 100 :gp-freq {18.0 95 1.0 5}}))
  (is (not (vintage/preseason-snapshot? {:n 100 :gp-freq {18.0 50 16.0 30 14.0 20}}))))

(deftest empty-vintage-fails-closed
  ;; A season with no projections must not read as "flat" and slip through.
  (is (= 0.0 (vintage/gp-flatness {:n 0 :gp-freq {}})))
  (is (not (vintage/preseason-snapshot? {:n 0 :gp-freq {}})))
  (is (not (vintage/preseason-snapshot? {}))))

(deftest vintage-row-carries-only-allowlisted-keys
  ;; The allowlist must fail CLOSED: Sleeper's projection entries embed a player
  ;; object whose team/injury/experience are of unknown vintage (the record is
  ;; rewritten after the season), so none of it may reach the board. Building the
  ;; row key-by-key means a new upstream field cannot silently join.
  (let [leaky {:player-id "1" :player-name "A B" :position "WR"
               :stats {:rec 10.0}
               :sleeper/injury-status "Questionable"   ; current vintage - leaks
               :sleeper/years-exp 7                    ; current vintage - leaks
               :team "SEA"                             ; may be current - leaks
               :bye 5}
        row   (vintage/vintage-row {:player leaky :gsis-id "00-1" :adp 12.5})]
    (is (= #{:player-id :player-name :position :gsis-id :stats :adp}
           (set (keys row))))
    (is (not (contains? row :sleeper/injury-status)))
    (is (not (contains? row :sleeper/years-exp)))
    (is (not (contains? row :team)))))

(deftest vintage-row-attaches-prior-usage-and-outcome-when-present
  (let [row (vintage/vintage-row
             {:player  {:player-id "1" :player-name "A B" :position "RB" :stats {}}
              :gsis-id "00-1"
              :adp     30.0
              :usage   {:wopr 0.5 :target-share 0.2 :air-yards-share 0.1
                        :targets-per-game 4.0 :carries-per-game 12.0
                        :ppg 14.0 :games 16.0}
              :outcome {:stats {:rush_yd 1200.0} :games 17.0}})]
    (is (= 0.5 (:prior/wopr row)))
    (is (= 14.0 (:prior/ppg row)))
    (is (= 17.0 (:actual/games row)))
    (is (= 1200.0 (get-in row [:actual/stats :rush_yd])))))

(deftest sleeper-adp-is-read-from-the-per-format-bundle
  ;; Ingestion moved ADP under :vendor/by-format. The harness kept reading the
  ;; flat :sleeper/adp, which resolved to nil for every player — and the `adp`
  ;; guard in `assemble` turned that into an empty board rather than an error,
  ;; so `--adp-source sleeper` reported a season with no players in it.
  (let [row {:player-id "4034"
             :vendor/by-format {:standard {:sleeper/adp 16.8}
                                :half-ppr {:sleeper/adp 11.2}
                                :ppr      {:sleeper/adp 8.1}}}]
    (is (= 8.1 (sleeper/adp-of row))
        "PPR, matching the flat key's PPR-preferred value before the move")
    (is (nil? (sleeper/adp-of {:player-id "x"}))
        "a player the vendor never priced stays nil")
    (is (nil? (sleeper/adp-of {:player-id "x" :sleeper/adp 3.0}))
        "and the old flat key is not silently honoured")))
