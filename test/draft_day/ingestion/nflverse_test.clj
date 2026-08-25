(ns draft-day.ingestion.nflverse-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.ingestion.nflverse :as nflverse]))

(def ^:private chase
  {"player_id" "00-0036900" "position" "WR" "games" "16"
   "targets" "185" "receptions" "125" "target_share" "0.304276315789474"})

(def ^:private rookie-shaped
  ;; A row the source has, but with the usage columns blank — nflverse writes
  ;; both "" and "NA" for missing.
  {"player_id" "00-0099999" "position" "RB" "games" "3"
   "targets" "" "receptions" "NA" "target_share" ""})

(deftest counts-stay-season-totals-and-the-share-stays-a-rate
  (let [[gsis cols] (nflverse/row->usage 2025 chase)]
    (is (= "00-0036900" gsis))
    (testing "counts are season totals, NOT divided by games"
      (is (= 185.0 (:nflverse/prior-targets cols)))
      (is (= 125.0 (:nflverse/prior-receptions cols))))
    (testing "target share is already a season rate and passes through untouched"
      ;; 0.304 / 16 games would be 0.019 — the trap this asserts against.
      (is (< 0.304 (:nflverse/prior-target-share cols) 0.305)))
    (is (= 16.0 (:nflverse/prior-games cols)))
    (is (= 2025 (:nflverse/prior-season cols)))))

(deftest a-blank-cell-is-absent-not-zero
  ;; Zero-filling would print a confident 0 for a column the source has no
  ;; opinion about, which on the board is indistinguishable from real futility.
  (let [[_ cols] (nflverse/row->usage 2025 rookie-shaped)]
    (is (not (contains? cols :nflverse/prior-targets)))
    (is (not (contains? cols :nflverse/prior-receptions)))
    (is (not (contains? cols :nflverse/prior-target-share)))
    (is (= 3.0 (:nflverse/prior-games cols)) "the columns it does have survive")))

(deftest non-fantasy-positions-are-dropped
  ;; nflverse publishes every player who took a snap. Keeping the linemen and
  ;; punters would bury the join's hit rate and trip the per-position warning.
  (is (nil? (nflverse/row->usage 2025 (assoc chase "position" "CB"))))
  (is (nil? (nflverse/row->usage 2025 (assoc chase "position" "LS"))))
  (is (some? (nflverse/row->usage 2025 (assoc chase "position" "TE")))))

(deftest a-row-with-no-gsis-id-has-nothing-to-join-on
  (is (nil? (nflverse/row->usage 2025 (assoc chase "player_id" ""))))
  (is (nil? (nflverse/row->usage 2025 (dissoc chase "player_id")))))

(deftest enrichment-and-positions-agree-on-which-rows-count
  (let [rows [chase (assoc chase "player_id" "00-0000001" "position" "P")]]
    (is (= #{"00-0036900"} (set (keys (nflverse/enrichment 2025 rows)))))
    (is (= {"00-0036900" "WR"} (nflverse/row-positions rows)))))

(deftest parses-csv-text-into-header-keyed-maps
  (let [rows (nflverse/parse-csv "player_id,position,targets\n00-0036900,WR,185\n")]
    (is (= [{"player_id" "00-0036900" "position" "WR" "targets" "185"}] rows))))
