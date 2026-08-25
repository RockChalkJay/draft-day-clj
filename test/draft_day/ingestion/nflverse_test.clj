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

;; ---- availability (the second question this one fetch answers) ----

(defn- row [gsis games & [pos]]
  {"player_id" gsis "position" (or pos "RB") "games" games})

(deftest season-length-follows-the-2021-expansion
  ;; The app only asks about recent seasons, but the benchmark harness reaches
  ;; back past the boundary, and a hardcoded 17 hands it a free missed game for
  ;; every player in every older season.
  (is (= 16 (nflverse/games-in-season 2020)))
  (is (= 17 (nflverse/games-in-season 2021)))
  (is (= 17 (nflverse/games-in-season 2025))))

(deftest games-are-clamped-to-the-season-they-were-played-in
  ;; The 2025 file really does carry an 18. Left alone it becomes a negative
  ;; missed-game total that averages away somebody else's real absence.
  (is (= ["00-1" 17.0] (nflverse/row->games 2025 (row "00-1" "18"))))
  (is (= ["00-1" 16.0] (nflverse/row->games 2019 (row "00-1" "17"))))
  (is (= ["00-1" 4.0]  (nflverse/row->games 2025 (row "00-1" "4")))))

(deftest a-row-with-no-games-figure-joins-nothing
  ;; Same BLANK-IS-NOT-ZERO rule as the usage columns: absent, not 0.
  (is (nil? (nflverse/row->games 2025 (row "00-1" ""))))
  (is (nil? (nflverse/row->games 2025 (row "00-1" "NA"))))
  (is (nil? (nflverse/row->games 2025 (row "" "17"))))
  (is (nil? (nflverse/row->games 2025 (row "00-1" "17" "CB")))))

(deftest availability-carries-the-fetched-window-not-just-the-hits
  ;; A gap means "he played none" only if that season was fetched at all, so the
  ;; season lengths ride along on every player. Without them a consumer cannot
  ;; tell a lost season from a lost download.
  (let [out (nflverse/availability {2024 [(row "00-1" "17")]
                                    2025 [(row "00-1" "4") (row "00-2" "17")]})]
    (is (= {2024 17.0 2025 4.0} (:nflverse/games-by-season (get out "00-1"))))
    (testing "a player with no row in a fetched season is simply absent from it"
      (is (= {2025 17.0} (:nflverse/games-by-season (get out "00-2")))))
    (testing "every player carries which seasons were fetched, and how long each was"
      (is (= {2024 17 2025 17} (:nflverse/games-seasons (get out "00-1"))))
      (is (= {2024 17 2025 17} (:nflverse/games-seasons (get out "00-2")))))))

(deftest availability-sizes-an-older-season-correctly
  ;; The lengths map is what lets `rankings.injury` size a missing season without
  ;; importing this namespace's calendar.
  (is (= {2020 16 2021 17}
         (:nflverse/games-seasons
          (get (nflverse/availability {2020 [(row "00-1" "16")]
                                       2021 [(row "00-1" "17")]}) "00-1")))))

(deftest a-player-in-no-fetched-season-gets-no-columns-at-all
  ;; Not a window full of zeroes — that is the shape that would make a
  ;; never-active depth player read as maximally fragile.
  (is (nil? (get (nflverse/availability {2025 [(row "00-1" "17")]}) "00-2"))))
