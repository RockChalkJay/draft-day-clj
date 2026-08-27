(ns draft-day.ingestion.nflverse-test
  (:require [clojure.test :refer [deftest is testing]]
            [jsonista.core :as json]
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

;; ---- what counts as having fetched a season ----
;;
;; `availability` treats every season it is handed as one that really happened,
;; and charges each player the difference between its length and what he played.
;; That makes `fetch-season-rows` the gate: anything it lets through as a
;; non-nil is a season the whole league is measured against, so every way a
;; fetch can fail short of a clean 404 has to collapse to nil here.

(deftest a-season-that-throws-does-not-take-the-source-down-with-it
  ;; The seasons go out concurrently and the thunks are not individually
  ;; wrapped, so an escaping exception would nil the entire nflverse result —
  ;; usage columns and every Risk bar, board-wide, for the cache window.
  (with-redefs [nflverse/http-get-string
                (fn [_] (throw (java.io.IOException. "connection reset")))]
    (is (nil? (nflverse/fetch-season-rows 2024)))))

(deftest a-200-with-an-empty-body-is-a-miss-not-an-empty-season
  ;; The shape that matters: survive as [] and the season is recorded as
  ;; fetched-and-nobody-played, which renders a 17/17/17 veteran as Brittle.
  (with-redefs [nflverse/http-get-string (fn [_] "")]
    (is (nil? (nflverse/fetch-season-rows 2024)))))

(deftest a-200-carrying-the-wrong-file-is-a-miss-too
  ;; A release URL that redirects to an error page parses without throwing and
  ;; yields rows with no GSIS id — indistinguishable downstream from a season
  ;; nobody played.
  (with-redefs [nflverse/http-get-string (fn [_] "<html><body>Not Found</body></html>")]
    (is (nil? (nflverse/fetch-season-rows 2024))))
  (testing "a real stats file is not rejected by the same check"
    (with-redefs [nflverse/http-get-string
                  (fn [_] "player_id,position,games,targets\n00-1,WR,17,120\n")]
      (is (= 1 (count (nflverse/fetch-season-rows 2024)))))))

;; ---- per-season stat lines (the third question this one fetch answers) ----

(def ^:private chase-line
  ;; Same row shape as the usage tests, with the production columns filled in.
  (assoc chase
         "passing_yards" "0" "passing_tds" "0"
         "rushing_yards" "32" "rushing_tds" "0"
         "receiving_yards" "1708" "receiving_tds" "17"))

(deftest a-season-line-speaks-sleeper-stat-keys
  ;; The tile puts a realized season beside a projected :stats line, so the two
  ;; have to be keyed the same way or the view has to translate between them.
  (let [[gsis stats] (nflverse/row->season-line chase-line)]
    (is (= "00-0036900" gsis))
    (is (= {:pass_yd 0.0 :pass_td 0.0
            :rush_yd 32.0 :rush_td 0.0
            :rec 125.0 :rec_yd 1708.0 :rec_td 17.0}
           stats))))

(deftest a-season-line-carries-no-games-count
  ;; :nflverse/games-by-season already answers that, clamped to the season's
  ;; length. A second, unclamped copy would disagree with the first for exactly
  ;; the players the clamp exists for — the 2025 file's 18-game rows.
  (let [[_ stats] (nflverse/row->season-line (assoc chase-line "games" "18"))]
    (is (not (contains? stats :games)))
    (is (not-any? #(= 18.0 %) (vals stats)))))

(deftest a-blank-stat-column-is-absent-from-the-line-not-zero
  ;; Worse here than in the usage columns: a line is read as a trend across three
  ;; seasons, so one zero-filled column is not a wrong number in isolation, it is
  ;; a decline the player never had.
  (let [[_ stats] (nflverse/row->season-line
                   (assoc chase-line "rushing_yards" "" "receiving_tds" "NA"))]
    (is (not (contains? stats :rush_yd)))
    (is (not (contains? stats :rec_td)))
    (is (= 1708.0 (:rec_yd stats)) "the columns it does have survive")))

(deftest a-row-with-no-stat-columns-at-all-yields-no-line
  ;; An empty line would render as a season the player was present for and did
  ;; nothing in, which is not what a row missing the columns means.
  (is (nil? (nflverse/row->season-line (row "00-1" "17"))))
  (is (nil? (nflverse/row->season-line (assoc chase-line "player_id" ""))))
  (is (nil? (nflverse/row->season-line (assoc chase-line "position" "CB")))))

(deftest history-is-a-season-ordered-vector-not-a-season-keyed-map
  ;; The shape is load-bearing: this is the one nflverse column read in the
  ;; browser, and it arrives as JSON. A map keyed by 2023 is written as the
  ;; string "2023" and decoded as the keyword :2023, so every lookup by the
  ;; integer season silently returns nil on the other side.
  (let [out (nflverse/history
             {2025 [chase-line (assoc chase-line "player_id" "00-0000002")]
              2024 [(assoc chase-line "receiving_yards" "1216")]})
        h   (get-in out ["00-0036900" :nflverse/history])]
    (is (vector? h))
    (testing "oldest season first, regardless of the order the seasons arrive in"
      (is (= [2024 2025] (mapv :season h))))
    (is (= 1216.0 (get-in h [0 :stats :rec_yd])))
    (is (= 1708.0 (get-in h [1 :stats :rec_yd])))
    (testing "a player with no row in a fetched season is simply absent from it"
      (is (= [2025] (mapv :season (get-in out ["00-0000002" :nflverse/history])))))
    (testing "a player in no fetched season gets no history key at all"
      (is (nil? (get out "00-0000009"))))))

(deftest a-season-keyed-history-would-not-survive-json
  ;; Guarding the reason for the vector, not just the vector: this asserts on the
  ;; actual round trip the browser performs, so a well-meaning change back to a
  ;; season-keyed map fails here rather than in a blank tile.
  (let [h (get-in (nflverse/history {2025 [chase-line]})
                  ["00-0036900" :nflverse/history])
        round-tripped (json/read-value (json/write-value-as-string h)
                                       (json/object-mapper {:decode-key-fn keyword}))]
    (is (= 2025 (:season (first round-tripped)))
        "the season survives as a number, not as the key \"2025\"")
    (is (= 1708.0 (get-in round-tripped [0 :stats :rec_yd])))))

(deftest kickers-get-no-history-at-all
  ;; A kicker joins, but every line-column is structurally zero for one. Three
  ;; seasons of {:pass_yd 0.0 :rush_yd 0.0 :rec 0.0 ...} is not a quiet career,
  ;; and downstream it is indistinguishable from a skill player who did nothing.
  (let [kicker (assoc chase-line "player_id" "00-0000003" "position" "K"
                      "passing_yards" "0" "passing_tds" "0"
                      "rushing_yards" "0" "rushing_tds" "0"
                      "receptions" "0" "receiving_yards" "0" "receiving_tds" "0")]
    (is (nil? (nflverse/row->season-line kicker)))
    (is (nil? (get (nflverse/history {2025 [kicker]}) "00-0000003")))
    (testing "but a kicker still joins for usage and availability"
      (is (some? (nflverse/row->usage 2025 kicker)))
      (is (some? (nflverse/row->games 2025 kicker))))))
