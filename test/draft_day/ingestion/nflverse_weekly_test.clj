(ns draft-day.ingestion.nflverse-weekly-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.ingestion.nflverse :as nflverse]
            [draft-day.ingestion.nflverse-weekly :as weekly]))

(defn- row
  "One weekly row. Extra columns override the defaults."
  [gsis week & {:as over}]
  (merge {"player_id"   gsis
          "position"    "WR"
          "season_type" "REG"
          "week"        (str week)}
         over))

;; ---- what counts as a row at all ----

(deftest playoff-rows-are-not-regular-season-games
  ;; A postseason week counted as a regular-season game is a corrupted
  ;; denominator under every per-game rate downstream — the same class of error
  ;; as an unclamped 18-game season.
  (is (= 4 (weekly/row-week (row "00-1" 4))))
  (is (nil? (weekly/row-week (row "00-1" 1 "season_type" "POST"))))
  (is (nil? (weekly/row-week (row "00-1" 1 "season_type" "PRE")))))

(deftest a-row-with-no-week-or-no-joinable-id-is-dropped
  (is (nil? (weekly/row-week (row "00-1" 3 "week" ""))))
  (is (empty? (weekly/season-rows [(row "00-1" 3 "player_id" "")] nil)))
  (is (empty? (weekly/season-rows [(row "00-1" 3 "position" "CB")] nil))
      "gated on the same positions the season file joins"))

;; ---- how far the season has got ----

(deftest through-week-is-read-off-the-data-not-the-calendar
  (let [rows [(row "00-1" 1) (row "00-1" 2) (row "00-2" 5)]]
    (is (= 5 (weekly/through-week (weekly/season-rows rows nil)))))
  (testing "no rows at all is week zero, which is preseason"
    (is (= 0 (weekly/through-week [])))))

(deftest as-of-week-replays-a-finished-season-as-one-in-progress
  ;; Without this the in-season half cannot be exercised until games are played:
  ;; the current season's file does not exist in preseason, and last season's is
  ;; a season with nothing left to project.
  (let [rows (mapv #(row "00-1" %) (range 1 11))]
    (is (= 10 (weekly/through-week (weekly/season-rows rows nil))))
    (is (= 6 (weekly/through-week (weekly/season-rows rows 6))))
    (is (= 6 (count (weekly/season-rows rows 6))))))

;; ---- accumulation ----

(defn- acc-of [rows] (weekly/accumulate (weekly/season-rows rows nil)))

(deftest season-to-date-sums-the-weeks-a-player-actually-played
  (let [rows [(row "00-1" 1 "receptions" "5" "receiving_yards" "60" "receiving_tds" "1")
              (row "00-1" 2 "receptions" "3" "receiving_yards" "41")
              (row "00-1" 4 "receptions" "7" "receiving_yards" "88" "receiving_tds" "2")]
        {:keys [games stats]} (:nflverse/season-to-date (get (acc-of rows) "00-1"))]
    (is (= 3 games)
        "his own rows, not weeks elapsed — week 3 is a game he did not play")
    (is (= 15.0 (:rec stats)))
    (is (= 189.0 (:rec_yd stats)))
    (is (= 3.0 (:rec_td stats)))))

(deftest games-counts-appearances-so-a-missed-game-is-not-charged-twice
  ;; Four weeks have been played; he appeared in two. Dividing his totals by
  ;; four would depress his per-game rate by exactly the games he was never on
  ;; the field for — punishing the absence a second time.
  (let [rows [(row "00-1" 1 "receptions" "5") (row "00-1" 2 "receptions" "5")
              (row "00-2" 1) (row "00-2" 2) (row "00-2" 3) (row "00-2" 4)]
        a    (acc-of rows)]
    (is (= 4 (get-in a ["00-2" :nflverse/season-to-date :games])))
    (is (= 2 (get-in a ["00-1" :nflverse/season-to-date :games])))
    (is (= 10.0 (get-in a ["00-1" :nflverse/season-to-date :stats :rec])))))

(deftest a-blank-column-is-absent-not-zero-even-across-a-sum
  ;; Same rule as the season file. A key the source never gave this player a
  ;; value for has to stay absent, or "no opinion" and "genuinely none" become
  ;; the same number.
  (let [rows [(row "00-1" 1 "receptions" "4" "rushing_yards" "")
              (row "00-1" 2 "receptions" "NA" "rushing_yards" "NA")]
        stats (get-in (acc-of rows) ["00-1" :nflverse/season-to-date :stats])]
    (is (= 4.0 (:rec stats)) "a blank week contributes nothing to a total")
    (is (not (contains? stats :rush_yd)))))

(deftest the-recent-window-is-the-seasons-last-weeks-not-the-players
  ;; A back inactive for a month should read as inactive, not as whoever he was
  ;; the last time he played.
  (let [rows (into [(row "00-hurt" 1 "receptions" "9") (row "00-hurt" 2 "receptions" "9")]
                   (mapv #(row "00-fit" % "receptions" "2") (range 1 9)))
        a    (acc-of rows)]
    (is (= 8 (weekly/through-week (weekly/season-rows rows nil))))
    (is (= 3 (get-in a ["00-fit" :nflverse/recent :games]))
        "weeks 6-8 of an 8-week season")
    (is (= 6.0 (get-in a ["00-fit" :nflverse/recent :stats :rec])))
    (is (nil? (:nflverse/recent (get a "00-hurt")))
        "nothing inside the window means no recent column at all, not a zero one")
    (is (= 18.0 (get-in a ["00-hurt" :nflverse/season-to-date :stats :rec]))
        "his season line is untouched")))

(deftest usage-volume-rides-alongside-the-scored-line
  ;; A back who is suddenly getting the carries is a buy before the touchdowns
  ;; arrive; points alone cannot see that.
  (let [rows [(row "00-1" 1 "targets" "4" "carries" "11")
              (row "00-1" 2 "targets" "6" "carries" "17")]
        std  (get-in (acc-of rows) ["00-1" :nflverse/season-to-date])]
    (is (= 10.0 (get-in std [:usage :targets])))
    (is (= 28.0 (get-in std [:usage :carries])))))

(deftest kickers-carry-the-two-columns-they-have
  (let [rows [(row "00-k" 1 "position" "K" "fg_made" "3" "pat_made" "2")]
        stats (get-in (acc-of rows) ["00-k" :nflverse/season-to-date :stats])]
    (is (= 3.0 (:fgm stats)))
    (is (= 2.0 (:xpm stats)))))

(deftest the-stat-map-reaches-every-weight-a-league-can-set
  ;; Wider than `nflverse/line-columns` on purpose: that one shows a history
  ;; tile, this one scores a partial season under the league's own weights, so
  ;; a weight with no column here is a rule the in-season board cannot apply.
  (is (every? (set (vals weekly/stat-columns))
              [:pass_yd :pass_td :pass_int :pass_2pt
               :rush_yd :rush_td :rush_2pt
               :rec :rec_yd :rec_td :rec_2pt
               :fum_lost :fgm :xpm])))

;; ---- the failure that matters is a 200 with the wrong body ----

(deftest a-body-that-is-not-this-file-is-reported-as-a-miss
  (with-redefs [nflverse/http-get-string
                (fn [_] "<html><body>Not Found</body></html>")]
    (is (nil? (weekly/fetch-rows 2026))))
  (with-redefs [nflverse/http-get-string (fn [_] nil)]
    (is (nil? (weekly/fetch-rows 2026))))
  (testing "a throw stays local rather than escaping into parallel/all"
    (with-redefs [nflverse/http-get-string
                  (fn [_] (throw (ex-info "read timeout" {})))]
      (is (nil? (weekly/fetch-rows 2026))))))

(deftest preseason-is-an-absent-source-not-an-empty-season
  ;; The whole point: a missing file must not reach a consumer as a season in
  ;; which every player has played zero games.
  (with-redefs [weekly/fetch-rows (fn [_] nil)]
    (is (nil? (weekly/fetch 2026)))))

(deftest fetch-reports-the-week-the-rows-actually-reach
  (with-redefs [weekly/fetch-rows
                (fn [_] [(row "00-1" 1 "receptions" "4")
                         (row "00-1" 2 "receptions" "6")
                         (row "00-1" 3 "receptions" "5" "season_type" "POST")])]
    (let [{:keys [by-key through-week positions]} (weekly/fetch 2025)]
      (is (= 2 through-week) "the POST row does not advance the season")
      (is (= 10.0 (get-in by-key ["00-1" :nflverse/season-to-date :stats :rec])))
      (is (= {"00-1" "WR"} positions)))))
