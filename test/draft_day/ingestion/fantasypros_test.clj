(ns draft-day.ingestion.fantasypros-test
  (:require [clojure.test :refer [deftest is]]
            [draft-day.ingestion.fantasypros :as fp]
            [draft-day.ingestion.match :as match]))

(def ^:private sample-html
  (str "<html><body><script>var ecrData = {\"sport\":\"NFL\",\"players\":["
       "{\"player_name\":\"Bijan Robinson\",\"player_team_id\":\"ATL\",\"player_position_id\":\"RB\","
       "\"player_bye_week\":\"11\",\"rank_ecr\":2,\"pos_rank\":\"RB1\",\"tier\":1,"
       "\"rank_min\":\"1\",\"rank_max\":\"5\",\"rank_ave\":\"2.98\",\"rank_std\":\"1.40\"},"
       "{\"player_name\":\"CeeDee Lamb\",\"player_team_id\":\"DAL\",\"player_position_id\":\"WR\","
       "\"player_bye_week\":\"7\",\"rank_ecr\":5,\"pos_rank\":\"WR2\",\"tier\":2,"
       "\"rank_min\":\"3\",\"rank_max\":\"9\",\"rank_ave\":\"5.5\",\"rank_std\":\"2.10\"}"
       "]};</script></body></html>"))

(deftest parse-extracts-and-types-fields
  (let [idx   (match/by-key (fp/parse-ecr sample-html))
        bijan (get idx (match/key-for "Bijan Robinson" "RB"))]
    (is (= 2 (count idx)))
    (is (= 2 (:fantasypros/ecr bijan)))
    (is (= 1 (:fantasypros/ecr-tier bijan)))
    (is (= 1.40 (:fantasypros/rank-std bijan)))     ; string -> double
    (is (nil? (:bye bijan)))                          ; bye now comes from Sleeper, not FantasyPros
    (is (= "RB1" (:fantasypros/pos-rank bijan)))))

(deftest parse-returns-nil-without-blob
  (is (nil? (fp/parse-ecr "<html>no ecr here</html>"))))

;; --- AAV ---

(defn- aav-row [pid v name-cell class]
  (str "<tr pid='" pid "' v='" v "'" class ">"
       "<td class='RankCell'></td><td>" name-cell "</td>"
       "<td class='AlignRight DollarValue AuctionControls'>$" v "</td>"
       "<td class='RealValue'>" v "</td></tr>"))

(def ^:private sample-aav-html
  (str "<html><body>"
       "<table class='ValueTable' id='OverallTable'><tbody>"
       (aav-row "17298" "31" "Josh Allen (BUF - QB)" " class=' PlayerQB''")
       (aav-row "0"     "0"  "Zero Value (FA - WR)"   " class=' PlayerWR''")   ; dropped: $0
       (aav-row "9999"  "2"  "Houston Texans (HOU - DST)" " class=' PlayerDST''")
       "</tbody></table>"
       ;; per-position table duplicates the QB row — must dedupe to one entry
       "<table class='ValueTable' id='QBTable'><tbody>"
       (aav-row "17298" "31" "Josh Allen (BUF - QB)" " class=' PlayerQB''")
       "</tbody></table>"
       "</body></html>"))

(deftest parse-aav-extracts-values-and-dedupes
  (let [idx (match/by-key (fp/parse-aav sample-aav-html))]
    (is (= 2 (count idx)))                                          ; $0 dropped, QB deduped
    (is (= 31.0 (:fantasypros/aav (get idx (match/key-for "Josh Allen" "QB")))))
    (is (= 2.0  (:fantasypros/aav (get idx (match/key-for "Houston Texans" "DST")))))))

(deftest parse-aav-returns-nil-on-garbage
  (is (nil? (fp/parse-aav "<html>no table here</html>"))))

;; --- Sleepers ---

(defn- sleeper-row [pid name]
  (str "<tr class='mpb-player-" pid " player-row' data-id='" pid "'>"
       "<td>1</td><td class='player-label'>"
       "<a href='#' class='fp-player-link fp-id-" pid "' fp-player-name=\"" name "\">"
       "<span class='full-name'>" name "</span></a> <small class='grey'>BUF</small></td></tr>"))

(def ^:private sample-sleeper-html
  (str "<html><body><table><tbody>"
       (sleeper-row "27339" "Denzel Boston")
       (sleeper-row "12345" "Jalen McMillan")
       ;; ad/filler row with no player link — must be dropped
       "<tr class='player-row ad-row'><td>ad</td></tr>"
       "</tbody></table></body></html>"))

(deftest parse-sleepers-marks-players-by-position
  (let [idx (match/by-key (fp/parse-sleepers sample-sleeper-html "WR"))]
    (is (= 2 (count idx)))                                   ; filler row dropped
    (is (true? (:fantasypros/sleeper? (get idx (match/key-for "Denzel Boston" "WR")))))
    (is (true? (:fantasypros/sleeper? (get idx (match/key-for "Jalen McMillan" "WR")))))
    ;; position comes from the argument, so the same name at another pos won't match
    (is (nil? (get idx (match/key-for "Denzel Boston" "RB"))))))

(deftest parse-sleepers-returns-nil-on-garbage
  (is (nil? (fp/parse-sleepers "<html>no rows here</html>" "WR"))))
