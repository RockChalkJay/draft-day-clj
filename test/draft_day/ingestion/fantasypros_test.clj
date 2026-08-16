(ns draft-day.ingestion.fantasypros-test
  (:require [clojure.test :refer [deftest is]]
            [draft-day.ingestion.fantasypros :as fp]
            [draft-day.ingestion.match :as match]
            [draft-day.scoring :as scoring]))

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

(deftest an-unknown-format-throws-rather-than-serving-ppr
  ;; Falling back to PPR is silent in the worst way: the fetch succeeds, the
  ;; join succeeds, the source reports a full row count, and a standard league
  ;; reads PPR ranks and prices with nothing anywhere saying so. Ingestion wraps
  ;; these in `best-effort`, so throwing costs one column and *shows*.
  (doseq [[label f] {"cheatsheet" fp/cheatsheet-url "auction" fp/aav-url}]
    (is (thrown? clojure.lang.ExceptionInfo (f :half))
        (str label ": an unrecognized format keyword"))
    (is (thrown? clojure.lang.ExceptionInfo (f "ppr"))
        (str label ": the string spelling is not the keyword"))
    (is (thrown? clojure.lang.ExceptionInfo (f nil))
        (str label ": a missing format")))
  (doseq [fmt scoring/formats]
    (is (string? (fp/cheatsheet-url fmt)) (str fmt " has a cheatsheet"))
    (is (string? (fp/aav-url fmt)) (str fmt " has an auction URL")))
  (is (not= (fp/aav-url :standard) (fp/aav-url :ppr))
      "the formats must actually address different pages"))

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

;; ---- per-position expert tier ----

(deftest parse-pos-ecr-takes-the-tier-and-nothing-else
  ;; The positional page carries rank_ecr, rank_std and the rest too, but those
  ;; already arrive from the overall cheatsheet. Emitting them here would put two
  ;; scrapes in a race to write the same columns, and the positional page's ranks
  ;; are position-relative — so the winner would sometimes be an ECR of 1 meaning
  ;; "RB1". Only the tier, which the overall page cannot give per position.
  (let [rows (fp/parse-pos-ecr sample-html)]
    (is (= 2 (count rows)))
    (is (= #{:key :fantasypros/ecr-pos-tier} (set (mapcat keys rows))))
    (is (= {(match/key-for "Bijan Robinson" "RB") 1
            (match/key-for "CeeDee Lamb" "WR")    2}
           (into {} (map (juxt :key :fantasypros/ecr-pos-tier)) rows)))))

(deftest parse-pos-ecr-returns-nil-without-blob
  (is (nil? (fp/parse-pos-ecr "<html>no ecr here</html>"))))

(deftest positional-urls-follow-what-fantasypros-actually-publishes
  (is (= "https://www.fantasypros.com/nfl/rankings/ppr-rb-cheatsheets.php"
         (fp/pos-cheatsheet-url "RB" :ppr)))
  (is (= "https://www.fantasypros.com/nfl/rankings/half-point-ppr-wr-cheatsheets.php"
         (fp/pos-cheatsheet-url "WR" :half-ppr)))
  (is (= "https://www.fantasypros.com/nfl/rankings/te-cheatsheets.php"
         (fp/pos-cheatsheet-url "TE" :standard))
      "standard is the bare page, not a prefixed one")
  (doseq [pos ["QB" "K" "DST"]]
    (is (apply = (map #(fp/pos-cheatsheet-url pos %) scoring/formats))
        (str pos " publishes one page for every format")))
  (is (= "https://www.fantasypros.com/nfl/rankings/qb-cheatsheets.php"
         (fp/pos-cheatsheet-url "QB" :ppr))
      "and it is the bare one — the prefixed URL 302s to the *overall* cheatsheet,
       which would parse a whole-board page as if it were one position"))

(deftest an-unknown-position-or-format-throws
  (is (thrown? clojure.lang.ExceptionInfo (fp/pos-cheatsheet-url "FLEX" :ppr)))
  (is (thrown? clojure.lang.ExceptionInfo (fp/pos-cheatsheet-url "RB" :six-point-ppr)))
  (is (= "https://www.fantasypros.com/nfl/rankings/qb-cheatsheets.php"
         (fp/pos-cheatsheet-url "QB" :six-point-ppr))
      "a format-invariant position ignores the format rather than throwing on it"))
