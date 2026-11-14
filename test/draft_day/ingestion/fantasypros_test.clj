(ns draft-day.ingestion.fantasypros-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
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

(defn- aav-for
  "The parsed value for `key` from a one-row #OverallTable built around
  `name-cell`, so a name-parsing case is one line of markup."
  [name-cell key]
  (let [html (str "<html><body><table class='ValueTable' id='OverallTable'><tbody>"
                  (aav-row "23180" "61" name-cell " class=' PlayerWR''")
                  "</tbody></table></body></html>")]
    (:fantasypros/aav (get (match/by-key (fp/parse-aav html)) key))))

(deftest parse-aav-reads-the-name-past-the-markup-around-it
  ;; FantasyPros puts the injury tag inside the name cell itself, after the
  ;; position. Read the cell whole against an end-anchored pattern and the row
  ;; does not parse at all -- which quietly cost the board 33 priced players,
  ;; the expensive end of it worst, since those are the knocks that get reported.
  ;;
  ;; Three cases and not one, because `parse-aav` carries two guards that fail in
  ;; opposite directions and the shape FantasyPros actually serves exercises
  ;; neither of them alone -- either guard could be deleted with the live case
  ;; still passing.
  (let [nacua (match/key-for "Puka Nacua" "WR")]
    (testing "the served shape: a trailing badge, as an element"
      (is (= 61.0 (aav-for (str "Puka Nacua (LAR - WR)"
                                "<span class='injury-tag' title='Groin'>DTD</span>")
                           nacua))))
    (testing "a trailing badge as bare text: only the unanchored match saves this"
      (is (= 61.0 (aav-for "Puka Nacua (LAR - WR) DTD" nacua))))
    (testing "a leading element: only the cell's own text saves this, since the
              whole text would key him as \"12Puka Nacua\""
      (is (= 61.0 (aav-for "<span class='rank'>12</span>Puka Nacua (LAR - WR)"
                           nacua))))))

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

(deftest a-format-varying-position-gets-one-url-per-format
  (let [urls (map #(fp/pos-cheatsheet-url "RB" %) scoring/formats)]
    (is (= 3 (count (distinct urls))))
    (is (every? #(re-find #"rb-cheatsheets\.php$" %) urls))))

(deftest a-format-invariant-position-gets-the-bare-page-whatever-the-format
  ;; Load-bearing, not an optimization: QB/K/DST's ppr- and half-point-ppr- URLs
  ;; 302 to the *overall* cheatsheet, so following one would parse a whole-board
  ;; page as if it were one position.
  (doseq [pos ["QB" "K" "DST"]]
    (is (= 1 (count (distinct (map #(fp/pos-cheatsheet-url pos %) scoring/formats))))
        (str pos " publishes one page for all three formats"))
    (is (not (re-find #"ppr" (fp/pos-cheatsheet-url pos :ppr)))
        (str pos " does not take a format prefix"))))

(deftest an-unknown-position-or-format-throws
  (is (thrown? clojure.lang.ExceptionInfo (fp/pos-cheatsheet-url "IDP" :ppr)))
  (is (thrown? clojure.lang.ExceptionInfo (fp/pos-cheatsheet-url "RB" :half)))
  (is (thrown? clojure.lang.ExceptionInfo (fp/pos-cheatsheet-url "RB" nil))))

(deftest parse-pos-ecr-takes-only-the-tier
  ;; The positional page carries rank_ecr and the rest too, but those already
  ;; arrive from the overall cheatsheet — and here they are position-relative, so
  ;; letting them through would sometimes write an ECR of 4 meaning "RB4".
  (let [rows (fp/parse-pos-ecr sample-html)]
    (is (= 2 (count rows)))
    (is (= #{:key :fantasypros/ecr-pos-tier} (set (mapcat keys rows))))
    (is (= 1 (:fantasypros/ecr-pos-tier
              (get (match/by-key rows) (match/key-for "Bijan Robinson" "RB")))))))

(deftest parse-pos-ecr-returns-nil-without-blob
  (is (nil? (fp/parse-pos-ecr "<html>no ecr here</html>"))))

(defn- ros-page [type pos & rows]
  (str "<html><body><script>var ecrData = {\"ranking_type_name\":\"" type "\",\"players\":["
       (clojure.string/join
        "," (map (fn [[name rank lo hi]]
                   (str "{\"player_name\":\"" name "\",\"player_position_id\":\"" pos "\","
                        "\"pos_rank\":\"" pos rank "\",\"rank_min\":\"" lo "\",\"rank_max\":\"" hi "\"}"))
                 rows))
       "]};</script></body></html>"))

(deftest ros-urls-follow-the-position-pages-rules
  (is (= "https://www.fantasypros.com/nfl/rankings/ros-ppr-rb.php" (fp/ros-url "RB" :ppr)))
  (is (= "https://www.fantasypros.com/nfl/rankings/ros-half-point-ppr-wr.php" (fp/ros-url "WR" :half-ppr)))
  (is (= "https://www.fantasypros.com/nfl/rankings/ros-te.php" (fp/ros-url "TE" :standard)))
  (is (= "https://www.fantasypros.com/nfl/rankings/ros-qb.php" (fp/ros-url "QB" :ppr))
      "the prefixed QB page is the preseason overall sheet")
  (is (thrown? clojure.lang.ExceptionInfo (fp/ros-url "RB" :half))))

(deftest parse-ros-takes-the-rank-and-its-spread
  (let [idx (match/by-key (fp/parse-ros-ecr (ros-page "ros" "RB" ["Ollie Gordon II" 24 24 29]) "RB"))
        gordon (get idx (match/key-for "Ollie Gordon II" "RB"))]
    (is (= "RB24" (:fantasypros/ros-pos-rank gordon)))
    (is (= 24 (:fantasypros/ros-rank-min gordon)))
    (is (= 29 (:fantasypros/ros-rank-max gordon)))))

(deftest parse-ros-refuses-a-page-that-is-not-the-ros-ranking-of-that-position
  (is (nil? (fp/parse-ros-ecr (ros-page "draft" "RB" ["A" 1 1 1]) "RB")) "a redirect to the preseason sheet")
  (is (nil? (fp/parse-ros-ecr (ros-page "ros" "WR" ["A" 1 1 1]) "RB")) "another position's page")
  (is (nil? (fp/parse-ros-ecr "<html>nothing</html>" "RB"))))

(deftest fetch-ros-reads-twelve-pages-and-scopes-them-by-format
  (let [asked (atom [])
        page  (fn [url]
                (swap! asked conj url)
                (let [pos (str/upper-case (second (re-find #"-([a-z]+)\.php$" url)))
                      fmt (cond (str/includes? url "half-point") "H" (str/includes? url "ppr") "P" :else "S")]
                  (ros-page "ros" pos [(str pos " " fmt) 1 1 1])))
        out   (with-redefs [fp/fetch-page page] (fp/fetch-ros-ecr))
        name-in (fn [fmt pos] (-> (get-in out [fmt (match/key-for (str pos " " ({:ppr "P" :half-ppr "H" :standard "S"} fmt)) pos)]) :fantasypros/ros-pos-rank))]
    (is (= 12 (count @asked)) "nine format pages, three bare ones")
    (is (= "RB1" (name-in :half-ppr "RB")))
    (is (= "RB1" (name-in :standard "RB")))
    (is (some? (get-in out [:ppr (match/key-for "QB S" "QB")])) "QB joins every format from his one page")
    (is (some? (get-in out [:half-ppr (match/key-for "K S" "K")])))))

(deftest fetch-ros-fails-whole-when-one-page-does
  (with-redefs [fp/fetch-page (fn [url] (when-not (str/includes? url "ros-te") (ros-page "ros" "RB" ["A" 1 1 1])))]
    (is (thrown? Exception (fp/fetch-ros-ecr)))))

(defn- weekly-page [type pos week & rows]
  (str "<html><body><script>var ecrData = {\"ranking_type_name\":\"" type "\",\"week\":\"" week
       "\",\"players\":["
       (str/join
        "," (map (fn [[name rank lo hi grade]]
                   (str "{\"player_name\":\"" name "\",\"player_position_id\":\"" pos "\","
                        "\"pos_rank\":\"" pos rank "\",\"rank_min\":\"" lo "\",\"rank_max\":\"" hi "\","
                        "\"start_sit_grade\":\"" grade "\"}"))
                 rows))
       "]};</script></body></html>"))

(deftest weekly-urls-are-the-position-pages-without-the-cheatsheet-suffix
  (is (= "https://www.fantasypros.com/nfl/rankings/ppr-rb.php" (fp/weekly-url "RB" :ppr)))
  (is (= "https://www.fantasypros.com/nfl/rankings/half-point-ppr-wr.php" (fp/weekly-url "WR" :half-ppr)))
  (is (= "https://www.fantasypros.com/nfl/rankings/te.php" (fp/weekly-url "TE" :standard)))
  (is (= "https://www.fantasypros.com/nfl/rankings/dst.php" (fp/weekly-url "DST" :ppr))
      "a defense has only the bare page")
  (is (thrown? clojure.lang.ExceptionInfo (fp/weekly-url "RB" :half))))

(deftest parse-weekly-takes-the-rank-its-spread-and-the-grade
  (let [idx    (match/by-key (fp/parse-weekly-ecr (weekly-page "weekly" "RB" 8 ["Jonah Coleman" 29 28 33 "C"]) "RB" 8))
        coleman (get idx (match/key-for "Jonah Coleman" "RB"))]
    (is (= "RB29" (:fantasypros/wk-pos-rank coleman)))
    (is (= 28 (:fantasypros/wk-rank-min coleman)))
    (is (= 33 (:fantasypros/wk-rank-max coleman)))
    (is (= "C" (:fantasypros/wk-grade coleman)))))

(deftest parse-weekly-refuses-a-page-that-is-not-this-weeks-ranking-of-that-position
  (is (nil? (fp/parse-weekly-ecr (weekly-page "ros" "RB" 8 ["A" 1 1 1 "A"]) "RB" 8)) "the rest-of-season sheet")
  (is (nil? (fp/parse-weekly-ecr (weekly-page "weekly" "WR" 8 ["A" 1 1 1 "A"]) "RB" 8)) "another position's page")
  (is (nil? (fp/parse-weekly-ecr (weekly-page "weekly" "RB" 7 ["A" 1 1 1 "A"]) "RB" 8)) "last week's ranks")
  (is (nil? (fp/parse-weekly-ecr "<html>nothing</html>" "RB" 8))))

(deftest fetch-weekly-reads-twelve-pages-and-scopes-them-by-format
  (let [asked (atom [])
        page  (fn [url]
                (swap! asked conj url)
                (let [pos (str/upper-case (second (re-find #"-?([a-z]+)\.php$" url)))
                      fmt (cond (str/includes? url "half-point") "H" (str/includes? url "ppr") "P" :else "S")]
                  (weekly-page "weekly" pos 8 [(str pos " " fmt) 1 1 1 "A"])))
        out   (with-redefs [fp/fetch-page page] (fp/fetch-weekly-ecr 8))
        rank  (fn [fmt pos] (get-in out [fmt (match/key-for (str pos " " ({:ppr "P" :half-ppr "H" :standard "S"} fmt)) pos)
                                     :fantasypros/wk-pos-rank]))]
    (is (= 12 (count @asked)) "nine format pages, three bare ones")
    (is (= "RB1" (rank :half-ppr "RB")))
    (is (= "RB1" (rank :standard "RB")))
    (is (some? (get-in out [:ppr (match/key-for "QB S" "QB")])) "QB joins every format from his one page")))

(deftest fetch-weekly-fails-whole-when-one-page-does
  (with-redefs [fp/fetch-page (fn [url] (when-not (str/includes? url "te.php") (weekly-page "weekly" "RB" 8 ["A" 1 1 1 "A"])))]
    (is (thrown? Exception (fp/fetch-weekly-ecr 8)))))

(deftest fetch-weekly-fails-whole-when-a-page-has-not-rolled-over
  (with-redefs [fp/fetch-page (fn [url]
                                (let [pos (str/upper-case (second (re-find #"-?([a-z]+)\.php$" url)))]
                                  (weekly-page "weekly" pos (if (= "DST" pos) 7 8) ["A" 1 1 1 "A"])))]
    (is (thrown? Exception (fp/fetch-weekly-ecr 8)))))
