(ns draft-day.benchmark.fantasypros-ecr-test
  "Six name formats across 2011-2020, all of which parse into plausible-looking
  garbage rather than errors when handled wrongly. Each is pinned."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [draft-day.benchmark.sources.fantasypros-archive :as fp-archive]
            [draft-day.benchmark.sources.fantasypros-ecr :as ecr])
  (:import [org.jsoup Jsoup]))

(deftest clean-name-handles-every-archived-format
  (testing "2012: position, team and bye packed into parentheses"
    (is (= "Arian Foster" (ecr/clean-name "Arian Foster (RB1, HOU, 8)"))))
  (testing "2013: team/bye in parentheses"
    (is (= "Adrian Peterson" (ecr/clean-name "Adrian Peterson (MIN/5)"))))
  (testing "2014: parentheses plus a trailing single-letter note flag"
    (is (= "LeSean McCoy" (ecr/clean-name "LeSean McCoy (PHI/7) P"))))
  (testing "2015: bare team then bye"
    (is (= "Antonio Brown" (ecr/clean-name "Antonio Brown PIT, 11"))))
  (testing "2016/2017: bare team"
    (is (= "Antonio Brown" (ecr/clean-name "Antonio Brown PIT"))))
  (testing "2018: the name repeated in abbreviated form, then team"
    (is (= "Todd Gurley" (ecr/clean-name "Todd Gurley T. Gurley LAR"))))
  (testing "2011: already clean"
    (is (= "Arian Foster" (ecr/clean-name "Arian Foster")))))

(deftest clean-name-does-not-mangle-initialled-names
  ;; The abbreviated-duplicate rule keys on WHITESPACE before the initial. Relax
  ;; it to bare "[A-Z]\\. " and "A.J. Brown" truncates to "A." — which is why the
  ;; fix for the 2018 spacing bug had to happen in text extraction instead.
  (is (= "A.J. Brown" (ecr/clean-name "A.J. Brown")))
  (is (= "T.J. Hockenson" (ecr/clean-name "T.J. Hockenson"))))

(deftest cell-text-inserts-separators-between-inline-elements
  ;; Jsoup's .text() concatenates adjacent inline elements with no separator, so
  ;; the 2018 cell became "Todd GurleyT. Gurley" and the abbreviation rule never
  ;; fired. Tags must become spaces, as the validated parse did.
  (let [td (.selectFirst (Jsoup/parse "<table><tr><td><a>Todd Gurley</a><span>T. Gurley</span> LAR</td></tr></table>") "td")]
    (is (= "Todd Gurley T. Gurley LAR" (ecr/cell-text td)))
    (is (= "Todd Gurley" (ecr/clean-name (ecr/cell-text td))))))

(deftest position-is-found-in-every-layout
  (is (= "RB" (ecr/cell-position ["1" "Adrian Peterson (MIN/5)" "RB1" "1"])))
  (testing "2011 uses a bare position cell"
    (is (= "RB" (ecr/cell-position ["2" "1" "8" "3" "Arian Foster" "RB" "HOU" "11"]))))
  (testing "2012 packs it into the name"
    (is (= "RB" (ecr/cell-position ["1" "Arian Foster (RB1, HOU, 8)" "1" "10"]))))
  (testing "no position at all yields nil rather than a guess"
    (is (nil? (ecr/cell-position ["1" "some text" "2"])))))

(deftest name-cell-ignores-analyst-commentary
  ;; Cheatsheet rows carry blurbs hundreds of characters long. Picking the cell
  ;; with the most letters selects those, which silently dropped 2017 from 97%
  ;; to 41% joined.
  (let [blurb "When you see a 25-year-old running back getting 25.8 combined carries and targets it is a given to put him as a clear-cut top player"]
    (is (= "Todd Gurley LAR" (ecr/name-cell ["1" "Todd Gurley LAR" "RB1" blurb])))))

(deftest parse-page-reads-both-eras-in-rank-order
  (let [legacy "<table><tr><td>1</td><td>Arian Foster (RB1, HOU, 8)</td><td>1</td><td>10</td></tr>
                <tr><td>2</td><td>Tom Brady (QB1, NE, 9)</td><td>1</td><td>9</td></tr></table>"
        modern "<table><tr><td>1</td><td><a>Todd Gurley</a><span>T. Gurley</span> LAR</td><td>RB1</td><td>12</td></tr>
                <tr><td>2</td><td><a>Davante Adams</a><span>D. Adams</span> GB</td><td>WR1</td><td>7</td></tr></table>"]
    (is (= [{:player-name "Arian Foster" :position "RB" :ecr 1}
            {:player-name "Tom Brady"    :position "QB" :ecr 2}]
           (ecr/parse-page legacy)))
    (is (= [{:player-name "Todd Gurley"   :position "RB" :ecr 1}
            {:player-name "Davante Adams" :position "WR" :ecr 2}]
           (ecr/parse-page modern)))))

(deftest parse-page-deduplicates-repeated-players
  ;; Some cheatsheets repeat a player across an overall table and per-position
  ;; sub-tables; counting him twice would distort both the pool and the ranks.
  (let [html "<table><tr><td>1</td><td>Arian Foster (RB1, HOU, 8)</td><td>1</td></tr>
              <tr><td>2</td><td>Tom Brady (QB1, NE, 9)</td><td>1</td></tr>
              <tr><td>1</td><td>Arian Foster (RB1, HOU, 8)</td><td>1</td></tr></table>"
        out  (ecr/parse-page html)]
    (is (= 2 (count out)))
    (is (= [1 2] (mapv :ecr out)))))

(deftest javascript-era-and-empty-pages-yield-nothing
  ;; 2021+ captures carry no player rows at all. Returning [] lets the season be
  ;; reported as uncaptured rather than crashing or inventing a pool.
  (is (= [] (ecr/parse-page "")))
  (is (= [] (ecr/parse-page "<html><body><script>var ecrData={};</script></body></html>"))))

(def ecr-json-html
  "The 2021+ era: no player rows in the HTML at all, everything in a JS payload."
  (str "<html><body><script>\n"
       "var ecrData = {\"count\":2,\"total_experts\":150,\"players\":["
       "{\"player_id\":19236,\"player_name\":\"Justin Jefferson\",\"player_position_id\":\"WR\","
       "\"rank_ecr\":1,\"rank_std\":\"1.61\",\"pos_rank\":\"WR1\",\"tier\":1},"
       "{\"player_id\":16393,\"player_name\":\"Christian McCaffrey\",\"player_position_id\":\"RB\","
       "\"rank_ecr\":2,\"rank_std\":\"2.40\",\"pos_rank\":\"RB1\",\"tier\":1},"
       "{\"player_id\":99999,\"player_name\":\"Some Kicker\",\"player_position_id\":\"K\","
       "\"rank_ecr\":3,\"rank_std\":\"5.0\",\"pos_rank\":\"K1\",\"tier\":9}"
       "]};\n</script></body></html>"))

(deftest javascript-era-parses-from-the-payload
  (let [out (ecr/parse-ecr-json ecr-json-html)]
    (is (= 2 (count out)) "K is not a scoring position here")
    (is (= "Justin Jefferson" (:player-name (first out))))
    (is (= 1 (:ecr (first out))))
    (is (= "19236" (:fp-id (first out))))
    (testing "vintage expert disagreement and tier come along free"
      (is (= 1.61 (:rank-std (first out))))
      (is (= 1 (:tier (first out)))))))

(deftest json-era-is-only-tried-when-the-table-yields-nothing
  ;; The HTML parser must stay authoritative for 2011-2020; the JSON branch is a
  ;; fallback, not a competing interpretation.
  (is (= [] (ecr/parse-page ecr-json-html))
      "the JS page has no table rows, so the table parser correctly finds none")
  (is (seq (ecr/parse-ecr-json ecr-json-html))))

(deftest malformed-payload-returns-nil-rather-than-throwing
  (is (nil? (ecr/parse-ecr-json "var ecrData = {not json};\n")))
  (is (nil? (ecr/parse-ecr-json "<html>no payload here</html>"))))

(deftest openers-cover-every-archived-season
  ;; preseason? refuses a season with no recorded opener, which is correct — and
  ;; silently cost 2011-2014 until the table was extended back from 2015.
  (doseq [season (range 2011 2026)]
    (is (contains? fp-archive/openers season)
        (str "no Week 1 opener recorded for " season
             " — captures for that season will be rejected outright"))))
