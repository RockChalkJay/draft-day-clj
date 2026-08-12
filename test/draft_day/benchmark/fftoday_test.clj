(ns draft-day.benchmark.fftoday-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.benchmark.sources.fftoday :as fftoday]
            [draft-day.scoring :as scoring])
  (:import [org.jsoup Jsoup]))

(defn table-html
  "FFToday's shape: a group header row with colspans, a column header row, then
  data rows. `groups` and `cols` are the two header rows."
  [groups cols row-cells]
  (str "<table>"
       "<tr class='tablehdr'>" (apply str groups) "</tr>"
       "<tr class='tableclmhdr'>" (apply str (map #(str "<td><b>" % "</b></td>") cols)) "</tr>"
       "<tr>" (apply str row-cells) "</tr>"
       "</table>"))

;; RB runs Rushing then Receiving.
(def rb-html
  (table-html ["<td colspan='4'>&nbsp;</td>"
               "<td colspan='3'>Rushing</td>"
               "<td colspan='3'>Receiving</td>"
               "<td>Fantasy</td>"]
              ["Chg" "Player" "Tm" "Bye" "Att" "Yds" "TD" "Rec" "Yds" "TD" "FPts"]
              ["<td>&nbsp;</td>"
               "<td><a href=\"/stats/players/15197/Todd_Gurley?LeagueID=1\">Todd Gurley</a></td>"
               "<td>LAR</td><td>12</td>"
               "<td>273</td><td>1,255</td><td>11</td>"
               "<td>71</td><td>677</td><td>4</td><td>283.2</td>"]))

;; WR runs Receiving then Rushing — the opposite order, same page family.
(def wr-html
  (table-html ["<td colspan='4'>&nbsp;</td>"
               "<td colspan='3'>Receiving</td>"
               "<td colspan='3'>Rushing</td>"
               "<td>Fantasy</td>"]
              ["Chg" "Player" "Tm" "Bye" "Rec" "Yds" "TD" "Att" "Yds" "TD" "FPts"]
              ["<td>&nbsp;</td>"
               "<td><a href=\"/stats/players/9012/Antonio_Brown?LeagueID=1\">Antonio Brown</a></td>"
               "<td>PIT</td><td>7</td>"
               "<td>101</td><td>1,366</td><td>10</td>"
               "<td>2</td><td>16</td><td>0</td><td>196.6</td>"]))

(deftest column-order-is-read-per-position-not-assumed
  ;; Running backs and wide receivers use OPPOSITE group orders on the same site.
  ;; An index-based parser reads a receiver's 101 receptions as rushing attempts
  ;; and his 2 rush attempts as receptions — a board that looks fine and is wrong.
  (let [rb (first (fftoday/parse-page rb-html))
        wr (first (fftoday/parse-page wr-html))]
    (testing "RB: rushing first"
      (is (= 1255.0 (get-in rb [:stats :rush_yd])))
      (is (= 11.0   (get-in rb [:stats :rush_td])))
      (is (= 71.0   (get-in rb [:stats :rec])))
      (is (= 677.0  (get-in rb [:stats :rec_yd]))))
    (testing "WR: receiving first"
      (is (= 101.0  (get-in wr [:stats :rec])))
      (is (= 1366.0 (get-in wr [:stats :rec_yd])))
      (is (= 16.0   (get-in wr [:stats :rush_yd]))))
    (testing "the receiver's receptions must NOT land in a rushing key"
      (is (not= 101.0 (get-in wr [:stats :rush_yd]))))))

(deftest row-cells-does-not-descend-into-nested-tables
  ;; FFToday wraps the projections table inside an outer layout table. Jsoup's
  ;; `select "td"` descends, so the wrapper row reported 578 cells instead of 11,
  ;; every stat index landed on page chrome, and rows parsed with correct names
  ;; and empty stat lines.
  (let [doc (Jsoup/parse "<table><tr id='outer'><td>chrome</td><td><table><tr><td>a</td><td>b</td></tr></table></td></tr></table>")
        tr  (.selectFirst doc "tr#outer")]
    (is (= 2 (count (fftoday/row-cells tr))) "direct children only")
    (is (= 4 (.size (.select tr "td"))) "select descends — this is the trap")))

(deftest commas-are-stripped-from-thousands
  (let [rb (first (fftoday/parse-page rb-html))]
    (is (= 1255.0 (get-in rb [:stats :rush_yd])))))

(deftest fpts-is-not-imported-as-a-stat
  ;; FPts is FFToday's own scoring. Importing it would silently override the
  ;; league's rules, which is the whole reason we keep the raw stat line.
  (let [rb (first (fftoday/parse-page rb-html))]
    (is (not (contains? (:stats rb) :fpts)))
    ;; 1255*.1 + 11*6 + 71*1 + 677*.1 + 4*6 = 125.5 + 66 + 71 + 67.7 + 24 = 354.2
    (is (< (Math/abs (- 354.2 (scoring/player-points rb (:ppr scoring/presets)))) 1e-9))))

(deftest identity-comes-from-the-stats-link
  (let [rb (first (fftoday/parse-page rb-html))]
    (is (= "15197" (:fftoday-id rb)))
    (is (= "Todd Gurley" (:player-name rb)))
    (is (= "LAR" (:team rb)))))

(deftest rows-that-do-not-match-the-header-width-are-rejected
  ;; Layout rows and section separators share the table; accepting them would
  ;; shift every stat by a column.
  (let [html (str "<table>"
                  "<tr class='tablehdr'><td colspan='4'>&nbsp;</td><td colspan='3'>Rushing</td>"
                  "<td colspan='3'>Receiving</td><td>Fantasy</td></tr>"
                  "<tr class='tableclmhdr'>"
                  (apply str (map #(str "<td><b>" % "</b></td>")
                                  ["Chg" "Player" "Tm" "Bye" "Att" "Yds" "TD" "Rec" "Yds" "TD" "FPts"]))
                  "</tr>"
                  "<tr><td colspan='11'><a href=\"/stats/players/1/Short_Row\">Short Row</a></td></tr>"
                  "</table>")]
    (is (= [] (fftoday/parse-page html)))))

(deftest unreadable-header-yields-no-rows-rather-than-mis-mapped-ones
  (is (= [] (fftoday/parse-page "")))
  (is (= [] (fftoday/parse-page "<html><body><p>nothing</p></body></html>")))
  (testing "a table with no group row is refused, not guessed at"
    (is (= [] (fftoday/parse-page
               "<table><tr class='tableclmhdr'><td><b>Player</b></td></tr>
                <tr><td><a href=\"/stats/players/1/X\">X</a></td></tr></table>")))))

(deftest position-ids-cover-the-scoring-positions
  (is (= #{"QB" "RB" "WR" "TE"} (set (vals fftoday/position-ids)))))
