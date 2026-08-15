(ns draft-day.benchmark.fantasypros-archive-test
  "The archive source has two failure modes that produce plausible-looking
  numbers instead of errors, so both are pinned here: a capture taken after
  kickoff, and a column order that changed between eras."
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.benchmark.sources.fantasypros-archive :as fpa]
            [draft-day.scoring :as scoring]))

;; 2021-2025 markup: RECEIVING then RUSHING, name in <a class="player-name">.
(def modern-html
  "<table><thead>
   <tr><td></td><td colspan=\"3\">RECEIVING</td><td colspan=\"3\">RUSHING</td><td colspan=\"2\">MISC</td></tr>
   <tr><th class=\"player-label\">Player</th><th>REC</th><th>YDS</th><th>TDS</th>
       <th>ATT</th><th>YDS</th><th>TDS</th><th>FL</th><th>FPTS</th></tr>
   </thead><tbody>
   <tr class=\"mpb-player-111 js-tr-game-select\">
     <td class=\"player-label\"><a class=\"player-name\">New Era</a> KC
       <a class=\"fp-player-link fp-id-111\" fp-player-name=\"New Era\"></a></td>
     <td>100.0</td><td>1,200.5</td><td>8.0</td>
     <td>3.0</td><td>20.0</td><td>0.0</td><td>1.0</td><td>180.0</td></tr>
   </tbody></table>")

;; 2015 markup: RUSHING then RECEIVING, and the name anchor has NO class.
(def legacy-html
  "<table><thead>
   <tr><td></td><td colspan=\"3\">RUSHING</td><td colspan=\"3\">RECEIVING</td><td colspan=\"2\">MISC</td></tr>
   <tr><th>Player</th><th>ATT</th><th>YDS</th><th>TDS</th>
       <th>REC</th><th>YDS</th><th>TDS</th><th>FL</th><th>FPTS</th></tr>
   </thead><tbody>
   <tr class=\"mpb-available mpb-player-222\">
     <td><a href=\"/x.php\">Old Era</a> <small>PIT</small>
       <a class=\"fp-player-link fp-id-222\" fp-player-name=\"Old Era\"></a></td>
     <td>4.8</td><td>23.2</td><td>0.0</td>
     <td>116.8</td><td>1,538.5</td><td>10.0</td><td>1.8</td><td>212.6</td></tr>
   </tbody></table>")

(deftest column-order-is-read-from-the-header-not-assumed
  ;; THE regression test for this namespace. FantasyPros swapped the WR column
  ;; groups between 2015 and 2023. A position-indexed parser reads 23.2 rushing
  ;; yards as 1,538.5 receiving yards for half the corpus and produces a board
  ;; that looks fine and is wrong.
  (let [modern (first (fpa/parse-page modern-html))
        legacy (first (fpa/parse-page legacy-html))]
    (testing "receiving-first layout"
      (is (= 100.0  (get-in modern [:stats :rec])))
      (is (= 1200.5 (get-in modern [:stats :rec_yd])))
      (is (= 8.0    (get-in modern [:stats :rec_td])))
      (is (= 20.0   (get-in modern [:stats :rush_yd])))
      (is (= 1.0    (get-in modern [:stats :fum_lost]))))
    (testing "rushing-first layout maps to the same semantic keys"
      (is (= 116.8  (get-in legacy [:stats :rec])))
      (is (= 1538.5 (get-in legacy [:stats :rec_yd])))
      (is (= 10.0   (get-in legacy [:stats :rec_td])))
      (is (= 23.2   (get-in legacy [:stats :rush_yd])))
      (is (= 1.8    (get-in legacy [:stats :fum_lost]))))
    (testing "the reversed layout must NOT put rushing yards in rec_yd"
      (is (not= 23.2 (get-in legacy [:stats :rec_yd]))))))

(deftest player-identity-survives-both-markup-eras
  (let [modern (first (fpa/parse-page modern-html))
        legacy (first (fpa/parse-page legacy-html))]
    (is (= "New Era" (:player-name modern)))
    (is (= "111"     (:fp-id modern)))
    ;; 2015's name anchor carries no class; selector-only extraction NPE'd here.
    (is (= "Old Era" (:player-name legacy)))
    (is (= "222"     (:fp-id legacy)))))

(deftest fpts-is-not-imported-as-a-stat
  ;; FantasyPros' own FPTS reflects THEIR scoring. The whole point is to re-score
  ;; the raw line under the league's rules, so FPTS must not leak into :stats.
  (let [stats (:stats (first (fpa/parse-page modern-html)))]
    (is (not (contains? stats :fpts)))
    ;; 100 rec + 1200.5 yds + 8 TD + 20 rush yds + 1 FL, under PPR:
    ;; 100 + 120.05 + 48 + 2 - 2 = 268.05
    (is (< (Math/abs (- 268.05 (scoring/player-points {:stats stats} (:ppr scoring/presets))))
           1e-9))))

(deftest preseason-gate-rejects-post-kickoff-captures
  ;; 2015 kicked off Sept 10. The Nov 21 capture of the same draft page shares
  ;; only 40 of ~170 rows with the Sept 8 one, so it is not the same artifact.
  (is (fpa/preseason? 2015 "20150908120000"))
  (is (fpa/preseason? 2015 "20150909235959"))
  (is (not (fpa/preseason? 2015 "20150910000000")))  ; kickoff day
  (is (not (fpa/preseason? 2015 "20151121050926")))  ; the mid-season capture
  (testing "a capture from a different year never counts"
    (is (not (fpa/preseason? 2015 "20140901000000")))
    (is (not (fpa/preseason? 2015 "20160101000000"))))
  (testing "a season with no recorded opener is refused rather than guessed"
    (is (not (fpa/preseason? 1999 "19990801000000")))))

(deftest unparseable-page-yields-no-rows-rather-than-garbage
  (is (= [] (fpa/parse-page "")))
  (is (= [] (fpa/parse-page "<html><body><p>no table here</p></body></html>")))
  (testing "a table whose header cannot be aligned is rejected, not guessed at"
    (is (= [] (fpa/parse-page
               "<table><thead><tr><td colspan=\"9\">RECEIVING</td></tr></thead>
                <tbody><tr class=\"mpb-player-1\"><td><a>X</a></td><td>1</td></tr></tbody></table>")))))
