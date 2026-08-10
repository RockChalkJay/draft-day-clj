(ns draft-day.benchmark.ids-test
  "The id bridge is load-bearing: untrimmed Sleeper-only joins reach ~28%
  coverage, trim + crosswalk reaches ~99%. Both halves are tested."
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.benchmark.sources.ids :as ids]
            [draft-day.ingestion.match :as match]))

(defn dp-row
  ([sleeper gsis name pos] (dp-row sleeper gsis name pos nil))
  ([sleeper gsis name pos fp]
   {"sleeper_id" sleeper "gsis_id" gsis "name" name "position" pos
    "fantasypros_id" (or fp "NA")}))

(deftest clean-gsis-trims-sleepers-leading-space
  ;; Sleeper emits " 00-0035229" for a large share of players; an untrimmed
  ;; equality join drops every one of them silently.
  (is (= "00-0035229" (ids/clean-gsis " 00-0035229")))
  (is (= "00-0034857" (ids/clean-gsis "00-0034857")))
  (testing "blank and nil collapse to nil rather than an empty-string key"
    (is (nil? (ids/clean-gsis "")))
    (is (nil? (ids/clean-gsis "   ")))
    (is (nil? (ids/clean-gsis nil)))))

(deftest crosswalk-skips-incomplete-rows
  (let [rows [(dp-row "4984" "00-0034857" "Josh Allen" "QB")
              (dp-row "9221" " 00-0036389" "Bijan Robinson" "RB")  ; leading space
              (dp-row "1111" "" "No Gsis" "WR")                    ; unusable
              (dp-row "" "00-0099999" "No Sleeper" "TE")]]         ; unusable
    (is (= {"4984" "00-0034857" "9221" "00-0036389"}
           (ids/crosswalk-from-rows rows)))))

(deftest resolver-prefers-own-id-then-falls-back
  (let [xwalk    {"9221" "00-0036389"}
        resolve* (ids/resolver xwalk)]
    (testing "the player's own (trimmed) gsis wins when present"
      (is (= "00-0034857" (resolve* "4984" " 00-0034857"))))
    (testing "crosswalk covers players Sleeper leaves without a gsis"
      (is (= "00-0036389" (resolve* "9221" nil)))
      (is (= "00-0036389" (resolve* "9221" ""))))
    (testing "unresolvable stays nil rather than inventing an id"
      (is (nil? (resolve* "0000" nil))))))

(deftest coverage-splits-by-resolution-path
  (let [xwalk (ids/crosswalk-from-rows [(dp-row "b" "00-0000002" "B" "RB")])
        ;; a resolves via its own field, b via the crosswalk, c not at all
        cov   (ids/coverage xwalk [["a" " 00-0000001"] ["b" nil] ["c" nil]])]
    (is (= 3 (:n cov)))
    (is (= 2 (:resolved cov)))
    (is (= 1 (:via-sleeper cov)))
    (is (= 1 (:via-crosswalk cov)))))

(deftest fp-crosswalk-treats-NA-as-absent
  ;; DynastyProcess writes the literal string "NA" for a missing fantasypros_id,
  ;; which is truthy. Counting it as present overstates coverage badly — the
  ;; column is only populated for ~70% of skill players.
  (let [xw (ids/fp-crosswalk-from-rows
            [(dp-row "1" "00-0030506" "Travis Kelce" "TE" "11594")
             (dp-row "2" "00-0036322" "Justin Jefferson" "WR" "NA")
             (dp-row "3" "00-0000003" "No Gsis" "WR" "")])]
    (is (= {"11594" "00-0030506"} xw))))

(deftest fp-resolver-falls-back-to-name-when-no-fp-id
  (let [fp-xw   (ids/fp-crosswalk-from-rows
                 [(dp-row "1" "00-0030506" "Travis Kelce" "TE" "11594")])
        name-xw (ids/name-crosswalk-from-rows
                 [(dp-row "2" "00-0036322" "Justin Jefferson" "WR" "NA")])
        resolve* (ids/fp-resolver fp-xw name-xw)]
    (testing "exact FantasyPros id wins"
      (is (= "00-0030506" (resolve* "11594" (match/key-for "Travis Kelce" "TE")))))
    (testing "the ~30% with no id on file resolve by normalized name"
      (is (= "00-0036322" (resolve* nil (match/key-for "Justin Jefferson" "WR")))))
    (testing "unresolvable stays nil"
      (is (nil? (resolve* "99999" (match/key-for "Nobody At All" "RB")))))))

(deftest nicknames-resolve-to-the-formal-name
  ;; ADP and ranking sources print the name a drafter says; DynastyProcess carries
  ;; the birth-certificate one. These recurred every season and were genuinely
  ;; draftable players, not fringe roster filler.
  (let [xw (ids/name-crosswalk-from-rows
            [(dp-row "1" "00-0035662" "Marquise Brown" "WR")
             (dp-row "2" "00-0036196" "Gabriel Davis" "WR")
             (dp-row "3" "00-0036919" "Kenneth Gainwell" "RB")])]
    (is (= "00-0035662" (get xw (match/key-for "Hollywood Brown" "WR"))))
    (is (= "00-0036196" (get xw (match/key-for "Gabe Davis" "WR"))))
    (is (= "00-0036919" (get xw (match/key-for "Kenny Gainwell" "RB"))))
    (testing "the formal name still resolves"
      (is (= "00-0035662" (get xw (match/key-for "Marquise Brown" "WR")))))))

(deftest junk-positions-register-under-every-skill-position
  ;; DynastyProcess files Rondale Moore as "XX". A position-keyed join then misses
  ;; on the POSITION even though the name is perfect, silently dropping him.
  (let [xw (ids/name-crosswalk-from-rows
            [(dp-row "1" "00-0036936" "Rondale Moore" "XX")])]
    (is (= "00-0036936" (get xw (match/key-for "Rondale Moore" "WR"))))
    (is (= "00-0036936" (get xw (match/key-for "Rondale Moore" "RB"))))
    (testing "a real position is NOT fanned out across the others"
      (let [clean (ids/name-crosswalk-from-rows
                   [(dp-row "2" "00-0000002" "Real Receiver" "WR")])]
        (is (= "00-0000002" (get clean (match/key-for "Real Receiver" "WR"))))
        (is (nil? (get clean (match/key-for "Real Receiver" "RB"))))))))

(deftest name-crosswalk-normalizes-like-the-app
  ;; Fantasy Football Calculator publishes names only, and writes "LeVeon Bell"
  ;; where Sleeper writes "Le'Veon Bell". Reusing ingestion.match/key-for means
  ;; this join normalizes exactly like the FantasyPros join already does.
  (let [xw (ids/name-crosswalk-from-rows
            [(dp-row "1" "00-0031687" "Le'Veon Bell" "RB")
             (dp-row "2" "00-0035229" "T.J. Hockenson" "TE")])]
    (is (= "00-0031687" (get xw (match/key-for "LeVeon Bell" "RB"))))
    (is (= "00-0035229" (get xw (match/key-for "TJ Hockenson" "TE"))))))
