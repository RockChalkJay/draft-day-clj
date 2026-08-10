(ns draft-day.ingestion.player-ids-test
  "The id bridge is load-bearing: untrimmed Sleeper-only joins reach ~28%
  coverage, trim + crosswalk reaches ~99%. Both halves are tested."
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.ingestion.player-ids :as ids]
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

;; ---- the pinned snapshot ----

(defn snap-row [m]
  (merge {"sleeper_id" "1" "gsis_id" "00-0000001" "name" "A Player"
          "position" "RB" "fantasypros_id" "NA" "espn_id" "NA" "pfr_id" "NA"
          "draft_year" "NA" "draft_round" "NA" "draft_pick" "NA"
          "draft_ovr" "NA" "birthdate" ""}
         m))

(deftest blank-and-na-are-both-absent
  (is (= "x" (ids/blank->nil " x ")))
  (is (nil? (ids/blank->nil "NA")))
  (is (nil? (ids/blank->nil "")))
  (is (nil? (ids/blank->nil "   ")))
  (is (nil? (ids/blank->nil nil))))

(deftest parse-long-field-handles-dynastyprocess-floats
  (is (= 2018 (ids/parse-long-field "2018.0")))
  (is (= 7 (ids/parse-long-field "7")))
  (is (nil? (ids/parse-long-field "NA")))
  (is (nil? (ids/parse-long-field nil)))
  (is (nil? (ids/parse-long-field "not a number"))))

(deftest snapshot-row-projects-only-what-is-present
  (testing "a row with no sleeper id is dropped — it is unreachable"
    (is (nil? (ids/snapshot-row (snap-row {"sleeper_id" "NA"})))))

  (testing "absent columns are omitted rather than stored as nil"
    (is (= {:sleeper "1" :gsis "00-0000001" :name "A Player" :position "RB"}
           (ids/snapshot-row (snap-row {})))))

  (testing "ids and draft capital carry through"
    (is (= {:sleeper "4984" :gsis "00-0034857" :fantasypros "17298"
            :espn "3918298" :pfr "AlleJo02" :name "Josh Allen" :position "QB"
            :draft-year 2018 :draft-round 1 :draft-pick 7 :draft-overall 7
            :birth-year 1996}
           (ids/snapshot-row
            (snap-row {"sleeper_id" "4984" "gsis_id" "00-0034857"
                       "fantasypros_id" "17298" "espn_id" "3918298"
                       "pfr_id" "AlleJo02" "name" "Josh Allen"
                       "position" "QB" "draft_year" "2018.0"
                       "draft_round" "1.0" "draft_pick" "7.0"
                       "draft_ovr" "7.0" "birthdate" "1996-05-21"}))))))

(deftest rows-to-snapshot-dedupes-on-sleeper-id
  (let [out (ids/rows->snapshot-rows
             [(snap-row {"sleeper_id" "1" "name" "First"})
              (snap-row {"sleeper_id" "1" "name" "Shadow"})
              (snap-row {"sleeper_id" "2" "name" "Other"})
              (snap-row {"sleeper_id" "NA" "name" "Unreachable"})])]
    (is (= ["First" "Other"] (mapv :name out))
        "first wins, so a refresh cannot reorder its way into a new mapping")))

(deftest snapshot-crosswalk-skips-rows-without-gsis
  (is (= {"1" "00-0000001"}
         (ids/snapshot-crosswalk [{:sleeper "1" :gsis "00-0000001"}
                                  {:sleeper "ARI"}]))))

(deftest committed-snapshot-is-internally-consistent
  (let [{:keys [schema-version n rows]} (ids/load-snapshot)]
    (is (= ids/snapshot-schema-version schema-version))
    (is (= n (count rows)) ":n must describe the rows it ships with")
    (is (> (count rows) 4000) "a truncated snapshot would silently thin the join")
    (is (every? :sleeper rows) "sleeper id is the join key; every row needs one")
    (is (= (count rows) (count (distinct (map :sleeper rows))))
        "a duplicate sleeper id would make derivation order-dependent")
    (is (> (count (ids/snapshot-crosswalk rows)) 4000))))
