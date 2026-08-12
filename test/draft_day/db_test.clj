(ns draft-day.db-test
  "Covers the pure roster/bye/column helpers in draft-day.db. These drive the
  board's red pulse, My Roster's amber marker and the persisted column layout,
  and until db moved to cljc none of them could be reached from the JVM."
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.scoring :as scoring]
            [draft-day.db :as db]))

;; ---- roster / teams ----

(deftest roster-template-expands-in-catalog-order
  (testing "each slot label repeats by its config count, in roster-order"
    (is (= ["QB" "RB" "RB" "WR" "WR" "TE" "FLEX" "K" "DST"
            "BENCH" "BENCH" "BENCH" "BENCH" "BENCH" "BENCH"]
           (db/roster-template db/default-roster))))

  (testing "an absent key contributes no slots rather than throwing"
    (is (= ["QB" "RB" "RB"] (db/roster-template {:qb 1 :rb 2}))))

  (testing "an all-zero config yields no slots"
    (is (= [] (db/roster-template {})))))

(deftest make-teams-named-builds-empty-full-bankroll-teams
  (let [teams (db/make-teams-named ["" "Zed" nil] {:qb 1 :rb 1} 200)]
    (testing "ids are positional"
      (is (= ["t0" "t1" "t2"] (mapv :team-id teams))))

    (testing "blank and nil names fall back to the positional default"
      (is (= ["You" "Zed" "Team 3"] (mapv :name teams))))

    (testing "every team starts with a full bankroll and an empty roster"
      (is (every? #(= 200 (:bankroll %)) teams))
      (is (every? (fn [t] (every? (comp nil? :player-id) (:roster t)))
                  teams))
      (is (= [{:pos "QB" :player-id nil} {:pos "RB" :player-id nil}]
             (:roster (first teams)))))))

(deftest make-teams-uses-default-names
  (is (= ["You" "Team 2" "Team 3"]
         (mapv :name (db/make-teams 3 {:qb 1} 200)))))

;; ---- roster exposure ----

(def by-id
  {"qb7"  {:position "QB" :bye 7}
   "rb5"  {:position "RB" :bye 5}
   "rb5b" {:position "RB" :bye 5}
   "rb6"  {:position "RB" :bye 6}
   "rb9"  {:position "RB" :bye 9}
   "wr9"  {:position "WR" :bye 9}})

(defn team-with [slots] {:roster (mapv (fn [[pos pid]] {:pos pos :player-id pid}) slots)})

(deftest roster-exposure-splits-starters-bench-and-open-slots
  (let [ex (db/roster-exposure
            (team-with [["QB" "qb7"] ["RB" "rb5"] ["WR" nil]
                        ["FLEX" "rb9"] ["K" nil] ["DST" nil]
                        ["BENCH" "rb6"] ["BENCH" nil]])
            by-id)]
    (testing "starters carry the player's resolved position, not the slot label"
      (is (= [{:player-id "qb7" :position "QB" :bye 7}
              {:player-id "rb5" :position "RB" :bye 5}]
             (:starters ex))))

    (testing "a filled FLEX slot is ignored even though it holds an RB"
      (is (not-any? #(= "rb9" (:player-id %)) (:starters ex))))

    (testing "bench holds the cover pool"
      (is (= [{:position "RB" :bye 6}] (:bench ex))))

    (testing "unfilled non-BENCH slots count, unfilled BENCH slots do not"
      (is (= 3 (:open-non-bench ex))))))

(deftest roster-exposure-tolerates-a-player-missing-from-the-universe
  (testing "an id absent from by-id yields nil position/bye rather than throwing"
    (is (= {:starters [{:player-id "ghost" :position nil :bye nil}]
            :bench [] :open-non-bench 0}
           (db/roster-exposure (team-with [["RB" "ghost"]]) {})))))

;; ---- board signal ----

(deftest board-bye-clash-pulses-only-while-the-lineup-fills
  (let [ex {:starters [{:player-id "rb5" :position "RB" :bye 5}]
            :open-non-bench 2}]
    (testing "same position on the same bye clashes"
      (is (db/board-bye-clash? "RB" 5 ex)))

    (testing "a different bye or a different position does not"
      (is (not (db/board-bye-clash? "RB" 6 ex)))
      (is (not (db/board-bye-clash? "WR" 5 ex))))

    (testing "a candidate with no known bye never pulses"
      (is (not (db/board-bye-clash? "RB" nil ex))))

    (testing "the board goes quiet once every starting slot is filled"
      (is (not (db/board-bye-clash? "RB" 5 (assoc ex :open-non-bench 0)))))))

;; ---- bye coverage ----
;; The `max 0, starters−covers` arithmetic per (position, bye) group.

(deftest uncovered-starter-ids-needs-one-bench-body-per-starter-in-a-week
  (testing "no starters, nothing uncovered"
    (is (= #{} (db/uncovered-starter-ids {:starters [] :bench []}))))

  (testing "a lone starter with no bench at all is uncovered"
    (is (= #{"rb5"}
           (db/uncovered-starter-ids
            {:starters [{:player-id "rb5" :position "RB" :bye 5}]
             :bench []}))))

  (testing "a bench body at the same position on a different bye covers it"
    (is (= #{}
           (db/uncovered-starter-ids
            {:starters [{:player-id "rb5" :position "RB" :bye 5}]
             :bench [{:position "RB" :bye 9}]}))))

  (testing "a bench body sharing the starter's bye is no cover"
    (is (= #{"rb5"}
           (db/uncovered-starter-ids
            {:starters [{:player-id "rb5" :position "RB" :bye 5}]
             :bench [{:position "RB" :bye 5}]}))))

  (testing "a bench body at another position is no cover"
    (is (= #{"rb5"}
           (db/uncovered-starter-ids
            {:starters [{:player-id "rb5" :position "RB" :bye 5}]
             :bench [{:position "WR" :bye 9}]}))))

  (testing "two starters in one week need two bench bodies; one covers one"
    (let [starters [{:player-id "rb5" :position "RB" :bye 5}
                    {:player-id "rb5b" :position "RB" :bye 5}]]
      (is (= 1 (count (db/uncovered-starter-ids
                       {:starters starters
                        :bench [{:position "RB" :bye 9}]}))))
      (is (= #{} (db/uncovered-starter-ids
                  {:starters starters
                   :bench [{:position "RB" :bye 9}
                           {:position "RB" :bye 10}]})))))

  (testing "one bench body covers starters across *different* weeks freely"
    (is (= #{}
           (db/uncovered-starter-ids
            {:starters [{:player-id "rb5" :position "RB" :bye 5}
                        {:player-id "rb6" :position "RB" :bye 6}]
             :bench [{:position "RB" :bye 9}]})))))

(deftest covers-starter-mirrors-the-uncovered-set
  (let [exposure {:starters [{:player-id "rb5" :position "RB" :bye 5}]
                  :bench []}]
    (testing "a same-position candidate on a different bye covers"
      (is (db/covers-starter? "RB" 9 exposure)))

    (testing "sharing the starter's bye does not cover"
      (is (not (db/covers-starter? "RB" 5 exposure))))

    (testing "another position does not cover"
      (is (not (db/covers-starter? "WR" 9 exposure))))

    (testing "a candidate with no known bye never covers"
      (is (not (db/covers-starter? "RB" nil exposure))))

    (testing "nothing to cover once the starter already has a bench body"
      (is (not (db/covers-starter?
                "RB" 9 (assoc exposure :bench [{:position "RB" :bye 9}])))))))

;; ---- columns ----

(deftest reconcile-columns-migrates-a-persisted-layout
  (testing "nil or empty stored config yields the full catalog at defaults"
    (is (= (db/default-columns) (db/reconcile-columns nil)))
    (is (= (db/default-columns) (db/reconcile-columns []))))

  (testing "a key no longer in the catalog is dropped"
    (is (not-any? #(= :tier (:key %))
                  (db/reconcile-columns [{:key :tier :visible? true}
                                         {:key :name :visible? true}]))))

  (testing "stored order and visibility survive; new catalog keys append"
    (let [out (db/reconcile-columns [{:key :vorp :visible? false}
                                     {:key :name :visible? true}])]
      (is (= [:vorp :name] (mapv :key (take 2 out)))
          "stored keys keep their stored order, ahead of the appended ones")
      (is (= [false true] (mapv :visible? (take 2 out)))
          "stored visibility is preserved, not reset to the catalog default")
      (is (= (set (map :key db/column-catalog)) (set (map :key out)))
          "every catalog key is present exactly once")
      (is (= (count db/column-catalog) (count out)))))

  (testing "reconciling is idempotent"
    (let [once (db/reconcile-columns [{:key :vorp :visible? false}])]
      (is (= once (db/reconcile-columns once))))))

(deftest column-catalog-is-internally-consistent
  (testing "keys are unique"
    (is (= (count db/column-catalog) (count db/columns-by-key))))

  (testing "every column can be sorted — a new column needs an accessor"
    (is (= #{} (into #{}
                     (remove db/sort-accessors)
                     (map :key db/column-catalog))))))

;; ---- initial db ----

(deftest default-db-is-internally-consistent
  (let [d (db/default-db)]
    (testing "teams match the configured league size and roster"
      (is (= 12 (count (:teams d))))
      (is (= (count (db/roster-template db/default-roster))
             (count (:roster (first (:teams d)))))))

    (testing "my-team-id names a team that exists"
      (is (some #(= (:my-team-id d) (:team-id %)) (:teams d))))

    (testing "every persisted key is present in the initial db"
      (is (every? #(contains? d %) db/persist-keys)))))

;; ---- player-id migration ----

(def ^:private universe
  [{:player-id "00-0034857" :ids {:sleeper "4984" :gsis "00-0034857"}}
   {:player-id "ARI"        :ids {:sleeper "ARI" :team "ARI"}}
   {:player-id "99999"      :ids {:sleeper "99999"}}])

(deftest sleeper-to-player-id-maps-unresolved-ids-to-themselves
  (is (= {"4984" "00-0034857" "ARI" "ARI" "99999" "99999"}
         (db/sleeper->player-id universe)))

  (testing "a player with no :ids envelope contributes nothing"
    (is (= {} (db/sleeper->player-id [{:player-id "x"}])))))

(deftest remap-draft-ids-rewrites-every-place-an-id-is-held
  (let [before {:drafted   {"4984" {:price 42 :team-id "t0"}
                            "ARI"  {:price 1 :team-id "t0"}}
                :picks     [{:player-id "4984" :price 42}
                            {:player-id "ARI" :price 1}]
                :watchlist #{"4984"}
                :nominated-id "4984"
                :teams     [{:team-id "t0"
                             :roster [{:pos "QB" :player-id "4984"}
                                      {:pos "RB" :player-id nil}]}]}
        after  (db/remap-draft-ids before (db/sleeper->player-id universe))]
    (is (= {"00-0034857" {:price 42 :team-id "t0"}
            "ARI"        {:price 1 :team-id "t0"}}
           (:drafted after)))
    (is (= ["00-0034857" "ARI"] (mapv :player-id (:picks after))))
    (is (= #{"00-0034857"} (:watchlist after)))
    (is (= "00-0034857" (:nominated-id after)))
    (is (= [{:pos "QB" :player-id "00-0034857"} {:pos "RB" :player-id nil}]
           (get-in after [:teams 0 :roster]))
        "an empty slot stays empty rather than becoming a remapped nil")
    (is (= 42 (get-in after [:drafted "00-0034857" :price]))
        "what a manager paid survives the remap")))

(deftest remap-draft-ids-is-idempotent
  (let [xwalk (db/sleeper->player-id universe)
        once  (db/remap-draft-ids {:drafted {"4984" {:price 42}}
                                   :picks [{:player-id "4984"}]
                                   :watchlist #{"4984"} :nominated-id nil
                                   :teams []}
                                  xwalk)]
    (is (= once (db/remap-draft-ids once xwalk))
        "running on already-migrated state must change nothing")))

(deftest remap-draft-ids-never-drops-an-unknown-id
  ;; A stale cache or the offline sample may not contain a drafted player. That
  ;; is not evidence the pick is wrong, and losing it would destroy a record of
  ;; what was actually paid.
  (let [after (db/remap-draft-ids {:drafted {"unknown" {:price 7}}
                                   :picks [{:player-id "unknown"}]
                                   :watchlist #{} :nominated-id nil :teams []}
                                  (db/sleeper->player-id universe))]
    (is (= {"unknown" {:price 7}} (:drafted after)))
    (is (= ["unknown"] (mapv :player-id (:picks after))))))

;; ---- scoring config ----

(deftest the-custom-editor-can-reach-every-stat-key
  ;; scoring-catalog and stat-keys are two independent literals. A key in one and
  ;; not the other is invisible either way round: a league import could set a
  ;; weight the editor cannot show, or the editor could offer a weight the scoring
  ;; engine ignores.
  (is (= (set scoring/stat-keys)
         (set (mapcat (fn [g] (map first (:stats g))) db/scoring-catalog)))))

(deftest the-editor-labels-every-stat-it-offers
  (doseq [{:keys [group stats]} db/scoring-catalog]
    (is (seq group))
    (doseq [[k label] stats]
      (is (keyword? k))
      (is (and (string? label) (seq label)) (str k " has no label")))))

(deftest unprojected-stats-are-real-stat-keys
  (is (every? (set scoring/stat-keys) db/unprojected-stats)))

(deftest reconcile-config-repairs-what-localstorage-may-hold
  (testing "a blob written before a key existed gets the current default"
    (is (= (:starting-bankroll db/default-config)
           (:starting-bankroll (db/reconcile-config {:num-teams 10})))))

  (testing "a key the app has since dropped does not survive"
    (is (not (contains? (db/reconcile-config {:num-tiers 5}) :num-tiers))))

  (testing "nil scoring — which the old enable-custom-scoring race could store —
            becomes the default rather than reaching Settings and throwing"
    (is (= (:scoring db/default-config) (:scoring (db/reconcile-config {:scoring nil})))))

  (testing "a custom map predating a stat key gains it at zero, not as a hole"
    (let [s (:scoring (db/reconcile-config {:scoring {:rec 1.0}}))]
      (is (= 1.0 (:rec s)))
      (is (= (set scoring/stat-keys) (set (keys s))))
      (is (zero? (:pass_td s)))))

  (testing "a preset keyword is left alone, and junk falls back"
    (is (= :half-ppr (:scoring (db/reconcile-config {:scoring :half-ppr}))))
    (is (= (:scoring db/default-config) (:scoring (db/reconcile-config {:scoring :bogus})))))

  (testing "partial nested maps are filled rather than replaced"
    (is (= (:bench db/default-roster) (:bench (:roster (db/reconcile-config {:roster {:qb 2}})))))
    (is (= 2 (:qb (:roster (db/reconcile-config {:roster {:qb 2}})))))))
