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

(deftest move-column-onto-reorders-by-key
  ;; :a and :c are hidden, so a reorder that only ever reasoned about the visible
  ;; columns would still have to leave these two in the right slots
  (let [cols [{:key :a :visible? false} {:key :b :visible? true}
              {:key :c :visible? false} {:key :d :visible? true}]
        ks   #(mapv :key %)]

    (testing "dragging rightwards drops the column just past the target"
      (is (= [:b :c :a :d] (ks (db/move-column-onto cols :a :c)))))

    (testing "dragging leftwards drops it just before the target"
      (is (= [:a :d :b :c] (ks (db/move-column-onto cols :d :b)))))

    (testing "both ends are reachable — the case that fails if the target index
              is measured after the removal instead of before it"
      (is (= [:b :c :d :a] (ks (db/move-column-onto cols :a :d))))
      (is (= [:d :a :b :c] (ks (db/move-column-onto cols :d :a)))))

    (testing "a hidden column rides along with its own key and visibility"
      (let [out (db/move-column-onto cols :d :b)]
        (is (= {:key :c :visible? false} (nth out 3)))
        (is (= (frequencies cols) (frequencies out))
            "nothing is invented, lost, or has its visibility rewritten")))

    (testing "a drop onto itself is a no-op"
      (is (= cols (db/move-column-onto cols :b :b))))

    (testing "an absent key is a no-op rather than a throw"
      (is (= cols (db/move-column-onto cols :gone :b)))
      (is (= cols (db/move-column-onto cols :b :gone))))

    (testing "every drop is a permutation — no move can drop or duplicate one"
      (let [all   (ks cols)
            drops (mapcat (fn [f] (map #(db/move-column-onto cols f %) all)) all)]
        (is (every? #(= (frequencies cols) (frequencies %)) drops))))))

(deftest move-onto-orders-bare-ids-the-same-way-it-orders-columns
  ;; The watch list holds player-ids, not maps, so it reorders through the same
  ;; function with `identity` for a key. If these two ever disagree, one of the
  ;; two drags on screen is lying about where the row will land.
  (let [ids [:a :b :c :d]]
    (is (= [:b :c :a :d] (db/move-watch-onto ids :a :c)) "downwards, just past")
    (is (= [:a :d :b :c] (db/move-watch-onto ids :d :b)) "upwards, just before")
    (is (= [:b :c :d :a] (db/move-watch-onto ids :a :d)) "the far end is reachable")
    (is (= ids (db/move-watch-onto ids :b :b)) "a drop onto itself is a no-op")
    (is (= ids (db/move-watch-onto ids :gone :b)) "an absent id is a no-op")
    (testing "every drop is a permutation"
      (is (every? #(= (frequencies ids) (frequencies %))
                  (mapcat (fn [f] (map #(db/move-watch-onto ids f %) ids)) ids))))
    (testing "it agrees with the column reorder on the same order"
      (let [cols (mapv #(hash-map :key %) ids)]
        (is (= (db/move-watch-onto ids :a :c)
               (mapv :key (db/move-column-onto cols :a :c))))))))

(deftest rank-key-is-a-total-order-over-the-whole-board
  ;; It moved out of subs.cljs so the watch-list sort could reach it, which also
  ;; put it under `lein test` for the first time.
  (let [p (fn [pos worth vorp pts] {:position pos :worth worth :vorp vorp :points pts})]
    (is (neg? (compare (db/rank-key (p "RB" 58 120.0 240.0))
                       (db/rank-key (p "WR" 55 110.0 230.0))))
        "more Worth sorts first")
    (testing "a $1 kicker sits below a $1 skill player, which is the only thing
              separating them once Worth ties"
      (is (pos? (compare (db/rank-key (p "K" 1 nil 130.0))
                         (db/rank-key (p "WR" 1 -20.0 90.0))))))
    (testing "a player the model never scored compares rather than throwing"
      (is (some? (compare (db/rank-key {:position "TE"})
                          (db/rank-key (p "TE" 4 10.0 100.0))))))))

(deftest sort-watchlist-rewrites-the-stored-order-once
  (let [by-id {"bijan" {:player-id "bijan" :position "RB" :pos-rank 1 :worth 58 :vorp 120.0 :points 260.0}
               "lamb"  {:player-id "lamb"  :position "WR" :pos-rank 1 :worth 55 :vorp 110.0 :points 250.0}
               "gibbs" {:player-id "gibbs" :position "RB" :pos-rank 2 :worth 51 :vorp 100.0 :points 240.0}
               "kicker" {:player-id "kicker" :position "K" :pos-rank 1 :worth 1 :vorp nil :points 130.0}}
        ids   ["kicker" "gibbs" "lamb" "bijan"]]
    (is (= ["bijan" "lamb" "gibbs" "kicker"] (db/sort-watchlist ids by-id :rank))
        "rank puts the best first and the kicker last")
    (is (= ["bijan" "lamb" "gibbs" "kicker"] (db/sort-watchlist ids by-id :worth))
        "worth is highest-first — and identical to :rank, since rank-key leads
         with Worth and every tie falls through to it. Both buttons ship anyway:
         Worth is the number on the row.")
    (is (= ["kicker" "bijan" "gibbs" "lamb"] (db/sort-watchlist ids by-id :position))
        "position groups alphabetically and orders by pos-rank inside the group")

    (testing ":worth cannot diverge from :rank, however the tails are shaped"
      (let [pos  ["QB" "RB" "WR" "TE" "K" "DST"]
            ps   (mapv (fn [i]
                         {:player-id (str i)
                          :position  (nth pos (mod (* i 7) 6))
                          :pos-rank  (inc (mod (* i 13) 40))
                          ;; the $0 and $1 tails are most of a real board
                          :worth     (nth [0 1 1 1 2 5 12 40] (mod (* i 5) 8))
                          :vorp      (when (pos? (mod i 5)) (double (mod (* i 31) 200)))
                          :points    (double (mod (* i 17) 300))})
                       (range 300))
            byid (db/index-by-id ps)
            all  (mapv :player-id ps)]
        (is (= (db/sort-watchlist all byid :worth) (db/sort-watchlist all byid :rank)))))

    (testing "every sort is a permutation — a watched player is never dropped"
      (is (every? #(= (frequencies ids) (frequencies (db/sort-watchlist ids by-id %)))
                  [:rank :worth :position])))

    (testing "an id the board cannot resolve keeps its place at the back rather
              than being sorted by a nil player — a drafted watch-list entry is
              still in the vector, and an undo has to restore him"
      (is (= ["bijan" "gibbs" "unknown" "drafted"]
             (db/sort-watchlist ["unknown" "gibbs" "drafted" "bijan"]
                                (select-keys by-id ["bijan" "gibbs"]) :rank))))

    (testing "before the first /api/rankings reply there is nothing to sort by,
              so the hand-built order survives untouched"
      (is (= ids (db/sort-watchlist ids {} :rank))))

    (is (= ids (db/sort-watchlist ids by-id :bargain))
        "an unknown key is a no-op: a reordered list is not a safe guess")
    (is (= [] (db/sort-watchlist [] by-id :rank)))

    (testing "it returns a vector — the order is stored, and conj/drag depend on it"
      (is (vector? (db/sort-watchlist ids by-id :rank)))
      (is (vector? (db/sort-watchlist ids by-id :bargain))))))

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

;; ---- tier scale ----

(deftest the-scale-follows-the-position-filter
  ;; The manager never picks a scale; choosing a position filter has already said
  ;; which question they are asking.
  (is (= :overall (db/tier-scale nil)))
  (is (= :position (db/tier-scale "RB"))))

(deftest player-tier-reads-the-scale-it-is-given
  (let [p {:tiers {:overall 5 :position 2} :tier 2}]
    (is (= 2 (db/player-tier p :position)))
    (is (= 5 (db/player-tier p :overall)))
    (is (nil? (db/player-tier {} :overall))
        "a board that predates the scales reads as untiered rather than throwing")))

(deftest usage-columns-are-catalogued-and-sortable
  ;; Every column the board can render needs a sort accessor, or clicking its
  ;; header silently sorts by nil. The usage columns arrive from two different
  ;; sources, so this checks the wiring rather than the values.
  (let [usage [:prior-tgt :prior-rec :prior-tgt-pct :proj-tgt :proj-rec]]
    (doseq [k usage]
      (is (contains? db/columns-by-key k) (str k " is missing from the catalog"))
      (is (contains? db/sort-accessors k) (str k " has no sort accessor")))
    (testing "they are opt-in — the board is already wide"
      (is (every? #(false? (:visible? %))
                  (filter (comp (set usage) :key) (db/default-columns)))))
    (testing "the accessors point at the keys ingestion actually ships"
      (is (= :nflverse/prior-targets (db/sort-accessors :prior-tgt)))
      (is (= :nflverse/prior-target-share (db/sort-accessors :prior-tgt-pct)))
      (is (= :espn/proj-targets (db/sort-accessors :proj-tgt))))))

(deftest pos-sort-key-orders-by-rank-not-by-spelling
  ;; The bug this accessor exists to prevent: the cell renders "RB1"/"RB10"/"RB2"
  ;; and `compare` on those strings reads digit by digit, so the column would
  ;; run 1, 10, 11, 2. Sorting the key itself has to give the numeric order.
  (let [p    (fn [pos n] {:position pos :pos-rank n})
        rbs  (shuffle [(p "RB" 2) (p "RB" 11) (p "RB" 1) (p "RB" 10)])]
    (is (= [1 10 11 2]
           (mapv :pos-rank
                 (sort-by #(str (:position %) (:pos-rank %)) rbs)))
        "sanity: this is what sorting the rendered label would have done")
    (is (= [1 2 10 11]
           (mapv :pos-rank (sort-by db/pos-sort-key rbs)))))
  (testing "positions stay grouped, ordered among themselves"
    (let [b (shuffle [{:position "WR" :pos-rank 1} {:position "RB" :pos-rank 2}
                      {:position "RB" :pos-rank 1} {:position "WR" :pos-rank 2}])]
      (is (= [["RB" 1] ["RB" 2] ["WR" 1] ["WR" 2]]
             (mapv (juxt :position :pos-rank) (sort-by db/pos-sort-key b))))))
  (testing "an unranked row sorts to the back of its own position, not the board"
    (let [b [{:position "RB" :pos-rank nil} {:position "RB" :pos-rank 1}
             {:position "WR" :pos-rank 1}]]
      (is (= [["RB" 1] ["RB" nil] ["WR" 1]]
             (mapv (juxt :position :pos-rank) (sort-by db/pos-sort-key b)))))))

;; ---- waiver board ----

(deftest the-two-catalogs-do-not-leak-into-each-other
  (testing "a waiver layout starts from its own catalog, not the board's"
    (is (= (mapv :key db/waiver-column-catalog)
           (mapv :key (db/default-waiver-columns)))))
  (testing "the two catalogs do not leak into each other"
    (let [waiver-keys (set (map :key db/waiver-column-catalog))
          board-keys  (set (map :key db/column-catalog))]
      (is (not (contains? waiver-keys :worth)) "no auction dollars on a waiver board")
      (is (not (contains? waiver-keys :market)))
      (is (not (contains? board-keys :bid)))
      (is (not-any? #(contains? board-keys (:key %))
                    (filter #(#{:bid :upgrade :ros :trend} (:key %))
                            db/waiver-column-catalog))))))

(deftest every-waiver-column-can-be-sorted-and-labelled
  ;; A column with no accessor silently falls back to :upgrade, so the header
  ;; lights up and the order does not change — which reads as a broken table.
  (doseq [{:keys [key label tooltip]} db/waiver-column-catalog]
    (is (contains? db/waiver-sort-accessors key) (str key " has no sort accessor"))
    (is (seq label) (str key " has no label"))
    (is (seq tooltip) (str key " has no tooltip"))))

(deftest waiver-rank-key-is-a-total-order
  ;; Upgrade alone is not one: most of a free-agent pool is worse than the man
  ;; you would drop, so the tail collapses onto near-equal negatives that a
  ;; stable sort would leave in whatever order the server emitted.
  (let [p (fn [nm up ros] {:player-name nm :upgrade up :ros-points ros})
        ps [(p "c" 0.0 10.0) (p "a" 0.0 90.0) (p "b" 12.0 5.0) (p "d" 0.0 90.0)]]
    (is (= ["b" "a" "d" "c"] (mapv :player-name (sort-by db/waiver-rank-key ps)))
        "upgrade first, then rest-of-season points, then the name")
    (testing "a missing value reads as 0 rather than throwing inside the key"
      (is (vector? (db/waiver-rank-key {})))
      (is (= 2 (count (sort-by db/waiver-rank-key [{} {:upgrade 1.0}])))))))

(deftest reconcile-league-sync-throws-away-what-it-cannot-repair
  ;; Unlike the other reconcilers this one may discard: a synced league is a
  ;; cache of somebody else's state, re-fetchable in one click, so a bad shape
  ;; costs a button press. A half-repaired one costs a board that believes the
  ;; wrong people are rostered.
  (is (nil? (db/reconcile-league-sync nil)))
  (is (nil? (db/reconcile-league-sync "nonsense")))
  (is (nil? (db/reconcile-league-sync {:waiver {:type :faab}})) "no :teams at all")
  (testing "a good one passes through with its teams intact"
    (let [ls {:teams [{:roster-id 1 :player-ids ["a" "b"] :starter-ids ["a"]}]
              :waiver {:type :faab :budget 100}}]
      (is (= ["a" "b"] (:player-ids (first (:teams (db/reconcile-league-sync ls))))))
      (is (= {:type :faab :budget 100} (:waiver (db/reconcile-league-sync ls)))))))

(deftest a-team-with-no-player-ids-is-repaired-not-trusted
  ;; The shape that actually matters: it reaches `waiver/rostered-index` as a
  ;; team holding nobody, and every player on it silently becomes a free agent.
  (let [out (db/reconcile-league-sync {:teams [{:roster-id 1}
                                               {:roster-id 2 :player-ids ["a" nil "b"]
                                                :active-ids ["a" nil]}
                                               "not a team"]})]
    (is (= 2 (count (:teams out))) "a non-map team is dropped")
    (is (= [] (:player-ids (first (:teams out)))))
    (is (= ["a" "b"] (:player-ids (second (:teams out)))) "nils inside are dropped")
    (is (every? vector? (map :starter-ids (:teams out))))
    (testing ":active-ids is repaired too — it decides whether a claim needs a drop"
      (is (= [] (:active-ids (first (:teams out)))))
      (is (= ["a"] (:active-ids (second (:teams out))))))))

(deftest a-persisted-sync-keeps-the-facts-that-let-it-be-redone
  ;; The league id makes a re-sync one click, and the seat count decides whether
  ;; a claim costs a drop. Both ride inside the league entry's :sync, which is
  ;; persisted, so the reconciler must not drop keys it does not recognise.
  (let [out (db/reconcile-league-sync
             {:teams [{:roster-id 1 :player-ids ["a"] :active-ids ["a"]}]
              :waiver {:type :faab :budget 100}
              :roster-size 15 :league-id "987654" :playoff-week-start 15})]
    (is (= "987654" (:league-id out)))
    (is (= 15 (:roster-size out)))
    (is (= 15 (:playoff-week-start out)))))

(deftest the-persisted-slice-carries-the-in-season-state
  ;; A sync that had to be redone on every page load would be a sync nobody
  ;; uses. The transient halves — the board itself and its request stamp — stay
  ;; out, exactly as :ranked and :recompute-seq do.
  ;;
  ;; The sync and the roster id now ride inside `:leagues`, because a manager has
  ;; one of each *per league*; `:active-league` is what says which of them the
  ;; board is currently about, and is useless without them.
  (is (every? (set db/persist-keys) [:accounts :leagues :active-league :waiver-columns]))
  (is (not-any? (set db/persist-keys) [:waivers :waiver-seq :waiver-status
                                       :league-choices])))

(deftest a-league-is-keyed-by-provider-as-well-as-by-id
  ;; A league id is only unique *within* a provider. A bare id collides the day a
  ;; second provider arrives, and the collision is the silent kind: an ESPN
  ;; league quietly reading a Sleeper league's rosters.
  (is (= "sleeper:123" (db/league-key "sleeper" "123")))
  (is (= "sleeper:123" (db/league-key :sleeper "123")) "a keyword provider spells the same")
  (is (not= (db/league-key "sleeper" "123") (db/league-key "espn" "123"))))

(deftest the-active-league-is-the-entry-not-the-key
  (let [db {:leagues {"sleeper:1" {:name "RaiderNation"}} :active-league "sleeper:1"}]
    (is (= {:name "RaiderNation"} (db/active-league db)))
    (is (nil? (db/active-league (assoc db :active-league "sleeper:9"))))
    (is (nil? (db/active-league {})))))

(deftest a-config-edit-is-written-to-the-league-not-only-to-the-copy
  ;; `:config` at the top of db is the *active* league's config, so every
  ;; existing consumer reads one key and knows nothing about leagues. That copy
  ;; is only safe while one function writes it: a writer that skipped the mirror
  ;; would have its edit reverted, silently, by the next switch away and back.
  (let [db  {:leagues {"sleeper:1" {:config {:scoring :ppr}}} :active-league "sleeper:1"
             :config {:scoring :ppr}}
        db' (db/update-config db assoc :scoring :standard)]
    (is (= :standard (get-in db' [:config :scoring])))
    (is (= :standard (get-in db' [:leagues "sleeper:1" :config :scoring]))))
  (testing "with no league active it is still just the config"
    (let [db' (db/set-config {:config {:scoring :ppr}} {:scoring :standard})]
      (is (= {:scoring :standard} (:config db')))
      (is (nil? (:leagues db'))))))

(deftest a-ranked-board-is-stamped-with-the-rules-it-was-priced-under
  ;; A board is valid for the rules it was priced under, not for a moment in
  ;; time — which is what separates "a pick out of date" (keep it readable) from
  ;; "about a different league" (say so).
  (is (= [:num-teams :roster :scoring :starting-bankroll]
         (vec (sort (keys (db/rules-stamp db/default-config)))))
      "exactly the four the server prices against")

  (testing "the client-only budget plan is not one of them"
    ;; It feeds no valuation, which is why `:set-position-budget` skips
    ;; `:recompute` — so a board is not made wrong by an edit to it.
    (is (= (db/rules-stamp db/default-config)
           (db/rules-stamp (assoc db/default-config :budget-plan {:rb 80})))))

  (testing "and each of the four does move it"
    (doseq [[k v] {:scoring :standard :num-teams 14 :starting-bankroll 300
                   :roster (assoc db/default-roster :bench 7)}]
      (is (not= (db/rules-stamp db/default-config)
                (db/rules-stamp (assoc db/default-config k v)))
          (str k " left the stamp unchanged")))))

(deftest an-archived-draft-names-the-league-it-was-drafted-in
  ;; It reads the active league entry. `get-in` on a key that no longer exists
  ;; yields nil rather than throwing, so pointing this at the wrong place would
  ;; cost every future archive its league with nothing to say so.
  (let [entry (db/archive-entry
               {:leagues {"sleeper:1" {:name "RaiderNation" :season "2026"}}
                :active-league "sleeper:1"
                :my-team-id "t0" :config {:starting-bankroll 200}
                :teams [] :drafted {} :picks [{:player-id "a"}]}
               "2026-09-06T00:00:00Z")]
    (is (= "RaiderNation" (:league entry)))
    (is (= "2026" (:season entry))))
  (testing "and says nothing rather than guessing when no league was connected"
    (let [entry (db/archive-entry {:picks [{:player-id "a"}]} "2026-09-06T00:00:00Z")]
      (is (nil? (:league entry)))
      (is (= 1 (count (:picks entry)))))))
