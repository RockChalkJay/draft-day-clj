(ns draft-day.rankings.waiver-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.rankings.waiver :as waiver]))

(defn- p
  "A board player. `:ids` carries the Sleeper id so the crosswalk has something
  to do, exactly as a real universe row does."
  [id pos pts & {:keys [sleeper]}]
  {:player-id id :player-name (str "P" id) :position pos :ros-points pts
   :ids {:sleeper (or sleeper id)}})

(def ^:private board
  (into [(p "star" "RB" 180.0) (p "good" "WR" 140.0) (p "ok" "WR" 90.0)
         (p "meh" "RB" 40.0) (p "bad" "TE" 10.0)]
        ;; A spread that runs below the drop candidate as well as above him:
        ;; most of a real free-agent pool is worse than the worst man rostered.
        (map #(p (str "filler" %) "WR" (- 80.0 (* 2.0 %))) (range 40))))

(def ^:private league
  {:teams [{:roster-id 1 :name "Mine"   :player-ids ["star" "meh"] :faab-left 60}
           {:roster-id 2 :name "Rivals" :player-ids ["bad"]        :faab-left 95}]
   :waiver {:type :faab :budget 100}})

(defn- run [& {:as over}]
  (waiver/waiver-board board (merge {:league league :my-roster-id 1 :roster-size 2
                                     :num-teams 12 :through-week 8 :season-games 17}
                                    over)))

;; ---- availability ----

(deftest a-rostered-player-is-not-on-the-waiver-board
  (let [{:keys [players rostered]} (run)
        ids (set (map :player-id players))]
    (is (not-any? ids ["star" "meh" "bad"]))
    (is (contains? ids "good"))
    (is (= {"star" "Mine" "meh" "Mine" "bad" "Rivals"} rostered)
        "who has him, answered without re-sending the rows")))

(deftest roster-ids-are-translated-out-of-the-providers-id-space
  ;; Sleeper hands back its own ids; the board is keyed by GSIS wherever one
  ;; resolved. An id with no entry maps to itself, which is how team defenses
  ;; and unmapped players survive.
  (let [b   [(p "00-gsis" "RB" 100.0 :sleeper "4034") (p "SF" "DST" 50.0)]
        out (waiver/waiver-board
             b {:league {:teams [{:roster-id 1 :name "Mine" :player-ids ["4034" "SF"]}]
                         :waiver {:type :rolling}}
                :my-roster-id 1 :num-teams 12 :through-week 4 :season-games 17})]
    (is (empty? (:players out)) "both are rostered, under either id spelling")
    (is (= {"00-gsis" "Mine" "SF" "Mine"} (:rostered out)))))

(deftest with-no-league-synced-everyone-is-free
  ;; Not an error — a manager who has not connected a league yet still gets a
  ;; rest-of-season ranking, which is useful on its own.
  (let [out (waiver/waiver-board board {:league nil :num-teams 12
                                        :through-week 8 :season-games 17})]
    (is (= (count board) (count (:players out))))
    (is (every? #(nil? (:bid %)) (:players out)))))

;; ---- what a claim costs ----

(deftest the-claim-costs-a-roster-spot-not-a-positional-slot
  ;; My worst player is an RB; the upgrade for a WR is still measured against
  ;; him, because he is the seat that has to come free.
  (let [{:keys [players]} (run)
        good (first (filter #(= "good" (:player-id %)) players))]
    (is (= "meh" (get-in good [:drop-candidate :player-id])))
    (is (= 100.0 (:upgrade good)) "140 rest-of-season points minus the 40 dropped")))

(deftest an-open-roster-spot-costs-nothing
  (let [{:keys [players]} (run :roster-size 6)
        good (first (filter #(= "good" (:player-id %)) players))]
    (is (nil? (:drop-candidate good)))
    (is (= 140.0 (:upgrade good)) "nothing is given up, so the gain is the whole line")))

(deftest an-unknown-roster-size-treats-the-roster-as-full
  ;; Naming a drop that was not needed costs a suggestion; missing one that was
  ;; costs a roster spot the manager did not know he was spending.
  (let [{:keys [players]} (run :roster-size nil)]
    (is (every? #(some? (:drop-candidate %)) players))))

(deftest a-rostered-player-the-board-cannot-value-is-not-assumed-worthless
  ;; "We have no projection for him" and "he is projected to score nothing" are
  ;; different claims, and only one of them is evidence.
  (let [lg  (assoc-in league [:teams 0 :player-ids] ["star" "meh" "ghost"])
        {:keys [players]} (run :league lg :roster-size 3)
        good (first (filter #(= "good" (:player-id %)) players))]
    (is (= "meh" (get-in good [:drop-candidate :player-id]))
        "the drop stays a player the manager can check")))

(deftest upgrade-can-be-negative-and-says-so
  ;; A free agent worse than my worst player is not an add. Clamping that to
  ;; zero would make the whole tail of the board look equally plausible.
  (let [{:keys [players]} (run)
        worse (first (filter #(= "bad" (:player-id %)) players))]
    (is (nil? worse) "he is rostered")
    (let [tail (filter #(neg? (:upgrade %)) players)]
      (is (seq tail))
      (is (every? #(zero? (:bid %)) tail) "and none of them is worth bidding on"))))

;; ---- the bid ----

(deftest bids-conserve-the-budget-across-the-claims-still-available
  ;; The property the whole rule stands on. `claims-left` bounds the pool, so
  ;; the players a manager could actually still add divide his budget between
  ;; them rather than every free agent rounding to nothing.
  (let [{:keys [players faab claims-left]} (run)
        n    claims-left
        top  (->> players (sort-by #(- (:upgrade %))) (take n))
        spend (reduce + 0 (map :bid top))]
    (is (pos? n))
    (is (<= (- (:left faab) n) spend (+ (:left faab) n))
        "the top claims sum to the budget, within a dollar of rounding each")))

(deftest fewer-runs-left-means-a-bigger-share-each
  ;; How FAAB actually behaves: hold back in September, spend it in December.
  (let [early (:bid (first (filter #(= "good" (:player-id %)) (:players (run :through-week 2)))))
        late  (:bid (first (filter #(= "good" (:player-id %)) (:players (run :through-week 15)))))]
    (is (< early late))))

(deftest the-fantasy-playoffs-end-the-bidding-season-not-the-nfls
  ;; A claim made once the playoffs are under way buys at most a game or two.
  (is (= 6 (waiver/claims-left {:through-week 8 :season-games 17 :playoff-week-start 15})))
  (is (= 9 (waiver/claims-left {:through-week 9 :season-games 17}))
      "no playoff week reported falls back to the NFL season, erring long")
  (is (= 0 (waiver/claims-left {:through-week 18 :season-games 17}))))

(deftest a-league-that-does-not-run-faab-gets-no-bid-at-all
  ;; A zero would read as "worth nothing"; the truth is "there is nothing to
  ;; bid". Same distinction league-sync keeps by reporting :faab-left nil.
  (let [lg  (assoc league :waiver {:type :rolling :budget 0})
        lg  (update lg :teams (fn [ts] (mapv #(dissoc % :faab-left) ts)))
        {:keys [players faab]} (run :league lg)]
    (is (every? #(nil? (:bid %)) players))
    (is (= :rolling (:type faab)))
    (is (nil? (:rival-max faab)))))

(deftest the-wire-spelling-of-faab-still-buys-players
  ;; The league round-trips through the browser as JSON, and `read-json-body`
  ;; keywordizes keys, not values — so a real request carries the string "faab".
  ;; Comparing against the keyword alone passed every server-side test and
  ;; produced a nil bid for every actual user.
  (is (waiver/faab? :faab))
  (is (waiver/faab? "faab"))
  (is (not (waiver/faab? :rolling)))
  (is (not (waiver/faab? "rolling")))
  (is (not (waiver/faab? nil)))
  (let [lg (assoc league :waiver {:type "faab" :budget 100})
        {:keys [players]} (run :league lg)]
    (is (some #(pos? (:bid %)) players)
        "somebody is worth real money under the string spelling too")))

(deftest a-spent-budget-has-no-bid-left-to-make
  (let [lg (assoc-in league [:teams 0 :faab-left] 0)
        {:keys [players]} (run :league lg)]
    (is (every? #(nil? (:bid %)) players))))

(deftest zero-is-a-real-bid-not-a-refusal
  ;; FAAB accepts $0, unlike the auction board where $0 meant undraftable and
  ;; the $1 floor existed to say so.
  (let [{:keys [players]} (run)
        marginal (filter #(zero? (:bid %)) players)]
    (is (seq marginal))
    (is (every? #(number? (:bid %)) marginal) "a number, not a nil")))

(deftest no-bid-ever-exceeds-what-is-left-to-spend
  (let [{:keys [players faab]} (run :through-week 17)]
    (is (every? #(<= (:bid %) (:left faab)) players))))

;; ---- rivals ----

(deftest rival-max-is-what-someone-else-could-outbid-me-with
  (is (= 95 (:rival-max (:faab (run)))))
  (testing "my own budget is not a rival's"
    (let [lg (assoc-in league [:teams 0 :faab-left] 999)]
      (is (= 95 (:rival-max (:faab (run :league lg))))))))

;; ---- replacement, computed over the whole league ----

(deftest replacement-level-is-a-property-of-the-league-not-of-who-is-left
  ;; Scoped to the free agents it would drift down every time a good player was
  ;; added, and the remaining scraps would start reading as starters.
  (let [full   (:replacement-levels (run))
        thin   (:replacement-levels
                (run :league (assoc-in league [:teams 1 :player-ids]
                                       (mapv :player-id (take 30 board)))))]
    (is (= full thin))))

(deftest the-draft-boards-vorp-does-not-travel-under-the-same-name
  ;; Two different scales under one key is the mistake `static-rankings`
  ;; documents about expert tiers.
  (let [{:keys [players]} (run)]
    (is (every? #(contains? % :ros-vorp) players))
    (is (not-any? #(contains? % :vorp) players))))

;; ---- display ----

(deftest trend-reads-opportunity-not-points
  ;; A receiver whose targets have dried up is a sell while his season line
  ;; still looks fine.
  (let [rising {:nflverse/season-to-date {:games 8 :usage {:targets 40.0}}
                :nflverse/recent         {:games 3 :usage {:targets 30.0}}}
        flat   {:nflverse/season-to-date {:games 8 :usage {:targets 40.0}}
                :nflverse/recent         {:games 3 :usage {:targets 15.0}}}]
    (is (= 2.0 (waiver/trend rising)) "10/game against a season rate of 5")
    (is (= 1.0 (waiver/trend flat)))
    (is (nil? (waiver/trend {})) "no in-season rows, no opinion")
    (is (nil? (waiver/trend {:nflverse/season-to-date {:games 0 :usage {}}})))))
