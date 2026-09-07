(ns draft-day.rankings.waiver-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.rankings.waiver :as waiver]
            [draft-day.scoring :as scoring]))

(defn sleeper-id
  "The Sleeper spelling of a board id.

  Deliberately *different* from `:player-id` for every player in this fixture,
  because a fixture where the two are equal makes `db/sleeper->player-id` an
  identity map — and an identity crosswalk hides every bug in which a caller
  forgets to translate. One did: the availability filter translated its roster
  ids and the drop candidate did not, so no real league resolved a single
  droppable player and every upgrade on the board was measured against a floor
  of zero. No real league has ids that match."
  [id]
  (str "s-" id))

(defn- p
  "A board player, carrying the `:ids` envelope a real universe row does."
  [id pos pts]
  {:player-id id :player-name (str "P" id) :position pos :ros-points pts
   :ids {:sleeper (sleeper-id id)}})

(def ^:private board
  (into [(p "star" "RB" 180.0) (p "good" "WR" 140.0) (p "ok" "WR" 90.0)
         (p "meh" "RB" 40.0) (p "bad" "TE" 10.0)]
        ;; A spread that runs below the drop candidate as well as above him:
        ;; most of a real free-agent pool is worse than the worst man rostered.
        (map #(p (str "filler" %) "WR" (- 80.0 (* 2.0 %))) (range 40))))

(defn- held
  "A team's roster, in the provider's id space — the only space a sync speaks."
  [& ids] (mapv sleeper-id ids))

(def ^:private league
  {:teams [{:roster-id 1 :name "Mine"   :player-ids (held "star" "meh")
            :active-ids (held "star" "meh") :faab-left 60}
           {:roster-id 2 :name "Rivals" :player-ids (held "bad")
            :active-ids (held "bad") :faab-left 95}]
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
  (let [b   [{:player-id "00-gsis" :position "RB" :ros-points 100.0
              :ids {:sleeper "4034"}}
             {:player-id "SF" :position "DST" :ros-points 50.0}]
        out (waiver/waiver-board
             b {:league {:teams [{:roster-id 1 :name "Mine" :player-ids ["4034" "SF"]}]
                         :waiver {:type :rolling}}
                :my-roster-id 1 :num-teams 12 :through-week 4 :season-games 17})]
    (is (empty? (:players out)) "both are rostered, under either id spelling")
    (is (= {"00-gsis" "Mine" "SF" "Mine"} (:rostered out)))))

(deftest the-drop-candidate-is-translated-too-not-just-the-availability-filter
  ;; The bug this fixture now exists to catch. `by-id` is keyed by :player-id;
  ;; a roster read without the crosswalk resolves nothing, so no drop is ever
  ;; named, the floor falls to 0 and every upgrade on the board is overstated by
  ;; the dropped player's whole rest-of-season line.
  (let [{:keys [players]} (run)
        good (first (filter #(= "good" (:player-id %)) players))]
    (is (some? (:drop-candidate good))
        "a roster in the provider's id space still resolves a droppable player")
    (is (= "meh" (get-in good [:drop-candidate :player-id])))
    (is (= 100.0 (:upgrade good)) "140 minus the 40 dropped, not the full 140")))

(deftest with-no-league-synced-everyone-is-free
  ;; Not an error — a manager who has not connected a league yet still gets a
  ;; rest-of-season ranking, which is useful on its own.
  (let [out (waiver/waiver-board board {:league nil :num-teams 12
                                        :through-week 8 :season-games 17})]
    (is (= (count board) (count (:players out))))
    (is (every? #(nil? (:bid %)) (:players out)))))

;; ---- what a claim costs ----

(deftest players-parked-on-ir-hold-no-seat-and-free-none
  ;; Both directions bite. Counted toward the roster they fill a team that is
  ;; not actually full; offered as a drop they free no seat for the claim being
  ;; priced — so a claim would be recommended against dropping a man whose
  ;; removal changes nothing.
  (let [lg (-> league
               (assoc-in [:teams 0 :player-ids] (held "star" "meh" "bad"))
               (assoc-in [:teams 0 :active-ids] (held "star" "meh")))
        {:keys [players]} (run :league lg :roster-size 3)
        good (first (filter #(= "good" (:player-id %)) players))]
    (is (nil? (:drop-candidate good))
        "two active men against three seats: a seat is open, IR notwithstanding")
    (is (= 140.0 (:upgrade good)))
    (is (not-any? #(= "bad" (:player-id %)) players)
        "but he is still rostered, not a free agent")))

(deftest the-leagues-own-seat-count-beats-the-requests
  ;; The browser derives its fallback from the *draft* config, which a manager
  ;; who synced without importing has never set to match this league.
  (let [lg (assoc league :roster-size 6)
        good (fn [out] (first (filter #(= "good" (:player-id %)) (:players out))))]
    (is (nil? (:drop-candidate (good (run :league lg :roster-size 2))))
        "six seats and two men held: the league says a seat is open")
    (is (some? (:drop-candidate (good (run :roster-size 2))))
        "with no league-side count the request's is still honoured")))

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
  (let [lg  (-> league
                (assoc-in [:teams 0 :player-ids] (held "star" "meh" "ghost"))
                (assoc-in [:teams 0 :active-ids] (held "star" "meh" "ghost")))
        {:keys [players]} (run :league lg :roster-size 3)
        good (first (filter #(= "good" (:player-id %)) players))]
    (is (= "meh" (get-in good [:drop-candidate :player-id]))
        "the drop stays a player the manager can check")))

;; ---- my own roster ----

(deftest my-roster-is-my-own-seats-valued
  ;; `:players` is free agents only and `:rostered` carries names, not numbers,
  ;; so nothing else in the reply can answer "what do I already have".
  (let [{:keys [my-roster]} (run)]
    (is (= ["star" "meh"] (mapv :player-id my-roster))
        "in the board's id space, not the provider's")
    (is (= ["Pstar" "Pmeh"] (mapv :player-name my-roster)))
    (is (= [180.0 40.0] (mapv :ros-points my-roster))
        "the line the browser cannot compute for itself")))

(deftest my-roster-marks-starters-and-parked-players
  (let [lg (-> league
               (assoc-in [:teams 0 :player-ids] (held "star" "meh" "bad"))
               (assoc-in [:teams 0 :active-ids] (held "star" "meh"))
               (assoc-in [:teams 0 :starter-ids] (held "star")))
        {:keys [my-roster]} (run :league lg :roster-size 3)
        by (into {} (map (juxt :player-id identity)) my-roster)]
    (is (:starter? (by "star")))
    (is (not (:starter? (by "meh"))) "held, but not in the lineup")
    (is (:parked? (by "bad")) "on IR: rostered, holding no seat a claim could take")
    (is (not (:parked? (by "star"))))))

(def ^:private ordering-board
  "Wide enough for a wrong order to show: two QBs, a K, a DST, and WRs on both
  sides of the lineup."
  (into board [(p "qb1" "QB" 300.0) (p "qb2" "QB" 250.0)
               (p "wr-flex" "WR" 130.0) (p "te1" "TE" 110.0)
               (p "k1" "K" 120.0) (p "dst1" "DST" 100.0)
               (p "rb-bench" "RB" 60.0)
               (p "wr-b1" "WR" 80.0) (p "wr-b2" "WR" 95.0)]))

(defn- ordered-roster
  "`:player-ids` shuffled against both orderings: a fixture already in the right
  order proves nothing."
  [& {:keys [starter-ids held-extra]}]
  (let [ids (into (held "k1" "wr-flex" "rb-bench" "qb1" "te1" "star"
                        "dst1" "qb2" "ok" "wr-b1" "wr-b2")
                  (or held-extra []))
        lg  {:teams [{:roster-id 1 :name "Mine" :player-ids ids :active-ids ids
                      :starter-ids (or starter-ids []) :faab-left 60}]
             :waiver {:type :faab :budget 100}}]
    (:my-roster
     (waiver/waiver-board ordering-board
                          {:league lg :my-roster-id 1 :roster-size 20
                           :num-teams 12 :through-week 8 :season-games 17}))))

(deftest my-roster-starters-come-back-in-the-leagues-lineup-order
  ;; Sleeper's `starters` array *is* the lineup, slot by slot, and the "0" it
  ;; writes into an unfilled slot must consume an index without moving anyone.
  (let [lineup (-> (held "qb1" "star" "ok" "te1")
                   (conj "0")
                   (into (held "wr-flex" "k1" "dst1")))
        roster (ordered-roster :starter-ids lineup)
        names  (->> roster (filter :starter?) (mapv :player-id))]
    (is (= ["qb1" "star" "ok" "te1" "wr-flex" "k1" "dst1"] names)
        "the FLEX receiver keeps his seat between the TE and the K")
    (is (every? :starter? (take 7 roster)) "starters lead the vector")))

(deftest my-roster-bench-is-ordered-by-position-then-by-points
  (let [roster (ordered-roster :starter-ids (held "qb1" "star" "ok" "te1"
                                                  "wr-flex" "k1" "dst1")
                               :held-extra [(sleeper-id "ghost")])
        bench  (->> roster (remove :starter?) (mapv :player-id))]
    (is (= ["qb2" "rb-bench" "wr-b2" "wr-b1" (sleeper-id "ghost")] bench)
        "QB before RB before WR, better points first inside a position")
    (is (:unvalued? (last roster))
        "a row the board cannot value keeps its seat, at the bottom of its block")))

(deftest a-league-with-no-lineup-set-falls-back-to-position-order
  ;; `starters` is null for a league nobody has set a lineup in. Nothing is a
  ;; starter, and the whole roster is one positionally ordered block.
  (let [roster (ordered-roster)]
    (is (not-any? :starter? roster))
    (is (= ["qb1" "qb2" "star" "rb-bench" "wr-flex" "wr-b2" "ok" "wr-b1"
            "te1" "k1" "dst1"]
           (mapv :player-id roster)))))

(deftest my-roster-marks-the-seat-a-claim-would-cost
  (let [{:keys [my-roster players]} (run)
        dropped (first (filter :drop? my-roster))
        good    (first (filter #(= "good" (:player-id %)) players))]
    (is (= "meh" (:player-id dropped)))
    (is (= (get-in good [:drop-candidate :player-id]) (:player-id dropped))
        "the panel marks the same man the drop note names")
    (is (= 1 (count (filter :drop? my-roster))) "exactly one seat is at stake"))
  (let [{:keys [my-roster]} (run :roster-size 6)]
    (is (not-any? :drop? my-roster) "with a seat open, nothing is at stake")))

(deftest a-roster-player-the-board-cannot-value-still-holds-a-seat
  ;; `drop-candidate` skips him on purpose — "no projection" is not "projected to
  ;; score nothing". But a *roster* that skips him shows fewer seats than the
  ;; manager has, with nothing saying why, which is how a missing crosswalk hides.
  (let [lg (-> league
               (assoc-in [:teams 0 :player-ids] (held "star" "meh" "ghost"))
               (assoc-in [:teams 0 :active-ids] (held "star" "meh" "ghost")))
        {:keys [my-roster]} (run :league lg :roster-size 3)
        ghost (first (filter :unvalued? my-roster))]
    (is (= 3 (count my-roster)) "every seat is accounted for")
    (is (= (sleeper-id "ghost") (:player-id ghost))
        "carrying the id, so the row is at least checkable")
    (is (nil? (:ros-points ghost)) "and not faked as zero")))

(deftest no-team-picked-is-not-the-same-as-an-empty-roster
  ;; The panel says "pick your team to see your roster" for one, and draws an
  ;; empty table for the other.
  (is (nil? (:my-roster (run :my-roster-id nil))))
  (is (nil? (:my-roster (waiver/waiver-board board {:league nil :num-teams 12
                                                    :through-week 8 :season-games 17})))
      "no league synced at all")
  (let [lg (-> league
               (assoc-in [:teams 0 :player-ids] [])
               (assoc-in [:teams 0 :active-ids] []))]
    (is (= [] (:my-roster (run :league lg)))
        "a picked team holding nobody is empty, not absent")))

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
                                       (mapv (comp sleeper-id :player-id)
                                             (take 30 board)))))]
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

(deftest week-points-score-under-the-league-weights
  ;; The weekly number is the league's, not the vendor's — same rule as the
  ;; season line, which is why the two are comparable at all.
  (let [board [{:player-id "a" :week/stats {:rush_yd 81.0 :rec 3.8 :rec_yd 32.0}}
               {:player-id "b"}]
        [a b] (waiver/with-week-points board (scoring/resolve-config :ppr))]
    (is (< (abs (- 15.1 (:week-points a))) 1e-9))          ; 8.1 + 3.8 + 3.2
    ;; Not projected this week: no key, not a zero.
    (is (not (contains? b :week-points)))))

(deftest form-points-are-per-game-under-the-league-weights
  ;; The window is a *total* over however many games the player has inside it,
  ;; so the division is what makes a three-week and a two-week window
  ;; comparable at all.
  (let [ppr (scoring/resolve-config :ppr)
        [a] (waiver/with-form-points
              [{:player-id "a"
                :nflverse/recent {:games 3 :stats {:rush_yd 240.0 :rec 15.0
                                                   :rec_yd 180.0 :rush_td 3.0}}}]
              ppr)]
    ;; 24 + 15 + 18 + 18 = 75 over 3 games
    (is (< (abs (- 25.0 (:form-points a))) 1e-9))))

(deftest form-points-are-absent-rather-than-zero
  ;; The preseason board, a rookie who has not debuted, and every DST — nflverse
  ;; publishes no DST row. A 0.0 would rank them below a player who has actually
  ;; been bad, which is a claim the data does not make.
  (let [ppr (scoring/resolve-config :ppr)
        got (fn [p] (:form-points (first (waiver/with-form-points [p] ppr))))]
    (is (nil? (got {:player-id "preseason"})))
    (is (nil? (got {:player-id "no-games" :nflverse/recent {:games 0 :stats {}}})))
    (is (nil? (got {:player-id "no-stats" :nflverse/recent {:games 2 :stats {}}})))))

;; ---- my roster as comparable rows ----

(deftest my-roster-players-are-full-rows-not-the-panel-shape
  ;; The comparison tile reads the same keys on both sides, so one shape means
  ;; one renderer. `:my-roster` is trimmed to what the panel draws and cannot
  ;; serve that.
  (let [{:keys [my-roster my-roster-players]} (run)
        by-id (into {} (map (juxt :player-id identity)) my-roster-players)]
    (is (= #{"star" "meh"} (set (keys by-id)))
        "the manager's own seats, in the board's id space")
    (is (= 180.0 (:ros-points (by-id "star"))))
    ;; Full rows carry what the panel shape drops — the id envelope among it.
    (is (contains? (by-id "star") :ids))
    (is (contains? (by-id "star") :ros-vorp))
    (is (not (contains? (first my-roster) :ids))
        "the panel shape stays trimmed")))

(deftest my-roster-players-cannot-be-claimed
  ;; You cannot claim a man you already hold, and a 0 would read as a claim
  ;; worth nothing rather than as a question that does not apply.
  (let [{:keys [my-roster-players]} (run)]
    (is (every? #(not (contains? % :upgrade)) my-roster-players))
    (is (every? #(not (contains? % :bid)) my-roster-players))
    ;; :trend does apply — it is a fact about the player, not about a claim.
    (is (every? #(contains? % :trend) my-roster-players))))

(deftest my-roster-players-are-absent-without-a-team
  ;; nil, not [] — the same distinction `:my-roster` keeps, so a caller cannot
  ;; read "no team picked" as "this roster is empty".
  (is (nil? (:my-roster-players (run :my-roster-id nil))))
  (is (nil? (:my-roster-players (waiver/waiver-board board {:league nil :num-teams 12
                                                            :through-week 8
                                                            :season-games 17})))))

(deftest no-team-picked-means-no-lineup-delta
  ;; The default state until the manager sets :my-roster-id. Without the guard
  ;; the empty lineup makes every free agent's delta his whole line — both
  ;; meaningless and numerically identical to :upgrade beside it, so nothing on
  ;; screen says the column is not answering.
  (let [slots (draft-day.db/starting-slots draft-day.db/default-roster)
        out   (waiver/waiver-board
               [(p "a" "WR" 200.0)]
               {:league {:teams [{:roster-id 1 :name "X"
                                  :player-ids [] :active-ids []}]}
                :my-roster-id nil :num-teams 12 :through-week 8
                :season-games 17 :starting-slots slots})
        row   (first (:players out))]
    (is (nil? (:my-roster out)) "precondition: no team picked")
    (is (not (contains? row :lineup-upgrade))
        "absent, not zero and not his whole line")))

;; ---- a drop that costs the lineup least ----

(defn- drop-for
  "The named drop for a roster of [id pos ros] triples, with and without slots."
  [rows slots]
  (let [ps    (mapv (fn [[id pos pts]] (p id pos pts)) rows)
        by-id (into {} (map (juxt :player-id identity)) ps)
        held  (mapv first rows)]
    (:player-id (waiver/drop-candidate held by-id (count rows) slots))))

(def ^:private lineup-slots
  (draft-day.db/starting-slots draft-day.db/default-roster))

(deftest a-deep-bench-still-names-the-player-the-old-rule-did
  ;; The property that keeps this from being a behaviour change on most rosters:
  ;; every bench player costs the lineup nothing, so the tiebreak decides and
  ;; the tiebreak is the old rule.
  (let [rows [["qb1" "QB" 260.0] ["rb1" "RB" 180.0] ["rb2" "RB" 150.0]
              ["wr1" "WR" 170.0] ["wr2" "WR" 120.0] ["te1" "TE" 95.0]
              ["k1" "K" 100.0] ["d1" "DST" 80.0]
              ["bench1" "WR" 60.0] ["bench2" "RB" 40.0]]]
    (is (= "bench2" (drop-for rows lineup-slots)))
    (is (= "bench2" (drop-for rows nil)) "and the points rule agrees here")))

(deftest the-only-kicker-is-not-the-drop-just-because-he-scores-least
  ;; The measured case. On a real 12-team league the lowest-scoring active
  ;; player was the manager's only kicker — a starter — so every claim was
  ;; priced as costing his whole line and 442 of 457 free agents came out
  ;; negative. The points rule still picks him; the lineup rule must not.
  ;; QB/RB/RB/WR/WR/TE/FLEX/K/DST seats a lineup of
  ;; qb1 rb1 rb2 wr1 wr3 te1 rb3(FLEX) k1 d1 — so wr2 at 120 is the only man
  ;; NOT starting, however healthy his number looks. Names say so: two players
  ;; called "bench" that both start is how the first draft of this test lied.
  (let [rows [["qb1" "QB" 260.0] ["rb1" "RB" 180.0] ["rb2" "RB" 150.0]
              ["wr1" "WR" 170.0] ["wr3" "WR" 140.0] ["rb3" "RB" 130.0]
              ["wr2" "WR" 120.0] ["te1" "TE" 95.0]
              ["k1" "K" 39.0] ["d1" "DST" 80.0]]]
    (is (= "k1" (drop-for rows nil))
        "precondition: the kicker is the lowest scorer, so the old rule takes him")
    (is (= "wr2" (drop-for rows lineup-slots))
        "the lineup rule takes the one man who is not starting")
    (is (not= "k1" (drop-for rows lineup-slots))
        "and never the only kicker, whose seat nobody else can fill")))

(deftest with-no-slots-the-points-rule-is-kept-exactly
  ;; A request that carried no roster config has no lineup to cost anything
  ;; against, so it must not silently get a different answer.
  (let [rows [["a" "WR" 10.0] ["b" "RB" 5.0] ["c" "TE" 7.0]]]
    (is (= "b" (drop-for rows nil)))))
