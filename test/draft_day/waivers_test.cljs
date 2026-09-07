(ns draft-day.waivers-test
  "The in-season half's browser-side behaviour: the stale-reply guard, the
  persisted-shape repair on the way in, and the sub that orders the board.

  None of this is reachable from `lein test` — run with
  `npx shadow-cljs compile test && node out/node-tests.js`."
  (:require [cljs.test :refer [deftest is testing use-fixtures]]
            [re-frame.core :as rf]
            [re-frame.db :as rdb]
            [re-frame.registrar :as registrar]
            [clojure.walk]
            [reagent.ratom]
            [draft-day.db :as db]
            [draft-day.fx]
            [draft-day.subs :as subs]
            [draft-day.views.board :as board]
            [draft-day.views.waivers :as waivers]
            [draft-day.events :as events]))

(defonce captured (atom {}))

(def ^:private stubs
  "Stand in for the real side effects. `:dispatch` is stubbed along with the
  rest, and that is the load-bearing one: re-frame's real `:dispatch` queues the
  event for a later tick, so an assertion about a follow-on dispatch made right
  after `dispatch-sync` would be racing the queue. Captured, it is synchronous
  and says exactly what the handler asked for — which is the thing under test."
  {:http     (fn [r] (swap! captured update :http conj r))
   :persist! (fn [r] (swap! captured update :persist conj r))
   :debounce (fn [r] (swap! captured update :debounce conj r))
   :dispatch (fn [r] (swap! captured update :dispatch conj r))})

(defonce ^:private real-fx
  ;; Captured at load, before any stub is registered. Every `-test` namespace
  ;; compiles into one node bundle, so a stub left registered here would
  ;; silently disarm these effects for whatever namespace runs next.
  (into {} (map (juxt identity #(registrar/get-handler :fx %))) (keys stubs)))

(defn- swap-fx! [m]
  (doseq [[id f] m]
    (rf/clear-fx id)
    (when f (rf/reg-fx id f))))

(use-fixtures :each
  {:before (fn []
             (swap-fx! stubs)
             (reset! captured {:http [] :persist [] :debounce [] :dispatch []})
             (rf/clear-subscription-cache!)
             (reset! rdb/app-db (db/default-db)))
   :after  (fn []
             (swap-fx! real-fx)
             (rf/clear-subscription-cache!)
             (reset! rdb/app-db (db/default-db)))})

(defn- last-http [] (last (:http @captured)))
(defn- dispatched [] (mapv first (:dispatch @captured)))
(defn- sub [q] (binding [reagent.ratom/*ratom-context* #js {}] @(rf/subscribe q)))

(def ^:private synced
  {:teams [{:roster-id 1 :name "Mine" :player-ids ["a"] :active-ids ["a"] :faab-left 60}
           {:roster-id 2 :name "Them" :player-ids ["b"] :active-ids ["b"] :faab-left 95}]
   :waiver {:type "faab" :budget 100}
   :roster-size 15 :league-id "987654"})

(def ^:private lk (db/league-key "sleeper" "987654"))

(defn- with-league!
  "Put a league in db and make it active, the way `:league-choose` would.

  Every one of these tests used to set `:league-sync` and `:my-roster-id` at the
  top of db. Both now live on the active league's entry, which is the whole
  point of the reshape: a manager with two leagues has two of each."
  ([] (with-league! nil nil))
  ([sync mine]
   (swap! rdb/app-db assoc
          :leagues {lk (cond-> {:provider "sleeper" :league-id "987654"}
                         sync (assoc :sync sync)
                         mine (assoc :my-roster-id mine))}
          :active-league lk)))

(defn- league-entry [] (get-in @rdb/app-db [:leagues lk]))

;; ---- the stale-reply guard ----

(deftest a-reply-computed-against-the-previous-roster-cannot-win
  ;; The identical hazard :recompute has, with a worse symptom: a full re-rank
  ;; takes long enough that overlapping requests answer out of order, and a
  ;; board computed before a claim landed would tell the manager a player he
  ;; just added is still available.
  (rf/dispatch-sync [:fetch-waivers])
  (let [first-seq (:waiver-seq @rdb/app-db)]
    (rf/dispatch-sync [:fetch-waivers])
    (let [second-seq (:waiver-seq @rdb/app-db)]
      (is (< first-seq second-seq) "each request takes a fresh stamp")
      (rf/dispatch-sync [:waivers-loaded second-seq {:players [{:player-id "new"}]}])
      (rf/dispatch-sync [:waivers-loaded first-seq {:players [{:player-id "stale"}]}])
      (is (= "new" (-> @rdb/app-db :waivers :players first :player-id))
          "the older reply is dropped, not merged"))))

(deftest a-failed-refresh-leaves-the-last-board-readable
  ;; Stale but readable beats blank; the status line is what says it is stale.
  (rf/dispatch-sync [:waivers-loaded 0 {:players [{:player-id "p"}]}])
  (rf/dispatch-sync [:waivers-failed "boom"])
  (is (= "p" (-> @rdb/app-db :waivers :players first :player-id)))
  (is (re-find #"boom" (:waiver-status @rdb/app-db))))

;; ---- the request ----

(deftest the-request-carries-the-league-the-browser-owns
  ;; Same statelessness the draft board runs on: the server holds nothing
  ;; between requests, so everything it needs rides on the call.
  (with-league! synced 1)
  (rf/dispatch-sync [:fetch-waivers])
  (let [b (:body (last-http))]
    (is (= "/api/waivers" (:url (last-http))))
    (is (= synced (:league b)))
    (is (= 1 (:my-roster-id b)))
    ;; A *fallback* only: the server prefers the synced league's own seat count,
    ;; because this one is derived from the draft config, which a manager who
    ;; synced without importing has never set to match his real league.
    (is (pos? (:roster-size b)) "so the board knows whether a claim costs a drop")
    (is (= (count (db/roster-template (get-in @rdb/app-db [:config :roster])))
           (:roster-size b)))))

;; ---- sync ----

(deftest a-sync-is-repaired-on-the-way-in-not-only-at-boot
  ;; It is the same shape localStorage will hand back next session, so a
  ;; provider that grew or dropped a field should fail here — where the status
  ;; line can say so — rather than a session later with nothing to explain it.
  (with-league!)
  (rf/dispatch-sync [:league-synced lk synced])
  (is (= 2 (count (get-in (league-entry) [:sync :teams]))))
  (is (re-find #"Synced" (:waiver-status @rdb/app-db)))
  (is (some #{:fetch-waivers} (dispatched))
      "and the board is refreshed against the rosters that just arrived")
  (testing "a reply that is not a league is refused rather than half-stored"
    (rf/dispatch-sync [:league-synced lk {:not "a league"}])
    (is (nil? (:sync (league-entry))))
    (is (re-find #"nothing usable" (:waiver-status @rdb/app-db)))))

(deftest a-sync-is-written-to-the-league-it-was-asked-for
  ;; The key rides on the request rather than being read off `:active-league` at
  ;; reply time. A manager who switches leagues while a sync is in flight would
  ;; otherwise have one league's rosters written into the other's entry — and
  ;; the waiver board would name players nobody in that league holds.
  (swap! rdb/app-db assoc
         :leagues {lk       {:provider "sleeper" :league-id "987654"}
                   "sleeper:other" {:provider "sleeper" :league-id "other"}}
         :active-league "sleeper:other")
  (rf/dispatch-sync [:league-synced lk synced])
  (is (= 2 (count (get-in (league-entry) [:sync :teams]))))
  (is (nil? (get-in @rdb/app-db [:leagues "sleeper:other" :sync]))
      "the league on screen is untouched by a reply that is not about it"))

(deftest picking-my-team-re-prices-the-board
  ;; Almost everything on the board is measured *from* this: the sync fires
  ;; :fetch-waivers while it is still nil, so the first board comes back with no
  ;; drop, no budget and every bid blank. Without a refetch, picking your team
  ;; changed a dropdown and nothing else until you happened to press Refresh.
  (with-league! synced nil)
  (rf/dispatch-sync [:set-my-roster-id 1])
  (is (= 1 (:my-roster-id (league-entry))))
  (is (some #{:fetch-waivers} (dispatched))))

(deftest the-league-id-comes-back-with-the-rosters
  ;; So a re-sync is one click. The input lives in a component-local atom that
  ;; empties on reload; without this the manager returns to persisted, month-old
  ;; rosters with no record of which league they came from.
  (with-league!)
  (rf/dispatch-sync [:league-synced lk synced])
  (rf/clear-subscription-cache!)
  (is (= "987654" (sub [:synced-league-id])))
  (is (contains? (last (:persist @captured)) :leagues)
      "and it is persisted along with them"))

(deftest a-sync-survives-a-reload
  (with-league!)
  (rf/dispatch-sync [:league-synced lk synced])
  (let [slice (last (:persist @captured))]
    (is (= 2 (count (get-in slice [:leagues lk :sync :teams])))
        "a sync redone on every page load is one nobody uses")
    (is (contains? slice :active-league) "including which of them is on screen")
    (is (not (contains? slice :waivers)) "the board itself is not persisted, like :ranked")))

;; ---- arriving at the tab ----

(deftest opening-the-tab-loads-its-board-once
  ;; A second full rank of the universe, so it is paid for by the manager who
  ;; asks for it — but only the first time, or every glance re-ranks the league.
  (rf/dispatch-sync [:set-view :waivers])
  (is (= [:fetch-waivers] (dispatched)))
  (swap! rdb/app-db assoc :waivers {:players []})
  (reset! captured {:http [] :persist [] :debounce [] :dispatch []})
  (rf/dispatch-sync [:set-view :board])
  (rf/dispatch-sync [:set-view :waivers])
  (is (empty? (dispatched)) "a refresh is a button, not a side effect of navigation")
  (is (= :waivers (:view @rdb/app-db)) "the view still changes either way"))

;; ---- the board sub ----

(defn- board-of [players & [over]]
  (rf/clear-subscription-cache!)
  (swap! rdb/app-db merge {:waivers {:players players}} over)
  (sub [:waiver-players]))

(deftest the-board-ranks-by-what-the-claim-gains
  (let [ps [{:player-id "a" :player-name "A" :position "WR" :upgrade 5.0 :ros-points 10.0}
            {:player-id "b" :player-name "B" :position "RB" :upgrade 50.0 :ros-points 90.0}
            {:player-id "c" :player-name "C" :position "WR" :upgrade -3.0 :ros-points 4.0}]
        out (board-of ps)]
    (is (= ["b" "a" "c"] (mapv :player-id out)))
    (is (= [1 2 3] (mapv :rank out)))))

(deftest the-rank-column-is-not-the-row-number
  ;; Same promise `:board-players` makes: `#` says where a player ranks, not
  ;; which row he happens to be on under the active sort.
  (let [ps [{:player-id "a" :player-name "A" :position "WR" :upgrade 5.0 :ros-points 10.0}
            {:player-id "b" :player-name "B" :position "RB" :upgrade 50.0 :ros-points 90.0}]
        out (board-of ps {:waiver-sort {:key :name :dir 1}})]
    (is (= ["a" "b"] (mapv :player-id out)) "sorted by name")
    (is (= [2 1] (mapv :rank out)) "but still ranked by upgrade")))

(deftest the-position-filter-is-shared-with-the-draft-board
  ;; A manager who filters to RB and switches tabs is still asking about running
  ;; backs; two filters that look identical but do not follow each other is the
  ;; worse surprise.
  (let [ps [{:player-id "a" :player-name "A" :position "WR" :upgrade 5.0}
            {:player-id "b" :player-name "B" :position "RB" :upgrade 50.0}]]
    (is (= ["b"] (mapv :player-id (board-of ps {:pos-filter "RB"}))))))

(deftest the-season-phase-has-three-answers-not-two
  ;; A boolean reported *preseason* whenever the board had simply not loaded —
  ;; so in week 10 an accented banner reading "no games played yet" sat over a
  ;; loading screen, and stayed there permanently if the request failed. That is
  ;; the exact misreading the banner exists to prevent.
  (swap! rdb/app-db assoc :waivers nil)
  (rf/clear-subscription-cache!)
  (is (= :unknown (sub [:season-phase])) "no board yet is not a claim about August")
  (swap! rdb/app-db assoc :waivers {:players [] :through-week 0})
  (rf/clear-subscription-cache!)
  (is (= :preseason (sub [:season-phase])))
  (swap! rdb/app-db assoc :waivers {:players [] :through-week 6})
  (rf/clear-subscription-cache!)
  (is (= :in-season (sub [:season-phase])))
  (testing "a failed refresh leaves it unknown rather than asserting preseason"
    (swap! rdb/app-db assoc :waivers nil)
    (rf/dispatch-sync [:waivers-failed "boom"])
    (rf/clear-subscription-cache!)
    (is (= :unknown (sub [:season-phase])))))

(deftest a-rostered-player-matching-the-search-says-who-has-him
  ;; Search for a rostered player and the free-agent table is simply empty,
  ;; which teaches the manager nothing. Names come from the universe the browser
  ;; already has, so answering costs no payload.
  (swap! rdb/app-db assoc
         :players [{:player-id "a" :player-name "Ja'Marr Chase" :position "WR"}]
         :search "chase"
         :waivers {:players [] :rostered {"a" "Mine"}})
  (rf/clear-subscription-cache!)
  (is (= [{:player-name "Ja'Marr Chase" :position "WR" :team "Mine"}]
         (sub [:rostered-matches])))
  (testing "and says nothing at all with no search"
    (swap! rdb/app-db assoc :search "")
    (rf/clear-subscription-cache!)
    (is (nil? (sub [:rostered-matches])))))

;; ---- cells ----

(deftest the-trend-column-does-not-colour-noise
  ;; A receiver at 1.04 has not earned an arrow, and a column that colours noise
  ;; stops being read at all.
  (is (nil? (waivers/trend-class 1.04)))
  (is (nil? (waivers/trend-class 0.9)))
  (is (= "trend-up" (waivers/trend-class 1.6)))
  (is (= "trend-down" (waivers/trend-class 0.4)))
  (is (nil? (waivers/trend-class nil))))

(deftest the-upgrade-cell-colours-and-prints-the-same-number
  ;; Colouring the raw value and printing the rounded one put a green dash on
  ;; the board for an upgrade of 0.4: `sign-class` saw a positive number while
  ;; `signed` dashed out the zero.
  (let [cell (fn [up] (waivers/cell :upgrade {:upgrade up} nil))
        cls  (fn [up] (:class (second (cell up))))
        txt  (fn [up] (last (cell up)))]
    (is (= "good" (cls 12.0)))
    (is (= "+12" (txt 12.0)))
    (is (= "warn" (cls -12.0)))
    (is (nil? (cls 0.4)) "rounds to zero, so it is not coloured either")
    (is (= "–" (txt 0.4)))
    (is (nil? (cls -0.4)))
    (is (= "–" (txt -0.4)))))

(deftest sorting-puts-players-with-nothing-to-say-last-in-both-directions
  ;; Same rule `sort-players` keeps: a nil is not a low value, it is an absent
  ;; one, and it must not float to the top when the column is reversed.
  (let [ps [{:player-id "a" :player-name "A" :bid 5 :upgrade 1.0}
            {:player-id "b" :player-name "B" :bid nil :upgrade 2.0}
            {:player-id "c" :player-name "C" :bid 9 :upgrade 3.0}]]
    (is (= ["c" "a" "b"] (mapv :player-id (subs/sort-waiver-players ps :bid -1))))
    (is (= ["a" "c" "b"] (mapv :player-id (subs/sort-waiver-players ps :bid 1))))))

;; ---- my own roster ----

(deftest no-team-picked-reads-differently-from-an-empty-roster
  ;; The whole reason the panel exists: the free-agent board never shows what the
  ;; manager already has, so a dropdown whose only job is to identify that roster
  ;; looked inert. nil has to reach the view as nil — normalizing it to [] would
  ;; make "pick your team" indistinguishable from "you hold nobody".
  (swap! rdb/app-db assoc :waivers {:players [] :my-roster nil})
  (rf/clear-subscription-cache!)
  (is (nil? (sub [:my-waiver-roster])))
  (swap! rdb/app-db assoc :waivers {:players [] :my-roster []})
  (rf/clear-subscription-cache!)
  (is (= [] (sub [:my-waiver-roster])) "picked, but holding nobody"))

(defn- render
  "A component's hiccup, rendered inside a reactive context.

  Same binding `sub` needs and for the same reason: the panel subscribes, and
  re-frame warns on every subscribe made outside one."
  [component]
  (binding [reagent.ratom/*ratom-context* #js {}] (pr-str (component))))

(defn- press!
  "Find the button labelled `label` in a component's hiccup and call its
  `:on-click`.

  Returns the events it dispatched.

  `render` stringifies, which is enough to assert what a panel *says* but not
  what a button *does* — and the bugs in the sync strip were all in the payload a
  click sends, which no amount of rendering reaches. `rf/dispatch` is redefined
  rather than read off `captured`: a view calls the dispatch *function*, which
  queues on the real router, and never touches the `:dispatch` effect the fixture
  stubs."
  [component label]
  (let [found (atom nil)
        seen  (atom [])]
    (clojure.walk/postwalk
     (fn [x]
       (when (and (vector? x) (= :button (first x)) (map? (second x))
                  (some #{label} (filter string? x)))
         (reset! found (:on-click (second x))))
       x)
     (binding [reagent.ratom/*ratom-context* #js {}] (component)))
    (if-let [f @found]
      (with-redefs [rf/dispatch (fn [ev] (swap! seen conj ev))]
        (f)
        @seen)
      (throw (ex-info (str "no button labelled " label) {})))))

(deftest the-roster-panel-says-which-state-it-is-in
  (let [text (fn [] (render waivers/my-roster-panel))]
    (swap! rdb/app-db assoc :leagues {} :active-league nil :waivers {:my-roster nil})
    (rf/clear-subscription-cache!)
    (is (re-find #"Sync a league" (text)) "no league connected at all")

    (with-league! synced nil)
    (swap! rdb/app-db assoc :waivers {:my-roster nil})
    (rf/clear-subscription-cache!)
    (is (re-find #"Pick your team" (text))
        "synced but no team chosen — the line that was missing")

    (with-league! synced nil)
    (swap! rdb/app-db assoc :waivers {:my-roster []})
    (rf/clear-subscription-cache!)
    (is (re-find #"holds nobody" (text)))))

(deftest the-strip-re-syncs-under-the-league-s-provider-not-the-account-s
  ;; Settings supports adding a league by pasting an id with no account
  ;; connected. Reading the provider off `:account` fell back to whichever
  ;; account happened to exist — nil in that case, which throws in
  ;; `db/league-key`, and the wrong one once ESPN arrives.
  (with-league! synced 1)
  (swap! rdb/app-db assoc :accounts {})
  (rf/clear-subscription-cache!)
  (is (= [[:sync-league {:provider "sleeper" :league-id "987654"}]]
         (press! waivers/sync-panel "Re-sync rosters"))))

(deftest a-league-whose-sync-failed-still-offers-to-retry-it
  ;; It has no `:name` until a reply lands, and gating the strip on the name made
  ;; it read as no league at all — hiding the retry on the one screen that
  ;; reports the failure.
  (swap! rdb/app-db assoc
         :leagues {lk {:provider "sleeper" :league-id "987654"}}
         :active-league lk
         :waivers {:my-roster nil})
  (rf/clear-subscription-cache!)
  (let [out (render waivers/sync-panel)]
    (is (re-find #"Re-sync rosters" out))
    (is (re-find #"987654" out) "and names itself by its id until the sync answers")
    (is (not (re-find #"No league active" out))))
  (is (= [[:sync-league {:provider "sleeper" :league-id "987654"}]]
         (press! waivers/sync-panel "Re-sync rosters"))))

(deftest the-roster-panel-splits-starters-from-bench-and-marks-the-seat-at-stake
  ;; The synced league knows the real lineup; the draft config's slot template
  ;; does not. And the marked seat is the same man the drop note names.
  (with-league! synced 1)
  (swap! rdb/app-db assoc
         :waivers {:my-roster [{:player-id "a" :player-name "Starter A" :position "RB"
                                :ros-points 180.0 :starter? true}
                               {:player-id "b" :player-name "Bench B" :position "WR"
                                :ros-points 40.0 :starter? false :drop? true}
                               {:player-id "s-ghost" :unvalued? true :parked? true}]})
  (rf/clear-subscription-cache!)
  (let [out (render waivers/my-roster-panel)]
    (is (re-find #"Starters" out))
    (is (re-find #"Bench" out))
    (is (re-find #"drop-seat" out) "the seat a claim would cost is marked")
    (is (re-find #"s-ghost" out)
        "a row the board could not value keeps its seat and shows its id")))

;; ---- connecting an account ----

(def ^:private owned
  "A synced league where the rosters say who owns them — which the sync has
  always carried and nothing used until now."
  {:teams [{:roster-id 1 :owner-id "u-other" :name "Theirs"
            :player-ids ["b"] :active-ids ["b"]}
           {:roster-id 7 :owner-id "u-me" :name "Mine"
            :player-ids ["a"] :active-ids ["a"]}]
   :waiver {:type "faab" :budget 100}
   :roster-size 15 :league-id "987654"})

(deftest connecting-an-account-answers-which-team-is-mine
  ;; The point of the unit. Until this, the manager had to pick his own roster
  ;; out of a list of twelve before the board could name a drop, price a bid or
  ;; draw his roster — and all three read as blank until he did.
  (with-league!)
  (swap! rdb/app-db assoc-in [:accounts "sleeper"] {:provider "sleeper" :user-id "u-me"})
  (rf/dispatch-sync [:league-synced lk owned])
  (is (= 7 (:my-roster-id (league-entry)))))

(deftest a-corrected-team-is-not-undone-by-the-next-sync
  ;; Co-managed teams and second accounts are real; a manager who overrode the
  ;; guess must keep his override.
  (with-league! nil 1)
  (swap! rdb/app-db assoc-in [:accounts "sleeper"] {:provider "sleeper" :user-id "u-me"})
  (rf/dispatch-sync [:league-synced lk owned])
  (is (= 1 (:my-roster-id (league-entry)))))

(deftest an-owner-nobody-matches-leaves-the-dropdown-to-answer
  (with-league!)
  (swap! rdb/app-db assoc-in [:accounts "sleeper"] {:provider "sleeper" :user-id "u-nobody"})
  (rf/dispatch-sync [:league-synced lk owned])
  (is (nil? (:my-roster-id (league-entry))) "a guess here would be worse than the prompt"))

(deftest matching-an-owner-is-string-identity-not-number-identity
  ;; Roster ids and owner ids cross the wire as strings; a fixture or an older
  ;; persisted shape may hold either.
  (is (= 7 (events/my-roster-id-for (:teams owned) "u-me")))
  (is (nil? (events/my-roster-id-for (:teams owned) nil)))
  (is (nil? (events/my-roster-id-for (:teams owned) "")))
  (is (nil? (events/my-roster-id-for [] "u-me"))))

(deftest one-league-is-not-a-choice
  (rf/dispatch-sync [:league-user-loaded
                     {:user {:user-id "u1" :display-name "jay"}
                      :leagues [{:league-id "L1" :name "Only" :num-teams 12}]}])
  (is (= {:provider "sleeper" :user-id "u1" :username "jay"}
         (get-in @rdb/app-db [:accounts "sleeper"]))
      "keyed by provider, so a second provider lands beside it rather than over it")
  (is (some #{:league-choose} (dispatched))
      "asking a manager to confirm the only possible answer is a step for nothing"))

(deftest several-leagues-wait-to-be-picked
  (rf/dispatch-sync [:league-user-loaded
                     {:user {:user-id "u1" :display-name "jay"}
                      :leagues [{:league-id "L1" :name "One"} {:league-id "L2" :name "Two"}]}])
  (is (= 2 (count (:league-choices @rdb/app-db))))
  (is (not (some #{:league-choose} (dispatched)))))

(deftest an-account-with-no-leagues-says-so-rather-than-failing
  (rf/dispatch-sync [:league-user-loaded
                     {:user {:user-id "u1" :display-name "jay"} :leagues []}])
  (is (= [] (:league-choices @rdb/app-db)) "looked up, and plays in none")
  (is (re-find #"no leagues" (:waiver-status @rdb/app-db)))
  (is (not (some #{:league-choose} (dispatched)))))

(deftest choosing-a-league-syncs-its-rosters-and-imports-its-rules
  ;; Two questions off one id. A manager who synced without importing gets a
  ;; board priced under the draft config's scoring rather than his league's.
  (rf/dispatch-sync [:league-choose {:league-id "L1" :name "One" :season "2026"}])
  (let [evs (dispatched)
        k   (db/league-key "sleeper" "L1")]
    (is (some #{:sync-league} evs))
    (is (some #{:import-league} evs))
    (is (= k (:active-league @rdb/app-db)) "and it becomes the league on screen")
    (is (= "One" (get-in @rdb/app-db [:leagues k :name]))
        "named from the picker, so the switcher has something to say before the sync lands")))

(deftest a-bare-league-id-is-accepted-as-well-as-a-picked-one
  ;; The Settings field for a league the connected account is not in.
  (rf/dispatch-sync [:league-choose "L9"])
  (is (= (db/league-key "sleeper" "L9") (:active-league @rdb/app-db)))
  (is (= "L9" (get-in @rdb/app-db [:leagues (db/league-key "sleeper" "L9") :league-id]))))

(deftest switching-leagues-re-prices-both-boards
  ;; The failure this whole reshape exists to prevent: scoring is per league, so
  ;; a switch that moved only the rosters would leave Worth priced under the
  ;; league you left, with nothing on screen to say so.
  (swap! rdb/app-db assoc
         :players [{:player-id "p1" :position "RB"}]
         :leagues {"sleeper:a" {:provider "sleeper" :league-id "a"
                                :config (assoc db/default-config :scoring :standard)}
                   "sleeper:b" {:provider "sleeper" :league-id "b"
                                :config (assoc db/default-config :scoring :ppr)}}
         :active-league "sleeper:a")
  (rf/dispatch-sync [:set-active-league "sleeper:b"])
  (is (= :ppr (get-in @rdb/app-db [:config :scoring]))
      "the active league's rules become the board's rules")
  (let [evs (dispatched)]
    (is (some #{:recompute} evs) "the draft board re-prices")
    (is (some #{:fetch-waivers} evs) "and so does the waiver board")))

(deftest switching-leagues-rebuilds-the-teams-the-new-config-describes
  ;; The config carries :num-teams, :starting-bankroll and the roster template,
  ;; and :teams is built from exactly those three. A switch that moved the config
  ;; without them sent a 12-team replacement level alongside ten teams' worth of
  ;; cash, so every dollar on the board was wrong with nothing on screen to say
  ;; so — and the League tab drew ten columns for a twelve-team league.
  (swap! rdb/app-db assoc
         :players [{:player-id "p1" :position "RB"}]
         :teams (db/make-teams 10 db/default-roster 200)
         :leagues {"sleeper:a" {:provider "sleeper" :league-id "a"
                                :config (assoc db/default-config :num-teams 10)}
                   "sleeper:b" {:provider "sleeper" :league-id "b"
                                :config (assoc db/default-config
                                               :num-teams 12 :starting-bankroll 300)}}
         :active-league "sleeper:a")
  (rf/dispatch-sync [:set-active-league "sleeper:b"])
  (is (= 12 (count (:teams @rdb/app-db))))
  (is (= 300 (:bankroll (first (:teams @rdb/app-db)))))
  (rf/dispatch-sync [:recompute])
  (let [b (:body (last-http))]
    (is (= 12 (:num-teams b)))
    (is (= 12 (count (get-in b [:league-state :teams])))
        "the count the server prices against and the count it is told must agree")))

(deftest a-draft-with-picks-in-it-keeps-its-teams-across-a-switch
  ;; Draft state is still one draft for one team (docs/TODO.md). Rebuilding the
  ;; teams under it would throw away the picks' bankrolls and rosters, which is
  ;; worse than the mismatch it avoids — so the switch leaves them exactly as
  ;; `:apply-config` does.
  (swap! rdb/app-db assoc
         :players [{:player-id "p1" :position "RB"}]
         :teams (db/make-teams 10 db/default-roster 200)
         :picks [{:player-id "gibbs" :price 43 :team-id "t0"}]
         :leagues {"sleeper:a" {:provider "sleeper" :league-id "a" :config db/default-config}
                   "sleeper:b" {:provider "sleeper" :league-id "b"
                                :config (assoc db/default-config :num-teams 12)}}
         :active-league "sleeper:a")
  (rf/dispatch-sync [:set-active-league "sleeper:b"])
  (is (= 10 (count (:teams @rdb/app-db)))))

(deftest switching-drops-the-previous-league-s-waiver-board
  ;; The waiver board states *facts* about a league — who is rostered, what your
  ;; FAAB is, which man you would drop. Under another league's name those are not
  ;; stale, they are false. `:ranked` is deliberately left alone: same universe,
  ;; different dollars, which is the staleness `:recompute-failed` tolerates on
  ;; purpose.
  (swap! rdb/app-db assoc
         :players [{:player-id "p1" :position "RB"}]
         :waivers {:players [{:player-id "a"}] :faab {:left 60}}
         :ranked  {:players [{:player-id "a"}]}
         :leagues {"sleeper:a" {:provider "sleeper" :league-id "a" :config db/default-config}
                   "sleeper:b" {:provider "sleeper" :league-id "b" :config db/default-config}}
         :active-league "sleeper:a")
  (rf/dispatch-sync [:set-active-league "sleeper:b"])
  (is (nil? (:waivers @rdb/app-db)) "so it reads as loading rather than as another league's")
  (is (some? (:ranked @rdb/app-db)) "but the draft board is not blanked on every switch"))

(deftest a-board-priced-under-another-league-s-rules-says-so
  ;; The claim this replaces was that `:ranked` survives a switch as "the same
  ;; universe under different dollars". It does not: scoring moves `:points`,
  ;; which moves VORP, the tiers and every rank; the bankroll and team count move
  ;; every dollar; and the vendor columns are flattened per scoring format, so
  ;; ECR and ADP move too. Name, team, bye and position are what survive.
  (swap! rdb/app-db assoc
         :players [{:player-id "p1" :position "RB"}]
         :leagues {"sleeper:a" {:provider "sleeper" :league-id "a" :name "Alpha"
                                :config (assoc db/default-config :scoring :ppr)}
                   "sleeper:b" {:provider "sleeper" :league-id "b" :name "Beta"
                                :config (assoc db/default-config
                                               :scoring :standard :starting-bankroll 300)}}
         :active-league "sleeper:a")
  (rf/dispatch-sync [:recompute])
  (rf/dispatch-sync [:ranked-loaded (:recompute-seq @rdb/app-db)
                     (db/rules-stamp (:config @rdb/app-db))
                     {:players [{:player-id "p1" :worth 51}]}])
  (rf/clear-subscription-cache!)
  (is (false? (sub [:board-rules-stale?])) "its own league's board is not stale")

  (rf/dispatch-sync [:set-active-league "sleeper:b"])
  (rf/clear-subscription-cache!)
  (is (true? (sub [:board-rules-stale?])))
  (is (re-find #"previous league" (render board/rules-banner))
      "and the board says it out loud rather than leaving it to the status line")

  (testing "and stops saying it once the matching reply lands"
    (rf/dispatch-sync [:recompute])
    (rf/dispatch-sync [:ranked-loaded (:recompute-seq @rdb/app-db)
                       (db/rules-stamp (:config @rdb/app-db))
                       {:players [{:player-id "p1" :worth 77}]}])
    (rf/clear-subscription-cache!)
    (is (false? (sub [:board-rules-stale?])))
    (is (= "nil" (render board/rules-banner)) "no banner, not an empty one")))

(deftest two-leagues-on-the-same-rules-switch-without-a-word
  ;; The reason the board is stamped with its rules rather than simply cleared on
  ;; every switch: when the rules are the same the board is *correct*, and
  ;; blanking it would cost a manager his screen for nothing.
  (swap! rdb/app-db assoc
         :players [{:player-id "p1" :position "RB"}]
         :leagues {"sleeper:a" {:provider "sleeper" :league-id "a" :config db/default-config}
                   "sleeper:b" {:provider "sleeper" :league-id "b" :config db/default-config}}
         :active-league "sleeper:a")
  (rf/dispatch-sync [:recompute])
  (rf/dispatch-sync [:ranked-loaded (:recompute-seq @rdb/app-db)
                     (db/rules-stamp (:config @rdb/app-db))
                     {:players [{:player-id "p1" :worth 51}]}])
  (rf/dispatch-sync [:set-active-league "sleeper:b"])
  (rf/clear-subscription-cache!)
  (is (false? (sub [:board-rules-stale?])))
  (is (some? (:ranked @rdb/app-db)) "and the board is still on screen"))

(deftest a-pick-without-its-reply-is-the-staleness-that-is-tolerated
  ;; Two kinds of stale that look the same in db. This one is a dollar or two out
  ;; of date and must stay readable — flagging it would make the banner noise and
  ;; the banner would stop being read.
  (swap! rdb/app-db assoc :players [{:player-id "p1" :position "RB"}])
  (rf/dispatch-sync [:recompute])
  (rf/dispatch-sync [:ranked-loaded (:recompute-seq @rdb/app-db)
                     (db/rules-stamp (:config @rdb/app-db))
                     {:players [{:player-id "p1" :worth 51}]}])
  (rf/dispatch-sync [:record-pick {:player-id "p1" :price "5" :team-id "t0" :position "RB"}])
  (rf/clear-subscription-cache!)
  (is (false? (sub [:board-rules-stale?]))))

(deftest a-recompute-that-never-lands-leaves-the-warning-up
  ;; The case the status line handled worst. `:recompute-failed` keeps `:ranked`
  ;; on purpose, so a failure right after a switch used to leave the previous
  ;; league's whole board under this league's name indefinitely, with one muted
  ;; grey string to explain it.
  (swap! rdb/app-db assoc
         :players [{:player-id "p1" :position "RB"}]
         :leagues {"sleeper:a" {:provider "sleeper" :league-id "a" :config db/default-config}
                   "sleeper:b" {:provider "sleeper" :league-id "b"
                                :config (assoc db/default-config :scoring :standard)}}
         :active-league "sleeper:a")
  (rf/dispatch-sync [:recompute])
  (rf/dispatch-sync [:ranked-loaded (:recompute-seq @rdb/app-db)
                     (db/rules-stamp (:config @rdb/app-db))
                     {:players [{:player-id "p1" :worth 51}]}])
  (rf/dispatch-sync [:set-active-league "sleeper:b"])
  (rf/dispatch-sync [:recompute-failed "offline"])
  (rf/clear-subscription-cache!)
  (is (true? (sub [:board-rules-stale?])))
  (is (some? (:ranked @rdb/app-db)) "still readable, as :recompute-failed intends")
  (is (re-find #"previous league" (render board/rules-banner))
      "but no longer silently claiming to be this league's"))

(deftest sorting-the-watch-list-refuses-while-the-board-is-another-league-s
  ;; The one place a stale read *writes*: the reordered list is persisted, so a
  ;; sort during the window would bake the previous league's ranking into stored
  ;; state where nothing later would reveal it.
  (swap! rdb/app-db assoc
         :players [{:player-id "p1" :position "RB"}]
         :watchlist ["gibbs" "lamb" "bijan"]
         :leagues {"sleeper:a" {:provider "sleeper" :league-id "a" :config db/default-config}
                   "sleeper:b" {:provider "sleeper" :league-id "b"
                                :config (assoc db/default-config :scoring :standard)}}
         :active-league "sleeper:a")
  (rf/dispatch-sync [:recompute])
  (rf/dispatch-sync [:ranked-loaded (:recompute-seq @rdb/app-db)
                     (db/rules-stamp (:config @rdb/app-db))
                     {:players [{:player-id "gibbs" :worth 51 :vorp 100.0}
                                {:player-id "bijan" :worth 58 :vorp 120.0}
                                {:player-id "lamb"  :worth 55 :vorp 110.0}]}])
  (rf/dispatch-sync [:set-active-league "sleeper:b"])
  (rf/dispatch-sync [:watch-sort :rank])
  (is (= ["gibbs" "lamb" "bijan"] (:watchlist @rdb/app-db))
      "the manager's own order is left exactly as it was"))

(deftest choosing-a-league-re-ranks-under-the-config-it-just-activated
  ;; A new league is seeded with the *previous* league's config until the import
  ;; answers — and if the import fails it keeps it. Without a recompute here the
  ;; board stayed priced under the league you left while the Scoring card showed
  ;; something else.
  (swap! rdb/app-db assoc :players [{:player-id "p1" :position "RB"}])
  (rf/dispatch-sync [:league-choose {:league-id "L1" :name "One"}])
  (is (some #{:recompute} (dispatched))))

(deftest a-failed-import-is-reported-where-the-league-was-chosen
  ;; The Settings card renders `:waiver-status`; the header renders `:status`.
  ;; Reporting into one of them put the failure on the tab nobody was looking at.
  (rf/dispatch-sync [:league-import-failed "404"])
  (is (re-find #"import failed" (:status @rdb/app-db)))
  (is (re-find #"import failed" (:waiver-status @rdb/app-db))))

(deftest a-sync-with-no-league-to-sync-says-so-rather-than-throwing
  ;; `db/league-key` calls `name` on the provider, and `(name nil)` throws in
  ;; ClojureScript — killing the event rather than reporting anything.
  (rf/dispatch-sync [:sync-league {:provider nil :league-id "123"}])
  (is (re-find #"no league" (:waiver-status @rdb/app-db)))
  (is (empty? (:http @captured)) "and nothing is sent"))

(deftest a-config-edit-follows-the-league-it-was-made-in
  ;; `:config` is a *copy* of the active league's config. Every writer goes
  ;; through `db/set-config`; one that skipped the mirror would have its edit
  ;; silently reverted by the next switch away and back.
  (swap! rdb/app-db assoc
         :players [{:player-id "p1" :position "RB"}]
         :leagues {"sleeper:a" {:provider "sleeper" :league-id "a" :config db/default-config}
                   "sleeper:b" {:provider "sleeper" :league-id "b"
                                :config (assoc db/default-config :scoring :standard)}}
         :active-league "sleeper:a")
  (rf/dispatch-sync [:select-scoring-preset :half-ppr])
  (rf/dispatch-sync [:set-active-league "sleeper:b"])
  (is (= :standard (get-in @rdb/app-db [:config :scoring])))
  (rf/dispatch-sync [:set-active-league "sleeper:a"])
  (is (= :half-ppr (get-in @rdb/app-db [:config :scoring]))
      "the edit is still there — it was written to the league, not only to the copy"))

(deftest a-background-import-does-not-narrate-itself-as-the-league-on-screen
  ;; Choose two leagues in quick succession and the late reply would otherwise
  ;; pop `✓ Imported "Old"` in the header and replace the Settings import report
  ;; — describing the league you had already switched away from.
  ;; Asserted on what the handler *asks for* rather than on db: `:dispatch` is
  ;; stubbed in this namespace, so a follow-on event never reaches its handler
  ;; and a db assertion here would pass whatever the handler did.
  (swap! rdb/app-db assoc
         :leagues {"sleeper:a" {:provider "sleeper" :league-id "a" :config db/default-config}
                   "sleeper:b" {:provider "sleeper" :league-id "b" :config db/default-config}}
         :active-league "sleeper:b")
  (rf/dispatch-sync [:league-import-loaded "sleeper:a"
                     {:name "Old" :season "2026" :scoring :standard
                      :unsupported-scoring ["fgm_50p"]}])
  (let [evs (dispatched)]
    (is (not (some #{:set-status} evs)))
    (is (not (some #{:set-import-report} evs)))
    (is (not (some #{:apply-config} evs))))
  (testing "while the league on screen still announces its own import"
    (reset! captured {:http [] :persist [] :debounce [] :dispatch []})
    (rf/dispatch-sync [:league-import-loaded "sleeper:b"
                       {:name "Mine" :season "2026" :scoring :ppr}])
    (let [evs (dispatched)]
      (is (some #{:set-status} evs))
      (is (some #{:set-import-report} evs))
      (is (some #{:apply-config} evs)))))

(deftest an-import-for-a-league-you-are-no-longer-on-is-stored-not-applied
  ;; Two leagues chosen in quick succession answer in whatever order the network
  ;; decides. Applying the late one would re-price the board you are looking at
  ;; under a league you left.
  (swap! rdb/app-db assoc
         :leagues {"sleeper:a" {:provider "sleeper" :league-id "a" :config db/default-config}
                   "sleeper:b" {:provider "sleeper" :league-id "b" :config db/default-config}}
         :active-league "sleeper:b")
  (rf/dispatch-sync [:league-import-loaded "sleeper:a"
                     {:name "Old" :season "2026" :scoring :standard :num-teams 10}])
  (is (= :standard (get-in @rdb/app-db [:leagues "sleeper:a" :config :scoring]))
      "kept, so switching to it later opens under its own rules")
  (is (not (some #{:apply-config} (dispatched)))
      "but the board on screen is left alone"))

(deftest the-connected-account-is-stored-but-the-league-list-is-not
  ;; The account is persisted now — the version bump this reshape needed was
  ;; already being paid, and a username retyped every session is a login the app
  ;; is pretending not to have.
  ;;
  ;; The league *list* is not. It is a snapshot of what an account plays in this
  ;; season, refetched in one click, and a stored copy would go stale the first
  ;; time a manager joined a league.
  (rf/dispatch-sync [:league-user-loaded
                     {:user {:user-id "u1" :display-name "jay"} :leagues []}])
  (let [slice (last (:persist @captured))]
    (is (= "jay" (get-in slice [:accounts "sleeper" :username])))
    (is (not (contains? slice :league-choices))))
  (testing "and everything a reload needs is in the persisted slice"
    (is (some #{:accounts} db/persist-keys))
    (is (some #{:leagues} db/persist-keys))
    (is (some #{:active-league} db/persist-keys))))

;; ---- this week's game ----

(deftest matchup-names-the-side-and-the-bye
  ;; Home/away is not on the weekly entry — both teams of a game share one
  ;; game_id — so it rides in from the schedule, and "vs" vs "@" is the only
  ;; thing that shows it landed.
  (is (= "vs NE" (waivers/week-matchup {:week/opponent "NE" :week/home? true} 1)))
  (is (= "@ SEA" (waivers/week-matchup {:week/opponent "SEA" :week/home? false} 1)))
  ;; A bye is read off the week the board is showing, not inferred from a
  ;; missing line — an unprojected starter also has no line.
  (is (= "Bye" (waivers/week-matchup {:bye 6} 6)))
  (is (= "–"   (waivers/week-matchup {:bye 6} 5)))
  ;; No week at all (preseason, or a weekly file that has not landed): a bye
  ;; cannot be claimed, so it is not.
  (is (= "–"   (waivers/week-matchup {:bye 6} nil))))

(deftest week-cell-dashes-rather-than-zeroes
  ;; Not projected and projected to score nothing are different answers, and a
  ;; 0 in this column would assert the second.
  (let [txt (fn [p] (last (waivers/cell :week p 3)))]
    (is (= 12 (txt {:week-points 11.6})))
    (is (= [:span.muted "–"] (txt {})))
    (is (= 0 (txt {:week-points 0.2})))))

(deftest projection-age-is-coarse
  ;; The question is whether the number predates today's news, not what minute
  ;; it landed.
  (let [ago #(.toISOString (js/Date. (- (js/Date.now) (* % 60000))))]
    (is (= "just now"      (waivers/relative-age (ago 0))))
    (is (= "35 minutes ago" (waivers/relative-age (ago 35))))
    (is (= "1 hour ago"    (waivers/relative-age (ago 62))))
    (is (= "3 hours ago"   (waivers/relative-age (ago 180))))
    (is (= "2 days ago"    (waivers/relative-age (ago 2880))))
    (is (nil? (waivers/relative-age nil)))))

(deftest week-note-is-absent-without-a-week
  ;; The weekly asset 404s until week 1 is played, so the board has to render
  ;; with these columns entirely absent rather than claiming week 0.
  (is (nil? (waivers/week-note {:through-week 0})))
  (is (some? (waivers/week-note {:week 4 :week-fetched-at nil}))))

(deftest matchup-prints-a-bare-opponent-when-the-side-is-unknown
  ;; nil :week/home? is the schedule not arriving, not an away game. Printing
  ;; "@ NE" for a home game would be confidently wrong; the opponent alone is
  ;; the part actually known.
  (is (= "NE" (waivers/week-matchup {:week/opponent "NE"} 1)))
  (is (= "NE" (waivers/week-matchup {:week/opponent "NE" :week/home? nil} 1)))
  ;; An explicit false still prints the away marker.
  (is (= "@ NE" (waivers/week-matchup {:week/opponent "NE" :week/home? false} 1))))
