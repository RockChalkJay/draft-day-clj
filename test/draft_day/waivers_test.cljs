(ns draft-day.waivers-test
  "The in-season half's browser-side behaviour: the stale-reply guard, the
  persisted-shape repair on the way in, and the sub that orders the board.

  None of this is reachable from `lein test` — run with
  `npx shadow-cljs compile test && node out/node-tests.js`."
  (:require [cljs.test :refer [deftest is testing use-fixtures]]
            [re-frame.core :as rf]
            [re-frame.db :as rdb]
            [re-frame.registrar :as registrar]
            [reagent.ratom]
            [draft-day.db :as db]
            [draft-day.fx]
            [draft-day.subs :as subs]
            [draft-day.views.waivers :as waivers]
            [draft-day.events]))

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
  {:teams [{:roster-id 1 :name "Mine" :player-ids ["a"] :faab-left 60}
           {:roster-id 2 :name "Them" :player-ids ["b"] :faab-left 95}]
   :waiver {:type "faab" :budget 100}})

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
  (swap! rdb/app-db assoc :league-sync synced :my-roster-id 1)
  (rf/dispatch-sync [:fetch-waivers])
  (let [b (:body (last-http))]
    (is (= "/api/waivers" (:url (last-http))))
    (is (= synced (:league b)))
    (is (= 1 (:my-roster-id b)))
    (is (pos? (:roster-size b)) "so the board knows whether a claim costs a drop")
    (is (= (count (db/roster-template (get-in @rdb/app-db [:config :roster])))
           (:roster-size b)))))

;; ---- sync ----

(deftest a-sync-is-repaired-on-the-way-in-not-only-at-boot
  ;; It is the same shape localStorage will hand back next session, so a
  ;; provider that grew or dropped a field should fail here — where the status
  ;; line can say so — rather than a session later with nothing to explain it.
  (rf/dispatch-sync [:league-synced synced])
  (is (= 2 (count (get-in @rdb/app-db [:league-sync :teams]))))
  (is (re-find #"Synced" (:waiver-status @rdb/app-db)))
  (is (some #{:fetch-waivers} (dispatched))
      "and the board is refreshed against the rosters that just arrived")
  (testing "a reply that is not a league is refused rather than half-stored"
    (rf/dispatch-sync [:league-synced {:not "a league"}])
    (is (nil? (:league-sync @rdb/app-db)))
    (is (re-find #"nothing usable" (:waiver-status @rdb/app-db)))))

(deftest a-sync-survives-a-reload
  (rf/dispatch-sync [:league-synced synced])
  (let [slice (last (:persist @captured))]
    (is (contains? slice :league-sync) "a sync redone on every page load is one nobody uses")
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

(deftest week-zero-is-reported-as-preseason-rather-than-as-a-board
  (swap! rdb/app-db assoc :waivers {:players [] :through-week 0})
  (rf/clear-subscription-cache!)
  (is (false? (sub [:in-season?])))
  (swap! rdb/app-db assoc :waivers {:players [] :through-week 6})
  (rf/clear-subscription-cache!)
  (is (true? (sub [:in-season?]))))

;; ---- cells ----

(deftest the-trend-column-does-not-colour-noise
  ;; A receiver at 1.04 has not earned an arrow, and a column that colours noise
  ;; stops being read at all.
  (is (nil? (waivers/trend-class 1.04)))
  (is (nil? (waivers/trend-class 0.9)))
  (is (= "trend-up" (waivers/trend-class 1.6)))
  (is (= "trend-down" (waivers/trend-class 0.4)))
  (is (nil? (waivers/trend-class nil))))

(deftest sorting-puts-players-with-nothing-to-say-last-in-both-directions
  ;; Same rule `sort-players` keeps: a nil is not a low value, it is an absent
  ;; one, and it must not float to the top when the column is reversed.
  (let [ps [{:player-id "a" :player-name "A" :bid 5 :upgrade 1.0}
            {:player-id "b" :player-name "B" :bid nil :upgrade 2.0}
            {:player-id "c" :player-name "C" :bid 9 :upgrade 3.0}]]
    (is (= ["c" "a" "b"] (mapv :player-id (subs/sort-waiver-players ps :bid -1))))
    (is (= ["a" "c" "b"] (mapv :player-id (subs/sort-waiver-players ps :bid 1))))))
