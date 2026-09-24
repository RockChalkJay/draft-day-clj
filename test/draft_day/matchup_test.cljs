(ns draft-day.matchup-test
  "The matchup tab's browser-side behaviour: the stale-reply guard, what a
  league switch is allowed to keep, and the subs that decide which game is on
  screen and which side of it is yours.

  None of this is reachable from `lein test` — run with
  `npx shadow-cljs compile test && node out/node-tests.js`."
  (:require [cljs.test :refer [deftest is testing use-fixtures]]
            [re-frame.core :as rf]
            [re-frame.db :as rdb]
            [re-frame.registrar :as registrar]
            [reagent.ratom]
            [draft-day.db :as db]
            [draft-day.fx]
            [draft-day.subs]
            [draft-day.views.matchup :as matchup]
            [draft-day.events]))

(defonce captured (atom {}))

(def ^:private stubs
  ;; As `waivers-test`: re-frame's real `:dispatch` queues for a later tick.
  {:http     (fn [r] (swap! captured update :http conj r))
   :persist! (fn [r] (swap! captured update :persist conj r))
   :debounce (fn [r] (swap! captured update :debounce conj r))
   :dispatch (fn [r] (swap! captured update :dispatch conj r))})

(defonce ^:private real-fx
  (into {} (map (juxt identity #(registrar/get-handler :fx %))) (keys stubs)))

(defn- swap-fx! [m]
  (doseq [[id f] m] (rf/clear-fx id) (when f (rf/reg-fx id f))))

(use-fixtures :each
  {:before (fn [] (swap-fx! stubs)
                  (reset! captured {:http [] :persist [] :debounce [] :dispatch []})
                  (rf/clear-subscription-cache!)
                  (reset! rdb/app-db (db/default-db)))
   :after  (fn [] (swap-fx! real-fx)
                  (rf/clear-subscription-cache!)
                  (reset! rdb/app-db (db/default-db)))})

(defn- last-http [] (last (:http @captured)))
(defn- dispatched [] (mapv first (:dispatch @captured)))
(defn- sub [q] (binding [reagent.ratom/*ratom-context* #js {}] @(rf/subscribe q)))

(def ^:private league
  {:provider "sleeper" :league-id "99" :name "The League" :my-roster-id 2
   :sync {:teams [{:roster-id 1 :name "Them"} {:roster-id 2 :name "Mine"}]}})

(defn- connect!
  "A synced league, in season. The matchup tab is only in the season's half of
  the app, so a league still on draft day would switch the view off it."
  []
  (swap! rdb/app-db assoc
         :leagues {"sleeper:99" league}
         :active-league "sleeper:99"
         :universe {:through-week 2}))

(def ^:private reply
  {:week 3 :my-roster-id 2
   :matchups [{:matchup-id 7 :roster-ids [3 4]}
              {:matchup-id 8 :roster-ids [1 2]}
              {:matchup-id nil :roster-ids [5]}]
   :teams [{:roster-id 1 :name "Them" :projected 100.0 :actual 90.0}
           {:roster-id 2 :name "Mine" :projected 110.0 :actual 95.0}
           {:roster-id 3 :name "Third" :projected 80.0 :actual 70.0}
           {:roster-id 4 :name "Fourth" :projected 85.0 :actual 75.0}
           {:roster-id 5 :name "Byed" :projected 60.0 :actual 55.0}]})


(deftest a-matchup-request-carries-who-to-ask
  ;; This board takes a live fetch server-side, so it needs the provider and
  ;; league id as well as the synced rosters.
  (connect!)
  (rf/dispatch-sync [:fetch-matchup])
  (let [{:keys [url body]} (last-http)]
    (is (= "/api/matchup" url))
    (is (= "sleeper" (:provider body)))
    (is (= "99" (:league-id body)))
    (is (= 2 (:my-roster-id body)))
    (is (some? (:league body)) "the synced rosters ride along as they do for waivers")))

(deftest no-league-asks-nobody-and-says-so
  ;; Otherwise a nil league id POSTs and comes back 400 — a server error
  ;; reported for a client-side fact.
  (rf/dispatch-sync [:fetch-matchup])
  (is (nil? (last-http)))
  (is (re-find #"No league connected" (:matchup-status @rdb/app-db))))

(deftest a-reply-about-an-older-request-cannot-win
  ;; The worst of the three races to lose: a stale score nothing contradicts.
  (connect!)
  (rf/dispatch-sync [:fetch-matchup])
  (let [first-seq (:matchup-seq @rdb/app-db)]
    (rf/dispatch-sync [:fetch-matchup])
    (let [second-seq (:matchup-seq @rdb/app-db)]
      (is (< first-seq second-seq) "each request takes a fresh stamp")
      (rf/dispatch-sync [:matchup-loaded second-seq {:week 4}])
      (rf/dispatch-sync [:matchup-loaded first-seq {:week 3}])
      (is (= 4 (-> @rdb/app-db :matchup :week)) "the older reply is dropped, not merged"))))

(deftest a-failure-leaves-the-previous-board-readable
  ;; A scoreboard a few minutes old still answers who is winning.
  (connect!)
  (rf/dispatch-sync [:matchup-loaded 0 reply])
  (rf/dispatch-sync [:matchup-failed "network down"])
  (is (some? (:matchup @rdb/app-db)))
  (is (re-find #"network down" (:matchup-status @rdb/app-db))))


(deftest opening-the-tab-fetches-once
  (connect!)
  (rf/dispatch-sync [:set-view :matchup])
  (is (= [:fetch-matchup] (dispatched)))
  (testing "and not again once it has a board"
    (rf/dispatch-sync [:matchup-loaded (:matchup-seq @rdb/app-db) reply])
    ;; The draft board has never been ranked in this db, and leaving for it is
    ;; that board's own first open — which is not this tab's business.
    (swap! rdb/app-db assoc :ranked {:players []})
    (reset! captured {:http [] :persist [] :debounce [] :dispatch []})
    (rf/dispatch-sync [:set-view :board])
    (rf/dispatch-sync [:set-view :matchup])
    (is (= [] (dispatched)) "a refresh is a button, not a side effect of navigation")))

(deftest switching-leagues-drops-the-matchup-rather-than-staling-it
  ;; Under another league's name those facts are false, not stale.
  (connect!)
  (swap! rdb/app-db assoc-in [:leagues "sleeper:100"]
         {:provider "sleeper" :league-id "100" :config {}})
  (rf/dispatch-sync [:matchup-loaded 0 reply])
  (swap! rdb/app-db assoc :matchup-pick 3)
  (rf/dispatch-sync [:set-active-league "sleeper:100"])
  (is (nil? (:matchup @rdb/app-db)))
  (is (nil? (:matchup-pick @rdb/app-db)) "and the picked game goes with it"))

(deftest a-league-switch-refetches-the-matchup-for-every-season-tab
  ;; `activate` drops the league you left, and the header's week is this reply's.
  (connect!)
  (swap! rdb/app-db assoc-in [:leagues "sleeper:100"] {:provider "sleeper" :league-id "100"})
  (testing "a league with no rosters yet syncs first"
    (rf/dispatch-sync [:set-active-league "sleeper:100"])
    (is (not (some #{:fetch-matchup} (dispatched))))
    (is (some #{:sync-league} (dispatched))))
  (doseq [v [:matchup :waivers]]
    (testing (str "on " v)
      (reset! captured {:http [] :persist [] :debounce [] :dispatch []})
      (swap! rdb/app-db assoc :view v)
      (rf/dispatch-sync [:set-active-league "sleeper:99"])
      (is (some #{:fetch-matchup} (dispatched))
          "every season tab shows the week, not only the two with a board")))
  (testing "and not from the draft half, which has no week to show"
    (reset! captured {:http [] :persist [] :debounce [] :dispatch []})
    (swap! rdb/app-db (fn [db] (-> db
                                   (assoc :view :board)
                                   (assoc-in [:leagues "sleeper:99" :phase] :draft))))
    (rf/dispatch-sync [:set-active-league "sleeper:99"])
    (is (not (some #{:fetch-matchup} (dispatched)))
        "the draft half picks it up from `:set-view`'s first-open fetch")))

(deftest a-reply-about-the-league-you-left-cannot-land-under-this-one
  ;; Asked on the matchup tab, then a switch from another tab, which does not
  ;; refetch — so only the switch itself can retire the request.
  (connect!)
  (swap! rdb/app-db assoc-in [:leagues "sleeper:100"]
         {:provider "sleeper" :league-id "100" :sync {:teams []}})
  (rf/dispatch-sync [:fetch-matchup])
  (let [asked (:matchup-seq @rdb/app-db)]
    (rf/dispatch-sync [:set-active-league "sleeper:100"])
    (rf/dispatch-sync [:matchup-loaded asked reply])
    (is (nil? (:matchup @rdb/app-db)))))

(deftest a-first-sync-on-the-matchup-tab-fetches-after-the-rosters-land
  ;; Fetching alongside the sync asked with no rosters: every team empty.
  (connect!)
  (swap! rdb/app-db assoc :view :matchup)
  (swap! rdb/app-db assoc-in [:leagues "sleeper:100"] {:provider "sleeper" :league-id "100"})
  (rf/dispatch-sync [:set-active-league "sleeper:100"])
  (is (not (some #{:fetch-matchup} (dispatched))) "not before the sync")
  (reset! captured {:http [] :persist [] :debounce [] :dispatch []})
  (rf/dispatch-sync [:league-synced "sleeper:100" {:teams [{:roster-id 1 :name "A"}]}])
  (is (some #{:fetch-matchup} (dispatched)) "but once it lands"))

(deftest a-failed-first-sync-says-so-on-the-matchup-tab
  ;; The fetch waits for the sync, so a sync that fails would otherwise leave
  ;; "Loading…" up for good — or the league you left's status under this name.
  (connect!)
  (swap! rdb/app-db assoc :view :matchup :matchup-status "Matchup failed: old league")
  (swap! rdb/app-db assoc-in [:leagues "sleeper:100"] {:provider "sleeper" :league-id "100"})
  (rf/dispatch-sync [:set-active-league "sleeper:100"])
  (is (nil? (:matchup-status @rdb/app-db)) "the old league's status does not carry over")
  (rf/dispatch-sync [:league-sync-failed "sleeper:100" "no such league" 404])
  (is (= "League sync failed: no such league" (:matchup-status @rdb/app-db))))

(deftest picking-my-team-moves-the-board-without-a-refetch
  ;; The reply was asked before a team was picked.
  (connect!)
  (swap! rdb/app-db assoc-in [:leagues "sleeper:99" :my-roster-id] nil)
  (rf/dispatch-sync [:matchup-loaded 0 (assoc reply :my-roster-id nil)])
  (is (= 7 (:matchup-id (sub [:selected-matchup]))) "the first game, with no team")
  (swap! rdb/app-db assoc-in [:leagues "sleeper:99" :my-roster-id] 2)
  (rf/clear-subscription-cache!)
  (is (= 8 (:matchup-id (sub [:selected-matchup]))))
  (is (= ["Mine" "Them"] (mapv :name (sub [:matchup-sides])))))

(deftest switching-games-costs-no-round-trip
  ;; Every roster came back in one reply.
  (connect!)
  (rf/dispatch-sync [:matchup-loaded 0 reply])
  (reset! captured {:http [] :persist [] :debounce [] :dispatch []})
  (rf/dispatch-sync [:set-matchup-pick "3"])
  (is (nil? (last-http))))


(deftest my-own-game-is-the-one-that-opens
  (connect!)
  (rf/dispatch-sync [:matchup-loaded 0 reply])
  (is (= 8 (:matchup-id (sub [:selected-matchup])))))

(deftest my-team-is-on-the-left-whichever-order-the-provider-sent
  ;; The provider's roster order is arbitrary; "mine on the left" is not.
  (connect!)
  (rf/dispatch-sync [:matchup-loaded 0 reply])
  (is (= ["Mine" "Them"] (mapv :name (sub [:matchup-sides]))))
  (testing "and when the provider already had me first"
    (rf/dispatch-sync [:matchup-loaded (:matchup-seq @rdb/app-db)
                       (assoc-in reply [:matchups 1 :roster-ids] [2 1])])
    (is (= ["Mine" "Them"] (mapv :name (sub [:matchup-sides]))))))

(deftest a-game-with-no-opponent-keeps-its-place
  ;; An odd league. Hiding it would take a manager's own week off his screen.
  (connect!)
  (rf/dispatch-sync [:matchup-loaded 0 reply])
  (let [byed (first (filter #(= [5] (:roster-ids %)) (sub [:matchup-games])))]
    (is (some? byed))
    (is (= ["Byed"] (:names byed)))))

(deftest a-pick-is-matched-without-assuming-an-integer-id
  ;; Ids belong to the provider, and one that is not base-10 would parse to
  ;; NaN, match no game, and snap silently back.
  (connect!)
  (rf/dispatch-sync [:matchup-loaded 0 reply])
  (rf/dispatch-sync [:set-matchup-pick "3"])
  (rf/clear-subscription-cache!)
  (is (= 7 (:matchup-id (sub [:selected-matchup]))) "the string form finds the game")
  (testing "and so does the number, since the reply's ids are numbers"
    (rf/dispatch-sync [:set-matchup-pick 5])
    (rf/clear-subscription-cache!)
    (is (= [5] (:roster-ids (sub [:selected-matchup]))))))

(deftest a-game-with-no-opponent-can-still-be-picked
  ;; Its nil matchup id is exactly what "nothing picked" looks like, which is
  ;; why the pick names a roster id.
  (connect!)
  (rf/dispatch-sync [:matchup-loaded 0 reply])
  (testing "nothing picked still opens on mine, not on the nil-id game"
    (is (= 8 (:matchup-id (sub [:selected-matchup])))))
  (testing "and the bye is reachable"
    (rf/dispatch-sync [:set-matchup-pick 5])
    (rf/clear-subscription-cache!)
    (is (= [5] (:roster-ids (sub [:selected-matchup]))))))

(deftest the-other-side-is-nil-rather-than-a-blank-team
  ;; So the view can say "no opponent" rather than draw half a table.
  (connect!)
  (swap! rdb/app-db assoc-in [:leagues "sleeper:99" :my-roster-id] 5)
  (rf/dispatch-sync [:matchup-loaded 0 reply])
  (let [[l r] (sub [:matchup-sides])]
    (is (= "Byed" (:name l)))
    (is (nil? r))))

(deftest a-roster-the-reply-does-not-name-still-labels-its-game
  (connect!)
  (rf/dispatch-sync [:matchup-loaded 0 (update reply :teams #(remove (comp #{3} :roster-id) %))])
  (let [g (first (filter #(= 7 (:matchup-id %)) (sub [:matchup-games])))]
    (is (= ["Roster 3" "Fourth"] (:names g)))))


;; The dash itself is `util/week-points`'s, and `util-test` pins it.
(deftest the-pending-class-is-what-keeps-the-two-apart
  ;; The muted, unbolded look is the only thing telling "not yet" from
  ;; "nothing", so it is rendered rather than asserted on a string.
  (let [pending (pr-str (matchup/player-cell {:player-id "a" :player-name "A"} :l 3))
        scored  (pr-str (matchup/player-cell {:player-id "a" :player-name "A" :actual 0.0} :l 3))]
    (is (re-find #"mu-a pending" pending))
    (is (not (re-find #"pending" scored)))))

(deftest the-two-sides-mirror-so-the-actuals-meet-at-the-centre
  ;; Player · Proj · Actual | seat | Actual · Proj · Player.
  (let [classes (fn [side]
                  (->> (matchup/player-cell {:player-id "a" :player-name "A" :slot "QB"
                                             :week-points 20.0 :actual 18.0} side 3)
                       (drop 2)
                       (mapv (fn [child] (re-find #"mu-who|mu-p|mu-a" (pr-str child))))))]
    (is (= ["mu-who" "mu-p" "mu-a"] (classes :l)))
    (is (= ["mu-a" "mu-p" "mu-who"] (classes :r)))))

(deftest an-unfilled-seat-mirrors-too
  ;; Three grid tracks either way, so a lone cell on the right lands under the
  ;; narrow Actual column and the word is clipped there.
  (let [classes (fn [side]
                  (->> (matchup/empty-cell side true)
                       (drop 2)
                       (mapv (fn [child] (re-find #"mu-who|mu-p|mu-a" (pr-str child))))))]
    (is (= ["mu-who" "mu-p" "mu-a"] (classes :l)))
    (is (= ["mu-a" "mu-p" "mu-who"] (classes :r))))
  (testing "and a bench that is merely shorter still names no seat"
    (is (not (re-find #"empty" (pr-str (matchup/empty-cell :r false)))))))

(deftest a-bench-player-names-his-own-position-in-his-meta
  ;; A bench row has no shared seat label down the middle to say it.
  (is (re-find #"RB · " (pr-str (matchup/player-cell {:player-id "a" :player-name "A"
                                                      :position "RB"} :l 3))))
  (is (not (re-find #"RB · " (pr-str (matchup/player-cell {:player-id "a" :player-name "A"
                                                           :position "RB" :slot "RB"} :l 3))))
      "a starter's seat is already down the middle"))

;; ---- the lineup each side draws ----

(def ^:private team-t
  {:roster-id 1
   :starters [{:player-id "a" :slot "RB" :week-points 10.0 :actual 4.0}]
   :bench    [{:player-id "b" :week-points 14.0 :actual nil}]
   :projected 10.0 :actual 4.0
   :optimal {:projected {:gain 4.0 :seats-locked? false
                         :starters [{:player-id "b" :slot "RB" :week-points 14.0 :moved-in? true}]
                         :bench    [{:player-id "a" :week-points 10.0 :actual 4.0 :moved-out? true}]}
             :actual nil}})

(deftest a-side-draws-its-set-lineup-until-asked-for-its-best
  (is (= ["a"] (mapv :player-id (:starters (matchup/side-lineup team-t :set)))))
  (let [best (matchup/side-lineup team-t :projected)]
    (is (= ["b"] (mapv :player-id (:starters best))))
    (is (:moved-out? (first (:bench best))) "the benched starter leads the bench")
    (is (= 14.0 (:projected best)) "and the totals are the best lineup's")
    (is (= :projected (:basis best))))
  (is (= ["a"] (mapv :player-id (:starters (matchup/side-lineup team-t :actual))))
      "a basis that is not available yet draws the set lineup rather than nothing"))

(deftest the-hint-says-what-can-still-be-done-or-what-was-left
  (is (= "Best by projection: +4.00 still possible" (matchup/lineup-hint team-t)))
  (is (= "Lineup locked"
         (matchup/lineup-hint (assoc-in team-t [:optimal :projected :seats-locked?] true))))
  (is (= "Best by actual: 9.50 left on the bench"
         (matchup/lineup-hint (assoc-in team-t [:optimal :actual] {:gain 9.5})))
      "once the week is final, regret outranks advice"))

(deftest a-best-lineup-already-set-gains-nothing-whatever-the-float-says
  ;; `optimal` sums the same players in two orders, so an unchanged lineup's
  ;; gain lands a float's width either side of zero.
  (is (= "Best by projection: no better lineup"
         (matchup/lineup-hint (assoc-in team-t [:optimal :projected :gain] 1e-14))))
  (doseq [g [0.0 -1e-14 1e-14]]
    (is (= "Best by actual: nothing left on the bench"
           (matchup/lineup-hint (assoc-in team-t [:optimal :actual] {:gain g})))))
  (let [best (matchup/side-lineup (assoc-in team-t [:optimal :projected :gain] 1e-14)
                                  :projected)]
    (is (not (re-find #"over set" (pr-str (matchup/totals-label best)))))))

(deftest the-set-lineup-totals-the-leagues-own-score
  ;; The header shows the score of record; a sum of per-player numbers can miss
  ;; it by a cent, and one team would read two scores.
  (is (= 108.52 (:actual (matchup/side-lineup (assoc team-t :official 108.52 :actual 108.51)
                                              :set))))
  (is (= 4.0 (:actual (matchup/side-lineup team-t :set))) "the sum, where there is no record"))

(deftest actual-is-offered-only-once-the-week-is-final
  (let [actual-btn (fn [t] (->> (matchup/lineup-control t :set)
                                (drop 1) first
                                (some #(when (= "Best by actual" (last %)) %))))]
    (is (:disabled (second (actual-btn team-t))))
    (is (not (:disabled (second (actual-btn (assoc-in team-t [:optimal :actual] {:gain 1.0}))))))))

(deftest choosing-a-lineup-is-per-team-and-a-new-game-resets-it
  (rf/dispatch-sync [:set-lineup-view 1 :projected])
  (rf/dispatch-sync [:set-lineup-view 2 :actual])
  (is (= {1 :projected 2 :actual} (:lineup-view @rdb/app-db)))
  (rf/dispatch-sync [:set-matchup-pick "3"])
  (is (= {} (:lineup-view @rdb/app-db))))

(deftest a-view-whose-basis-is-gone-falls-back-with-its-button
  (is (= :projected (matchup/shown-view team-t :projected)))
  (is (= :set (matchup/shown-view team-t :actual))
      "the actual basis goes with the scoreboard; a lit button over the set lineup is worse")
  (is (= :set (matchup/shown-view team-t :set))))

(deftest a-best-lineup-marks-who-it-moves
  (is (re-find #"moved-in" (pr-str (matchup/player-cell {:player-id "b" :player-name "B"
                                                         :slot "RB" :moved-in? true} :l 3))))
  (is (re-find #"▼" (pr-str (matchup/player-cell {:player-id "a" :player-name "A"
                                                  :moved-out? true} :r 3)))))
