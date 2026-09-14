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
  ;; Same arrangement, and the same reason, as `waivers-test`: re-frame's real
  ;; `:dispatch` queues for a later tick, so a captured one is the only way to
  ;; assert synchronously about a follow-on dispatch.
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

(defn- connect! []
  (swap! rdb/app-db assoc
         :leagues {"sleeper:99" league}
         :active-league "sleeper:99"))

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

;; ---- the request ----

(deftest a-matchup-request-carries-who-to-ask
  ;; Unlike the other two boards this one takes a live fetch server-side, so it
  ;; needs the provider and league id as well as the synced rosters.
  (connect!)
  (rf/dispatch-sync [:fetch-matchup])
  (let [{:keys [url body]} (last-http)]
    (is (= "/api/matchup" url))
    (is (= "sleeper" (:provider body)))
    (is (= "99" (:league-id body)))
    (is (= 2 (:my-roster-id body)))
    (is (some? (:league body)) "the synced rosters ride along as they do for waivers")))

(deftest no-league-asks-nobody-and-says-so
  ;; A bare fetch with nothing connected would POST a nil league id and come
  ;; back 400, reporting a server error for a client-side fact.
  (rf/dispatch-sync [:fetch-matchup])
  (is (nil? (last-http)))
  (is (re-find #"No league connected" (:matchup-status @rdb/app-db))))

(deftest a-reply-about-an-older-request-cannot-win
  ;; The same guard the other two boards carry, with the worst symptom of the
  ;; three: a scoreboard answering out of order shows a score that has since
  ;; changed, and nothing on screen contradicts it.
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

;; ---- navigation ----

(deftest opening-the-tab-fetches-once
  (connect!)
  (rf/dispatch-sync [:set-view :matchup])
  (is (= [:fetch-matchup] (dispatched)))
  (testing "and not again once it has a board"
    (rf/dispatch-sync [:matchup-loaded (:matchup-seq @rdb/app-db) reply])
    (reset! captured {:http [] :persist [] :debounce [] :dispatch []})
    (rf/dispatch-sync [:set-view :board])
    (rf/dispatch-sync [:set-view :matchup])
    (is (= [] (dispatched)) "a refresh is a button, not a side effect of navigation")))

(deftest switching-leagues-drops-the-matchup-rather-than-staling-it
  ;; It states who you are playing and what he has scored. Under another
  ;; league's name those are not stale, they are false.
  (connect!)
  (swap! rdb/app-db assoc-in [:leagues "sleeper:100"]
         {:provider "sleeper" :league-id "100" :config {}})
  (rf/dispatch-sync [:matchup-loaded 0 reply])
  (swap! rdb/app-db assoc :matchup-pick 3)
  (rf/dispatch-sync [:set-active-league "sleeper:100"])
  (is (nil? (:matchup @rdb/app-db)))
  (is (nil? (:matchup-pick @rdb/app-db)) "and the picked game goes with it"))

(deftest a-league-switch-refetches-only-when-the-tab-is-on-screen
  ;; Every other tab picks it up from the first-open fetch, and a live fetch per
  ;; league switch is not free.
  (connect!)
  (swap! rdb/app-db assoc-in [:leagues "sleeper:100"] {:provider "sleeper" :league-id "100"})
  (testing "on another tab"
    (rf/dispatch-sync [:set-active-league "sleeper:100"])
    (is (not (some #{:fetch-matchup} (dispatched)))))
  (testing "on the matchup tab"
    (reset! captured {:http [] :persist [] :debounce [] :dispatch []})
    (swap! rdb/app-db assoc :view :matchup)
    (rf/dispatch-sync [:set-active-league "sleeper:99"])
    (is (some #{:fetch-matchup} (dispatched)))))

(deftest switching-games-costs-no-round-trip
  ;; Every roster in the league came back in one reply, which is why the server
  ;; values all of them rather than the pair asked for.
  (connect!)
  (rf/dispatch-sync [:matchup-loaded 0 reply])
  (reset! captured {:http [] :persist [] :debounce [] :dispatch []})
  (rf/dispatch-sync [:set-matchup-pick "3"])
  (is (nil? (last-http))))

;; ---- which game, and which side ----

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
  ;; Roster ids belong to the provider — the seam underneath exists for that —
  ;; and the picker hands back a raw string. Parsing it would give NaN for a
  ;; provider whose ids are not base-10, match no game, and snap silently back.
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
  ;; It carries a nil matchup id, which is exactly what "nothing picked" looks
  ;; like — so identifying a game by that made the bye both the default
  ;; selection and impossible to select on purpose. The pick names a ROSTER id
  ;; for this reason.
  (connect!)
  (rf/dispatch-sync [:matchup-loaded 0 reply])
  (testing "nothing picked still opens on mine, not on the nil-id game"
    (is (= 8 (:matchup-id (sub [:selected-matchup])))))
  (testing "and the bye is reachable"
    (rf/dispatch-sync [:set-matchup-pick 5])
    (rf/clear-subscription-cache!)
    (is (= [5] (:roster-ids (sub [:selected-matchup]))))))

(deftest the-other-side-is-nil-rather-than-a-blank-team
  ;; So the view can say "no opponent" instead of drawing half a table with no
  ;; explanation.
  (connect!)
  (rf/dispatch-sync [:matchup-loaded 0 (assoc reply :my-roster-id 5)])
  (let [[l r] (sub [:matchup-sides])]
    (is (= "Byed" (:name l)))
    (is (nil? r))))

(deftest a-roster-the-reply-does-not-name-still-labels-its-game
  (connect!)
  (rf/dispatch-sync [:matchup-loaded 0 (update reply :teams #(remove (comp #{3} :roster-id) %))])
  (let [g (first (filter #(= 7 (:matchup-id %)) (sub [:matchup-games])))]
    (is (= ["Roster 3" "Fourth"] (:names g)))))

;; ---- cells ----

(deftest a-dash-is-not-a-zero
  ;; The distinction this whole screen turns on. `:actual` is nil until the
  ;; player's game kicks off, and rendering that as 0.0 would turn a Sunday
  ;; morning into nine bad performances.
  (is (= "–" (matchup/fmt nil)))
  (is (= "0.0" (matchup/fmt 0.0)) "he played and did nothing, which is a result"))

(deftest points-keep-a-decimal
  ;; Weekly points are small enough that whole numbers would collapse 8.4 and
  ;; 12.6 into a comparison nobody can make.
  (is (= "12.6" (matchup/fmt 12.64)))
  (is (= "8.4" (matchup/fmt 8.44))))

(deftest a-league-that-reports-no-record-shows-none
  ;; A dash beside a team name reads as a score.
  (is (= "5–3" (matchup/record-label {:wins 5 :losses 3})))
  (is (nil? (matchup/record-label {})))
  (is (nil? (matchup/record-label {:wins 5}))))

(deftest the-pending-class-is-what-keeps-the-two-apart
  ;; Rendered rather than asserted on a string: the muted, unbolded look is the
  ;; only thing distinguishing "not yet" from "nothing".
  (let [pending (pr-str (matchup/player-cell {:player-id "a" :player-name "A"} :l 3))
        scored  (pr-str (matchup/player-cell {:player-id "a" :player-name "A" :actual 0.0} :l 3))]
    (is (re-find #"mu-a pending" pending))
    (is (not (re-find #"pending" scored)))))
