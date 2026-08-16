(ns draft-day.events-test
  "The scoring bugs that actually blanked or froze the board all lived here, and
  none of this was reachable from `lein test` — there was no cljs test runner at
  all. Run with `npx shadow-cljs compile test && node out/node-tests.js`."
  (:require [cljs.test :refer [deftest is testing use-fixtures]]
            [re-frame.core :as rf]
            [re-frame.db :as rdb]
            [re-frame.registrar :as registrar]
            [draft-day.db :as db]
            [draft-day.fx]
            [draft-day.scoring :as scoring]
            [draft-day.events]))

(defonce captured (atom {}))

(def ^:private stubs
  "Stand in for the real side effects: node has no fetch target and no
  localStorage, and we want to see what was *requested* rather than wait on it."
  {:http     (fn [r] (swap! captured update :http conj r))
   :persist! (fn [r] (swap! captured update :persist conj r))
   :debounce (fn [r] (swap! captured update :debounce conj r))})

(defonce ^:private real-fx
  ;; Captured at load, before any stub is registered, so the fixture can put the
  ;; real handlers back. Every `-test` namespace compiles into one node bundle,
  ;; so a stub left registered here would silently disarm these effects for
  ;; whatever namespace is added next.
  (into {} (map (juxt identity #(registrar/get-handler :fx %))) (keys stubs)))

(defn- swap-fx!
  "Replace the :fx handlers in `m`, clearing first so re-frame has nothing to
  warn about overwriting."
  [m]
  (doseq [[id f] m]
    (rf/clear-fx id)
    (when f (rf/reg-fx id f))))

(defn- loaded-db
  "app-db as it stands once players have arrived — :recompute is a no-op before
  that, so most of these events need it."
  []
  (assoc (db/default-db)
         :players [{:player-id "p1" :position "RB"}]
         :universe-status "1 players · sample"))

(use-fixtures :each
  {:before (fn []
             (swap-fx! stubs)
             (reset! captured {:http [] :persist [] :debounce []})
             (reset! rdb/app-db (loaded-db)))
   :after  (fn [] (swap-fx! real-fx))})

(defn- scoring-now [] (get-in @rdb/app-db [:config :scoring]))
(defn- last-http [] (last (:http @captured)))

;; ---- a weight you cannot type ----

(deftest a-cleared-weight-box-cannot-reach-the-request
  ;; parseFloat("") is NaN, JSON.stringify writes NaN as null, and the server used
  ;; to throw on (zero? nil) -> 400 -> the board went blank and stayed blank,
  ;; because the bad weight was persisted to localStorage on the way through.
  (rf/dispatch-sync [:enable-custom-scoring])
  (doseq [bad [js/NaN nil "" js/Infinity]]
    (rf/dispatch-sync [:set-scoring-weight :rec bad])
    (is (= 0 (:rec (scoring-now))) (str "weight " (pr-str bad) " survived as itself")))
  (is (every? #(number? (val %)) (scoring-now))
      "no non-number ever lands in the persisted config"))

(deftest a-real-weight-still-goes-through
  (rf/dispatch-sync [:enable-custom-scoring])
  (rf/dispatch-sync [:set-scoring-weight :rec 0.5])
  (is (= 0.5 (:rec (scoring-now))))
  (is (= [{:id :recompute :event [:recompute]}] (:debounce @captured))
      "and asks for a debounced recompute rather than one per keystroke"))

;; ---- custom scoring without a round trip ----

(deftest custom-scoring-is-available-before-any-request-resolves
  ;; It used to seed from an async /api/scoring/presets reply. Picking Custom
  ;; first wrote nil, which the server silently read as PPR while the Settings
  ;; page threw on (name nil).
  (is (empty? (:http @captured)) "nothing has been fetched")
  (rf/dispatch-sync [:enable-custom-scoring])
  (is (map? (scoring-now)))
  (is (= (:ppr scoring/presets) (scoring-now)) "seeded from the preset that was active"))

(deftest enabling-custom-scoring-twice-keeps-your-edits
  (rf/dispatch-sync [:enable-custom-scoring])
  (rf/dispatch-sync [:set-scoring-weight :rec 0.25])
  (rf/dispatch-sync [:enable-custom-scoring])
  (is (= 0.25 (:rec (scoring-now)))))

(deftest switching-to-a-preset-sends-that-preset
  (rf/dispatch-sync [:select-scoring-preset :standard])
  (is (= :standard (scoring-now)))
  ;; A handler's own :fx [[:dispatch …]] goes through the async router, so the
  ;; recompute it queues is driven here to see what the request would carry.
  (rf/dispatch-sync [:recompute])
  (is (= :standard (:scoring (:body (last-http))))))

;; ---- out-of-order responses ----

(deftest only-the-newest-rankings-reply-may-write-the-board
  ;; Each reply re-ranks the whole universe, so latency varies and a slow reply
  ;; computed under the previous scoring config could land last and stick.
  (rf/dispatch-sync [:recompute])
  (rf/dispatch-sync [:recompute])
  (let [n (:recompute-seq @rdb/app-db)]
    (is (= 2 n))
    (rf/dispatch-sync [:ranked-loaded (dec n) {:players [{:player-id "stale"}]}])
    (is (nil? (:ranked @rdb/app-db)) "the superseded reply is dropped")

    (rf/dispatch-sync [:ranked-loaded n {:players [{:player-id "fresh"}]}])
    (is (= "fresh" (-> @rdb/app-db :ranked :players first :player-id)))

    (testing "a reply that arrives even later, from an older request, still loses"
      (rf/dispatch-sync [:ranked-loaded (dec n) {:players [{:player-id "stale"}]}])
      (is (= "fresh" (-> @rdb/app-db :ranked :players first :player-id))))))

(deftest every-recompute-carries-the-scoring-config-as-it-stands
  (rf/dispatch-sync [:select-scoring-preset :half-ppr])
  (rf/dispatch-sync [:recompute])
  (is (= :half-ppr (:scoring (:body (last-http)))))
  (rf/dispatch-sync [:enable-custom-scoring])
  (rf/dispatch-sync [:recompute])
  (is (= (:half-ppr scoring/presets) (:scoring (:body (last-http))))))

;; ---- failure leaves something readable ----

(deftest a-failed-recompute-keeps-the-old-board-and-says-so
  (rf/dispatch-sync [:recompute])
  (rf/dispatch-sync [:ranked-loaded (:recompute-seq @rdb/app-db) {:players [{:player-id "p1"}]}])
  (rf/dispatch-sync [:recompute-failed "boom"])
  (is (= "p1" (-> @rdb/app-db :ranked :players first :player-id))
      "stale but readable beats blank")
  (is (re-find #"failed" (:status @rdb/app-db)))

  (testing "and the next success clears the error"
    (rf/dispatch-sync [:recompute])
    (rf/dispatch-sync [:ranked-loaded (:recompute-seq @rdb/app-db) {:players []}])
    (is (= "1 players · sample" (:status @rdb/app-db)))))

(deftest a-successful-recompute-does-not-stamp-over-someone-elses-status
  (rf/dispatch-sync [:set-status "✓ Imported \"RaiderNation\" (2026)"])
  (rf/dispatch-sync [:recompute])
  (rf/dispatch-sync [:ranked-loaded (:recompute-seq @rdb/app-db) {:players []}])
  (is (= "✓ Imported \"RaiderNation\" (2026)" (:status @rdb/app-db)))

  (testing "not even when it is clearing an earlier failure of its own"
    ;; The real sequence: a recompute fails, the manager imports a league, and
    ;; the import's own recompute lands 250 ms later. The import's message has
    ;; to survive that.
    (rf/dispatch-sync [:recompute-failed "boom"])
    (rf/dispatch-sync [:set-status "✓ Imported \"RaiderNation\" (2026)"])
    (rf/dispatch-sync [:recompute])
    (rf/dispatch-sync [:ranked-loaded (:recompute-seq @rdb/app-db) {:players []}])
    (is (= "✓ Imported \"RaiderNation\" (2026)" (:status @rdb/app-db)))))

;; ---- which way a column opens ----

(deftest a-rank-shaped-column-opens-best-first
  ;; FP T sorted the wrong way round: it is a tier, so 1 is the best number, but
  ;; it opened descending and put tier 16 on top of the board.
  (doseq [k [:name :team :position :rank :adp :ecr :fp-tier]]
    (rf/dispatch-sync [:set-sort k])
    (is (= {:key k :dir 1} (:sort @rdb/app-db)) (str k " opens ascending")))
  (doseq [k [:worth :value :vorp :bargain]]
    (rf/dispatch-sync [:set-sort k])
    (is (= {:key k :dir -1} (:sort @rdb/app-db)) (str k " opens descending")))
  (testing "and clicking the same header again flips it"
    (rf/dispatch-sync [:set-sort :fp-tier])
    (rf/dispatch-sync [:set-sort :fp-tier])
    (is (= {:key :fp-tier :dir -1} (:sort @rdb/app-db)))))

;; ---- a config change made before the universe lands is not lost ----

(deftest a-scoring-change-with-no-players-yet-is-picked-up-on-load
  (reset! rdb/app-db (db/default-db))                    ; no :players
  (rf/dispatch-sync [:select-scoring-preset :standard])
  (is (empty? (:http @captured)) "nothing to rank against yet")
  (rf/dispatch-sync [:players-loaded {:players [{:player-id "p1"}] :count 1 :source "sample"}])
  (is (= :standard (scoring-now)) "and the choice survived the wait"))

;; ---- boot repairs what localStorage may hold ----

(deftest boot-repairs-a-config-from-an-older-shape
  (with-redefs [draft-day.fx/load-persisted (fn [] {:config {:scoring nil :num-tiers 5}})]
    (rf/dispatch-sync [:boot])
    (is (= (:scoring db/default-config) (scoring-now)) "nil scoring cannot reach Settings")
    (is (not (contains? (:config @rdb/app-db) :num-tiers)) "a dropped key does not linger")
    (is (seq (:columns @rdb/app-db)))))

;; ---- tier strategy ----

(deftest choosing-a-tier-technique-persists-without-refetching
  ;; The server computes every strategy at both scales on each recompute, so the
  ;; switcher is a display choice. If this ever fires :http, every tier switch
  ;; mid-draft costs a full re-rank for a number already on the client.
  (rf/dispatch-sync [:set-tier-strategy :ecr])
  (is (= :ecr (get-in @rdb/app-db [:config :tier-strategy])))
  (is (empty? (:http @captured)) "no rankings request")
  (is (empty? (:debounce @captured)) "and none queued either")
  (is (= :ecr (:tier-strategy (:config (last (:persist @captured)))))
      "the choice is written to the persisted config slice"))

(deftest an-unrecognized-tier-technique-is-ignored
  (rf/dispatch-sync [:set-tier-strategy :cliffs])
  (rf/dispatch-sync [:set-tier-strategy :not-a-strategy])
  (is (= :cliffs (get-in @rdb/app-db [:config :tier-strategy]))
      "a stale build's dispatch cannot persist a strategy that tiers nothing"))

(deftest boot-defaults-the-tier-technique-for-a-config-that-predates-it
  (rf/dispatch-sync [:boot {:config {:num-teams 10 :scoring :ppr}}])
  (is (= db/default-tier-strategy
         (get-in @rdb/app-db [:config :tier-strategy]))))
