(ns draft-day.events-test
  "The scoring bugs that actually blanked or froze the board all lived here, and
  none of this was reachable from `lein test` — there was no cljs test runner at
  all. Run with `npx shadow-cljs compile test && node out/node-tests.js`."
  (:require [cljs.test :refer [deftest is testing use-fixtures]]
            [re-frame.core :as rf]
            [re-frame.db :as rdb]
            [re-frame.registrar :as registrar]
            [draft-day.db :as db]
            [draft-day.fx :as fx]
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

;; ---- a dragged column keeps its new slot across a refresh ----

(deftest a-reordered-column-reaches-localstorage
  (let [keys-now #(mapv :key (:columns @rdb/app-db))
        [a b c]  (take 3 (keys-now))]
    (rf/dispatch-sync [:move-column-onto a c])
    (is (= [b c a] (take 3 (keys-now)))
        "the board header hands over keys, so what is hidden cannot skew the move")
    (is (= (:columns @rdb/app-db) (:columns (last (:persist @captured))))
        "and the new order is in the persisted slice — this is the whole
         hard-refresh guarantee, since :boot keeps the stored order")
    (testing "reordering is display-only; it must not re-rank the board"
      (is (empty? (:http @captured))))))

;; ---- the watch list keeps the manager's own order ----

(deftest starring-appends-and-a-drag-reorders
  (let [wl #(:watchlist @rdb/app-db)]
    (doseq [id ["gibbs" "chase" "nua"]]
      (rf/dispatch-sync [:watch-toggle id]))
    (is (= ["gibbs" "chase" "nua"] (wl))
        "a new star goes to the bottom — it never jumps the ones already ranked")

    (rf/dispatch-sync [:watch-toggle "chase"])
    (is (= ["gibbs" "nua"] (wl)) "toggling off removes without disturbing the rest")

    (rf/dispatch-sync [:watch-toggle "chase"])
    (rf/dispatch-sync [:move-watch-onto "chase" "gibbs"])
    (is (= ["chase" "gibbs" "nua"] (wl)) "a drag upwards lands before the target")
    (is (= (wl) (:watchlist (last (:persist @captured))))
        "and the order is in the persisted slice — the hard-refresh guarantee")

    (rf/dispatch-sync [:watch-remove "gibbs"])
    (is (= ["chase" "nua"] (wl)))

    (testing "none of it re-ranks the board: the watch list feeds no valuation"
      (is (empty? (:http @captured))))))

(deftest sorting-the-watch-list-rewrites-the-order-and-leaves-it-alone
  (let [wl #(:watchlist @rdb/app-db)]
    (swap! rdb/app-db assoc :ranked
           {:players [{:player-id "gibbs" :position "RB" :pos-rank 2 :worth 51 :vorp 100.0 :points 240.0}
                      {:player-id "bijan" :position "RB" :pos-rank 1 :worth 58 :vorp 120.0 :points 260.0}
                      {:player-id "lamb"  :position "WR" :pos-rank 1 :worth 55 :vorp 110.0 :points 250.0}]})
    (doseq [id ["gibbs" "lamb" "bijan"]] (rf/dispatch-sync [:watch-toggle id]))
    (reset! captured {:http [] :persist [] :debounce []})

    (rf/dispatch-sync [:watch-sort :rank])
    (is (= ["bijan" "lamb" "gibbs"] (wl)))
    (is (= (wl) (:watchlist (last (:persist @captured))))
        "the new order is persisted — it is the stored one now, not a view")

    (testing "it is one-shot: a later drag is not undone by anything"
      (rf/dispatch-sync [:move-watch-onto "gibbs" "bijan"])
      (is (= ["gibbs" "bijan" "lamb"] (wl))))

    (rf/dispatch-sync [:watch-sort :position])
    (is (= ["bijan" "gibbs" "lamb"] (wl)) "grouped by position, best first inside")

    (testing "sorting re-ranks nothing: the watch list feeds no valuation"
      (is (empty? (:http @captured))))))

;; ---- a config change made before the universe lands is not lost ----

(deftest a-scoring-change-with-no-players-yet-is-picked-up-on-load
  (reset! rdb/app-db (db/default-db))                    ; no :players
  (rf/dispatch-sync [:select-scoring-preset :standard])
  (is (empty? (:http @captured)) "nothing to rank against yet")
  (rf/dispatch-sync [:players-loaded {:players [{:player-id "p1"}] :count 1 :source "sample"}])
  (is (= :standard (scoring-now)) "and the choice survived the wait"))

;; ---- boot loads only what this version wrote ----

(defn- with-fake-storage
  "Run `f` against an empty in-memory localStorage. Node has none, and the
  version gate is the one piece of persistence with a decision in it."
  [f]
  (let [store (atom {})
        prev  (.-localStorage js/globalThis)]
    (set! (.-localStorage js/globalThis)
          #js {:getItem (fn [k] (get @store k nil))
               :setItem (fn [k v] (swap! store assoc k v) nil)})
    (try (f store) (finally (set! (.-localStorage js/globalThis) prev)))))

(deftest saved-state-is-read-back-only-under-the-version-that-wrote-it
  (with-fake-storage
    (fn [store]
      (let [write! #(swap! store assoc fx/store-key (pr-str %))]
        (testing "a blob this version stamped comes back whole"
          (write! {:v fx/storage-version :state {:my-team-id "t3"}})
          (is (= {:my-team-id "t3"} (fx/load-persisted))))

        (testing "a blob from another version is dropped rather than repaired"
          (write! {:v (inc fx/storage-version) :state {:my-team-id "t3"}})
          (is (nil? (fx/load-persisted))))

        (testing "an unstamped blob — every shape written before the stamp — is
                  dropped the same way"
          (write! {:my-team-id "t3" :watchlist #{"gibbs"}})
          (is (nil? (fx/load-persisted))))

        (testing "unreadable junk is nil, not a throw at boot"
          (swap! store assoc fx/store-key "{:v 1 :state")
          (is (nil? (fx/load-persisted))))

        (testing "nothing stored at all"
          (swap! store dissoc fx/store-key)
          (is (nil? (fx/load-persisted))))))))

(deftest boot-opens-at-defaults-when-there-is-nothing-to-load
  (with-redefs [draft-day.fx/load-persisted (fn [] nil)]
    (rf/dispatch-sync [:boot])
    (is (= (:scoring db/default-config) (scoring-now)))
    (is (= (db/default-columns) (:columns @rdb/app-db)))))

(deftest boot-takes-a-loaded-slice-as-it-stands
  ;; It was written by this version, so there is nothing to reconcile: whatever
  ;; the slice holds wins, and every key it does not hold comes from default-db.
  (with-redefs [draft-day.fx/load-persisted
                (fn [] {:my-team-id "t3" :watchlist ["gibbs"]})]
    (rf/dispatch-sync [:boot])
    (is (= "t3" (:my-team-id @rdb/app-db)))
    (is (= ["gibbs"] (:watchlist @rdb/app-db)))
    (is (= (db/default-columns) (:columns @rdb/app-db)) "and the rest is default")))

;; ---- the shape the version stands for ----

(deftest the-persisted-shape-is-pinned-to-the-version-that-reads-it
  ;; Nothing repairs a stored blob any more, so `fx/storage-version` is the only
  ;; thing standing between a changed shape and a manager reading it under the
  ;; old one. Remembering to bump it is exactly the kind of discipline that gets
  ;; forgotten, and both failure modes are silent for existing users only: a new
  ;; column never appears on their board, a removed one renders as a column of
  ;; dashes under a blank header. So the shape is written down here — change any
  ;; of these three and this test fails until the version moves with them.
  (is (= 1 fx/storage-version)
      "the shapes below changed: bump fx/storage-version and update this test")

  (is (= [:rank :ecr :name :team :bye :position :worth :value :market :espn-value
          :fp-aav :bargain :vorp :risk :inj :edge :adp :tier :fp-tier :proj
          :ceiling :floor :prior-tgt :prior-rec :prior-tgt-pct :proj-tgt :proj-rec]
         (mapv :key db/column-catalog))
      "a stored :columns vector is keyed off this list")

  (is (= [:budget-plan :num-teams :roster :scoring :starting-bankroll]
         (vec (sort (keys db/default-config))))
      "a stored :config is this map")

  (is (= [:config :teams :drafted :picks :columns :my-team-id :watchlist]
         db/persist-keys)
      "and this is everything that gets stored at all"))
