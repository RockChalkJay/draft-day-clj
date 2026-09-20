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
  (let [n     (:recompute-seq @rdb/app-db)
        rules (db/rules-stamp (:config @rdb/app-db))]
    (is (= 2 n))
    (rf/dispatch-sync [:ranked-loaded (dec n) rules {:players [{:player-id "stale"}]}])
    (is (nil? (:ranked @rdb/app-db)) "the superseded reply is dropped")

    (rf/dispatch-sync [:ranked-loaded n rules {:players [{:player-id "fresh"}]}])
    (is (= "fresh" (-> @rdb/app-db :ranked :players first :player-id)))

    (testing "a reply that arrives even later, from an older request, still loses"
      (rf/dispatch-sync [:ranked-loaded (dec n) rules {:players [{:player-id "stale"}]}])
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
  (rf/dispatch-sync [:ranked-loaded (:recompute-seq @rdb/app-db)
                     (db/rules-stamp (:config @rdb/app-db)) {:players [{:player-id "p1"}]}])
  (rf/dispatch-sync [:recompute-failed "boom"])
  (is (= "p1" (-> @rdb/app-db :ranked :players first :player-id))
      "stale but readable beats blank")
  (is (re-find #"failed" (:status @rdb/app-db)))

  (testing "and the next success clears the error"
    (rf/dispatch-sync [:recompute])
    (rf/dispatch-sync [:ranked-loaded (:recompute-seq @rdb/app-db)
                       (db/rules-stamp (:config @rdb/app-db)) {:players []}])
    (is (= "1 players · sample" (:status @rdb/app-db)))))

(deftest a-successful-recompute-does-not-stamp-over-someone-elses-status
  (rf/dispatch-sync [:set-status "✓ Imported \"RaiderNation\" (2026)"])
  (rf/dispatch-sync [:recompute])
  (rf/dispatch-sync [:ranked-loaded (:recompute-seq @rdb/app-db)
                     (db/rules-stamp (:config @rdb/app-db)) {:players []}])
  (is (= "✓ Imported \"RaiderNation\" (2026)" (:status @rdb/app-db)))

  (testing "not even when it is clearing an earlier failure of its own"
    ;; The real sequence: a recompute fails, the manager imports a league, and
    ;; the import's own recompute lands 250 ms later. The import's message has
    ;; to survive that.
    (rf/dispatch-sync [:recompute-failed "boom"])
    (rf/dispatch-sync [:set-status "✓ Imported \"RaiderNation\" (2026)"])
    (rf/dispatch-sync [:recompute])
    (rf/dispatch-sync [:ranked-loaded (:recompute-seq @rdb/app-db)
                       (db/rules-stamp (:config @rdb/app-db)) {:players []}])
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
    ;; `:ranked-rules` alongside `:ranked`, because that is what `:ranked-loaded`
    ;; always writes and what says this board is priced under the rules in force.
    ;; `:watch-sort` refuses without it — see the stale-read-that-writes guard.
    (swap! rdb/app-db assoc
           :ranked-rules (db/rules-stamp (:config @rdb/app-db))
           :ranked
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
  ;; dashes under a blank header. So the shapes are written down here — change
  ;; any of them and this test fails until the version moves with them.
  ;;
  ;; Both catalogs are pinned, not just the draft board's: `:waiver-columns` is
  ;; persisted the same way, and a Waivers column is exactly the kind of change
  ;; that would otherwise slip past.
  (is (= 10 fx/storage-version)
      "the shapes below changed: bump fx/storage-version and update this test")

  (is (= "espn:{SWID}" (db/account-key "espn" "{SWID}"))
      "a stored :accounts map is keyed off this, and a league entry names it")

  (is (= [:rank :ecr :name :team :bye :position :worth :value :market :espn-value
          :fp-aav :bargain :vorp :risk :inj :edge :adp :tier :fp-tier :proj
          :ceiling :floor :prior-tgt :prior-rec :prior-tgt-pct :proj-tgt :proj-rec]
         (mapv :key db/column-catalog))
      "a stored :columns vector is keyed off this list")

  (is (= [:rank :name :team :position :bye :ros :week :week-rank :opp :upgrade
          :lineup :bid :trend :form :gp :risk :inj :ros-vorp :tgt :car
          :preseason :ecr]
         (mapv :key db/waiver-column-catalog))
      "and a stored :waiver-columns vector off this one")

  (is (= [:budget-plan :num-teams :roster :scoring :starting-bankroll]
         (vec (sort (keys db/default-config))))
      "a stored :config is this map")

  (is (= [:bench :dst :flex :k :qb :rb :te :wr]
         (vec (sort (keys db/default-roster))))
      "including its nested roster, which a new bench slot would change")

  (is (= [:config :teams :drafted :picks :columns :my-team-id :watchlist
          :accounts :leagues :active-league :waiver-columns :phase]
         db/persist-keys)
      "and this is everything that gets stored at all")

  (is (= "sleeper:123" (db/league-key "sleeper" "123"))
      "and a league is stored under provider *and* id — see `db/league-key`"))

;; ---- the draft archive ----

(deftest an-archived-draft-outlives-a-storage-version-bump
  ;; The whole reason it has its own key and its own version. A bump exists to
  ;; stop *live* state being read under a shape it was not written for, and
  ;; dropping the blob is the right answer for a column layout. A completed
  ;; draft is a record of something that happened; discarding it because a
  ;; Waivers column moved would be absurd.
  (with-fake-storage
    (fn [store]
      (swap! store assoc fx/drafts-key
             (pr-str {:v fx/drafts-version
                      :drafts [{:archived-at "2026-09-06" :picks [{:player-id "a"}]}]}))
      ;; the live slice, written under a version that is about to move
      (swap! store assoc fx/store-key
             (pr-str {:v (inc fx/storage-version) :state {:my-team-id "t3"}}))
      (is (nil? (fx/load-persisted)) "live state is dropped, as designed")
      (is (= 1 (count (fx/read-drafts))) "and the archive is untouched by it"))))

(deftest an-unreadable-or-mis-stamped-archive-reads-as-empty-not-as-a-crash
  (with-fake-storage
    (fn [store]
      (is (= [] (fx/read-drafts)) "nothing stored at all")
      (swap! store assoc fx/drafts-key "{:v 1 :drafts [")
      (is (= [] (fx/read-drafts)) "unreadable")
      (swap! store assoc fx/drafts-key
             (pr-str {:v (inc fx/drafts-version) :drafts [{:picks [1]}]}))
      (is (= [] (fx/read-drafts)) "written under a different archive version"))))

(deftest archiving-appends-rather-than-replaces
  ;; A manager drafts in more than one league and more than one season.
  (with-fake-storage
    (fn [_]
      (rf/dispatch-sync [:archive-draft])          ; nothing drafted yet
      (is (= [] (fx/read-drafts)) "an empty shell is worse than no entry")
      (swap! rdb/app-db assoc :picks [{:player-id "a" :price 5}]
             :teams [{:team-id "t0"}] :my-team-id "t0")
      (rf/dispatch-sync [:archive-draft])
      (swap! rdb/app-db assoc :picks [{:player-id "b" :price 9}])
      (rf/dispatch-sync [:archive-draft])
      (let [ds (fx/read-drafts)]
        (is (= 2 (count ds)))
        (is (= ["a" "b"] (mapv #(-> % :picks first :player-id) ds))
            "oldest first"))
      (testing "pressing it again on an unchanged draft adds nothing"
        (rf/dispatch-sync [:archive-draft])
        (is (= 2 (count (fx/read-drafts)))
            "two entries differing only in timestamp read as two drafts"))
      (testing "but a draft that has moved on is a real second checkpoint"
        (swap! rdb/app-db update :picks conj {:player-id "c" :price 2})
        (rf/dispatch-sync [:archive-draft])
        (is (= 3 (count (fx/read-drafts))))))))

(deftest starting-a-draft-archives-the-one-it-destroys
  ;; The only moment a completed draft is thrown away, and it happens by pressing
  ;; a button labelled Start Draft — so the record is taken here rather than left
  ;; to one somebody has to remember to press.
  (with-fake-storage
    (fn [_]
      (swap! rdb/app-db assoc
             :picks [{:player-id "gibbs" :price 43}]
             :teams [{:team-id "t0" :name "crazy rich asians"}]
             :my-team-id "t0"
             :leagues {"sleeper:1" {:name "RaiderNation" :season "2026"}}
             :active-league "sleeper:1")
      (rf/dispatch-sync [:start-draft {:num-teams 12 :starting-bankroll 200 :team-names []}])
      (let [[d] (fx/read-drafts)]
        (is (= "RaiderNation" (:league d)) "the league synced at the time, as context")
        (is (= "2026" (:season d)))
        (is (= ["gibbs"] (mapv :player-id (:picks d))))
        (is (= 200 (get-in d [:config :starting-bankroll]))
            "the config it was drafted under — $43 means nothing without it"))
      (is (empty? (:picks @rdb/app-db)) "and the live board is reset, as before"))))

(deftest a-fresh-start-draft-archives-nothing
  (with-fake-storage
    (fn [_]
      (rf/dispatch-sync [:start-draft {:num-teams 10 :starting-bankroll 100 :team-names []}])
      (is (= [] (fx/read-drafts))))))

;; ---- one Escape, one effect ----
;; The tile and the modal both close on Escape and both used to want their own
;; document listener. Two listeners are order-dependent: whichever ran second
;; would see `:modal` already nil and clear `:compare` anyway, so shutting the
;; modal would silently take away the pair underneath it. The precedence is a
;; rule in one event instead, which is what makes it assertable at all.

(deftest escape-closes-the-modal-and-leaves-the-comparison-alone
  (reset! rdb/app-db (assoc (loaded-db)
                            :view :waivers
                            :modal {:kind :player-detail :player-id "p1"}
                            :compare ["p1" "p2"]))
  (rf/dispatch-sync [:escape-pressed])
  (is (nil? (:modal @rdb/app-db)))
  (is (= ["p1" "p2"] (:compare @rdb/app-db))
      "the pair he was holding survives the modal closing over it"))

(deftest escape-clears-the-comparison-once-no-modal-is-open
  (reset! rdb/app-db (assoc (loaded-db) :view :waivers :compare ["p1" "p2"]))
  (rf/dispatch-sync [:escape-pressed])
  (is (= [] (:compare @rdb/app-db))))

(deftest escape-on-another-tab-leaves-the-comparison-alone
  ;; The listener lives in `core/app` and is attached for the life of the app,
  ;; where it used to unmount with the tile. Without the view guard, holding a
  ;; pair and pressing Escape anywhere else would lose it with nothing on screen
  ;; acknowledging that anything happened.
  (doseq [v [:board :league :settings]]
    (reset! rdb/app-db (assoc (loaded-db) :view v :compare ["p1" "p2"]))
    (rf/dispatch-sync [:escape-pressed])
    (is (= ["p1" "p2"] (:compare @rdb/app-db))
        (str "Escape on " v " cleared a comparison held on the waivers tab"))))

(deftest a-modal-closes-from-any-tab
  ;; The modal is not view-scoped the way the tile is: it opens over whatever is
  ;; on screen, so Escape has to reach it from anywhere.
  (reset! rdb/app-db (assoc (loaded-db) :view :board
                            :modal {:kind :player-detail :player-id "p1"}))
  (rf/dispatch-sync [:escape-pressed])
  (is (nil? (:modal @rdb/app-db))))

(deftest escape-with-nothing-open-changes-nothing
  (let [before (loaded-db)]
    (reset! rdb/app-db before)
    (rf/dispatch-sync [:escape-pressed])
    (is (= before @rdb/app-db))))

;; ---- a modal that carries an argument ----

(deftest a-modal-is-named-the-same-way-whether-or-not-it-carries-an-argument
  ;; `:modal` held a bare keyword until one of them needed a player id. Both
  ;; shapes are read through `db/modal-kind`, so the two argument-less modals
  ;; keep working and the next one to need an argument costs a map rather than a
  ;; fourth spelling of the comparison.
  (is (= :start-draft (db/modal-kind {:kind :start-draft})))
  (is (= :player-detail (db/modal-kind {:kind :player-detail :player-id "p1"})))
  (is (= :reset-cache (db/modal-kind :reset-cache)) "the bare keyword still reads")
  (is (nil? (db/modal-kind nil)) "nothing open"))

(deftest showing-a-player-detail-modal-carries-the-id
  (rf/dispatch-sync [:show-modal {:kind :player-detail :player-id "p1"}])
  (is (= {:kind :player-detail :player-id "p1"} (:modal @rdb/app-db)))
  (rf/dispatch-sync [:close-modal])
  (is (nil? (:modal @rdb/app-db))))

(deftest a-connected-league-s-scoring-cannot-be-edited
  ;; The Scoring section draws it read-only; the events hold the same line, so
  ;; nothing can write a league's rules behind the import's back.
  (swap! rdb/app-db assoc :active-league "sleeper:1")
  (let [before (get-in @rdb/app-db [:config :scoring])]
    (rf/dispatch-sync [:enable-custom-scoring])
    (rf/dispatch-sync [:set-scoring-weight :rec 0.25])
    (rf/dispatch-sync [:select-scoring-preset :standard])
    (is (= before (get-in @rdb/app-db [:config :scoring])))))

(deftest a-connected-league-s-shape-cannot-be-edited-either
  ;; Re-sync re-imports team count and roster too, so an edit to either was
  ;; reverted on the next press without a word.
  (swap! rdb/app-db assoc
         :leagues {"sleeper:1" {:provider "sleeper" :league-id "1"
                                :rules {:status :imported :bankroll? false}}}
         :active-league "sleeper:1")
  (let [before (:config @rdb/app-db)]
    (rf/dispatch-sync [:edit-config {:num-teams 8 :roster (assoc (:roster before) :bench 9)}])
    (is (= (select-keys before [:num-teams :roster])
           (select-keys (:config @rdb/app-db) [:num-teams :roster])))
    (is (empty? (:debounce @captured)) "and nothing left to apply re-ranks nothing"))
  (testing "a budget the import did not bring stays the manager's"
    (rf/dispatch-sync [:edit-config {:starting-bankroll 300}])
    (is (= 300 (get-in @rdb/app-db [:config :starting-bankroll]))))
  (testing "one it did bring does not"
    (swap! rdb/app-db assoc-in [:leagues "sleeper:1" :rules :bankroll?] true)
    (rf/dispatch-sync [:edit-config {:starting-bankroll 150}])
    (is (= 300 (get-in @rdb/app-db [:config :starting-bankroll])))))

(deftest with-no-league-every-setting-is-the-manager-s
  (rf/dispatch-sync [:edit-config {:num-teams 10 :starting-bankroll 150}])
  (is (= 10 (get-in @rdb/app-db [:config :num-teams])))
  (is (= 150 (get-in @rdb/app-db [:config :starting-bankroll]))))

(deftest start-draft-is-not-a-way-round-the-league-s-settings
  ;; The modal draws them read-only; the event holds the same line.
  (with-fake-storage
    (fn [_]
      (swap! rdb/app-db assoc
             :leagues {"sleeper:1" {:provider "sleeper" :league-id "1"
                                    :rules {:status :imported :bankroll? true}}}
             :active-league "sleeper:1")
      (swap! rdb/app-db update :config assoc :num-teams 10 :starting-bankroll 300)
      (rf/dispatch-sync [:start-draft {:num-teams 14 :starting-bankroll 100 :team-names []}])
      (is (= 10 (get-in @rdb/app-db [:config :num-teams])))
      (is (= 300 (get-in @rdb/app-db [:config :starting-bankroll])))
      (is (= 10 (count (:teams @rdb/app-db))) "and the teams are the league's"))))

;; ---- the two halves ----

(defn- dispatched
  "The events the handler under `f` dispatched, captured rather than queued — a
  real `:dispatch` runs on a later tick, after the assertion."
  [f]
  (let [seen (atom [])
        real (registrar/get-handler :fx :dispatch)]
    (swap-fx! {:dispatch #(swap! seen conj %)})
    (try (f) (finally (swap-fx! {:dispatch real})))
    @seen))

(defn- dispatched-views
  "The `:set-view`s among them."
  [f]
  (keep (fn [[e v]] (when (= e :set-view) v)) (dispatched f)))

(defn- universe-at [week]
  [:players-loaded {:players [] :count 0 :source "x" :universe {:through-week week}}])

(def ^:private drafted-league
  {:active-league "sleeper:1"
   :leagues {"sleeper:1" {:provider "sleeper" :league-id "1"
                          :sync {:teams [] :drafted? true}}}})

(deftest the-app-opens-on-the-half-the-season-is-in
  (is (= [:team]
         (dispatched-views
          #(rf/dispatch-sync [:players-loaded {:players [] :count 0 :source "x"
                                               :universe {:through-week 3}}])))
      "week 3 has been played: open in season"))

(deftest the-app-opens-on-the-board-before-the-season
  (is (= [:board] (dispatched-views #(rf/dispatch-sync (universe-at 0))))))

(deftest the-app-draws-neither-half-until-it-knows-which-it-is-in
  ;; Placing the board first meant a mid-season manager saw the draft half, and
  ;; Start Draft, for as long as the universe took.
  (with-fake-storage
    (fn [_]
      (is (empty? (dispatched-views #(rf/dispatch-sync [:boot]))))
      (is (nil? (:view @rdb/app-db))))))

(deftest a-league-drafted-on-its-host-opens-in-season-at-boot
  ;; Its sync already says so, so there is no week to wait for — and week 1's
  ;; Sunday, before any stats exist, is exactly when it is wanted.
  (with-fake-storage
    (fn [store]
      (swap! store assoc fx/store-key
             (pr-str {:v fx/storage-version :state drafted-league}))
      (is (= [:team] (dispatched-views #(rf/dispatch-sync [:boot])))))))

(deftest a-universe-reload-leaves-the-view-where-the-manager-put-it
  ;; The board stays up after the last pick so it can be undone; a cache reset
  ;; refetching the universe must not carry him off it.
  (swap! rdb/app-db assoc :view :board)
  (is (empty? (dispatched-views #(rf/dispatch-sync (universe-at 3))))))

(deftest a-failed-load-still-places-the-view
  (is (= [:board] (dispatched-views #(rf/dispatch-sync [:load-failed "down"])))
      "nothing says season, so draft day — where the failure is read")
  (swap! rdb/app-db merge drafted-league)
  (is (= [:team] (dispatched-views #(rf/dispatch-sync [:load-failed "down"])))
      "while a league that has drafted is still in season"))

(deftest an-espn-league-s-season-opens-on-my-team
  ;; There is no ESPN matchup board; opening on one was a 400 every boot.
  (swap! rdb/app-db assoc :active-league "espn:1"
         :leagues {"espn:1" {:provider "espn" :league-id "1"}})
  (is (= [:team] (dispatched-views #(rf/dispatch-sync (universe-at 3))))))

(deftest switching-mode-before-the-week-is-known-stores-no-override
  ;; There is nothing yet for the choice to disagree with; storing one here
  ;; outlived the load that would have agreed with it.
  (swap! rdb/app-db assoc :active-league "sleeper:1" :view :settings
         :leagues {"sleeper:1" {:provider "sleeper" :league-id "1"}})
  (is (= [:team] (dispatched-views #(rf/dispatch-sync [:switch-mode :season]))))
  (is (nil? (get-in @rdb/app-db [:leagues "sleeper:1" :phase]))))

(deftest the-matchup-waits-for-the-league-s-rosters
  ;; Asked with none, every team came back empty: a scoreboard of blank lineups.
  (swap! rdb/app-db assoc :active-league "sleeper:1"
         :leagues {"sleeper:1" {:provider "sleeper" :league-id "1"}})
  (rf/dispatch-sync [:fetch-matchup])
  (is (empty? (:http @captured)))
  (is (re-find #"Re-sync" (:matchup-status @rdb/app-db)))
  (testing "opening the tab syncs first; the sync's reply asks for the matchup"
    (is (= [[:sync-league {:provider "sleeper" :league-id "1"}]]
           (dispatched #(rf/dispatch-sync [:set-view :matchup]))))))

(deftest a-host-with-no-matchup-board-is-not-asked-for-one
  (swap! rdb/app-db assoc :active-league "espn:1"
         :leagues {"espn:1" {:provider "espn" :league-id "1" :sync {:teams []}}})
  (rf/dispatch-sync [:fetch-matchup])
  (is (empty? (:http @captured)))
  (is (re-find #"ESPN" (:matchup-status @rdb/app-db))))

(deftest switching-mode-lands-on-that-mode-and-stores-only-a-disagreement
  (swap! rdb/app-db assoc :active-league "sleeper:1" :view :settings
         :leagues {"sleeper:1" {:provider "sleeper" :league-id "1"}}
         :universe {:through-week 0})
  (is (= [:team] (dispatched-views #(rf/dispatch-sync [:switch-mode :season])))
      "it leaves Settings for the season's first tab")
  (is (= :season (get-in @rdb/app-db [:leagues "sleeper:1" :phase]))
      "preseason by the data, so choosing Season is an override")
  (swap! rdb/app-db assoc :view :matchup)
  (is (= [:board] (dispatched-views #(rf/dispatch-sync [:switch-mode :draft]))))
  (is (nil? (get-in @rdb/app-db [:leagues "sleeper:1" :phase]))
      "choosing what the data says goes back to automatic"))

(deftest a-league-switch-leaves-a-tab-the-new-league-s-phase-does-not-have
  (swap! rdb/app-db assoc
         :view :board
         :active-league "sleeper:1"
         :leagues {"sleeper:1" {:provider "sleeper" :league-id "1"}
                   "sleeper:2" {:provider "sleeper" :league-id "2" :phase :season
                                :sync {:teams []}}})
  (dispatched-views #(rf/dispatch-sync [:set-active-league "sleeper:2"]))
  (is (= :team (:view @rdb/app-db))))

(deftest opening-my-team-loads-its-roster-and-the-week
  (let [seen (atom [])
        real (registrar/get-handler :fx :dispatch)]
    (swap-fx! {:dispatch #(swap! seen conj %)})
    (try (rf/dispatch-sync [:set-view :team])
         (finally (swap-fx! {:dispatch real})))
    (is (= #{:fetch-waivers :fetch-matchup} (set (map first @seen))))))

(deftest opening-the-league-tab-first-loads-what-it-reads
  ;; Reached from Settings after a phase change, nothing else had loaded them:
  ;; the manager's own players drew as plain text and the header had no week.
  (swap! rdb/app-db assoc :active-league "sleeper:1"
         :leagues {"sleeper:1" {:provider "sleeper" :league-id "1" :sync {:teams []}}})
  (is (= #{:fetch-waivers :fetch-matchup}
         (set (map first (dispatched #(rf/dispatch-sync [:set-view :rosters])))))))

(deftest a-league-switch-on-any-season-tab-refetches-the-week
  ;; The switch drops the matchup, and the header's week is only ever its.
  (swap! rdb/app-db assoc :view :rosters :active-league "sleeper:1"
         :leagues {"sleeper:1" {:provider "sleeper" :league-id "1" :phase :season
                                :sync {:teams []}}
                   "sleeper:2" {:provider "sleeper" :league-id "2" :phase :season
                                :sync {:teams []}}})
  (is (some #{:fetch-matchup}
            (map first (dispatched #(rf/dispatch-sync [:set-active-league "sleeper:2"]))))))
