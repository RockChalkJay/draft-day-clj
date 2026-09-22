(ns draft-day.views.settings-test
  (:require [cljs.test :refer [deftest is testing use-fixtures]]
            [clojure.walk]
            [re-frame.core :as rf]
            [re-frame.db :as rdb]
            [draft-day.db :as db]
            [draft-day.events]
            [draft-day.subs]
            [draft-day.test-render :as render :refer [render press!]]
            [draft-day.views.settings :as settings]))

(use-fixtures :each
  {:before (fn []
             (rf/clear-subscription-cache!)
             (reset! rdb/app-db (db/default-db)))
   :after  (fn []
             (rf/clear-subscription-cache!)
             (reset! rdb/app-db (db/default-db)))})

(def ^:private sleeper-ak (db/account-key "sleeper" "u1"))
(def ^:private espn-ak (db/account-key "espn" "{S}"))

(defn- accounts! [& kvs]
  (swap! rdb/app-db assoc :accounts (apply hash-map kvs))
  (rf/clear-subscription-cache!))

(def ^:private sleeper-acct
  {:provider "sleeper" :user-id "u1" :username "rockchalkjay"
   :credentials {:username "rockchalkjay"}})

(def ^:private espn-acct
  {:provider "espn" :user-id "{S}" :username "Jason H"
   :credentials {:swid "{S}" :espn-s2 "SUPERSECRETsentinel"}})

(defn- card [] (render settings/connected-accounts))

(deftest the-connect-form-is-drawn-from-the-catalog-not-written-out
  ;; The whole point of the catalog: a second host costs an entry there and two
  ;; defmethods, not a second form. If this starts naming providers, it has
  ;; stopped being data.
  (let [html (card)]
    (is (re-find #"Sleeper" html))
    (is (re-find #"ESPN" html))
    (is (re-find #"Sleeper username" html) "the first host's field, by its catalog label")))

(deftest a-secret-field-is-masked-and-never-rendered
  (accounts! espn-ak espn-acct)
  (let [html (card)]
    (is (not (re-find #"SUPERSECRETsentinel" html))
        "a stored session cookie must not reach the markup, masked or otherwise")))

(deftest the-form-folds-away-once-there-is-an-account
  ;; Connecting is rare and this card is read far more often than it is used.
  (is (re-find #"Connect" (card)) "open when there is nothing connected")
  (accounts! sleeper-ak sleeper-acct)
  (let [html (card)]
    (is (re-find #"Add account" html))
    (is (not (re-find #"Sleeper username" html)))))

(deftest each-account-lists-only-its-own-leagues
  ;; The bug the two flat lists invited: a league read through one account's
  ;; credentials shown under another's name can be refreshed by neither.
  (accounts! sleeper-ak sleeper-acct espn-ak espn-acct)
  (swap! rdb/app-db assoc
         :leagues {"sleeper:1" {:provider "sleeper" :league-id "1" :name "Dynasty Dorks"
                                :account-key sleeper-ak}
                   "espn:9"    {:provider "espn" :league-id "9" :name "The Big Show"
                                :account-key espn-ak}})
  (rf/clear-subscription-cache!)
  (let [html (card)]
    (is (re-find #"rockchalkjay" html))
    (is (re-find #"Jason H" html))
    (is (re-find #"Dynasty Dorks" html))
    (is (re-find #"The Big Show" html))))

(deftest a-league-added-with-no-account-is-listed-rather-than-hidden
  (swap! rdb/app-db assoc
         :leagues {"sleeper:9" {:provider "sleeper" :league-id "9" :name "Orphan"}})
  (rf/clear-subscription-cache!)
  (is (re-find #"No account" (card)))
  (is (re-find #"Orphan" (card))))

(deftest an-expired-session-says-so-and-offers-the-way-back
  (accounts! espn-ak (assoc espn-acct :credentials-stale? true))
  (let [html (card)]
    (is (re-find #"Session expired" html))
    (is (re-find #"Reconnect" html))))

(deftest each-disconnect-names-its-own-account
  ;; One handler per group, each carrying its own key. A single shared one — or
  ;; one that read "the connected account" — would disconnect the wrong host
  ;; the moment there were two.
  (accounts! sleeper-ak sleeper-acct espn-ak espn-acct)
  (let [fire  (fn [f] (let [seen (atom [])]
                        (with-redefs [rf/dispatch #(swap! seen conj %)] (f))
                        (first @seen)))
        fired (->> (render/buttons settings/connected-accounts)
                   (filter (fn [[t _]] (= "Disconnect" t)))
                   (map (fn [[_ f]] (fire f))))]
    (is (= #{[:disconnect-account sleeper-ak] [:disconnect-account espn-ak]}
           (set fired)))))

(deftest a-listing-that-broke-is-not-an-account-that-plays-in-nothing
  (accounts! espn-ak espn-acct)
  (swap! rdb/app-db assoc :league-choices-error {espn-ak "ESPN would not list them"})
  (rf/clear-subscription-cache!)
  (is (re-find #"Couldn't list" (card)))
  (testing "and an account that really plays in none says that instead"
    (swap! rdb/app-db assoc :league-choices-error {} :league-choices {espn-ak []})
    (rf/clear-subscription-cache!)
    (is (re-find #"no leagues this season" (card)))))

(deftest a-discovered-league-can-be-added-under-its-own-account
  (accounts! espn-ak espn-acct)
  (swap! rdb/app-db assoc
         :league-choices {espn-ak [{:league-id "9" :name "The Big Show" :num-teams 12}]})
  (rf/clear-subscription-cache!)
  (is (re-find #"The Big Show" (card)))
  (is (= [[:league-choose espn-ak {:league-id "9" :name "The Big Show" :num-teams 12}]]
         (press! settings/connected-accounts "The Big Show"))
      "with the account key that authorizes reading it, not the first one connected"))

(deftest a-league-already-stored-drops-out-of-the-discovered-list
  ;; It gains its controls in place rather than appearing twice under different
  ;; affordances, which is what the two disconnected lists used to do.
  (accounts! sleeper-ak sleeper-acct)
  (swap! rdb/app-db assoc
         :league-choices {sleeper-ak [{:league-id "1" :name "Dynasty Dorks"}]}
         :leagues {"sleeper:1" {:provider "sleeper" :league-id "1" :name "Dynasty Dorks"
                                :account-key sleeper-ak}})
  (rf/clear-subscription-cache!)
  (is (= 1 (count (re-seq #"Dynasty Dorks" (card))))))

(deftest an-added-league-leaves-the-discovered-list-rather-than-doubling
  (is (= [{:league-id "2"}]
         (settings/unadded-choices "sleeper" [{:league-id "1"} {:league-id "2"}]
                                   [["sleeper:1" {}]])))
  (is (= [{:league-id "1"}]
         (settings/unadded-choices "espn" [{:league-id "1"}] [["sleeper:1" {}]]))
      "keyed by provider too, or an ESPN league vanishes behind a Sleeper one
       that happens to share its id"))

;; ---- sections ----

(defn- sections
  "Each Settings section as `[shown? markup]`, in sidebar order."
  []
  (->> (render/hiccup settings/settings)
       (tree-seq coll? seq)
       (filter #(and (vector? %) (= :div.settings-section (first %))))
       (map (fn [[_ attrs & body]] [(not (:hidden attrs)) (pr-str body)]))))

(defn- shown [] (some (fn [[shown? html]] (when shown? html)) (sections)))

(deftest settings-shows-one-section-at-a-time
  ;; The whole fix for the stretching: cards that are not in the open section
  ;; are `display: none`, so not on the page to be stretched against.
  (is (= 1 (count (filter first (sections)))))
  (let [html (shown)]
    (is (re-find #"Leagues & Accounts" html) "opens on accounts")
    (is (re-find #"Add account|Connect" html))
    (is (not (re-find #"Danger Zone" html)))
    (is (not (re-find #"Budget Plan" html))))
  (rf/dispatch-sync [:set-settings-section :draft])
  (rf/clear-subscription-cache!)
  (let [html (shown)]
    (is (re-find #"Budget Plan" html))
    (is (re-find #"Draft Archive" html))
    (is (not (re-find #"Add account" html)))))

(deftest a-section-you-leave-keeps-what-you-typed
  ;; Hidden, never unmounted: an unmounted connect form loses its local atoms,
  ;; and with them a half-pasted ESPN cookie, when the manager glances at
  ;; another section.
  (rf/dispatch-sync [:set-settings-section :scoring])
  (rf/clear-subscription-cache!)
  (is (= (count db/settings-sections) (count (sections)))
      "every section is still in the tree")
  (is (re-find #"Add account|Connect" (pr-str (map second (sections))))
      "including the one holding the connect form"))

(deftest every-section-is-reachable-from-the-sidebar
  (is (= (set (map (fn [[k _]] [:set-settings-section k]) db/settings-sections))
         (set (mapcat (fn [[_ label]] (press! settings/settings-nav label))
                      db/settings-sections)))))

(deftest the-sidebar-says-what-is-waiting-inside-a-section
  (is (not (re-find #"nav-badge|nav-dot" (render settings/settings-nav)))
      "nothing to report, nothing drawn")
  (accounts! espn-ak (assoc espn-acct :credentials-stale? true))
  (swap! rdb/app-db assoc
         :active-league "sleeper:1"
         :leagues {"sleeper:1" {:rules {:status :imported :unsupported ["fg_50p" "pts_allow_0"]}}})
  (rf/clear-subscription-cache!)
  (let [html (render settings/settings-nav)]
    (is (re-find #"nav-dot" html) "an expired session marks Leagues & Accounts")
    (is (re-find #"nav-badge.*\b2\b" html) "and the count of dropped rules marks Scoring")))

(deftest unapplied-rules-are-listed-one-by-one
  ;; A comma-joined run of underscore keys has nowhere to wrap and ran straight
  ;; out of the card.
  (swap! rdb/app-db assoc
         :active-league "sleeper:1"
         :leagues {"sleeper:1" {:rules {:status :imported :unsupported ["fg_50p" "pts_allow_0"]}}})
  (rf/clear-subscription-cache!)
  (let [html (render settings/import-warning)]
    (is (= 2 (count (re-seq #":span\.rule-chip\b" html))))))

(deftest a-rule-nobody-projects-reads-differently-from-one-nobody-models
  ;; Collapsing the two is what made the old warning confusing: a rule with no
  ;; key scores nothing anywhere, while a rule the model holds but no projection
  ;; carries still scores the weeks that have happened.
  (swap! rdb/app-db assoc
         :active-league "sleeper:1"
         :config {:scoring {:rec 1.0 :pts_allow_0 10.0 :yds_allow_550p -6.0}}
         :leagues {"sleeper:1" {:rules {:status :imported :unsupported []}}})
  (rf/clear-subscription-cache!)
  (let [html (render settings/import-warning)]
    (is (re-find #"2 rules have no projection" html))
    (is (re-find #"pts_allow_0" html))
    (is (re-find #"yds_allow_550p" html))
    (is (not (re-find #"not modelled" html))
        "nothing was dropped, so nothing claims to have been"))
  (testing "a rule the league scores and Sleeper projects says nothing at all"
    (swap! rdb/app-db assoc :config {:scoring {:rec 1.0 :pts_allow_14_20 1.0}})
    (rf/clear-subscription-cache!)
    (is (nil? (settings/import-warning)))))

(deftest connect-a-league-opens-the-accounts-section
  (swap! rdb/app-db assoc :settings-section :data)
  (rf/dispatch-sync [:set-view :settings :leagues])
  (is (= :settings (:view @rdb/app-db)))
  (is (= :leagues (:settings-section @rdb/app-db)))
  (testing "a plain view change leaves the section alone"
    (swap! rdb/app-db assoc :settings-section :scoring)
    (rf/dispatch-sync [:set-view :settings])
    (is (= :scoring (:settings-section @rdb/app-db)))))

(deftest an-import-report-speaks-only-for-its-own-league
  ;; A badge that kept League A's dropped rules on screen under League B would
  ;; be a claim about B.
  ;; Both halves, and they are read from different places: the dropped rules
  ;; off the league's `:rules` and the unprojected ones off its own `:config`.
  ;; A report is a claim about one league, so neither half may outlive it.
  (swap! rdb/app-db assoc
         :active-league "sleeper:1"
         :leagues {"sleeper:1" {:rules {:status :imported :unsupported ["bonus_rec_te"]}
                                :config {:scoring {:rec 1.0 :pts_allow_0 10.0}}}
                   "espn:9"    {:rules {:status :imported :unsupported []}
                                :config {:scoring {:rec 1.0}}}})
  (rf/clear-subscription-cache!)
  (is (re-find #"nav-badge" (render settings/settings-nav)))
  (let [html (render settings/import-warning)]
    (is (re-find #"bonus_rec_te" html))
    (is (re-find #"pts_allow_0" html) "and the half read off the config"))
  (swap! rdb/app-db assoc :active-league "espn:9")
  (rf/clear-subscription-cache!)
  (is (not (re-find #"nav-badge" (render settings/settings-nav))))
  (is (nil? (settings/import-warning))
      "neither half survives the switch"))

(deftest a-connected-league-s-scoring-is-shown-not-edited
  ;; Its rules are its import's: an edit would be lost to the next Re-sync, and
  ;; allowing one is what kept Re-sync from refreshing them.
  (swap! rdb/app-db assoc
         :active-league "sleeper:1"
         :leagues {"sleeper:1" {:provider "sleeper" :league-id "1" :name "Dynasty"
                                :rules {:status :imported :unsupported []}}})
  (swap! rdb/app-db assoc-in [:config :scoring] {:rec 0.5 :pass_td 6})
  (rf/clear-subscription-cache!)
  (let [html (render settings/section-body :scoring)]
    (is (re-find #"Dynasty" html) "it says whose rules these are")
    (is (not (re-find #"Preset" html)) "no preset to pick")
    (is (not (re-find #":on-change" html)) "and no field that writes back")
    (is (not (re-find #"scoring-warning" html)) "a clean import warns about nothing")))

(deftest a-failed-import-says-the-board-is-on-the-old-rules
  (swap! rdb/app-db assoc
         :active-league "sleeper:1"
         :leagues {"sleeper:1" {:provider "sleeper" :league-id "1"
                                :rules {:status :failed :error "league not found"}}})
  (rf/clear-subscription-cache!)
  (let [html (render settings/section-body :scoring)]
    (is (re-find #"league not found" html))
    (is (re-find #"previous settings, which may be another league's" html)))
  (is (= [[:import-league {:provider "sleeper" :league-id "1"}]]
         (press! settings/section-body "Retry import" :scoring))))

(deftest with-no-league-the-scoring-is-the-manager-s-to-set
  (rf/clear-subscription-cache!)
  (let [html (render settings/section-body :scoring)]
    (is (re-find #"Preset" html))
    (is (not (re-find #"previous settings" html)))))

(defn- league! [rules & {:as entry}]
  (swap! rdb/app-db assoc
         :active-league "sleeper:1"
         :leagues {"sleeper:1" (merge {:provider "sleeper" :league-id "1" :name "Dynasty"
                                       :season "2026" :rules rules}
                                      entry)})
  (rf/clear-subscription-cache!))

(deftest a-connected-league-s-roster-and-teams-are-shown-not-edited
  ;; Re-sync re-imports them, so an edit here was reverted on the next press.
  (league! {:status :imported :unsupported [] :bankroll? true})
  (swap! rdb/app-db update :config assoc :num-teams 10 :starting-bankroll 300)
  (let [html (render settings/section-body :roster)]
    (is (re-find #"From .*Dynasty.* \(2026\)" html) "it says whose settings these are")
    (is (re-find #":value \"10\"" html) "the league's own team count")
    (is (re-find #":value \"300\"" html) "and its budget")
    (is (not (re-find #":on-change" html)) "and no field that writes back")))

(deftest a-league-with-no-auction-budget-leaves-the-budget-to-the-manager
  ;; A snake league publishes none, and a locked field there could never be set.
  (league! {:status :imported :unsupported [] :bankroll? false})
  (let [html (render settings/section-body :roster)]
    (is (re-find #"didn't include an auction budget" html))
    (is (= 1 (count (re-seq #":on-change" html))) "only the budget is editable")))

(deftest an-import-in-flight-is-not-a-failure
  ;; A red "not imported yet" with a Retry, on every league for the second
  ;; before its first import answers, sent a duplicate import when pressed.
  (league! nil)
  (swap! rdb/app-db assoc :importing #{"sleeper:1"})
  (rf/clear-subscription-cache!)
  (let [html (render settings/section-body :scoring)]
    (is (re-find #"Importing" html))
    (is (not (re-find #"Retry" html)))
    (is (not (re-find #"scoring-warning" html))))
  (is (not (re-find #"nav-dot" (render settings/settings-nav)))
      "and the sidebar does not flag it either"))

(deftest a-failed-retry-still-lists-what-the-last-import-could-not-apply
  (league! {:status :failed :error "down" :unsupported ["fg_50p"] :bankroll? true})
  (let [html (render settings/section-body :scoring)]
    (is (re-find #"last imported settings" html) "the board is on this league's older settings")
    (is (re-find #"rule-chip" html) "whose gaps are still listed")))

(deftest a-league-whose-settings-never-arrived-is-flagged-everywhere
  (league! {:status :failed :error "down"})
  (let [nav (render settings/settings-nav)]
    (is (= 2 (count (re-seq #"nav-dot" nav))) "on Scoring and on Roster & League"))
  (is (re-find #"settings not imported" (card)) "and on the league's own row"))

(deftest each-league-row-says-and-sets-its-own-phase
  (accounts! sleeper-ak sleeper-acct)
  (swap! rdb/app-db assoc
         :leagues {"sleeper:1" {:provider "sleeper" :league-id "1" :name "Dynasty Dorks"
                                :account-key sleeper-ak :phase :season}}
         :active-league nil)
  (rf/clear-subscription-cache!)
  (let [h      (render/hiccup settings/connected-accounts)
        select (atom nil)]
    (clojure.walk/prewalk
     (fn [x]
       (when (and (vector? x) (= :select (first x)) (= "Phase" (:aria-label (second x))))
         (reset! select x))
       x)
     h)
    (is (= "season" (:value (second @select))) "a stored override shows as itself")
    (let [seen (atom [])]
      (with-redefs [rf/dispatch #(swap! seen conj %)]
        ((:on-change (second @select)) #js {:target #js {:value "auto"}}))
      (is (= [[:set-phase "sleeper:1" nil]] @seen)
          "and Auto clears it, for that league rather than the active one"))))
