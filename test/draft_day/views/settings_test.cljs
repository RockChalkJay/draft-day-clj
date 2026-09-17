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
        fired (for [[t f] (render/buttons settings/connected-accounts)
                    :when (= "Disconnect" t)]
                (fire f))]
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

(deftest settings-shows-one-section-at-a-time
  ;; The whole fix for the stretching: cards that are not in the open section
  ;; are not on the page to be stretched against.
  (let [html (render settings/settings)]
    (is (re-find #"Leagues & Accounts" html) "opens on accounts")
    (is (re-find #"Add account|Connect" html))
    (is (not (re-find #"Danger Zone" html)))
    (is (not (re-find #"Budget Plan" html))))
  (rf/dispatch-sync [:set-settings-section :draft])
  (rf/clear-subscription-cache!)
  (let [html (render settings/settings)]
    (is (re-find #"Budget Plan" html))
    (is (re-find #"Draft Archive" html))
    (is (not (re-find #"Add account" html)))))

(deftest every-section-is-reachable-from-the-sidebar
  (is (= (set (map (fn [[k _]] [:set-settings-section k]) db/settings-sections))
         (set (for [[_ label] db/settings-sections
                    ev (press! settings/settings-nav label)]
                ev)))))

(deftest the-sidebar-says-what-is-waiting-inside-a-section
  (is (not (re-find #"nav-badge|nav-dot" (render settings/settings-nav)))
      "nothing to report, nothing drawn")
  (accounts! espn-ak (assoc espn-acct :credentials-stale? true))
  (swap! rdb/app-db assoc :import-report {:league-key nil :unsupported-scoring ["fg_50p" "pts_allow_0"]})
  (rf/clear-subscription-cache!)
  (let [html (render settings/settings-nav)]
    (is (re-find #"nav-dot" html) "an expired session marks Leagues & Accounts")
    (is (re-find #"nav-badge.*\b2\b" html) "and the count of dropped rules marks Scoring")))

(deftest unapplied-rules-are-listed-one-by-one
  ;; A comma-joined run of underscore keys has nowhere to wrap and ran straight
  ;; out of the card.
  (swap! rdb/app-db assoc :import-report {:league-key nil :unsupported-scoring ["fg_50p" "pts_allow_0"]})
  (rf/clear-subscription-cache!)
  (let [html (render settings/import-warning)]
    (is (= 2 (count (re-seq #":span\.rule-chip\b" html))))))

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
  ;; Nothing clears the report on a switch, and a badge that kept League A's
  ;; dropped rules on screen under League B would be a claim about B.
  (swap! rdb/app-db assoc
         :active-league "sleeper:1"
         :import-report {:league-key "sleeper:1" :unsupported-scoring ["fg_50p"]})
  (rf/clear-subscription-cache!)
  (is (re-find #"nav-badge" (render settings/settings-nav)))
  (is (re-find #"rule-chip" (render settings/import-warning)))
  (swap! rdb/app-db assoc :active-league "espn:9")
  (rf/clear-subscription-cache!)
  (is (not (re-find #"nav-badge" (render settings/settings-nav))))
  (is (nil? (settings/import-warning))))

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
