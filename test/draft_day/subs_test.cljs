(ns draft-day.subs-test
  "Subscriptions that read the universe provenance the API sends alongside the
  players, and the Settings copy built from them. Not reachable from
  `lein test` — run with
  `npx shadow-cljs compile test && node out/node-tests.js`."
  (:require [cljs.test :refer [deftest is testing use-fixtures]]
            [re-frame.core :as rf]
            [re-frame.db :as rdb]
            [reagent.ratom]
            [draft-day.db :as db]
            [draft-day.subs :as subs]
            [draft-day.views.settings :as settings]))

;; Every `-test` namespace compiles into one node bundle and they all share
;; `rdb/app-db`, so what this fixture sets up it also has to put back — the
;; sibling events-test makes the same promise about the :fx handlers it swaps.
(use-fixtures :each
  {:before (fn [] (reset! rdb/app-db (db/default-db)))
   :after  (fn []
             (rf/clear-subscription-cache!)
             (reset! rdb/app-db (db/default-db)))})

(defn- with-sources [scoring sources]
  (swap! rdb/app-db #(-> %
                         (assoc-in [:config :scoring] scoring)
                         (assoc :universe {:sources sources}))))

(defn- gaps []
  ;; Subscriptions are reactions: derefed outside a reactive context they warn
  ;; and cache forever. A fresh cache plus a stub context is what a view would
  ;; give them, without pulling in a renderer.
  (rf/clear-subscription-cache!)
  (binding [reagent.ratom/*ratom-context* #js {}]
    @(rf/subscribe [:vendor-gaps])))

(def ^:private landed {:ok? true :rows 300 :matched 280})

(deftest a-format-scrape-that-failed-is-reported-for-that-format-only
  ;; Each FantasyPros scrape is independently best-effort, so the standard
  ;; cheatsheet can fail while PPR succeeds. Before this, the cache was served
  ;; for a day and only standard leagues were affected — silently.
  (let [sources {:fantasypros/aav-standard {:ok? false}
                 :fantasypros/aav-ppr      landed
                 :fantasypros/ecr-standard landed
                 :fantasypros/ecr-ppr      landed}]
    (testing "a standard league is told its auction values are missing"
      (with-sources :standard sources)
      (is (= [:fantasypros/aav] (gaps))))

    (testing "a PPR league in the same universe has nothing to report"
      (with-sources :ppr sources)
      (is (= [] (gaps))))

    (testing "both halves failing are named together, ranks first"
      (with-sources :standard (assoc sources :fantasypros/ecr-standard {:ok? false}))
      (is (= [:fantasypros/ecr :fantasypros/aav] (gaps))))))

(deftest a-scrape-that-answered-but-delivered-nothing-is-still-a-gap
  ;; `:ok? false` is only the loud half. A vendor renaming a JSON key leaves the
  ;; parse returning an empty (but non-nil) seq, which the join records as
  ;; `{:ok? true :rows 0}`; match keys drifting leaves plenty of rows and
  ;; `:matched 0`. Both blank every column, and keying off `:ok?` missed both.
  (testing "parsed to nothing"
    (with-sources :ppr {:fantasypros/ecr-ppr {:ok? true :rows 0 :matched 0}
                        :fantasypros/aav-ppr landed})
    (is (= [:fantasypros/ecr] (gaps))))

  (testing "published rows that landed on nobody"
    (with-sources :ppr {:fantasypros/ecr-ppr landed
                        :fantasypros/aav-ppr {:ok? true :rows 150 :matched 0}})
    (is (= [:fantasypros/aav] (gaps)))))

(deftest a-custom-scoring-map-is-matched-to-its-nearest-format
  ;; A custom config names no format; the :rec weight picks the nearest, the
  ;; same way the server picks which vendor columns to flatten.
  (let [sources {:fantasypros/aav-standard {:ok? false}
                 :fantasypros/aav-ppr      landed}]
    (with-sources {:rec 0.0 :rec_yd 0.1} sources)
    (is (= [:fantasypros/aav] (gaps)) "no receptions reads as standard")

    (with-sources {:rec 1.0 :rec_yd 0.1} sources)
    (is (= [] (gaps)) "a point per catch reads as PPR")))

(deftest no-provenance-yet-is-not-a-gap
  ;; Before /api/players answers there is nothing to judge, and a universe from
  ;; an older server may carry no :sources at all. "We never looked" is not
  ;; "it is missing" — the bundled sample predates provenance entirely.
  (is (= [] (gaps)))
  (with-sources :ppr nil)
  (is (= [] (gaps)))
  (with-sources :ppr {:espn landed})
  (is (= [] (gaps)) "an unrelated source says nothing about FantasyPros"))

(deftest the-notice-names-the-consequence-that-actually-follows
  ;; ECR sets tiers and the rank spread behind the Floor/Ceiling band; AAV is
  ;; the market price. Blaming prices when the ranks failed points the manager
  ;; at the one number still worth trusting.
  (is (nil? (settings/vendor-gap-message [])))

  (let [{:keys [headline detail]} (settings/vendor-gap-message [:fantasypros/ecr])]
    (is (= "FantasyPros expert ranks are missing for your scoring format." headline))
    (is (re-find #"Floor/Ceiling" detail))
    (is (not (re-find #"market prices" detail))
        "prices are fine — the auction values landed"))

  (let [{:keys [headline detail]} (settings/vendor-gap-message [:fantasypros/aav])]
    (is (= "FantasyPros auction values are missing for your scoring format." headline))
    (is (re-find #"market prices lean on ESPN alone" detail))
    (is (not (re-find #"Floor/Ceiling" detail))))

  (let [{:keys [headline detail]} (settings/vendor-gap-message subs/vendor-gap-sources)]
    (is (= "FantasyPros expert ranks and auction values are missing for your scoring format."
           headline))
    (is (re-find #"Floor/Ceiling" detail))
    (is (re-find #"market prices" detail))))

;; ---- board tiers ----
;; :board-players is where the active tier technique and scale are resolved onto
;; each row's :tier, so the views can stay ignorant of both.

(defn- board-players []
  (rf/clear-subscription-cache!)
  (binding [reagent.ratom/*ratom-context* #js {}]
    @(rf/subscribe [:board-players])))

(defn- tiered-board! [strategy pos-filter]
  (swap! rdb/app-db
         #(-> %
              (assoc-in [:config :tier-strategy] strategy)
              (assoc :pos-filter pos-filter)
              (assoc :drafted {})
              (assoc :ranked
                     {:players
                      [{:player-id "rb1" :player-name "A Back" :position "RB" :worth 50
                        :tier 1 :tiers {:cliffs {:overall 2 :position 1}
                                        :ecr    {:overall 4 :position 3}}}
                       {:player-id "wr1" :player-name "B Wide" :position "WR" :worth 40
                        :tier 2 :tiers {:cliffs {:overall 3 :position 2}
                                        ;; ranked by neither ECR scale
                                        :ecr    {}}}]}))))

(defn- tier-of [rows id]
  (:tier (first (filter #(= id (:player-id %)) rows))))

(deftest the-board-reads-the-overall-scale-until-a-position-is-filtered
  (tiered-board! :cliffs nil)
  (is (= 2 (tier-of (board-players) "rb1")) "overall while unfiltered")
  (tiered-board! :cliffs "RB")
  (is (= 1 (tier-of (board-players) "rb1")) "positional once filtered"))

(deftest switching-strategy-changes-every-tier-and-nothing-else
  ;; The instant-switch guarantee: the server ships every strategy at both
  ;; scales, so flipping the config key is the whole operation — no refetch.
  (tiered-board! :cliffs nil)
  (let [before (board-players)]
    (swap! rdb/app-db assoc-in [:config :tier-strategy] :ecr)
    (let [after (board-players)]
      (is (= 2 (tier-of before "rb1")))
      (is (= 4 (tier-of after "rb1")) "same row, the other technique's number")
      (is (= (map :player-id before) (map :player-id after))
          "the row set and its order are untouched"))))

(deftest a-player-the-strategy-cannot-tier-gets-nil-not-a-borrowed-number
  ;; wr1 has cliff tiers but no ECR tier on either scale. Under :ecr it must read
  ;; as untiered rather than quietly showing its cliff tier.
  (tiered-board! :ecr nil)
  (is (nil? (tier-of (board-players) "wr1")))
  (tiered-board! :cliffs nil)
  (is (= 3 (tier-of (board-players) "wr1"))
      "and the cliff tier is still there when that strategy is active"))

(deftest an-unknown-persisted-strategy-falls-back-rather-than-unranking-everything
  (tiered-board! :cliffs nil)
  (swap! rdb/app-db assoc-in [:config :tier-strategy] :no-such-thing)
  (is (every? nil? (map :tier (board-players)))
      "an unknown strategy really does tier nothing — which is why reconcile-config repairs it")
  (is (= db/default-tier-strategy
         (:tier-strategy (db/reconcile-config {:tier-strategy :no-such-thing})))))
