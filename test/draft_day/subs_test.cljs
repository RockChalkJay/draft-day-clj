(ns draft-day.subs-test
  "Subscriptions that read the universe provenance the API sends alongside the
  players. Not reachable from `lein test` — run with
  `npx shadow-cljs compile test && node out/node-tests.js`."
  (:require [cljs.test :refer [deftest is testing use-fixtures]]
            [re-frame.core :as rf]
            [re-frame.db :as rdb]
            [reagent.ratom]
            [draft-day.db :as db]
            [draft-day.subs]))

(use-fixtures :each {:before (fn [] (reset! rdb/app-db (db/default-db)))})

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

(deftest a-format-scrape-that-failed-is-reported-for-that-format-only
  ;; Each of the six FantasyPros scrapes is independently best-effort, so the
  ;; standard cheatsheet can fail while PPR succeeds. Before this, the cache was
  ;; served for a day and only standard leagues were affected — silently.
  (let [sources {:fantasypros/aav-standard {:ok? false}
                 :fantasypros/aav-ppr      {:ok? true :rows 300}
                 :fantasypros/ecr-standard {:ok? true :rows 300}
                 :fantasypros/ecr-ppr      {:ok? true :rows 300}}]
    (testing "a standard league is told its auction values are missing"
      (with-sources :standard sources)
      (is (= ["auction values"] (gaps))))

    (testing "a PPR league in the same universe has nothing to report"
      (with-sources :ppr sources)
      (is (= [] (gaps))))

    (testing "both halves failing are named together"
      (with-sources :standard (assoc sources :fantasypros/ecr-standard {:ok? false}))
      (is (= #{"expert ranks" "auction values"} (set (gaps)))))))

(deftest a-custom-scoring-map-is-matched-to-its-nearest-format
  ;; A custom config names no format; the :rec weight picks the nearest, the
  ;; same way the server picks which vendor columns to flatten.
  (let [sources {:fantasypros/aav-standard {:ok? false}
                 :fantasypros/aav-ppr      {:ok? true}}]
    (with-sources {:rec 0.0 :rec_yd 0.1} sources)
    (is (= ["auction values"] (gaps)) "no receptions reads as standard")

    (with-sources {:rec 1.0 :rec_yd 0.1} sources)
    (is (= [] (gaps)) "a point per catch reads as PPR")))

(deftest no-provenance-yet-is-not-a-gap
  ;; Before /api/players answers there is nothing to judge, and a universe from
  ;; an older server may carry no :sources at all.
  (is (= [] (gaps)))
  (with-sources :ppr nil)
  (is (= [] (gaps))))
