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

(defn- board-ranks
  "player-id -> :rank, as the board would compute it for `players`."
  [players]
  (rf/clear-subscription-cache!)
  (swap! rdb/app-db assoc :ranked {:players players})
  (binding [reagent.ratom/*ratom-context* #js {}]
    (into {} (map (juxt :player-id :rank)) @(rf/subscribe [:board-players]))))

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

;; ---- board rank ----

(deftest players-priced-at-zero-are-still-ranked-against-each-other
  ;; Everything below replacement prices at $0 — every skill player the model
  ;; scored under replacement, plus all of K and DST. Ranking on Worth alone left
  ;; that whole block in the order the server happened to emit, which is position
  ;; grouping: 549 of 633 players, among them a receiver FantasyPros ranked 44th
  ;; who came out 200th. VORP, then points, still separate them where dollars
  ;; cannot.
  (let [ranks (board-ranks [{:player-id "cheap"  :worth 0 :vorp 0 :points 90}
                            {:player-id "rich"   :worth 40 :vorp 120 :points 300}
                            {:player-id "mid"    :worth 0 :vorp 0 :points 140}
                            {:player-id "decent" :worth 12 :vorp 30 :points 200}])]
    (is (= 1 (ranks "rich")))
    (is (= 2 (ranks "decent")))
    (is (= 3 (ranks "mid")) "more projected points ranks above")
    (is (= 4 (ranks "cheap"))))

  (testing "VORP outranks points among the $0 block"
    (let [ranks (board-ranks [{:player-id "low-vorp"  :worth 0 :vorp 1 :points 300}
                              {:player-id "high-vorp" :worth 0 :vorp 5 :points 10}])]
      (is (= 1 (ranks "high-vorp")))
      (is (= 2 (ranks "low-vorp")))))

  (testing "a player the model never scored sorts last rather than throwing"
    (let [ranks (board-ranks [{:player-id "unscored"}
                              {:player-id "scored" :worth 0 :vorp 0 :points 5}])]
      (is (= 1 (ranks "scored")))
      (is (= 2 (ranks "unscored"))))))

(deftest worth-still-decides-the-order-wherever-it-can
  ;; The tie-break may only reach players Worth cannot separate. Distinct dollars
  ;; decide the order outright, whatever VORP and points say.
  (let [ranks (board-ranks [{:player-id "c" :worth 5  :vorp 999 :points 999}
                            {:player-id "a" :worth 50 :vorp 1   :points 1}
                            {:player-id "b" :worth 20 :vorp 500 :points 500}])]
    (is (= {"a" 1 "b" 2 "c" 3} ranks)))

  (testing "priced players sharing a dollar figure are separated by VORP"
    ;; Worth is whole dollars, so ties are ordinary well above replacement — on
    ;; the sample board 84 priced players hold 40-odd distinct prices. Those ties
    ;; used to fall out in server order too; they just moved by less.
    (let [ranks (board-ranks [{:player-id "lo" :worth 30 :vorp 40 :points 250}
                              {:player-id "hi" :worth 30 :vorp 45 :points 210}
                              {:player-id "up" :worth 31 :vorp 1  :points 10}])]
      (is (= {"up" 1 "hi" 2 "lo" 3} ranks)
          "a dollar more still outranks any VORP"))))

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
