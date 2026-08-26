(ns draft-day.views.watchlist-test
  "The watch list's own drag. Covered here rather than in `db_test` because what
  the insertion line claims and what the reorder does are two different pieces of
  code, and the bug worth catching is them disagreeing."
  (:require [cljs.test :refer [deftest is testing]]
            [draft-day.db :as db]
            [draft-day.views.util :as util]
            [draft-day.views.watchlist :as wl]))

(deftest watch-drags-are-not-column-drags
  ;; Distinct MIMEs so neither drop target accepts the other's payload: a column
  ;; dropped on a watch row (or a player on a board header) would reorder against
  ;; a key that is not in that vector — a no-op that looks like a broken drag.
  (is (not= util/column-mime util/watch-mime))
  (testing "and neither is droppable text — the search box takes text"
    (is (re-find #"^application/" util/watch-mime))))

(deftest drop-edge-follows-the-direction-of-travel
  (let [ids ["gibbs" "chase" "nacua" "bowers"]]
    (testing "dragging down, the line sits under the target — where the row lands"
      (is (= "drop-below" (wl/drop-edge ids "gibbs" "nacua"))))

    (testing "dragging up, it sits above it"
      (is (= "drop-above" (wl/drop-edge ids "bowers" "chase"))))

    (testing "no line on the row being dragged — there is no gap there"
      (is (nil? (wl/drop-edge ids "chase" "chase"))))

    (testing "a player not in the list draws nothing rather than guessing"
      (is (nil? (wl/drop-edge ids "hall" "chase")))
      (is (nil? (wl/drop-edge ids nil "chase"))))))

(deftest the-line-lands-where-the-reorder-actually-puts-the-row
  ;; The one assertion that ties the two halves together: whatever edge the line
  ;; drew, the row must end up on that side of the target.
  (let [ids ["a" "b" "c" "d"]]
    (doseq [from ids to ids
            :when (not= from to)]
      (let [after (db/move-watch-onto ids from to)
            edge  (wl/drop-edge ids from to)
            pos   #(.indexOf (to-array after) %)]
        (is (= (if (= edge "drop-below") (inc (pos to)) (dec (pos to)))
               (pos from))
            (str "dragging " from " onto " to " drew " edge))))))

(deftest the-line-still-tells-the-truth-when-a-watched-player-is-drafted
  ;; The rows on screen are the undrafted watch list, so `drop-edge` reads a
  ;; shorter vector than `move-watch-onto` does. Filtering preserves relative
  ;; order, so the two still agree — but that is the argument the keyed event
  ;; exists to make, and it is worth pinning down.
  (let [stored   ["a" "drafted" "b" "c"]
        rendered ["a" "b" "c"]
        pos      (fn [v x] (.indexOf (to-array v) x))]
    (doseq [from rendered to rendered
            :when (not= from to)]
      (let [after (db/move-watch-onto stored from to)
            edge  (wl/drop-edge rendered from to)]
        (is (= (if (= edge "drop-below") (inc (pos after to)) (dec (pos after to)))
               (pos after from))
            (str "dragging " from " onto " to " drew " edge))
        (is (= 4 (count after)) (str "the drafted id is not lost: " after))))))
