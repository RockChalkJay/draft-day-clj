(ns draft-day.views.watchlist
  "Manager-curated list of players to nominate, filled from the board's ★ and
  ordered by hand."
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [re-frame.core :as rf]
            [draft-day.views.util :as util]))

(defn drop-edge
  "Which edge of the row `over` the insertion line goes on, given the order the
  rows are actually in (`ids`). Dragging down, the dragged row lands *after* the
  target; dragging up, before it — the same asymmetry `db/move-onto` produces,
  so the line always shows where the row will really end up."
  [ids dragging over]
  (let [idx  (zipmap ids (range))
        from (idx dragging)
        to   (idx over)]
    (when (and from to (not= from to))
      (if (< from to) "drop-below" "drop-above"))))

(defn watch-row [p ids drag]
  (let [id (:player-id p)
        {:keys [dragging over]} @drag]
    [:div.w-row
     {:on-click      #(rf/dispatch [:set-nominated id])
      :title         "Put on the block"
      :draggable     true
      :on-drag-start (fn [e]
                       (util/watch-drag-start! e id)
                       (reset! drag {:dragging id :over nil}))
      :on-drag-over  (fn [e]
                       (.preventDefault e)
                       (swap! drag assoc :over id))
      ;; The × button is a child: crossing onto it fires dragleave on the row,
      ;; and clearing on that blinks the insertion line off mid-drag.
      :on-drag-leave (fn [e]
                       (when (util/left-element? e)
                         (swap! drag update :over (fn [o] (when-not (= o id) o)))))
      :on-drop       (fn [e]
                       (.preventDefault e)
                       (rf/dispatch [:move-watch-onto (:dragging @drag) id])
                       (reset! drag {}))
      ;; A drag abandoned off the list still ends, so nothing stays lit.
      :on-drag-end   #(reset! drag {})
      :class         (->> [(when (= dragging id) "dragging")
                           (when (= over id) (drop-edge ids dragging id))]
                          (filter some?)
                          (str/join " "))}
     [:span.w-grip {:title "Drag to reorder"} "⠿"]
     [:span.w-name (:player-name p) [:span.w-pos (util/pos-label p)]]
     [:span.w-worth (util/money (:worth p))]
     [:span.w-mkt (util/money-rnd (:market p))]
     [:button.w-remove {:title    "Remove from watch list"
                        :on-click (fn [e]
                                    (.stopPropagation e)
                                    (rf/dispatch [:watch-remove id]))}
      "×"]]))

(defn watchlist-panel []
  ;; Owns the transient drag state in a local atom — it is pointer state, not app
  ;; state; only the committed reorder reaches app-db. Same split as the board
  ;; header's column drag.
  (let [drag (r/atom {})]
    (fn []
      (let [players @(rf/subscribe [:watchlist-players])
            ids     (mapv :player-id players)]
        [:div.watchlist-panel
         [:div.watchlist-head
          [:h3 "👀 Watch List"]
          [:span.w-count (str (count players) " watched")]]
         (if (seq players)
           [:div.watchlist
            (map (fn [p] ^{:key (:player-id p)} [watch-row p ids drag]) players)]
           [:div.w-empty "Star players on the board to track them here."])]))))
