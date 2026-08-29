(ns draft-day.views.watchlist
  "Manager-curated list of players to nominate, filled from the board's ★ and
  ordered by hand — or, once by hand is tedious, by one press of a sort button
  that rewrites that order and then leaves it alone."
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

(def sort-buttons
  "The three one-shot re-sorts, in the order they are offered. Labels are short
  because the panel is a narrow column; the title carries the rest."
  [{:key :rank     :label "Rank"  :title "Reorder the list best-first by overall rank"}
   {:key :worth    :label "Worth" :title "Reorder the list by Worth, highest first"}
   {:key :position :label "Pos"   :title "Group the list by position, best first inside each"}])

(defn sort-controls
  "Buttons, not a sort mode — each press rewrites the stored order once and hands
  it back. So there is no active button to light up and no direction arrow to
  flip, which is exactly what tells the manager the list is still his to drag."
  []
  [:div.w-sort
   [:span.w-sort-label "Sort"]
   (for [{:keys [key label title]} sort-buttons]
     ^{:key key}
     [:button {:title    (str title " — you can still drag rows afterwards")
               :on-click #(rf/dispatch [:watch-sort key])}
      label])])

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
         ;; Its own row rather than a third item in the head: the panel is a
         ;; 320px column, and squeezing three things onto that line wrapped the
         ;; heading. Nothing to reorder below two rows, and an inert control
         ;; reads as broken rather than as unnecessary.
         (when (> (count players) 1) [sort-controls])
         (if (seq players)
           [:div.watchlist
            (map (fn [p] ^{:key (:player-id p)} [watch-row p ids drag]) players)]
           [:div.w-empty "Star players on the board to track them here."])]))))
