(ns draft-day.views.columns
  "Column picker: show/hide via checkboxes, rearrange via drag-and-drop."
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [draft-day.db :as db]))

(defn column-picker []
  ;; Tracks the dragged column's key, not its index: :move-column-onto is keyed
  ;; so the picker and the board header can share one event (see views.board).
  (let [drag-key (r/atom nil)]
    (fn []
      (let [cols @(rf/subscribe [:columns])]
        [:ul.col-picker
         (map (fn [c]
                (let [k (:key c)]
                  ^{:key k}
                  [:li.col-item
                   {:draggable true
                    :on-drag-start #(reset! drag-key k)
                    :on-drag-over  #(.preventDefault %)
                    :on-drop       #(do (rf/dispatch [:move-column-onto @drag-key k])
                                        (reset! drag-key nil))
                    :on-drag-end   #(reset! drag-key nil)}
                   [:span.drag-handle "⠿"]
                   [:label
                    [:input {:type "checkbox" :checked (:visible? c)
                             :on-change #(rf/dispatch [:toggle-column k])}]
                    " " (:label (db/columns-by-key k))]]))
              cols)]))))
