(ns draft-day.views.columns
  "Column picker: show/hide via checkboxes, rearrange via drag-and-drop."
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [draft-day.db :as db]))

(defn column-picker []
  (let [drag-idx (r/atom nil)]
    (fn []
      (let [cols @(rf/subscribe [:columns])]
        [:ul.col-picker
         (for [[i c] (map-indexed vector cols)]
           ^{:key (:key c)}
           [:li.col-item
            {:draggable true
             :on-drag-start #(reset! drag-idx i)
             :on-drag-over  #(.preventDefault %)
             :on-drop       #(when (and @drag-idx (not= @drag-idx i))
                               (rf/dispatch [:move-column @drag-idx i])
                               (reset! drag-idx nil))}
            [:span.drag-handle "⠿"]
            [:label
             [:input {:type "checkbox" :checked (:visible? c)
                      :on-change #(rf/dispatch [:toggle-column (:key c)])}]
             " " (:label (db/columns-by-key (:key c)))]])]))))
