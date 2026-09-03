(ns draft-day.views.columns
  "Column picker: show/hide via checkboxes, rearrange via drag-and-drop.

  Parameterized over which board it is picking for, because the draft board and
  the waiver board have separate catalogs, separate persisted column vectors and
  separate events — but exactly one set of drag mechanics, and those are the
  fiddly part (see `db/move-onto` and `util/left-element?` for what makes them
  fiddly). Copying the component per board would mean fixing every drag bug
  twice."
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [draft-day.db :as db]
            [draft-day.views.util :as util]))

(def board-picker
  {:sub :columns :labels db/columns-by-key
   :toggle :toggle-column :move :move-column-onto})

(def waiver-picker
  {:sub :waiver-columns :labels db/waiver-columns-by-key
   :toggle :toggle-waiver-column :move :move-waiver-column-onto})

(defn picker
  "The picker for one board's `opts` (see `board-picker` / `waiver-picker`)."
  [_opts]
  ;; Tracks the dragged column's key, not its index: the move events are keyed
  ;; so the picker and the board header can share one (see views.board).
  (let [drag-key (r/atom nil)]
    (fn [{:keys [sub labels toggle move]}]
      (let [cols @(rf/subscribe [sub])]
        [:ul.col-picker
         (map (fn [c]
                (let [k (:key c)]
                  ^{:key k}
                  [:li.col-item
                   {:draggable true
                    :on-drag-start (fn [e]
                                     (util/column-drag-start! e k)
                                     (reset! drag-key k))
                    :on-drag-over  #(.preventDefault %)
                    ;; preventDefault or the browser runs its own drop action on
                    ;; top of the reorder
                    :on-drop       (fn [e]
                                     (.preventDefault e)
                                     (rf/dispatch [move @drag-key k])
                                     (reset! drag-key nil))
                    :on-drag-end   #(reset! drag-key nil)}
                   [:span.drag-handle "⠿"]
                   [:label
                    [:input {:type "checkbox" :checked (:visible? c)
                             :on-change #(rf/dispatch [toggle k])}]
                    " " (:label (get labels k))]]))
              cols)]))))

(defn column-picker [] [picker board-picker])
(defn waiver-column-picker [] [picker waiver-picker])
