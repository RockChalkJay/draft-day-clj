(ns draft-day.views.columns
  "Column picker: a dropdown checklist to show and hide columns, and a reset.
  Columns are reordered by dragging the board's headers.

  Parameterized over which board it is picking for, because the draft board and
  the waiver board have separate catalogs, persisted column vectors and events
  but one picker."
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [draft-day.db :as db]))

(def board-picker
  {:sub :position-columns :labels db/columns-by-key
   :toggle :toggle-column :reset :reset-columns})

(def waiver-picker
  {:sub :league-waiver-columns :labels db/waiver-columns-by-key
   :toggle :toggle-waiver-column :reset :reset-waiver-columns
   :groups db/waiver-column-groups})

(defn grouped
  "`[[heading cols] ...]` in `groups`' order, or one untitled group for a
  catalog that names none."
  [cols labels groups]
  (if (seq groups)
    (keep (fn [[g heading]]
            (when-let [cs (seq (filter #(= g (:group (labels (:key %)))) cols))]
              [heading cs]))
          groups)
    [[nil cols]]))

(defn checklist
  "The open picker: every column as a checkbox under its group's heading, two
  columns wide so no group is below a scroll, and a reset."
  [{:keys [labels toggle groups] reset-event :reset} cols]
  [:div.col-menu {:role "dialog" :aria-label "Columns"}
   [:div.col-groups
    (map (fn [[heading cs]]
           ^{:key (or heading "all")}
           [:div.col-group
            (when heading [:div.col-group-head heading])
            (map (fn [c]
                   (let [k (:key c)]
                     ^{:key k}
                     [:label.col-check {:class (when (or (:faab-only? c) (:off-position? c)) "off")
                                        :title (cond
                                                 (:faab-only? c) "This league doesn't run FAAB"
                                                 (:off-position? c) "Not tracked for the position selected")}
                      [:input {:type "checkbox" :checked (boolean (:visible? c))
                               :disabled (or (:faab-only? c) (:off-position? c))
                               :on-change #(rf/dispatch [toggle k])}]
                      " " (:label (labels k))]))
                 cs)])
         (grouped cols labels groups))]
   [:div.col-menu-foot
    [:button.link-btn {:on-click #(rf/dispatch [reset-event])} "Reset to defaults"]
    [:span.muted (str (count (filter :visible? cols)) " shown")]]])

(defn picker
  "A \"Columns\" button opening `checklist`. Order is changed by dragging the
  board's own headers (`board/header-cell`), not here. Closes on a click
  outside it, or on Escape pressed inside it — stopped there, since the app's
  one document-level Escape clears the comparison."
  [_opts]
  (let [open? (r/atom false)
        node  (atom nil)
        away  (fn [e] (when (and @open? @node (not (.contains @node (.-target e))))
                        (reset! open? false)))]
    (r/create-class
     {:component-did-mount    #(.addEventListener js/document "mousedown" away)
      :component-will-unmount #(.removeEventListener js/document "mousedown" away)
      :reagent-render
      (fn [{:keys [sub] :as opts}]
        (let [cols @(rf/subscribe [sub])]
          [:div.col-picker-wrap {:ref         #(reset! node %)
                                 :on-key-down (fn [e]
                                                (when (and @open? (= "Escape" (.-key e)))
                                                  (.stopPropagation e)
                                                  (reset! open? false)))}
           [:button.col-toggle {:aria-expanded @open?
                                :on-click #(swap! open? not)}
            "Columns ▾"]
           (when @open? [checklist opts cols])]))})))

(defn column-picker [] [picker board-picker])
(defn waiver-column-picker [] [picker waiver-picker])
