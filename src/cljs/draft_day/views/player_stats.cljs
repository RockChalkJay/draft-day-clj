(ns draft-day.views.player-stats
  "The season trend table on the On-the-block tile and the player card: three
  completed seasons of realized production against the upcoming season's
  projection, or in season against what he has done so far.

  Rendering only. Which rows a position gets, which seasons are columns, and
  what counts as a row worth drawing all live in `draft-day.stat-lines`, which
  is cljc so `lein test` covers it."
  (:require [draft-day.stat-lines :as sl]
            [re-frame.core :as rf]))

(defn cell
  "One number, rounded to whole, or a ratio already formatted (`118/171`). Yards and touchdowns are counted in whole units;
  the doubles are an artifact of the CSV, not precision anybody wants to read."
  [v]
  (cond (number? v) (js/Math.round v)
        (string? v) v
        :else       "–"))

(defn stat-table
  "The table for `p` (a *universe* player — the ranked board has no history on
  it), or nil when there is nothing to draw: a kicker, a defense, or a player
  with no numbers at all."
  ([p season] (stat-table p season {}))
  ([p season opts]
   (when-let [{:keys [seasons proj-season rookie? so-far? rows]} (sl/stat-table p season opts)]
     [:div.nt-stats-col
      (when rookie?
        [:div.nt-nohist "No NFL history yet"])
      [:table.nt-stats
       [:thead
        [:tr
         [:th.lbl]
         (map (fn [s] ^{:key s} [:th.num s]) seasons)
         ;; Preseason, the one column that is not a fact, labelled as a claim;
         ;; in season, what he has done so far.
         (if so-far?
           [:th.num.so-far (str proj-season " so far")]
           [:th.num.proj (str proj-season " proj")])]]
       [:tbody
        (map (fn [{:keys [label values proj]}]
               ^{:key label}
               [:tr
                [:th.lbl label]
                ;; Zipped against `seasons` positionally — `stat-lines` guarantees one
                ;; value per column, including the nils.
                (map (fn [s v] ^{:key s} [:td.num (cell v)]) seasons values)
                [:td.num {:class (if so-far? "so-far" "proj")} (cell proj)]])
             rows)]]])))

(defn nominated-stats
  "The table for whoever is on the block, read from the universe rather than the
  board. Nothing renders for a player the universe has not loaded yet."
  []
  (let [id      @(rf/subscribe [:nominated-id])
        p       (get @(rf/subscribe [:universe-by-id]) id)
        season  (:season @(rf/subscribe [:universe]))]
    (when p
      [stat-table p season])))
