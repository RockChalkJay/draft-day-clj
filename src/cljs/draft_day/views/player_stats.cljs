(ns draft-day.views.player-stats
  "The season trend table on the On-the-block tile: three completed seasons of
  realized production against the upcoming season's projection.

  Rendering only. Which rows a position gets, which seasons are columns, and
  what counts as a row worth drawing all live in `draft-day.stat-lines`, which
  is cljc so `lein test` covers it."
  (:require [draft-day.stat-lines :as sl]
            [re-frame.core :as rf]))

(defn cell
  "One number, rounded to whole. Yards and touchdowns are counted in whole units;
  the doubles are an artifact of the CSV, not precision anybody wants to read."
  [v]
  (if (number? v) (js/Math.round v) "–"))

(defn stat-table
  "The table for `p` (a *universe* player — the ranked board has no history on
  it), or nil when there is nothing to draw: a kicker, a defense, or a player
  with no numbers at all."
  [p season]
  (when-let [{:keys [seasons proj-season rookie? rows]} (sl/stat-table p season)]
    [:div.nt-stats-col
     (when rookie?
       [:div.nt-nohist "No NFL history yet"])
     [:table.nt-stats
      [:thead
       [:tr
        [:th.lbl]
        (for [s seasons] ^{:key s} [:th.num s])
        ;; The projection is the one column here that is not a fact, so it is
        ;; labelled as a claim rather than as just the newest year.
        [:th.num.proj (str proj-season " proj")]]]
      [:tbody
       (for [{:keys [label values proj]} rows]
         ^{:key label}
         [:tr
          [:th.lbl label]
          ;; Zipped against `seasons` positionally — `stat-lines` guarantees one
          ;; value per column, including the nils.
          (for [[s v] (map vector seasons values)]
            ^{:key s} [:td.num (cell v)])
          [:td.num.proj (cell proj)]])]]]))

(defn nominated-stats
  "The table for whoever is on the block, read from the universe rather than the
  board. Nothing renders for a player the universe has not loaded yet."
  []
  (let [id      @(rf/subscribe [:nominated-id])
        p       (get @(rf/subscribe [:universe-by-id]) id)
        season  (:season @(rf/subscribe [:universe]))]
    (when p
      [stat-table p season])))
