(ns draft-day.views.board
  (:require [re-frame.core :as rf]
            [draft-day.db :as db]
            [draft-day.views.util :as util]))

;; ---- cell formatting ----

(defn- n0 [n] (if (number? n) (js/Math.round n) "–"))
(defn- n1 [n] (if (number? n) (.toFixed n 1) "–"))

(defn- cliff-marker [p]
  (when (and (:tcm p) (> (:tcm p) 1.1))
    [:span.badge {:title "Tier cliff — steep drop to the next player"} " 🚨"]))

(defn- sleeper-badge
  "The 💤 emoji, CSS-`filter` tinted toward turquoise. The emoji font paints its
  own colors and ignores CSS `color`, so a hue-rotate/saturate/brightness filter
  is the only way to shift the actual glyph — approximate, not an exact color."
  [p]
  (when (:fantasypros/sleeper? p)
    [:span.sleeper-z {:title "FantasyPros sleeper"} " 💤"]))

(defn- star-toggle
  "Adds/removes the player from the watch list. Stops propagation so starring
  a row doesn't also nominate them."
  [p]
  (let [on? (contains? @(rf/subscribe [:watch-set]) (:player-id p))]
    [:button.star-btn {:class    (if on? "star-on" "star-off")
                       :title    (if on? "On watch list" "Add to watch list")
                       :on-click (fn [e]
                                   (.stopPropagation e)
                                   (rf/dispatch [:watch-toggle (:player-id p)]))}
     (if on? "⭐" "☆")]))

(defn- cell [k p]
  (case k
    :rank     [:td.num.muted (:rank p)]
    :name     [:td.name [star-toggle p] (:player-name p) (cliff-marker p) (sleeper-badge p)]
    :team     [:td.muted (:team p)]
    :position [:td [:span.pill (:position p)]]
    :worth    [:td.num.bold (util/money (:worth p))]
    :value    [:td.num.muted (util/money (:value p))]
    :espn-value [:td.num.muted (util/money-rnd (:espn/auction-value p))]
    :fp-aav   [:td.num.muted (util/money (:fantasypros/aav p))]
    :market   [:td.num.muted (util/money (:market p))]
    :edge     (let [e (:edge p)]
                [:td.num {:class (cond (and (number? e) (pos? e)) "good"
                                       (and (number? e) (neg? e)) "warn")}
                 (if (and (number? e) (not (zero? e))) (str (when (pos? e) "+") e) "–")])
    :bargain  (let [b (:bargain p)]
                [:td.num {:class (cond (and (number? b) (pos? b)) "good"
                                       (and (number? b) (neg? b)) "warn")}
                 (if (and (number? b) (not (zero? b))) (str (when (pos? b) "+") b) "–")])
    :adp      [:td.num (if-let [a (:sleeper/adp p)] (n1 a) "–")]
    :proj     [:td.num (n0 (:points p))]
    :ceiling  [:td.num.good (n0 (:ceiling p))]
    :floor    [:td.num.muted (n0 (:floor p))]
    :vorp     [:td.num (n0 (:vorp p))]
    :ecr      [:td.num (or (:fantasypros/ecr p) "–")]
    :fp-tier  [:td.num.muted (or (:fantasypros/ecr-tier p) "–")]
    :inj      [:td (or (:sleeper/injury-status p) "–")]
    :bye      (let [clash? (db/board-bye-clash? (:position p) (:bye p)
                                                @(rf/subscribe [:my-bye-exposure]))]
                [:td {:class (when clash? "bye-clash")
                      :title (when clash?
                               (str "Bye clash — you already start a " (:position p)
                                    " on bye " (:bye p)))}
                 (or (:bye p) "–")])
    [:td "–"]))

;; ---- header + rows ----

(defn- header-cell [col sort]
  (let [k (:key col)
        d (db/columns-by-key k)
        active? (= (:key sort) k)]
    [:th {:on-click #(rf/dispatch [:set-sort k])
          :title (:tooltip d)
          :class (when active? "sorted")}
     (:label d)
     [:span.sort-ind (cond (not active?) " ↕" (= -1 (:dir sort)) " ▼" :else " ▲")]]))

;; ---- tier colors ----

;; A green-to-red sweep across however many tiers a position turns out to have,
;; rather than six fixed CSS classes with everything past tier 6 clamped into the
;; same color. The ramp carries the ordering (tier 1 best, last tier is the
;; below-replacement tail) and the count now comes from the data, so the palette
;; has to follow it. Custom properties do the work; styles.css consumes them.

(defn tier-hue [t n]
  (let [span (max 1 (dec n))]
    (- 150.0 (* 150.0 (/ (dec (max 1 t)) (double span))))))

(defn- tier-style [t n]
  (let [h (tier-hue t n)]
    {"--tier-bg"   (str "hsla(" h ", 68%, 55%, .20)")
     "--tier-line" (str "hsl(" h ", 68%, 60%)")}))

(defn- player-row [p cols nominated color-tier? n-tiers]
  [:tr (cond-> {:class [(when (= nominated (:player-id p)) "selected")
                        ;; tier row-coloring only when filtered to a single position
                        (when color-tier? "tier-row")]
                :on-click #(rf/dispatch [:set-nominated (:player-id p)])}
         color-tier? (assoc :style (tier-style (or (:tier p) 1) n-tiers)))
   (map (fn [{k :key}] ^{:key k}
          [cell k p]) cols)])

;; ---- tier key ----

(defn- tier-key
  "Legend of the tier stripe colors, showing every tier present on the board."
  [players n-tiers]
  (let [tiers (->> players (keep :tier) distinct sort)]
    (when (seq tiers)
      [:div.tier-key
       [:span.tier-key-label "Tiers"]
       (map (fn [t]
              ^{:key t}
              [:span.tier-key-item
               [:i.tier-swatch {:style {:background (str "hsl(" (tier-hue t n-tiers) ", 68%, 60%)")}}]
               (str "T" t)])
            tiers)])))

;; ---- filters ----

(def ^:private positions ["QB" "RB" "WR" "TE" "K" "DST"])

(defn- pos-filter []
  (let [active @(rf/subscribe [:pos-filter])
        pos-button-fn (fn [pos]
                        ^{:key pos}
                        [:button {:class (when (= active pos) "on")
                                  :on-click #(rf/dispatch [:set-pos-filter pos])} pos])]
    [:div.pos-filter
     [:button {:class (when (nil? active) "on") :on-click #(rf/dispatch [:set-pos-filter nil])} "All"]
     (map pos-button-fn positions)]))

(defn- search-box []
  (let [q @(rf/subscribe [:search])]
    [:input.search {:type "text" :placeholder "Search player or team…"
                    :value q
                    :on-change #(rf/dispatch [:set-search (.. % -target -value)])}]))

;; ---- board ----

(defn board []
  (let [players     @(rf/subscribe [:board-players])
        cols        @(rf/subscribe [:visible-columns])
        sort        @(rf/subscribe [:sort])
        nominated   @(rf/subscribe [:nominated-id])
        ;; color rows by tier only when filtered to a single position
        color-tier? (some? @(rf/subscribe [:pos-filter]))
        n-tiers     (reduce max 1 (keep :tier players))]
    [:div.board-wrap
     [:div.board-controls
      [:div.filters [pos-filter] [search-box]]
      (when color-tier? [tier-key players n-tiers])]
     [:div.table-scroll
      [:table.board
       [:thead [:tr 
                (map (fn [col] ^{:key (:key col)} 
                       [header-cell col sort]) cols)]]
       [:tbody
        (map (fn [p]
               ^{:key (:player-id p)}
               [player-row p cols nominated color-tier? n-tiers])
             players)]]]]))
