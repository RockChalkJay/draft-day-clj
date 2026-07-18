(ns draft-day.views.board
  (:require [re-frame.core :as rf]
            [draft-day.db :as db]))

;; ---- cell formatting ----

(defn- money [n] (if (and (number? n) (pos? n)) (str "$" n) "–"))
(defn- money0 [n] (if (and (number? n) (pos? n)) (str "$" (js/Math.round n)) "–"))
(defn- n0 [n] (if (number? n) (js/Math.round n) "–"))
(defn- n1 [n] (if (number? n) (.toFixed n 1) "–"))

(defn- cliff-marker [p]
  (when (and (:tcm p) (> (:tcm p) 1.1))
    [:span.badge {:title "Tier cliff — steep drop to the next player"} " 🚨"]))

(defn- divergence-badge
  "🔼 ceiling-play / 🛡 safe-floor when a player's Worth swings notably vs the
  active lens (worth-floor/worth-ceiling come from the API)."
  [p]
  (let [w  (:worth p) wf (:worth-floor p) wc (:worth-ceiling p)
        thr (when (number? w) (max 3 (* 0.15 w)))]
    (cond
      (and thr (number? wc) (>= (- wc w) thr))
      [:span.badge.up {:title "Ceiling play — worth more under the Ceiling lens"} " 🔼"]
      (and thr (number? wf) (>= (- wf w) thr))
      [:span.badge.safe {:title "Safe floor — worth more under the Floor lens"} " 🛡"])))

(defn- sleeper-badge
  "💤 when the player is on any FantasyPros positional sleeper list."
  [p]
  (when (:fantasypros/sleeper? p)
    [:span.badge {:title "FantasyPros sleeper"} " 💤"]))

(defn- cell [k p]
  (case k
    :rank     [:td.num.muted (:rank p)]
    :name     [:td.name (:player-name p) (cliff-marker p) (divergence-badge p) (sleeper-badge p)]
    :team     [:td.muted (:team p)]
    :position [:td [:span.pill (:position p)]]
    :worth    [:td.num.bold (money (:worth p))]
    :value    [:td.num.muted (money (:value p))]
    :espn-value [:td.num.muted (money0 (:espn/auction-value p))]
    :fp-aav   [:td.num.muted (money (:fantasypros/aav p))]
    :market   [:td.num.muted (money (:market p))]
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
    :inj      [:td (or (:sleeper/injury-status p) "–")]
    :bye      [:td (or (:bye p) "–")]
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

(defn- player-row [p cols nominated color-tier?]
  [:tr {:class [(when (= nominated (:player-id p)) "selected")
                ;; tier row-coloring only when filtered to a single position
                (when color-tier? (str "tier-row-" (min 6 (or (:tier p) 1))))]
        :on-click #(rf/dispatch [:set-nominated (:player-id p)])}
   (for [col cols] (with-meta (cell (:key col) p) {:key (str (:key col))}))])

;; ---- tier key ----

;; Matches the tier-row stripe colors in styles.css.
(def ^:private tier-stripe-colors
  {1 "#34e29a" 2 "#4aa8ff" 3 "#f2c53d" 4 "#f57e34" 5 "#f0555f" 6 "#b083f0"})

(defn- tier-key
  "Legend of the tier stripe colors, showing only the tiers present on the board."
  [players]
  (let [tiers (->> players (keep :tier) (map #(min 6 %)) distinct sort)]
    (when (seq tiers)
      [:div.tier-key
       [:span.tier-key-label "Tiers"]
       (for [t tiers]
         ^{:key t}
         [:span.tier-key-item
          [:i.tier-swatch {:style {:background (get tier-stripe-colors t)}}]
          (str "T" t)])])))

;; ---- filters ----

(def ^:private positions ["QB" "RB" "WR" "TE" "K" "DST"])

(defn- pos-filter []
  (let [active @(rf/subscribe [:pos-filter])]
    [:div.pos-filter
     [:button {:class (when (nil? active) "on") :on-click #(rf/dispatch [:set-pos-filter nil])} "All"]
     (for [pos positions]
       ^{:key pos}
       [:button {:class (when (= active pos) "on")
                 :on-click #(rf/dispatch [:set-pos-filter pos])} pos])]))

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
        color-tier? (some? @(rf/subscribe [:pos-filter]))]
    [:div.board-wrap
     [:div.board-controls [pos-filter] [search-box]]
     (when color-tier? [tier-key players])
     [:div.table-scroll
      [:table.board
       [:thead [:tr (for [col cols] ^{:key (:key col)} [header-cell col sort])]]
       [:tbody
        (for [p players]
          ^{:key (:player-id p)}
          [player-row p cols nominated color-tier?])]]]]))
