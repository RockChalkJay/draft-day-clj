(ns draftday.views.board
  (:require [re-frame.core :as rf]
            [draftday.db :as db]))

;; ---- cell formatting ----

(defn- money [n] (if (and (number? n) (pos? n)) (str "$" n) "–"))
(defn- n0 [n] (if (number? n) (js/Math.round n) "–"))
(defn- n1 [n] (if (number? n) (.toFixed n 1) "–"))

(def ^:private tier-colors {1 "#3ddc97" 2 "#79cf86" 3 "#e2c05a" 4 "#e0955a" 5 "#e06a6a"})
(defn- tier-color [t] (get tier-colors (min 5 (max 1 (or t 1))) "#8b93a5"))

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

(defn- cell [k p]
  (case k
    :rank     [:td.num.muted (:rank p)]
    :name     [:td.name (:player-name p) (divergence-badge p)]
    :team     [:td.muted (:team p)]
    :position [:td [:span.pill (:position p)]]
    :tier     (let [t (:tier p)]
                [:td [:span.tier-badge {:style {:background (tier-color t)}}
                      t (when (and (:tcm p) (> (:tcm p) 1.1)) " 🚨")]])
    :worth    [:td.num.bold (money (:worth p))]
    :value    [:td.num.muted (money (:value p))]
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

(defn- player-row [p cols nominated]
  [:tr {:class (when (= nominated (:player-id p)) "selected")
        :on-click #(rf/dispatch [:set-nominated (:player-id p)])}
   (for [col cols] (with-meta (cell (:key col) p) {:key (str (:key col))}))])

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
  (let [players   @(rf/subscribe [:board-players])
        cols      @(rf/subscribe [:visible-columns])
        sort      @(rf/subscribe [:sort])
        nominated @(rf/subscribe [:nominated-id])]
    [:div.board-wrap
     [:div.board-controls [pos-filter] [search-box]]
     [:div.table-scroll
      [:table.board
       [:thead [:tr (for [col cols] ^{:key (:key col)} [header-cell col sort])]]
       [:tbody
        (for [p players]
          ^{:key (:player-id p)}
          [player-row p cols nominated])]]]]))
