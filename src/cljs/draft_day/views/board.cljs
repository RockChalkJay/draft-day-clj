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
    ;; already resolved to the active strategy and scale by :board-players
    :tier     [:td.num (or (:tier p) "–")]
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

;; The one thing this palette has to do is make *adjacent* tiers look different,
;; since that is the boundary a manager reads a group off of. A smooth hue sweep
;; cannot, because it divides one fixed perceptual budget across however many
;; tiers a position turns out to have: the old green-to-red HSL ramp left
;; neighbouring fills on the 6-tier RB board 6.4 dE2000 apart — rgb(24,57,29) vs
;; (40,57,29) vs (56,57,29), three shades of one dark green — and a 7th tier
;; would have squeezed them further.
;;
;; So the two jobs are split. The hue ramp carries only the *ordering* (green =
;; tier 1, red = the below-replacement tail), and banding carries the
;; *separation*: consecutive tiers alternate between a pale, lightly-washed fill
;; and a deep, saturated one, which holds neighbours ~11 dE2000 apart no matter
;; how many tiers there are. Colors are OKLCH so equal hue steps look equal;
;; sRGB hue is badly compressed through green-to-yellow, which is exactly where
;; the old ramp went muddy. Fills stay dark enough to keep body text past 8:1.
;;
;; Custom properties do the work; styles.css consumes them.

(def ^:private tier-chroma 0.14)
(def tier-hue-best 150.0)
(def tier-hue-worst 22.0)

(def ^:private tier-untiered
  "The bucket for players the active strategy has no tier for — FantasyPros ranks
  about three quarters of the board, so under :ecr the rest land here.

  Chroma 0 on purpose: it sits off the hue ramp entirely, because a grey that
  read as 'greener than the last tier' would look like a tier. Paired with a
  dashed left stripe in styles.css so 'no tier' is not carried by colour alone."
  {:l 0.60 :alpha 0.10})

;; The alternating bands. :pale is a bright color laid on thin, :deep a dimmer
;; one laid on thick — they differ in lightness *and* in strength, so a pair
;; stays apart even where the two hues are close.
(def ^:private tier-bands
  {:pale {:l 0.84 :alpha 0.20}
   :deep {:l 0.62 :alpha 0.42}})

(defn tier-hue
  "Hue for 1-indexed tier `t` of `n`, ramping green (best) to red (worst)."
  [t n]
  (let [span (max 1 (dec n))
        f    (/ (dec (max 1 t)) (double span))]
    (+ tier-hue-best (* (- tier-hue-worst tier-hue-best) f))))

(defn tier-band
  "Which of the two alternating bands tier `t` sits in. Tier 1 is pale, so the
  best tier is also the brightest."
  [t]
  (if (odd? (max 1 t)) :pale :deep))

(defn tier-color
  "Full-strength color for tier `t` of `n` — the row's left stripe and its
  legend swatch. A nil tier is the untiered bucket's neutral grey."
  [t n]
  (if (nil? t)
    (str "oklch(" (:l tier-untiered) " 0 0)")
    (let [{:keys [l]} (tier-bands (tier-band t))]
      (str "oklch(" l " " tier-chroma " " (.toFixed (tier-hue t n) 1) ")"))))

(defn tier-fill
  "Row background for tier `t` of `n`: `tier-color` laid over the board at the
  band's strength. A nil tier is the untiered bucket's neutral grey."
  [t n]
  (if (nil? t)
    (str "oklch(" (:l tier-untiered) " 0 0 / " (:alpha tier-untiered) ")")
    (let [{:keys [l alpha]} (tier-bands (tier-band t))]
      (str "oklch(" l " " tier-chroma " " (.toFixed (tier-hue t n) 1) " / " alpha ")"))))

(defn- tier-style [t n]
  {"--tier-bg"   (tier-fill t n)
   "--tier-line" (tier-color t n)})

(defn- player-row [p cols nominated n-tiers tier-start?]
  [:tr {:class [(when (= nominated (:player-id p)) "selected")
                "tier-row"
                (when (nil? (:tier p)) "tier-untiered")
                (when tier-start? "tier-start")]
        :style (tier-style (:tier p) n-tiers)
        :on-click #(rf/dispatch [:set-nominated (:player-id p)])}
   (map (fn [{k :key}] ^{:key k}
          [cell k p]) cols)])

(defn tier-starts
  "For each row, whether it opens a new tier — i.e. its tier differs from the row
  above it. Drives the rule drawn across a tier boundary.

  Read off the rendered order rather than off the tier numbers, because the two
  are not the same thing: the board is sorted by whichever column the manager
  clicked, and sorting by Bye or Team interleaves tiers. The first row never
  opens a tier; the header's own bottom border already closes the top."
  [players]
  (map (fn [p prev] (and (some? prev) (not= (:tier p) (:tier prev))))
       players (cons nil players)))

;; ---- tier key ----

(defn- tier-key
  "Legend of the tier stripe colors, showing every tier present on the board,
  plus the untiered bucket when the active strategy has players it cannot tier."
  [players n-tiers]
  (let [tiers     (->> players (keep :tier) distinct sort)
        unranked? (some #(nil? (:tier %)) players)]
    (when (or (seq tiers) unranked?)
      [:div.tier-key
       [:span.tier-key-label "Tiers"]
       (map (fn [t]
              ^{:key t}
              [:span.tier-key-item
               [:i.tier-swatch {:style {:background (tier-color t n-tiers)}}]
               (str "T" t)])
            tiers)
       (when unranked?
         [:span.tier-key-item.tier-key-unranked
          [:i.tier-swatch {:style {:background (tier-color nil n-tiers)}}]
          "Unranked"])])))

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

(defn- tier-strategy-picker
  "Which technique the board tiers by. Switching is instant: the server ships
  every strategy at both scales with each recompute, so this dispatches no
  fetch."
  []
  (let [active @(rf/subscribe [:tier-strategy])]
    [:div.tier-strategy.seg
     [:span.seg-label "Tier by"]
     (map (fn [{:keys [key label tooltip]}]
            ^{:key key}
            [:button {:class (when (= active key) "on")
                      :title tooltip
                      :on-click #(rf/dispatch [:set-tier-strategy key])}
             label])
          db/tier-strategy-catalog)]))

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
        ;; Rows are tier-coloured in every view now. They used to be coloured
        ;; only under a position filter, because :tier was per-position and a
        ;; tier 2 RB beside a tier 2 WR meant nothing; the board now carries an
        ;; overall scale as well and `:board-players` picks the one that matches
        ;; the current filter, so both views have a coherent number to colour by.
        n-tiers     (reduce max 1 (keep :tier players))]
    [:div.board-wrap
     [:div.board-controls
      [:div.filters [pos-filter] [tier-strategy-picker] [search-box]]
      [tier-key players n-tiers]]
     [:div.table-scroll
      [:table.board
       [:thead [:tr 
                (map (fn [col] ^{:key (:key col)} 
                       [header-cell col sort]) cols)]]
       [:tbody
        (map (fn [p tier-start?]
               ^{:key (:player-id p)}
               [player-row p cols nominated n-tiers tier-start?])
             players (tier-starts players))]]]]))
