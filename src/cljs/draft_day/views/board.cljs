(ns draft-day.views.board
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [re-frame.core :as rf]
            [draft-day.db :as db]
            [draft-day.views.util :as util]))

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
  "Full-strength color for tier `t` of `n` — the row's left stripe and the rule
  drawn across a tier boundary."
  [t n]
  (let [{:keys [l]} (tier-bands (tier-band t))]
    (str "oklch(" l " " tier-chroma " " (.toFixed (tier-hue t n) 1) ")")))

(defn tier-fill
  "Row background for tier `t` of `n`: `tier-color` laid over the board at the
  band's strength."
  [t n]
  (let [{:keys [l alpha]} (tier-bands (tier-band t))]
    (str "oklch(" l " " tier-chroma " " (.toFixed (tier-hue t n) 1) " / " alpha ")")))

;; ---- injury-risk bar ----

;; The Risk cell is a glyph, not a number: five segments, filled to the level.
;; Two consequences are load-bearing rather than decorative.
;;
;; Fill *length* and hue encode the same number, so the scale still reads for
;; someone who cannot separate the hues — which a single coloured dot would not.
;; And because the cell carries no text at all, the `title`/`aria-label` is the
;; only thing a hover or a screen reader has to go on, so it is never omitted.
;;
;; Hues come from `tier-hue` at (level, 5) rather than a second green-to-red ramp
;; invented here. `tier-color` itself is deliberately not reused: its pale/deep
;; banding exists to hold *adjacent table rows* apart and means nothing on a
;; five-step scale read one cell at a time.

(def risk-levels 5)
(def ^:private risk-lightness 0.70)
(def ^:private risk-chroma 0.14)

(defn risk-color
  "Fill colour for risk `level` — green at 1, red at `risk-levels`."
  [level]
  (str "oklch(" risk-lightness " " risk-chroma " "
       (.toFixed (tier-hue level risk-levels) 1) ")"))

(def risk-words
  "The word for each level. The board column has no room for these — that is why
  it is a bar — but the on-the-block tile does, and a legend has to say what the
  five segments mean somewhere."
  {1 "Durable" 2 "Sturdy" 3 "Average" 4 "Fragile" 5 "Brittle"})

(defn risk-bar
  "The five-segment bar for `level`. Every filled segment takes the *level's*
  colour rather than its own position's, so a risk-4 bar is four orange segments
  and not a gradient — length and hue then say one thing, twice."
  [level]
  [:span.risk-bar
   (for [i (range 1 (inc risk-levels))]
     ^{:key i}
     [:span.risk-seg {:class (when (<= i level) "on")
                      :style (when (<= i level) {:background (risk-color level)})}])])

;; ---- cell formatting ----

(defn format-whole [n] (if (number? n) (js/Math.round n) "–"))
(defn format-one-decimal [n] (if (number? n) (.toFixed n 1) "–"))
(defn pct
  "A rate as a percentage. Plain `defn` so a test can reach it — the ×100 and
  the dash for a rookie's missing rate are the two things worth pinning."
  [n]
  (if (number? n) (str (.toFixed (* 100 n) 1) "%") "–"))

(defn prior-season-title
  "Tooltip naming the season a usage number came from. The column header can
  only say \"last season\" — it is one static string for the whole board — but
  the row knows the year, so the cell can say it outright."
  [p]
  (when-let [y (:nflverse/prior-season p)] (str y " season")))

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
    :position [:td [:span.pill (util/pos-label p)]]
    :worth    [:td.num.bold (util/money (:worth p))]
    :value    [:td.num.muted (util/money (:value p))]
    :espn-value [:td.num.muted (util/money-rnd (:espn/auction-value p))]
    :fp-aav   [:td.num.muted (util/money (:fantasypros/aav p))]
    :market   [:td.num.muted (util/money (:market p))]
    ;; Shared with the on-the-block tile, which renders the same difference with
    ;; a "$" — a unit that would be noise repeated down two hundred rows, and the
    ;; only thing that differs between the two.
    :edge     [:td.num {:class (util/sign-class (:edge p))} (util/signed (:edge p))]
    :adp      [:td.num (if-let [a (:sleeper/adp p)] (format-one-decimal a) "–")]
    :proj     [:td.num (format-whole (:points p))]
    :ceiling  [:td.num.good (format-whole (:ceiling p))]
    :floor    [:td.num.muted (format-whole (:floor p))]
    :vorp     [:td.num (format-whole (:vorp p))]
    :ecr      [:td.num (or (:fantasypros/ecr p) "–")]
    ;; already resolved to the active scale by :board-players
    :tier     [:td.num (or (:tier p) "–")]
    :fp-tier  [:td.num.muted (or (:fantasypros/ecr-tier p) "–")]
    ;; Usage reads as context, not as live valuation, so it stays muted like
    ;; :value and :adp. A rookie has no prior-season row at all, which is the
    ;; nil these formatters already render as a dash.
    :prior-tgt     [:td.num.muted {:title (prior-season-title p)} (format-whole (:nflverse/prior-targets p))]
    :prior-rec     [:td.num.muted {:title (prior-season-title p)} (format-whole (:nflverse/prior-receptions p))]
    :prior-tgt-pct [:td.num.muted {:title (prior-season-title p)} (pct (:nflverse/prior-target-share p))]
    :proj-tgt      [:td.num.muted (format-whole (:espn/proj-targets p))]
    :proj-rec      [:td.num.muted (format-whole (:espn/proj-receptions p))]
    ;; A glyph with no number in it, so the title is the whole of its text.
    ;; A player the scale has no opinion about (a rookie) gets the board's dash,
    ;; never an empty five-segment track — an unfilled bar reads as "level 0,
    ;; safest", which is the opposite of "no history to judge".
    :risk     (let [lvl (:injury-risk p)
                    txt (or (:injury/reason p) "No injury history to judge")]
                [:td.risk {:title txt :aria-label txt}
                 (if lvl [risk-bar lvl] [:span.muted "–"])])
    :inj      (let [st (:sleeper/injury-status p)]
                [:td {:class (when (db/serious-injury? st) "inj-serious")}
                 (or st "–")])
    :bye      (let [clash? (db/board-bye-clash? (:position p) (:bye p)
                                                @(rf/subscribe [:my-bye-exposure]))]
                [:td {:class (when clash? "bye-clash")
                      :title (when clash?
                               (str "Bye clash — you already start a " (:position p)
                                    " on bye " (:bye p)))}
                 (or (:bye p) "–")])
    [:td "–"]))

;; ---- header + rows ----

;; A header is both a sort button and a drag handle. That is not a conflict:
;; a completed HTML5 drag suppresses the click, and a plain click never starts
;; one, so :on-click below still owns the tap.

(defn drop-side
  "Which edge of the hovered column to draw the insertion line on, given the
  order the manager can actually see (`ks`, the visible keys). Dragging
  rightwards the column lands after the hovered one, leftwards before it — so
  the line has to follow the direction of travel or it points at the wrong gap."
  [ks dragging over]
  (let [idx (zipmap ks (range))
        from (idx dragging)
        to   (idx over)]
    (when (and from to (not= from to))
      (if (< from to) "drop-after" "drop-before"))))

(defn header-cell [col sort drag ks]
  (let [k (:key col)
        d (db/columns-by-key k)
        active? (= (:key sort) k)
        {:keys [dragging over]} @drag]
    [:th {:on-click #(rf/dispatch [:set-sort k])
          :title (:tooltip d)
          :draggable true
          :on-drag-start (fn [e]
                           (util/column-drag-start! e k)
                           (reset! drag {:dragging k :over nil}))
          :on-drag-over  (fn [e]
                           (.preventDefault e)
                           (swap! drag assoc :over k))
          ;; only when the pointer left the cell outright — crossing into the
          ;; sort arrow inside it fires dragleave too, and clearing on that
          ;; blinks the insertion line off until the next dragover
          :on-drag-leave (fn [e]
                           (when (util/left-element? e)
                             (swap! drag update :over (fn [o] (when-not (= o k) o)))))
          :on-drop       (fn [e]
                           (.preventDefault e)
                           (rf/dispatch [:move-column-onto (:dragging @drag) k])
                           (reset! drag {}))
          ;; A drag abandoned off the row still ends, so nothing stays lit.
          :on-drag-end   #(reset! drag {})
          :class (->> [(when active? "sorted")
                       (when (= dragging k) "dragging")
                       (when (= over k) (drop-side ks dragging k))]
                      (filter some?)
                      (str/join " "))}
     (:label d)
     [:span.sort-ind (cond (not active?) " ↕" (= -1 (:dir sort)) " ▼" :else " ▲")]]))

(defn board-header
  "The header row. Owns the transient drag state in a local atom — it is pointer
  state, not app state; only the committed reorder reaches app-db."
  [_cols _sort]
  (let [drag (r/atom {})]
    (fn [cols sort]
      (let [ks (mapv :key cols)]
        [:tr (map (fn [col]
                    ^{:key (:key col)}
                    [header-cell col sort drag ks])
                  cols)]))))

(defn- tier-style [t n]
  {"--tier-bg"   (tier-fill t n)
   "--tier-line" (tier-color t n)})

(defn player-row [p cols nominated n-tiers tier-start? show-bands]
  [:tr {:class (->> [(when (= nominated (:player-id p)) "selected")
                     (when show-bands "tier-row")
                     (when (and show-bands tier-start?) "tier-start")]
                    (filter some?)
                    (str/join " "))
        :style (when show-bands (tier-style (or (:tier p) 1) n-tiers))
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
        ;; Tier bands visible only when sorting by :rank to avoid distraction when
        ;; sorting by other columns.
        show-bands  (= (:key sort) :rank)
        ;; Rows are tier-coloured in every view now. They used to be coloured
        ;; only under a position filter, because :tier was per-position and a
        ;; tier 2 RB beside a tier 2 WR meant nothing; the board now carries an
        ;; overall scale too and `:board-players` picks the one that matches the
        ;; current filter, so both views have a coherent number to colour by.
        n-tiers     (reduce max 1 (keep :tier players))]
    [:div.board-wrap
     [:div.board-controls
      [:div.filters [pos-filter] [search-box]]]
     [:div.table-scroll
      [:table.board
       [:thead [board-header cols sort]]
       [:tbody
        (map (fn [p tier-start?]
               ^{:key (:player-id p)}
               [player-row p cols nominated n-tiers tier-start? show-bands])
             players (tier-starts players))]]]]))
