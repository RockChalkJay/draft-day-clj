(ns draft-day.views.compare
  "Two players side by side, on the two horizons the waiver board carries.

  The tile exists because the horizons routinely *disagree*: a player who is
  better this Sunday and worse from here is a completely different decision from
  one who is better at both, and two columns of numbers hide that behind mental
  arithmetic. Every row is anchored at a centre line and leans toward whoever
  leads, so agreement reads as one direction and a split reads as a zigzag.

  A bar is only drawn where one side is actually *better*. Games played is
  sample size and Bid is a price — neither has a winner, and a bar there would
  assert a verdict the number does not carry. Same restraint the board applies
  to `:trend` and `:injury-risk`.

  It floats without a backdrop. The interaction is holding one player and
  clicking down the board through challengers, and a scrim swallows exactly
  those clicks — see `.cmp-float` in styles.css."
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [draft-day.views.board :as board]
            [draft-day.views.controls :as controls]
            [draft-day.views.util :as util]
            [draft-day.views.waivers :as waivers]))

;; ---- the comparison itself ----

(defn opportunity-per-game
  "Targets plus carries per game, or nil. Volume rather than points, and the
  same measure `waiver/trend` is a ratio of — a role is what a claim is buying."
  [{:nflverse/keys [season-to-date]}]
  (let [{:keys [games usage]} season-to-date]
    (when (and games (pos? games))
      (/ (+ (or (:targets usage) 0) (or (:carries usage) 0)) games))))

(defn lean
  "How far a row leans and which way: `{:side :l|:r :frac 0..1}`, or nil.

  nil when either value is missing or the two are equal — no bar at all rather
  than a bar of zero, so an empty track never has to mean two different things.
  `better` is `:lower` for a metric that inverts (injury risk).

  The gap is scaled against the larger magnitude and doubled, because the
  interesting comparisons are close ones: an honest 5% gap rendered at 5% of
  half a track is invisible, and a row nobody can read is a row that is not
  there."
  [a b better]
  (when (and (number? a) (number? b) (not= a b))
    (let [scale (max (abs a) (abs b))]
      (when (pos? scale)
        (let [a-wins? (if (= better :lower) (< a b) (> a b))]
          {:side (if a-wins? :l :r)
           :frac (min 1.0 (* 2.0 (/ (abs (- a b)) scale)))})))))

(defn ahead
  "Which of the two players leads on `f`, or nil when they tie or either is
  missing."
  [a b f]
  (let [va (f a) vb (f b)]
    (when (and (number? va) (number? vb) (not= va vb))
      (if (> va vb) a b))))

(defn on-bye? [p week]
  (and week (= week (:bye p)) (not (number? (:week-points p)))))

(defn reading-line
  "One sentence naming what the two horizons show.

  It never says which player to take: when they disagree there is no answer
  without knowing whether the manager is buying this Sunday or the rest of the
  year, and a tile that guessed would be confidently wrong half the time. It
  names the split and stops."
  [a b week]
  (let [bye (first (filter #(on-bye? % week) [a b]))
        wk  (ahead a b :week-points)
        ros (ahead a b :ros-points)]
    (cond
      bye [:span [:b (:player-name bye)] " is on bye this week."]

      (and wk ros (not= wk ros))
      [:span [:b (:player-name wk)] " projects higher this week; "
       [:b (:player-name ros)] " is the better rest-of-season hold."]

      (and wk ros)
      [:span [:b (:player-name wk)] " is ahead on both."]

      ros [:span [:b (:player-name ros)] " is ahead rest-of-season, and no weekly "
           "projection separates them."]

      :else nil)))

(def bands
  "The tile's three bands, in reading order: the question, the evidence for it,
  and what the claim costs.

  `:band` rather than slicing one flat list by index — the boundaries were
  `subvec`s, so inserting a metric anywhere above the last one silently moved
  a row into the wrong band and still rendered. This is the place a metric gets
  added, so it must be the place that says where the metric goes."
  [:horizon :evidence :claim])

(def rows
  "What the tile compares. `:bar?` false where neither side is better; `:big?`
  marks the two horizons, which are the question rather than the evidence."
  [{:band :horizon  :label "This week"      :f :week-points :big? true
    :fmt board/format-whole}
   {:band :horizon  :label "Rest of season" :f :ros-points  :big? true
    :fmt board/format-whole}
   {:band :evidence :label "Trend"          :f :trend :fmt waivers/format-trend}
   {:band :evidence :label "Opportunity / game" :f opportunity-per-game
    :fmt #(if (number? %) (.toFixed % 1) "–")}
   {:band :evidence :label "Games played"   :bar? false
    :f #(get-in % [:nflverse/season-to-date :games])
    :fmt #(if (number? %) % "–")}
   {:band :evidence :label "Injury risk"    :f :injury-risk :better :lower
    :fmt #(if (number? %) % "–")}
   {:band :claim    :label "Upgrade"        :f :upgrade
    :fmt #(if (number? %) (util/signed (js/Math.round %)) "–")}
   {:band :claim    :label "Bid"            :f :bid :bar? false
    :fmt #(if (number? %) (str "$" %) "–")}])

(def rows-by-band (group-by :band rows))

;; ---- rendering ----

(defn value-cell [side v fmt winner?]
  [:div {:class (str "cmp-v " (name side) (when winner? " win"))} (fmt v)])

(defn metric-row [{:keys [label f fmt better bar? big?] :or {bar? true}} a b]
  (let [va (f a)
        vb (f b)
        ;; The track is drawn only when both sides are numbers, so an empty one
        ;; means "even" and nothing else. Drawing it whenever the row *could*
        ;; compare made a preseason board of missing data look like four ties.
        track? (and bar? (number? va) (number? vb))
        lean   (when track? (lean va vb better))]
    [:div {:class (str "cmp-row" (when big? " big"))}
     [value-cell :l va fmt (= :l (:side lean))]
     [:div.cmp-mid
      [:div.cmp-lbl label]
      (when track?
        [:div.cmp-bar
         (when lean
           [:i {:class (name (:side lean))
                :style {:width (str (* 50.0 (:frac lean)) "%")}}])])]
     [value-cell :r vb fmt (= :r (:side lean))]]))

(defn face
  "Silhouette underneath, headshot on top. Same arrangement as `controls/face`
  and for its reason: a missing *or broken* image hides itself and falls through,
  so there is no load state to track."
  [headshot]
  [:div.cmp-face
   [controls/silhouette 40]
   (when headshot
     [:img {:src headshot :alt ""
            :on-error #(set! (.. % -target -style -display) "none")}])])

(defn player-head [p side headshot week]
  [:div {:class (str "cmp-who " (name side))}
   [face headshot]
   [:div
    [:div.cmp-name (:player-name p)]
    [:p.cmp-meta (util/pos-label p) " · " (or (:team p) "FA")
     " · " (waivers/week-matchup p week)]]])

(defn compare-tile []
  ;; Escape closes. `modal.cljs` has no key handling to borrow — this tile is the
  ;; first thing here that opens over the page without a scrim to click away.
  (r/with-let [on-key (fn [e]
                        (when (= "Escape" (.-key e))
                          (rf/dispatch [:compare-clear])))
               _      (.addEventListener js/document "keydown" on-key)]
    (let [players @(rf/subscribe [:compare-players])
          week    (:week @(rf/subscribe [:waiver-meta]))
          by-id   @(rf/subscribe [:universe-by-id])
          ;; A waiver row carries no `[:ids :sleeper]`, so the headshot comes off
          ;; the universe the browser already holds — the same indirection
          ;; `player-stats/nominated-stats` uses, and for the same reason.
          shot    (fn [p] (util/headshot-url (get by-id (:player-id p))))
          [a b]   players]
      (when a
        [:div.cmp-float
         [:div.cmp-tile
          [:div.cmp-top
           [:span.cmp-label "Compare"]
           [:button.cmp-close {:on-click #(rf/dispatch [:compare-clear])
                               :title "Close (Esc)"
                               :aria-label "Close comparison"} "✕"]]
          [:div.cmp-head
           [player-head a :l (shot a) week]
           [:div.cmp-week (if week (str "Week " week) "Rest of season")]
           (if b
             [player-head b :r (shot b) week]
             [:div.cmp-empty "Pick another player to compare."])]
          (when b
            [:<>
             [:div.cmp-band
              (for [r (rows-by-band :horizon)]
                ^{:key (:label r)} [metric-row r a b])
              (when-let [line (reading-line a b week)]
                [:p.cmp-read line])]
             [:div.cmp-band
              (for [r (rows-by-band :evidence)]
                ^{:key (:label r)} [metric-row r a b])]
             [:div.cmp-band
              (for [r (rows-by-band :claim)]
                ^{:key (:label r)} [metric-row r a b])
              ;; From whichever side is a free agent — with a rostered player on
              ;; the left, only the right one carries a claim.
              (when-let [drop (some :drop-candidate [a b])]
                [:p.cmp-note
                 "A claim costs a roster spot. Yours would come from "
                 (:player-name drop) "."])]])]]))
    (finally
      (.removeEventListener js/document "keydown" on-key))))
