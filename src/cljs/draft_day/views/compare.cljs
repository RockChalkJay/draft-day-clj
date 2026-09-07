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

  The weekly row takes that restraint one step further, because the bar there
  used to assert something measurement does not support: two players a few
  ranks apart are a coin flip (see `draft-day.confidence`). So its track has
  three states, and they have to stay distinguishable — no track at all when
  there is no weekly line, a centred muted fill when the board cannot separate
  them, and a directional accent fill only when it can. Collapsing the first
  two is the bug #52 shipped once, where missing data rendered as a tie.

  It floats without a backdrop. The interaction is holding one player and
  clicking down the board through challengers, and a scrim swallows exactly
  those clicks — see `.cmp-float` in styles.css."
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [draft-day.confidence :as confidence]
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
  names the split and stops.

  `sep` is `confidence/separation` for the pair, and it is required rather than
  defaulted — see the coin-flip branch for what a `sep`-less call would print."
  [a b week sep]
  (let [bye   (first (filter #(on-bye? % week) [a b]))
        ;; A weekly lead the measurement calls a coin flip is not a lead. This
        ;; sentence used to read it straight off `:week-points`, so on a close
        ;; pair the tile printed "X is ahead on both" directly above "Too close
        ;; to call" — two sentences disagreeing about the same number. Worse was
        ;; the split, which framed a buy-Sunday-or-hold decision around a weekly
        ;; difference that is not there.
        even? (confidence/coin-flip? sep)
        wk    (when-not even? (ahead a b :week-points))
        ros   (ahead a b :ros-points)]
    (cond
      bye [:span [:b (:player-name bye)] " is on bye this week."]

      (and wk ros (not= wk ros))
      [:span [:b (:player-name wk)] " projects higher this week; "
       [:b (:player-name ros)] " is the better rest-of-season hold."]

      (and wk ros)
      [:span [:b (:player-name wk)] " is ahead on both."]

      ;; Deliberately not merged with the branch below, though both end at
      ;; rest-of-season. That one means *neither player has a weekly line* — a
      ;; bye, or nobody projects him — and says so. This one means the line
      ;; exists and does not discriminate, which `separation-line` states
      ;; underneath in the terms it was measured in. Saying it twice, in two
      ;; vocabularies, is what this branch exists to avoid.
      (and ros even?)
      [:span [:b (:player-name ros)] " is ahead rest-of-season."]

      ros [:span [:b (:player-name ros)] " is ahead rest-of-season, and no weekly "
           "projection separates them."]

      :else nil)))

(defn separation-line
  "What the weekly rank gap is worth, said as measured rather than as a rate.

  nil where `confidence/separation` declines to answer, which is most of the
  board — a bye, a cross-position pair, a DST. Saying nothing is the point:
  this only speaks where there is a measurement behind it."
  [a sep]
  (when sep
    (let [pos  (:position a)
          gap  (:gap sep)
          apart (str gap " " pos (when (> gap 1) "s") " apart")]
      ;; Three sentences of one shape: the verdict, the gap, then what the gap
      ;; was measured to be worth. Parallel because they appear in the same slot
      ;; and a manager reads them as one another's alternatives.
      (case (:level sep)
        :coin-flip [:span "Too close to call — " apart
                    ", which the weekly projection calls right about half the time."]
        :slight    [:span "A slight edge — " apart
                    ", which the weekly projection calls right closer to six times in ten."]
        :clear     [:span "A clear gap — " apart
                    ", which the weekly projection has usually called right."]))))

(defn week-rank-label
  "\"WR8\" under a weekly number, or nil. The ordinal carries its own scale for
  the reason the board's Wk# column does — and it is what keeps the row legible
  on exactly the comparisons where the bar deliberately says nothing."
  [p]
  (when-let [n (:week-pos-rank p)]
    (str (:position p) n)))

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
    :fmt board/format-whole :sub week-rank-label :calibrated? true}
   {:band :horizon  :label "Rest of season" :f :ros-points  :big? true
    :fmt board/format-whole}
   {:band :evidence :label "Trend"          :f :trend :fmt waivers/format-trend}
   ;; What the role has been worth, against what the projection expects of it.
   ;; The disagreement is the waiver-wire buy, so it belongs beside the horizons
   ;; rather than folded into them.
   ;;
   ;; It keeps a directional bar, and `draft-day.confidence` deliberately does
   ;; not reach it: form is *realized* production, not a forecast. A player who
   ;; scored more over the last three weeks did outscore the other, and the bar
   ;; says that happened rather than predicting it will. The calibration exists
   ;; because a weekly projection is a claim about a game nobody has played.
   {:band :evidence :label "Form / game"    :f :form-points
    :fmt board/format-one-decimal}
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

(defn value-cell [side v fmt winner? sub]
  [:div {:class (str "cmp-v " (name side) (when winner? " win"))}
   (fmt v)
   (when sub [:span.cmp-sub sub])])

(defn metric-row
  "One row. `sep` is `confidence/separation` for the pair, and only a row marked
  `:calibrated?` consults it — the rest have no measurement behind them."
  [{:keys [label f fmt better bar? big? sub calibrated?] :or {bar? true}} a b sep]
  (let [va (f a)
        vb (f b)
        ;; The track is drawn only when both sides are numbers, so an empty one
        ;; means "even" and nothing else. Drawing it whenever the row *could*
        ;; compare made a preseason board of missing data look like four ties.
        track? (and bar? (number? va) (number? vb))
        ;; A measured tie, which is not the same as no data and must not look
        ;; like it. The needle rests at zero rather than the track being absent.
        even?  (and calibrated? (confidence/coin-flip? sep))
        lean   (when (and track? (not even?)) (lean va vb better))]
    [:div {:class (str "cmp-row" (when big? " big"))}
     [value-cell :l va fmt (= :l (:side lean)) (when sub (sub a))]
     [:div.cmp-mid
      [:div.cmp-lbl label]
      (when track?
        [:div.cmp-bar
         (cond
           even? [:i.even]
           lean  [:i {:class (name (:side lean))
                      :style {:width (str (* 50.0 (:frac lean)) "%")}}])])]
     [value-cell :r vb fmt (= :r (:side lean)) (when sub (sub b))]]))

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
          [a b]   players
          sep     (when b (confidence/separation a b))]
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
                ^{:key (:label r)} [metric-row r a b sep])
              (when-let [line (reading-line a b week sep)]
                [:p.cmp-read line])
              (when-let [line (separation-line a sep)]
                [:p.cmp-cal line])]
             [:div.cmp-band
              (for [r (rows-by-band :evidence)]
                ^{:key (:label r)} [metric-row r a b sep])]
             [:div.cmp-band
              (for [r (rows-by-band :claim)]
                ^{:key (:label r)} [metric-row r a b sep])
              ;; From whichever side is a free agent — with a rostered player on
              ;; the left, only the right one carries a claim.
              (when-let [drop (some :drop-candidate [a b])]
                [:p.cmp-note
                 "A claim costs a roster spot. Yours would come from "
                 (:player-name drop) "."])]])]]))
    (finally
      (.removeEventListener js/document "keydown" on-key))))
