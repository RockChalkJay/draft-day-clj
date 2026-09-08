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

  The bands run in the order a decision is made: the two horizons are the
  question, what a claim costs and gains is the answer, and the evidence is why.
  The answer used to sit last, which was fine at nine rows and is not at twelve
  — it ended up below the fold on a laptop, under the band it is a conclusion of.

  A row whose metric is missing on *both* sides is dropped rather than dashed,
  and a band that empties out says why in a sentence. This is not the same
  restraint as the bar's: a dash is already legible as \"the board cannot say\",
  but a dozen of them stacked is punctuation rather than a comparison. One side
  missing keeps the row, because that asymmetry is itself the answer.

  It floats without a backdrop. The interaction is holding one player and
  clicking down the board through challengers, and a scrim swallows exactly
  those clicks — see `.cmp-float` in styles.css."
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [draft-day.confidence :as confidence]
            [draft-day.db :as db]
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
  "The tile's three bands, in reading order: the question, what a claim would
  cost and gain, then the evidence for both. See the ns docstring for why the
  answer moved above the evidence.

  `:band` rather than slicing one flat list by index — the boundaries were
  `subvec`s, so inserting a metric anywhere above the last one silently moved
  a row into the wrong band and still rendered. This is the place a metric gets
  added, so it must be the place that says where the metric goes."
  [:horizon :claim :evidence])

(defn claim-points
  "A signed points difference for the claim band, where 0 is a real answer.

  Deliberately not `util/signed`, which dashes zero out because a board column
  has no bar beside it to disagree with. Here there is one, and a dash sitting
  next to a drawn bar reads as missing data. Zero means he would never crack the
  lineup, which is the most common true thing this row has to say."
  [n]
  (if (number? n)
    (let [r (js/Math.round n)]
      (if (zero? r) "0" (util/signed r)))
    "–"))

(defn plain
  "A number as itself, for a rank or a count that neither rounds nor scales."
  [n]
  (if (number? n) n "–"))

(def rows
  "What the tile compares — everything the board can say about the two players.

  This is the only place both sides are on screen at once, so a metric the row
  carries and this list omits is a comparison the manager has to make by
  scrolling between two lines of a table. `db/waiver-column-catalog` is the same
  list for the board and the two drift apart silently: Lineup became the column
  the board *sorts by* while the tile still did not mention it. `compare-test`
  fails on a catalog key that has not been given a decision here.

  Three deliberate omissions. Targets and carries, because
  `opportunity-per-game` is both of them over games played and the raw counts
  add a scale rather than a fact. Bye and Opp, because a schedule has no winner
  and Opp is already in the head. And the injury designation, which is a word
  among tabular numbers — it is a chip on the name, where the board puts it too.

  `:bar?` false where neither side can be better, `:better :lower` where the
  metric inverts, `:big?` on the two horizons, and `:tip` on a row whose label
  cannot carry its own definition."
  [{:band :horizon  :label "This week"      :f :week-points :big? true
    :fmt board/format-whole :sub week-rank-label :calibrated? true}
   {:band :horizon  :label "Rest of season" :f :ros-points  :big? true
    :fmt board/format-whole}
   ;; The same horizon in the only unit that survives a cross-position pair: a
   ;; quarterback's 190 and a tight end's 120 are not a comparison and their
   ;; VORPs are. It sits under Rest of season rather than among the evidence
   ;; because it is that row restated, not support for it. nil for K and DST,
   ;; which have no replacement level — see `db/vorp-sort-key`.
   {:band :horizon  :label "Over replacement" :f :ros-vorp :fmt board/format-whole
    :tip (str "Rest-of-season points above a replacement player at his position"
              " — the one number that compares a QB to a TE")}
   ;; Lineup leads the claim band because it leads the board and prices the bid.
   ;; Upgrade under it is the bench question, which is a different one — see
   ;; `db/waiver-rank-key` for why both are kept rather than one replacing
   ;; the other.
   {:band :claim    :label "Lineup gain"    :f :lineup-upgrade :fmt claim-points
    :tip (str "Rest-of-season points this claim adds to your starting lineup,"
              " after the drop. 0 means he would never start")}
   {:band :claim    :label "Upgrade"        :f :upgrade :fmt claim-points
    :tip (str "Rest-of-season points over the player you would drop, whether or"
              " not he would ever start")}
   {:band :claim    :label "Bid"            :f :bid :bar? false
    :fmt #(if (number? %) (str "$" %) "–")}
   {:band :evidence :label "Trend"          :f :trend :fmt waivers/format-trend
    :tip (str "Recent opportunity per game against his season rate — above"
              " 1.0× means the role is growing")}
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
    :fmt board/format-one-decimal
    :tip "Points per game over the last three weeks, under your league's rules"}
   {:band :evidence :label "Opportunity / game" :f opportunity-per-game
    :fmt #(if (number? %) (.toFixed % 1) "–")
    :tip "Targets plus carries per game this season"}
   {:band :evidence :label "Games played"   :bar? false
    :f #(get-in % [:nflverse/season-to-date :games])
    :fmt plain}
   ;; Both preseason, so neither is evidence about now — they are what the
   ;; season so far is disagreeing with, which is the whole waiver-wire case.
   {:band :evidence :label "Preseason"      :f :points :fmt board/format-whole
    :tip (str "What he was projected for before the season — the number the"
              " rest-of-season line is correcting")}
   {:band :evidence :label "Expert rank"    :f :fantasypros/ecr :better :lower
    :fmt plain
    :tip "FantasyPros expert consensus rank, preseason. Lower is better"}
   {:band :evidence :label "Injury risk"    :f :injury-risk :better :lower
    :fmt plain
    :tip (str "Games missed per season over the last three, 1 (durable) to"
              " 5 (fragile)")}])

(def rows-by-band (group-by :band rows))

;; ---- rendering ----

(defn value-cell [side v fmt winner? sub]
  [:div {:class (str "cmp-v " (name side) (when winner? " win"))}
   (fmt v)
   (when sub [:span.cmp-sub sub])])

(defn metric-row
  "One row. `sep` is `confidence/separation` for the pair, and only a row marked
  `:calibrated?` consults it — the rest have no measurement behind them."
  [{:keys [label f fmt better bar? big? sub tip calibrated?] :or {bar? true}} a b sep]
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
      ;; `title` rather than a second line of type: half these labels are
      ;; self-evident, and a definition under each one would put more words on
      ;; the tile than numbers. A nil leaves the attribute off entirely.
      [:div.cmp-lbl {:title tip} label]
      (when track?
        [:div.cmp-bar
         (cond
           even? [:i.even]
           lean  [:i {:class (name (:side lean))
                      :style {:width (str (* 50.0 (:frac lean)) "%")}}])])]
     [value-cell :r vb fmt (= :r (:side lean)) (when sub (sub b))]]))

(defn row-has-value?
  "Does either side carry this row's metric?"
  [{:keys [f]} a b]
  (or (some? (f a)) (some? (f b))))

(defn band
  "One band's rows with the empty ones dropped, or nil when none survive.

  nil rather than an empty seq, because the caller says something different for
  a band with nothing to show — see the claim band in `compare-tile`."
  [k a b sep]
  (when-let [rs (seq (filter #(row-has-value? % a b) (rows-by-band k)))]
    (for [r rs] ^{:key (:label r)} [metric-row r a b sep])))

(defn status-chip
  "The current injury designation beside the name, or nil.

  Abbreviated to fit the head's column — the full word is on the hover — and
  only the serious set takes `--warn`, so a Questionable does not shout like an
  IR. `db/serious-injury?` is the one copy of that set."
  [p]
  (when-let [st (:sleeper/injury-status p)]
    [:span {:class (str "cmp-status" (when (db/serious-injury? st) " serious"))
            :title st}
     (if (> (count st) 3) (subs st 0 1) st)]))

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
    [:div.cmp-name (:player-name p) [status-chip p]]
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
            (let [claim (band :claim a b sep)]
              [:<>
               [:div.cmp-band
                (band :horizon a b sep)
                (when-let [line (reading-line a b week sep)]
                  [:p.cmp-read line])
                (when-let [line (separation-line a sep)]
                  [:p.cmp-cal line])]
               [:div.cmp-band
                (or claim
                    ;; The band empties on exactly one pair: two players the
                    ;; manager already holds, neither of whom can be claimed. A
                    ;; free agent beside a rostered player keeps its rows and
                    ;; dashes the side with no claim to make, which is the
                    ;; asymmetry `row-has-value?` is careful not to hide.
                    [:p.cmp-note "You hold both of these players, so there is "
                     "no claim to price."])
                ;; From whichever side is a free agent — with a rostered player
                ;; on the left, only the right one carries a claim.
                (when-let [drop (and claim (some :drop-candidate [a b]))]
                  [:p.cmp-note
                   "A claim costs a roster spot. Yours would come from "
                   (:player-name drop) "."])]
               (when-let [ev (band :evidence a b sep)]
                 [:div.cmp-band ev])]))]]))
    (finally
      (.removeEventListener js/document "keydown" on-key))))
