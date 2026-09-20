(ns draft-day.views.matchup
  "Your team against your opponent's, for the week being played.

  ONE ROW PER SEAT, FACING. The seat label runs down the middle and each side
  mirrors the other — Player · Proj · Actual | seat | Actual · Proj · Player —
  so the two teams' numbers meet at the centre and a reader compares two
  quarterbacks by looking across a few inches of one line, rather than holding
  a number in his head between two tables. Names sit at the outer edges, where
  a long one has room to run. It is the
  arrangement `views.compare` already uses for two players, one scale up — and
  the reason the alternative (two independent tables) was not taken is that the
  comparison is the whole point of the screen.

  PROJECTED IS MUTED AND ACTUAL IS BOLD. They are not the same kind of number: a
  projection is a claim about a game and an actual is a result, so the one that
  happened carries the weight. Before kickoff the bold column is empty, which is
  the honest look for a screen that has nothing to report yet.

  A DASH IS NOT A ZERO, and this is the one screen where that distinction is
  most easily lost. `:actual` is nil until the player's game starts (see
  `rankings.matchup`), so a dash means \"not yet\" and a 0.0 means he played and
  did nothing. Rendering the first as the second would turn a Sunday morning
  into nine bad performances.

  THE BENCH IS DRAWN, dimmed, under the starters. Without it the optimal-lineup
  panel names players who are nowhere on screen, and \"start T. Hill\" is not
  advice if the reader cannot see what T. Hill did.

  IT DOES NOT REFETCH TO SWITCH GAMES. The server values every roster in the
  league in one reply, so the picker is a filter over data already in hand —
  `rankings.matchup/matchup-board` explains why that is affordable."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [draft-day.db :as db]
            [draft-day.views.util :as util]
            [draft-day.views.waivers :as waivers]))


(defn fmt
  "One decimal, or a dash. Weekly points are small enough that rounding to whole
  numbers would collapse 8.4 and 12.6 into a comparison nobody can make."
  [v]
  (if (number? v) (.toFixed v 1) "–"))

(defn player-cell
  "A player's name, his NFL game, and the two numbers.

  `side` mirrors the layout: name, projection, actual on the left; actual,
  projection, name on the right, so both actuals sit against the centre. A
  bench player's meta leads with his own position, because a bench row has no
  shared seat label to say it. The name opens the detail modal by dispatching
  rather than by requiring it — the require cycle `core/app` warns about."
  [p side week]
  (let [nums [^{:key :p} [:div.mu-p (fmt (:week-points p))]
              ^{:key :a} [:div {:class (str "mu-a" (when-not (number? (:actual p)) " pending"))}
                          (fmt (:actual p))]]
        who  [:div.mu-who {:key :who}
              (if (:unvalued? p)
                [:span.muted {:title (str "No projection for id " (:player-id p))}
                 (:player-id p)]
                [:button.name-btn
                 {:on-click #(rf/dispatch [:show-modal {:kind :player-detail
                                                        :player-id (:player-id p)}])
                  :tab-index -1
                  :title "Player detail"}
                 (:player-name p)])
              (when (:parked? p) [:span.mu-meta {:title "IR or taxi"} "IR"])
              [:span.mu-meta (str (when (and (nil? (:slot p)) (:position p))
                                    (str (:position p) " · "))
                                  (waivers/week-matchup p week))]]]
    (into [:div {:class (str "mu-side " (name side))}]
          (if (= side :l) (cons who nums) (conj (vec (reverse nums)) who)))))

(defn column-head
  "The labels over both sides, in the same mirrored order as every row."
  []
  [:div.mu-hdr
   [:div.mu-side.l [:div.mu-who "Player"] [:div.mu-p "Proj"] [:div.mu-a "Actual"]]
   [:div.mu-slot "Pos"]
   [:div.mu-side.r [:div.mu-a "Actual"] [:div.mu-p "Proj"] [:div.mu-who "Player"]]])

(defn empty-cell
  "The other side of a row this team has nobody for.

  `seat?` is the whole distinction: an unfilled *seat* is a fact about a lineup
  and is said out loud, while one bench being shorter than the other is not — a
  bench is a list, not a set of positions, so labelling the shorter one's tail
  \"empty\" would invent a seat nobody has.

  Mirrored on `side` exactly as `player-cell` is, and for the same reason: the
  side's grid has three tracks in mirror order, so a lone name cell on the right
  lands in the 56px Actual column and the word is clipped under the wrong
  header."
  [side seat?]
  (let [nums [^{:key :p} [:div.mu-p] ^{:key :a} [:div.mu-a]]
        who  [:div.mu-who {:key :who} (when seat? [:span.mu-empty "empty"])]]
    (into [:div {:class (str "mu-side " (name side))}]
          (if (= side :l) (cons who nums) (conj (vec (reverse nums)) who)))))

(defn seat-row
  "One row, both sides. `l` and `r` may be nil — a bench shorter than the other,
  or a game with only one team in it."
  [slot l r week seat? key-part]
  ^{:key key-part}
  [:div.mu-row
   (if l [player-cell l :l week] [empty-cell :l seat?])
   [:div.mu-slot slot]
   (if r [player-cell r :r week] [empty-cell :r seat?])])


(defn team-head [t side]
  (let [mine? (= side :l)]
    [:div {:class (str "mu-team" (when-not mine? " r"))}
     [:div.mu-name
      (if mine?
        [:<> (:name t) [:span.mu-rec (db/record-label t)]]
        [:<> [:span.mu-rec (db/record-label t)] (:name t)])]
     [:div {:class (str "mu-score" (when-not (number? (:actual t)) " pending"))}
      (fmt (or (:official t) (:actual t)))]
     [:div.mu-proj (str "projected " (fmt (:projected t)))]]))

(defn optimal-panel
  "What the best legal lineup would have been, on whichever basis is selected.

  It says nothing at all when there is nothing to say — a lineup that is already
  optimal gets a sentence, not an empty list of swaps."
  [t basis side]
  (let [o     (get-in t [:optimal basis])
        label (if (= basis :actual) "actual points" "projection")]
    [:div {:class (str "mu-optbox" (when (= side :r) " r"))}
     [:div.mu-optline "Best legal lineup by " label ": "
      [:b (fmt (:total o))]]
     (if (seq (:in o))
       [:<>
        [:div.mu-swap
         (for [x (:in o)]
           ^{:key (str "in" (:player-id x))}
           [:div [:span.mu-in "▲ Start " (:player-name x)]
            " (" (:position x) ", " (fmt (:points x)) ")"])
         (for [x (:out o)]
           ^{:key (str "out" (:player-id x))}
           [:div [:span.mu-out "▼ Sit " (:player-name x)]
            " (" (:position x) ", " (fmt (:points x)) ")"])]
        [:div.mu-optline
         (if (= basis :actual)
           (str (fmt (:gain o)) " left on the bench.")
           (str "This lineup is " (fmt (:gain o)) " short of its best."))]]
       [:div.mu-swap "Nothing on the bench would help."])]))

(defn basis-toggle []
  (let [basis @(rf/subscribe [:optimal-basis])]
    [:span.mu-basis
     (for [[k label tip]
           [[:projected "Projected" "What the best lineup would be, by this week's projection — the version you can still act on"]
            [:actual "Actual" "What the best lineup would have been, by what was actually scored"]]]
       ^{:key k}
       [:button {:class (when (= basis k) "on")
                 :title tip
                 :on-click #(rf/dispatch [:set-optimal-basis k])}
        label])]))

(defn game-picker
  "Every game in the league, the manager's own first. A game with one team in it
  is labelled rather than hidden — see `subs/:matchup-games`."
  []
  (let [games    @(rf/subscribe [:matchup-games])
        selected @(rf/subscribe [:selected-matchup])]
    (when (seq games)
      [:select.mu-pick
       ;; Valued by a roster id, and kept a string: ids belong to the
       ;; provider, and one that is not base-10 would parse to NaN.
       {:value (str (first (:roster-ids selected)))
        :title "Which game to show"
        :on-change #(rf/dispatch [:set-matchup-pick (.. % -target -value)])}
       (for [g (sort-by (complement :mine?) games)]
         ^{:key (first (:roster-ids g))}
         [:option {:value (str (first (:roster-ids g)))}
          (str (str/join " vs " (:names g))
               (when (= 1 (count (:names g))) " — no opponent this week")
               (when (:mine? g) " (yours)"))])])))

(defn week-strip []
  (let [m      @(rf/subscribe [:matchup])
        status @(rf/subscribe [:matchup-status])]
    [:div.mu-strip
     [:span
      (if (:week m) (str "Week " (:week m)) "No week to show")
      ;; Dated for `waivers/week-note`'s reason: it revises through the week.
      (when-let [at (util/fetched-at-label (:week-fetched-at m))]
        (str " · projection updated " at))]
     [:span.grow
      [game-picker]
      [:span.muted "Optimal by"]
      [basis-toggle]
      [:button.secondary {:on-click #(rf/dispatch [:fetch-matchup])} "Refresh"]]
     (when status [:span.mu-status status])]))


(defn rows
  "Starters as seats, then a bench divider, then the bench.

  Both sides are walked to the longer length so an uneven pair still lines up
  seat for seat — the shorter side draws an empty cell rather than shifting
  everybody up."
  [l r week]
  (let [seat-rows (fn [ls rs seat? tag]
                    (map-indexed
                     (fn [i _]
                       (let [a (nth ls i nil) b (nth rs i nil)]
                         ;; Two benches pair by index, but a centre label would
                         ;; assert a shared seat neither player is in.
                         (seat-row (when seat? (or (:slot a) (:slot b) "–"))
                                   a b week seat? (str tag i))))
                     (range (max (count ls) (count rs)))))]
    [:<>
     [column-head]
     (seat-rows (:starters l) (:starters r) true "s")
     ;; Child order mirrors `player-cell`, or the totals do not line up with
     ;; the columns they are totalling.
     [:div.mu-tot
      [:div.mu-side.l [:div.mu-who "Starters"] [:div.mu-p (fmt (:projected l))]
       [:div.mu-a (fmt (:actual l))]]
      [:div.mu-slot]
      [:div.mu-side.r [:div.mu-a (fmt (:actual r))] [:div.mu-p (fmt (:projected r))]
       [:div.mu-who "Starters"]]]
     [:div.mu-sep "Bench"]
     (seat-rows (:bench l) (:bench r) false "b")]))

(defn matchup-view []
  (let [m      @(rf/subscribe [:matchup])
        [l r]  @(rf/subscribe [:matchup-sides])
        basis  @(rf/subscribe [:optimal-basis])
        status @(rf/subscribe [:matchup-status])
        week   (:week m)]
    [:div.mu-view
     [week-strip]
     (cond
       ;; Four states, not two: which one decides what to do next.
       (and (nil? m) status) [:div.mu-card [:p.muted status]]
       (nil? m)              [:div.mu-card [:p.muted "Loading this week's matchup…"]]
       (nil? l)              [:div.mu-card
                              [:p.muted "No matchup this week. Connect a league under "
                               "Settings, or check that your team is picked."]]
       :else
       [:div.mu-card
        [:div.mu-head
         [team-head l :l]
         [:div.mu-vs (if week (str "Week " week) "–")]
         (if r [team-head r :r] [:div.mu-team.r [:div.mu-name.muted "No opponent"]])]
        [rows l (or r {}) week]
        [:div.mu-opt
         [optimal-panel l basis :l]
         [:div.mu-slot]
         (when r [optimal-panel r basis :r])]])]))
