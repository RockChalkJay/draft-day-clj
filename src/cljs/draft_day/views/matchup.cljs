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

  THE BENCH IS DRAWN, dimmed, under the starters, so a best lineup that benches
  somebody still shows what he did.

  EACH SIDE SHOWS ITS SET LINEUP OR ITS BEST ONE, on its own control: the lineup
  that scores, the best one still possible by projection (moving only players
  whose games have not started), or the best one by what was scored (once every
  game is final). The best lineup is drawn in place of the set one rather than
  described in a box beside it — a player moved in is marked ▲, a starter who
  loses his seat leads the bench marked ▼ — because \"start T. Hill\" is easier
  to weigh with T. Hill in the row he would take.

  IT DOES NOT REFETCH TO SWITCH GAMES. The server values every roster in the
  league in one reply, so the picker is a filter over data already in hand —
  `rankings.matchup/matchup-board` explains why that is affordable."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [draft-day.views.waivers :as waivers]))


(defn fmt
  "One decimal, or a dash. Weekly points are small enough that rounding to whole
  numbers would collapse 8.4 and 12.6 into a comparison nobody can make."
  [v]
  (if (number? v) (.toFixed v 1) "–"))

(defn record-label
  "`5–3`, or nothing. A league that reports no record must not render a dash
  where a record goes — an em dash beside a team name reads as a score."
  [t]
  (when (and (:wins t) (:losses t))
    (str (:wins t) "\u2013" (:losses t))))

(defn player-cell
  "A player's name, his NFL game, and the two numbers.

  `side` mirrors the layout: name, projection, actual on the left; actual,
  projection, name on the right, so both actuals sit against the centre. A
  bench player's meta leads with his own position, because a bench row has no
  shared seat label to say it. The name opens the detail modal by dispatching
  rather than by requiring it — the require cycle `core/app` warns about."
  [p side week]
  (let [mark (cond (:moved-in? p)  [:span.mu-mv.in {:title "Moved in by the best lineup"} "▲"]
                   (:moved-out? p) [:span.mu-mv.out {:title "Benched by the best lineup"} "▼"])
        nums [^{:key :p} [:div.mu-p (fmt (:week-points p))]
              ^{:key :a} [:div {:class (str "mu-a" (when-not (number? (:actual p)) " pending"))}
                          (fmt (:actual p))]]
        who  [:div.mu-who {:key :who}
              (when (= side :l) mark)
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
                                  (waivers/week-matchup p week))]
              (when (= side :r) mark)]]
    (into [:div {:class (str "mu-side " (name side) (when (:moved-in? p) " moved-in"))}]
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
  \"empty\" would invent a seat nobody has."
  [side seat?]
  [:div {:class (str "mu-side " (name side))}
   [:div.mu-who (when seat? [:span.mu-empty "empty"])]])

(defn seat-row
  "One row, both sides. `l` and `r` may be nil — a bench shorter than the other,
  or a game with only one team in it."
  [slot l r week seat? key-part]
  ^{:key key-part}
  [:div.mu-row
   (if (and l (not (:empty? l))) [player-cell l :l week] [empty-cell :l seat?])
   [:div.mu-slot slot]
   (if (and r (not (:empty? r))) [player-cell r :r week] [empty-cell :r seat?])])


(def lineup-views
  [[:set "Set lineup"] [:projected "Best by projection"] [:actual "Best by actual"]])

(defn lineup-hint
  "One line under a side's control saying what its best lineup would change,
  by the basis that can currently be believed: by what was scored once every
  game is final, by projection before that."
  [t]
  (let [{:keys [projected actual]} (:optimal t)]
    (cond
      actual                         (str "Best by actual: " (fmt (:gain actual))
                                          " left on the bench")
      (:locked? projected)           "Lineup locked"
      (and projected (pos? (:gain projected)))
      (str "Best by projection: +" (fmt (:gain projected)) " still possible")
      projected                      "Best by projection: no better lineup")))

(defn side-lineup
  "What a side draws under view `v`: its rows, and what its totals row says.
  A best lineup that is not available — Actual before the week is final — draws
  the set lineup, so a view cannot strand a side on nothing."
  [t v]
  (if-let [o (when (not= v :set) (get-in t [:optimal v]))]
    (let [seated (remove :empty? (:starters o))
          sum    (fn [k] (when (some #(number? (k %)) seated)
                           (reduce + 0 (keep k seated))))]
      {:starters  (:starters o)
       :bench     (:bench o)
       :basis     v
       :gain      (:gain o)
       :projected (sum :week-points)
       :actual    (sum :actual)})
    {:starters  (:starters t)
     :bench     (:bench t)
     :projected (:projected t)
     :actual    (:actual t)}))

(defn lineup-control
  "Set lineup | Best by projection | Best by actual, for one side."
  [t v]
  (let [actual? (some? (get-in t [:optimal :actual]))]
    [:div.mu-lineup
     (for [[k label] lineup-views
           :let [off? (and (= k :actual) (not actual?))]]
       ^{:key k}
       [:button {:class    (when (= v k) "on")
                 :disabled off?
                 :title    (when off? "Available once all games are final")
                 :on-click #(rf/dispatch [:set-lineup-view (:roster-id t) k])}
        label])]))

(defn team-head [t side v]
  (let [mine? (= side :l)]
    [:div {:class (str "mu-team" (when-not mine? " r"))}
     [:div.mu-name
      (if mine?
        [:<> (:name t) [:span.mu-rec (record-label t)]]
        [:<> [:span.mu-rec (record-label t)] (:name t)])]
     [:div {:class (str "mu-score" (when-not (number? (:actual t)) " pending"))}
      (fmt (or (:official t) (:actual t)))]
     [:div.mu-proj (str "projected " (fmt (:projected t)))]
     [lineup-control t v]
     (when-let [hint (lineup-hint t)] [:span.mu-lock hint])]))

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
      (when-let [at (waivers/fetched-at-label (:week-fetched-at m))]
        (str " · projection updated " at))]
     [:span.grow
      [game-picker]
      [:button.secondary {:on-click #(rf/dispatch [:fetch-matchup])} "Refresh"]]
     (when status [:span.mu-status status])]))


(defn totals-label
  "\"Starters\" under a set lineup; under a best one, which basis and what it
  gains over the lineup that is set."
  [{:keys [basis gain]}]
  [:div.mu-who
   (if basis
     [:<> "Best lineup "
      [:span.mu-optnote (if (= basis :actual) "by actual" "by projection")]
      (when (and (number? gain) (pos? gain))
        [:span.mu-gain (str "+" (fmt gain) " over set")])]
     "Starters")])

(defn rows
  "Starters as seats, then a bench divider, then the bench.

  Both sides are walked to the longer length so an uneven pair still lines up
  seat for seat — the shorter side draws an empty cell rather than shifting
  everybody up."
  [l r week]
  ;; `l` and `r` here are `side-lineup`s, not teams.
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
      [:div.mu-side.l [totals-label l] [:div.mu-p (fmt (:projected l))]
       [:div.mu-a (fmt (:actual l))]]
      [:div.mu-slot]
      [:div.mu-side.r [:div.mu-a (fmt (:actual r))] [:div.mu-p (fmt (:projected r))]
       [totals-label r]]]
     [:div.mu-sep "Bench"]
     (seat-rows (:bench l) (:bench r) false "b")]))

(defn matchup-view []
  (let [m      @(rf/subscribe [:matchup])
        [l r]  @(rf/subscribe [:matchup-sides])
        views  @(rf/subscribe [:lineup-view])
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
        (let [lv (get views (:roster-id l) :set)
              rv (when r (get views (:roster-id r) :set))]
          [:<>
           [:div.mu-head
            [team-head l :l lv]
            [:div.mu-vs (if week (str "Week " week) "–")]
            (if r [team-head r :r rv] [:div.mu-team.r [:div.mu-name.muted "No opponent"]])]
           [rows (side-lineup l lv) (if r (side-lineup r rv) {}) week]])])]))
