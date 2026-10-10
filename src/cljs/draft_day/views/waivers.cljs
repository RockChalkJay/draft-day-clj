(ns draft-day.views.waivers
  "The in-season board: who is free in your league, how much better he is than
  the man you would drop, and what to bid.

  Nothing here is priced in auction dollars. The draft board's Worth and Value
  divide a bankroll among a whole roster on one night; a waiver claim is a single
  seat against a budget spent down over months. See `rankings.waiver` for why
  those two numbers cannot be the same number.

  The header states the week out loud, and says so most loudly when there is no
  week — in preseason the rest-of-season board *is* the draft board, which is the
  honest answer but the one a manager is most likely to misread as live."
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [re-frame.core :as rf]
            [draft-day.db :as db]
            [draft-day.views.board :as board]
            [draft-day.views.columns :as columns]
            [draft-day.views.util :as util]))

;; ---- cells ----

(defn trend-class
  "Rising, falling, or neither. The band around 1.0 is deliberately wide: a
  receiver at 1.04 has not earned an arrow, and colouring noise is how a column
  stops being read at all."
  [t]
  (cond (nil? t) nil
        (>= t 1.25) "trend-up"
        (<= t 0.75) "trend-down"
        :else nil))

(defn format-trend [t]
  (if (number? t) (str (.toFixed t 2) "×") "–"))

;; ---- the bid ----

(defn win-pct
  "A chance as a whole percent, \"76%\", or nil. Only a certainty prints 100%
  and only an impossibility 0%: rounding 99.6% up would state what the model
  does not claim."
  [p]
  (when (number? p)
    (str (cond (>= p 1) 100
               (<= p 0) 0
               :else    (-> (js/Math.round (* 100 p)) (max 1) (min 99)))
         "%")))

(def priced-out
  "Below this chance a bid rarely lands, and the cell says so in red."
  0.25)

(defn format-adds
  "Sleeper's trending count, compact: 152.6k, or the count under a thousand."
  [n]
  (cond (not (number? n)) "–"
        (>= n 1000)       (str (.toFixed (/ n 1000) 1) "k")
        :else             (str n)))

(defn bid-source
  "Where the bid's prices came from, off the reply's `:bidding`."
  [{:keys [source auctions]}]
  (if (= "league" (some-> source name))
    (str "From " auctions " league auctions + Sleeper-wide")
    "From Sleeper-wide auctions"))

(defn bid-title
  "The Bid cell's tooltip: the top rival bid, the likeliest rivals, the sure bid
  against the walk-away, and the source. nil where there is no bid."
  [{:keys [bid walk-away bid-sure competition]} bidding]
  (when (number? bid)
    (let [[p50 p90] (:top competition)]
      (str/join
       "\n"
       (concat
        [(if p50
           (str "Top rival bid: usually ≤ $" p50 ", rarely over $" p90)
           "No other team likely to bid")]
        (map (fn [{:keys [name faab-left p]}]
               (str name (when (number? faab-left) (str " · $" faab-left " left"))
                    " · " (win-pct p)))
             (:threats competition))
        [(str (if (number? bid-sure) (str "90% sure: $" bid-sure) "90% sure: out of reach")
              " · worth $" walk-away " to you")
         (bid-source bidding)])))))

(defonce ^{:doc "The Bid tooltip on screen, `{:text :x :y}`, or nil."}
  bid-tip (r/atom nil))

(defn show-bid-tip!
  "Open the Bid tooltip under the hovered cell. The page's own rather than a
  `title`, which the browser draws at its own small size; fixed to the viewport
  so the board's scroll container cannot clip it on the bottom rows."
  [e text]
  (let [rect (.getBoundingClientRect (.-currentTarget e))]
    (reset! bid-tip {:text text :x (.-left rect) :y (+ (.-bottom rect) 4)})))

(defn bid-tip-popover
  "Cleared when the board goes, or a tab switched mid-hover would bring it back
  open over nothing."
  []
  (r/create-class
   {:component-will-unmount #(reset! bid-tip nil)
    :reagent-render
    (fn []
      (when-let [{:keys [text x y]} @bid-tip]
        [:div.bid-tip {:style {:left x :top y} :aria-hidden true} text]))}))

(defn bid-cell
  "`$12 · 76%`: a bid and its chance, left-aligned so the dollars line up
  whatever the chance's width, or a dash where there is no bid. A nil bid is
  \"this league does not bid\" and $0 is a legal bid that wins at the minimum;
  they must not read alike. Hovering opens `tip` in `bid-tip-popover`."
  [amount chance tip]
  (if (number? amount)
    [:td.bid {:aria-label     tip
              :on-mouse-enter #(show-bid-tip! % tip)
              :on-mouse-leave #(reset! bid-tip nil)}
     [:b (str "$" amount)]
     (when (number? chance)
       [:span {:class (if (< chance priced-out) "warn" "muted")} (str " · " (win-pct chance))])]
    [:td.bid "–"]))

(defn typical-title
  "The Bid column's tooltip: the suggested bid above `bid-title`'s rivals."
  [p bidding]
  (when-let [rest (bid-title p bidding)]
    (str "Suggested bid: $" (:bid p) (some->> (:win-prob p) win-pct (str " · ")) "\n" rest)))

(def matchup-source util/matchup-source)

(def week-matchup util/week-matchup)

(defn kickoff-title
  "The Opp cell's tooltip, or nil. A tooltip rather than a column: a catalog
  entry is persisted shape and would cost every manager his layout."
  [p]
  (when-let [at (util/kickoff-label (:kickoff/at p))]
    (let [done (util/kickoff-status-label (:kickoff/status p) (:kickoff/detail p))]
      (str at (when done (str " — " done))))))

(defn open-detail!
  "Open the detail modal for `p` without also toggling him into the comparison.

  The whole row carries `:compare-toggle`, so the name has to stop the bubble —
  the arrangement `board/star-toggle` uses, and for its reason.

  `tabIndex -1` deliberately. A `<button>` is a tab stop by default, and six
  hundred of them means tabbing off the search box walks the entire table, which
  is precisely the objection `docs/TODO.md` raises against the cheap fix for
  keyboard access. The element keeps its button semantics — a pointer, a role, a
  name a screen reader can read — and the roving-tabindex grid pattern that TODO
  calls for is what will hand it a stop back, for both boards at once. This is
  the not-making-it-worse answer, recorded as a choice rather than left as an
  oversight."
  [e p]
  (.stopPropagation e)
  (rf/dispatch [:show-modal {:kind :player-detail :player-id (:player-id p)}]))

(defn cell*
  "`cell` for a position the column applies to."
  [k p week bidding]
  (case k
    :rank      [:td.num.muted (:rank p)]
    :name      [:td.player
                (util/player-ident
                 p week
                 {:on-click #(open-detail! % p)
                  :extra    [(when-let [st (:sleeper/injury-status p)]
                               (when (db/serious-injury? st)
                                 [:span.inj-flag {:title st} " ⚠"]))]})]
    :position  [:td (util/pos-label (assoc p :pos-rank (db/season-rank p)))]
    :bye       [:td.num (or (:bye p) "–")]
    ;; No weekly line is not a weekly zero: he is on bye, or nobody projects
    ;; him. A 0 would claim he plays and does nothing. Which of the two it is
    ;; is worth saying here rather than only in Opp, which is off by default —
    ;; a bye is the common reason this cell is empty.
    :week      (let [pts (:week-points p)]
                 [:td.num (cond
                            (number? pts) (util/week-points pts)
                            (and week (= week (:bye p))) [:span.muted "Bye"]
                            :else [:span.muted "–"])])
    ;; The position travels with the ordinal, because the whole point of the
    ;; column is that "WR19" means something where "4.2" does not.
    :week-rank (let [n (:week-pos-rank p)]
                 [:td.num.muted (if n (str (:position p) n) "–")])
    :opp       [:td.muted {:title (kickoff-title p)} (week-matchup p week)]
    ;; The headline. Signed, because a free agent worse than the man you would
    ;; drop is not an add — and flattening that to zero would make the whole
    ;; tail of the pool look equally plausible.
    ;;
    ;; Rounded ONCE, and both the colour and the digits read from that. Colouring
    ;; the raw value and printing the rounded one put a green dash on the board
    ;; for an upgrade of 0.4 — `sign-class` saw a positive number while `signed`
    ;; dashed out the zero. Same rule `controls/val-cell` states: the colour and
    ;; the digits have to come from the same value.
    :upgrade   (let [n (js/Math.round (or (:upgrade p) 0))]
                 [:td.num {:class (util/sign-class n)} (util/signed n)])
    ;; Absent when the request carried no roster config, which is a different
    ;; answer from a lineup this claim would not change.
    ;; Signed and coloured like Upg, because it goes negative for a real reason:
    ;; the drop can be a starter, and a claim that costs you lineup points is
    ;; exactly what this column exists to show. Absent (a request that carried
    ;; no roster config) is a dash, not a zero.
    :lineup    (let [n (:lineup-upgrade p)]
                 (if (number? n)
                   (let [r (js/Math.round n)]
                     [:td.num {:class (util/sign-class r)} (util/signed r)])
                   [:td.num [:span.muted "–"]]))
    :bid       (bid-cell (:bid p) (:win-prob p) (bid-title p bidding))
    :typical   (bid-cell (:typical-bid p) (:typical-win p) (typical-title p bidding))
    :pts       [:td.num (board/format-one-decimal (:season-points p))]
    :avg       [:td.num (board/format-one-decimal (:season-ppg p))]
    :rec       [:td.num.muted (board/format-whole (get-in p [:nflverse/season-to-date :stats :rec]))]
    :yds       [:td.num.muted (board/format-whole (db/season-yards p))]
    :td        [:td.num.muted (board/format-whole (db/season-tds p))]
    :rivals    [:td.num (if (number? (:rivals p)) (.toFixed (:rivals p) 1) "–")]
    :adds      [:td.num.muted (format-adds (:trending/adds p))]
    :trend     [:td.num {:class (trend-class (:trend p))
                         :title (when (:trend p)
                                  (db/column-tip (db/waiver-columns-by-key :trend) (:position p)))}
                (format-trend (:trend p))]
    ;; One decimal, unlike the whole-number season projections beside it: this
    ;; is a per-game rate and rounding 8.4 and 8.6 both to 8 hides the comparison
    ;; the column exists to make.
    :form      [:td.num.muted (board/format-one-decimal (:form-points p))]
    :gp        [:td.num (or (:season-gp p) (get-in p [:nflverse/season-to-date :games]) "–")]
    :tgt       [:td.num.muted (board/format-whole
                               (get-in p [:nflverse/season-to-date :usage :targets]))]
    :car       [:td.num.muted (board/format-whole
                               (get-in p [:nflverse/season-to-date :usage :carries]))]
    :ros-vorp  [:td.num (board/format-whole (:ros-vorp p))]
    :risk      (let [lvl (:injury-risk p)
                     txt (or (:injury/reason p) "No injury history to judge")]
                 [:td.risk {:title txt :aria-label txt}
                  (if lvl [board/risk-bar lvl] [:span.muted "–"])])
    :inj       (let [st (:sleeper/injury-status p)]
                 [:td {:class (when (db/serious-injury? st) "inj-serious")}
                  (or st "–")])
    [:td "–"]))

(defn cell
  "One board cell. `bidding` is the reply's account of where bids came from,
  which only the Bid cell reads. A column that says nothing about the player's
  position (targets on a kicker) is a dash rather than a zero."
  ([k p week] (cell k p week nil))
  ([k p week bidding]
   (if (db/applies? (db/waiver-columns-by-key k) (:position p))
     (cell* k p week bidding)
     [:td.num.muted "–"])))

;; ---- header ----

(def waiver-columns
  "`board/header-cell`'s options for this board: click sorts, drag reorders."
  {:labels db/waiver-columns-by-key :sort :set-waiver-sort :move :move-waiver-column-onto})

;; ---- panels ----

(defn my-roster-panel
  "What the manager already has, beside what he could claim.

  The board is free agents only, so without this the tab never shows the roster
  a claim is measured against — and the `My team` dropdown in Settings, whose
  whole job is to identify that roster, appears to do nothing when it changes.
  Picking a team moves Upgrade, Bid and the budget, but all of those are numbers
  elsewhere on the screen; this is the part that visibly answers 'which team am
  I'.

  The roster is `:my-roster`, the same one My Team and the League tab draw:
  starters in the league's own lineup order, because the synced league knows
  the real lineup and the draft config's slot template does not. A row nobody
  could resolve keeps its seat and says so rather than vanishing."
  []
  (let [roster    @(rf/subscribe [:my-roster])
        synced?   @(rf/subscribe [:league-synced?])
        team      @(rf/subscribe [:my-sync-team])
        pickable  @(rf/subscribe [:comparable-by-id])
        comparing (set @(rf/subscribe [:compare]))
        week      (:week @(rf/subscribe [:season-header]))]
    [:div.roster-panel.waiver-roster
     [:div.roster-head [:h3 "My Roster"]]
     (cond
       (not synced?)
       [:p.muted "Sync a league to see your roster."]

       ;; This is the line that was missing — it says what the dropdown is for.
       (nil? team)
       [:p.muted "Pick your team under Settings to see your roster and what a claim would cost."]

       (nil? roster)
       [:p.muted "Loading your roster…"]

       (every? empty? (vals roster))
       [:p.muted "This team holds nobody yet."]

       :else
       (let [row (fn [p]
                   ;; Only a player the waiver board has a row for can be
                   ;; compared: any other id in `:compare` never resolves, and
                   ;; the click would silently do nothing. He keeps his seat.
                   (let [pick? (contains? pickable (:player-id p))]
                     ^{:key (:player-id p)}
                     [:tr {:class (->> [(when (:drop? p) "drop-seat")
                                        (when (:parked? p) "parked")
                                        (when pick? "pickable")
                                        (when (comparing (:player-id p)) "comparing")]
                                       (remove nil?) (str/join " "))
                           ;; The other half of the comparison: a claim is
                           ;; usually weighed against a man you already hold, and
                           ;; this panel is where he is on screen.
                           :on-click (when pick?
                                       #(rf/dispatch [:compare-toggle (:player-id p)]))}
                      ;; A starter reads his seat, as on My Team: a FLEX
                      ;; receiver is not a WR seat.
                      [:td.slot (if (:starter? p)
                                  (or (:slot p) "–")
                                  (or (:position p) "–"))]
                      [:td.slot-player
                       (util/player-ident p week {:on-click #(open-detail! % p)})
                      (when (:drop? p) [:span.drop-tag {:title "A claim would cost this seat"} " ↓"])]
                      [:td.num.muted (or (:bye p) "–")]
                      [:td.num (util/week-points (:week-points p))]]))
             group (fn [label rows]
                     (when (seq rows)
                       [:<> [:tr.roster-group [:td {:col-span 4} label]] (map row rows)]))
             {:keys [starters bench parked]} roster]
         [:table.roster
          [:thead [:tr [:th.slot "Pos"] [:th "Player"] [:th.num "Bye"] [:th.num "Wk"]]]
          [:tbody
           (group "Starters" (mapv #(assoc % :starter? true) starters))
           (group "Bench" bench)
           (group "IR" (mapv #(assoc % :parked? true) parked))]]))]))

(defn week-note
  "How old this week's projection is.

  It revises through the week as injury news and inactives land, so a board that
  cannot date it implies it is current — and on a Sunday morning that is the
  difference between a projection and a wrong answer."
  [{:keys [week week-fetched-at]}]
  (when week
    (let [at (util/fetched-at-label week-fetched-at)]
      [:span.week-age (str "Week " week " projection"
                           (when at (str ", updated " at)))])))

(defn setup-note
  "What the manager has to do before this board can say more, or nil. The
  inputs are all in Settings, so this only says where."
  [{:keys [league? connected? synced? team?]}]
  (cond
    (not league?) (if connected?
                    "No league active — pick one under Settings."
                    (str "No league connected — this ranks everyone, not who is actually "
                         "free. Connect your account under Settings."))
    (and synced? (not team?)) "Pick your team under Settings to see what a claim would cost."))

(defn status-line
  "One muted line over the board: which season it is for, how old the week's
  projection is, and anything standing between the manager and a real answer.

  In three states, not two. Preseason is said out loud rather than left to be
  inferred from an empty GP column: in August this board is the draft board,
  and a manager who reads it as live is reading a projection as a result. But
  that claim is only worth making when it is *known* — asserting it while the
  board is still loading, or after a failed refresh, states a fact about the
  season on no evidence at all.

  The budget, the rival's budget and the waiver runs left used to sit in panels
  of their own. FAAB is in the header on every season tab; the other two only
  ever explained the Bid column, and they are gone."
  []
  (let [{:keys [through-week] :as m} @(rf/subscribe [:waiver-meta])
        {:keys [connected? my-team-name]} @(rf/subscribe [:account])
        phase  @(rf/subscribe [:season-phase])
        note   (setup-note {:league?    (some? @(rf/subscribe [:active-league]))
                            :connected? connected?
                            :synced?    @(rf/subscribe [:league-synced?])
                            :team?      (some? my-team-name)})
        status @(rf/subscribe [:waiver-status])]
    [:div.week-banner {:class (when (= phase :preseason) "preseason")}
     (case phase
       :in-season (str "Rest-of-season, through week " through-week)
       :preseason "Preseason — no games played yet, so this is the full-season projection."
       "Loading the rest-of-season board…")
     [week-note m]
     (when note [:span.week-extra note])
     (when status [:span.week-extra status])]))

;; ---- the view ----

(defn waivers-view []
  (let [players @(rf/subscribe [:waiver-players])
        cols    @(rf/subscribe [:visible-waiver-columns])
        sort    @(rf/subscribe [:waiver-sort])
        week    (:week @(rf/subscribe [:waiver-meta]))
        bidding (:bidding @(rf/subscribe [:waivers]))
        comparing (set @(rf/subscribe [:compare]))]
    [:div.waivers-view
     [status-line]
     [:div.board-controls
      [:div.filters [board/pos-filter] [board/search-box]]
      [columns/waiver-column-picker]]
     [:div.waiver-body
      [:div.table-scroll
       [:table.board
        [:thead [board/board-header waiver-columns cols sort]]
        [:tbody
         (map (fn [p]
                ^{:key (:player-id p)}
                [:tr {:class (->> [(when (and (:drop-candidate p)
                                              (pos? (or (:upgrade p) 0)))
                                     "upgrade")
                                   (when (comparing (:player-id p)) "comparing")]
                                  (remove nil?) (str/join " "))
                      ;; Click to compare. A second row fills the other slot; a
                      ;; third evicts the older, so one player can be held while
                      ;; the board is clicked through challengers.
                      :on-click #(rf/dispatch [:compare-toggle (:player-id p)])}
                 (map (fn [{k :key}] ^{:key k} [cell k p week bidding]) cols)])
              players)]]]
      [:aside.waiver-roster-col [my-roster-panel]]]
     [bid-tip-popover]
     ;; Sleeper's data, on Sleeper's terms: say whose it is wherever it shows.
     (when (some #(= :adds (:key %)) cols)
       [:div.source-note "Adds: Sleeper trending adds, last 48 hours · via Sleeper"])
     (when-let [rostered @(rf/subscribe [:rostered-matches])]
       (when (seq rostered)
         [:div.rostered-note
          "Not free: "
          (->> rostered
               (map (fn [{:keys [player-name position team]}]
                      (str player-name " (" position ") — " team)))
               (interpose ", ")
               (into [:span]))]))
     (when-let [drop (:drop-candidate (first players))]
       [:div.drop-note
        "A claim costs a roster spot. Yours would come from "
        [:strong (:player-name drop)]
        (str " (" (:position drop) ").")])]))
