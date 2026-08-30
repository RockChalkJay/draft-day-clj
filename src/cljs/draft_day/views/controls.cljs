(ns draft-day.views.controls
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [draft-day.db :as db]
            [draft-day.views.board :as board]
            [draft-day.views.player-stats :as player-stats]
            [draft-day.views.util :as util]))

(defn- silhouette []
  [:svg {:width 64 :height 64 :view-box "0 0 24 24" :fill "none"
         :stroke "currentColor" :stroke-width 1.4}
   [:circle {:cx 12 :cy 8 :r 4}]
   [:path {:d "M4 21c0-4 3.6-7 8-7s8 3 8 7"}]])

(defn- face
  "Silhouette underneath, headshot on top — a missing image hides itself and
  falls through to the silhouette, so no load-state bookkeeping is needed."
  [p]
  [:div.nt-face
   [silhouette]
   [:img.nt-img {:src      (util/headshot-url p)
                 :alt      ""
                 :on-error #(set! (.. % -target -style -display) "none")}]])

(defn val-cell
  "One cell of the value strip: a label over a number, in one of three tones.

  `:money` — an already-formatted dollar string, green, the app's colour for
             dollars.
  `:pts`   — an already-formatted points string, plain text. Green means dollars
             in every other cell of this strip, so rendering VORP in it would
             read as a price.
  `:signed` — the *raw* number, formatted and coloured here. Barg and Edge are
             the two cells that are a verdict rather than an amount, and green-up
             / red-down is the same rule the board's own Barg and Edge use.

  `:signed` takes the number rather than a string on purpose: the colour and the
  digits have to come from the same value, and a cell handed both separately can
  be given one player's number and another's sign with nothing to catch it.

  `opts` may carry a `:title`, which is how Mkt still says which two vendors it
  is the consensus of."
  ([label v tone] (val-cell label v tone nil))
  ([label v tone {:keys [title]}]
   ;; `title` is a hover, and `aria-label` on a plain div is in the part of the
   ;; name calculation browsers are free to ignore. Mkt's tooltip is now the only
   ;; place ESPN's and FantasyPros' own prices exist on this tile, so the same
   ;; sentence also goes in as text that is read but not drawn.
   [:div.nt-val (when title {:title title})
    (when title [:span.sr-only title])
    [:span.lbl label]
    [:span.amt {:class (case tone
                         :pts    "pts"
                         :signed (str "signed " (or (util/sign-class v) "zero"))
                         nil)}
     (if (= :signed tone) (util/signed-money v) v)]]))

(defn- bye-tag
  "The nominated player's bye, colored like the board: red pulse when drafting
  would stack a starter's bye, green when it would cover an uncovered starter."
  [p]
  (when-let [b (:bye p)]
    (let [exp   @(rf/subscribe [:my-bye-exposure])
          pos   (:position p)
          clash? (db/board-bye-clash? pos b exp)
          cover? (and (not clash?) (db/covers-starter? pos b exp))]
      [:span " · Bye "
       [:span {:class (cond clash? "bye-clash" cover? "bye-cover")
               :title (cond clash? (str "You already start a " pos " on bye " b)
                            cover? (str "Covers one of your uncovered " pos " starters"))}
        b]])))

(defn- risk-tag
  "The nominated player's injury risk, as the board's bar plus the word the board
  has no room for. This is the one place a manager is about to commit money, so
  it is worth the extra characters here even though the column stays a glyph."
  [p]
  (when-let [lvl (:injury-risk p)]
    (let [txt (:injury/reason p)]
      [:span " · "
       [:span.nt-risk {:title txt :aria-label txt}
        [board/risk-bar lvl]
        [:span.nt-risk-word {:class (when (db/serious-injury? (:sleeper/injury-status p))
                                      "inj-serious")}
         (get board/risk-words lvl)]]])))

(defn market-title
  "The two vendor prices Mkt is the consensus of, for its tooltip.

  A vendor that has no price is still named, with a dash, so the sentence says
  *which* one is missing rather than quietly becoming a sentence about the other.
  But when neither has one there is no consensus to describe — `market-price`
  returns nil and the cell renders a dash — so the tooltip says that instead of
  offering an average of two dashes."
  [p]
  (let [espn (:espn/auction-value p)
        fp   (:fantasypros/aav p)]
    (if (or espn fp)
      (str "Market price — consensus of ESPN " (util/money-rnd espn)
           " and FantasyPros " (util/money-rnd fp)
           ", scaled to your league")
      "No market price — neither ESPN nor FantasyPros lists this player")))

(defn tier-chip
  "The player's tier, at the scale the board is currently reading.

  Deliberately not the flat `:tier` the server ships. That alias is always the
  *positional* tier, while the board resolves `:tier` per position filter in
  `:board-players` — so reading it here would put \"Tier 1\" on the tile beside a
  Tier column showing an overall tier for the same player, with nothing on screen
  to say they are two different questions."
  [p]
  (let [scale (db/tier-scale @(rf/subscribe [:pos-filter]))]
    (when-let [t (db/player-tier p scale)]
      [:div.nt-tier {:title (if (= :position scale)
                              (str "Tier " t " among " (:position p) "s")
                              (str "Tier " t " across the whole board"))}
       "Tier" [:b t]])))

(defn- nominate-form
  "Form-2 so the bid/team live in local reagent atoms (synchronous updates — no
  dropped keystrokes on a fast controlled input). Keyed on the player so it
  remounts fresh per nomination."
  [p]
  (let [bid  (r/atom "")
        team (r/atom @(rf/subscribe [:my-team-id]))]
    (fn [p]
      (let [teams @(rf/subscribe [:teams])]
        [:div.nom-tile
         [:div.nt-label "On the block"]
         ;; Who he is. Two children, not three: the season table used to be a
         ;; third column here and spent the tile's width competing with the
         ;; headshot for it.
         [:div.nt-head
          [face p]
          [:div.nt-main
           [:div.nt-name (:player-name p)]
           [:div.nt-meta (util/pos-label p) " · " (:team p) [bye-tag p] [risk-tag p]]]
          [tier-chip p]]
         ;; What he costs. Two the model says, two the market says, and the two
         ;; differences between them — which are the numbers a manager is
         ;; actually reading when they decide whether to raise.
         ;;
         ;; ESPN and FP$ used to have cells of their own here. They are what Mkt
         ;; is the consensus *of* (see `rankings/market.clj`), so three of six
         ;; cells were spent on one idea while Barg and Edge, which the board has
         ;; had all along, were absent. They keep their numbers on Mkt's tooltip.
         [:div.nt-vals
          [val-cell "Worth" (util/money (:worth p)) :money]
          [val-cell "Value" (util/money (:value p)) :money]
          [val-cell "Mkt" (util/money-rnd (:market p)) :money {:title (market-title p)}]
          [val-cell "Edge" (:edge p) :signed]
          [val-cell "VORP" (util/points (:vorp p)) :pts]]
         ;; What he has done. Reads the universe, not `p` — the ranked board
         ;; carries no season history. Renders nothing at all for a kicker or a
         ;; defense, which is why the band's rule lives on the table's own
         ;; wrapper rather than on anything here.
         [player-stats/nominated-stats]
         [:div.nt-actions
          [:label.nt-bid-label "Bid $"]
          [:input.bid {:type        "number"
                       :placeholder "0"
                       :value       @bid
                       :min         1
                       :on-change   #(reset! bid (.. % -target -value))}]
          [:select {:value     @team
                    :on-change #(reset! team (.. % -target -value))}
           (map (fn [t]
                  ^{:key (:team-id t)}
                  [:option {:value (:team-id t)} (str (:name t) " (" (util/money (:bankroll t)) ")")])
                teams)]
          [:button.primary
           {:disabled (or (nil? @bid) (= "" @bid))
            :on-click #(rf/dispatch [:record-pick {:player-id (:player-id p)
                                                   :price     @bid
                                                   :team-id   @team
                                                   :position  (:position p)}])}
           "Record Pick"]]]))))

(defn nominate-tile []
  (let [nominated @(rf/subscribe [:nominated-id])
        p (get @(rf/subscribe [:players-by-id]) nominated)]
    (if p
      ^{:key nominated} [nominate-form p]
      [:div.nom-tile
       [:div.nt-label "On the block"]
       [:div.nt-head
        [:div.nt-face [silhouette]]
        [:div.nt-main [:div.muted "Click a player or watch-list entry to nominate…"]]]])))
