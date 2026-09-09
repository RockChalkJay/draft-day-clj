(ns draft-day.views.player-detail
  "One player, close up: who he is, what the board says about him, and the
  seasons behind it.

  The comparison tile answers \"which of these two\"; this answers \"who is
  this\". They are different questions and the tile is deliberately bad at the
  second — it shows two players and no history, because a season trend table
  drawn twice side by side is unreadable.

  WHAT IT READS, AND WHY THAT IS TWO DOCUMENTS. The metric readout comes from
  the *waiver board* row (`:comparable-by-id`, which already unions the free
  agents with the manager's own roster — the right index for all three places a
  name can be clicked). The face and the season history come from the
  *universe* (`:universe-by-id`), because `routes/without-history` strips
  `:nflverse/history` off every board response and a waiver row carries no
  `[:ids :sleeper]` to build a headshot from. `views.compare` already reaches
  for the universe for exactly the second reason.

  They are held apart rather than merged. A merge would make \"which document is
  this fact from\" unanswerable at a glance, and the two carry different
  freshness — the board is re-POSTed on every refresh, the universe is fetched
  once and cached for a day. The board half is always present, since it is how
  the id was obtained; the universe half can be missing while `/api/players` is
  still in flight, and the sections that depend on it simply do not render.

  ORDER: identity, then what the board says, then the evidence for it. That is
  `metrics/bands`' own \"question, answer, evidence\" ordering with the head in
  front and the history behind, and it is stated here so a future edit inherits
  the argument rather than re-deriving it.

  IT IS MOUNTED FROM `core.cljs`, not from the waivers view. This namespace
  requires `views.compare`, which requires `views.waivers`; a board surface that
  required this back would be a cycle ClojureScript will not load. The boards
  reach it by dispatching `:show-modal` — an event, not a require."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [draft-day.views.compare :as compare]
            [draft-day.views.controls :as controls]
            [draft-day.views.metrics :as metrics]
            [draft-day.views.player-stats :as player-stats]
            [draft-day.views.util :as util]
            [draft-day.views.waivers :as waivers]))

(def band-labels
  "A heading per `metrics/bands` entry.

  The tile needs none — its two player heads are the legend, and a heading over
  a two-column comparison would say what the columns already say. Read down one
  column, though, and three unlabelled groups separated by rules are just
  thirteen numbers. `player-detail-test` asserts this covers `metrics/bands`
  exactly, so a fourth band cannot ship as an unlabelled section."
  {:horizon  "Projection"
   :claim    "The claim"
   :evidence "Evidence"})

(defn metric-row
  "One metric, label left and value right.

  `:bar?`, `:better` and `:calibrated?` are ignored here: all three are
  statements about a comparison, and there is no second player to lean toward.
  Ignoring them is why `metrics/rows` can be one list — see its ns docstring."
  [{:keys [label f fmt sub tip]} p]
  (let [v (f p)]
    [:div.pd-row
     ;; `title` rather than a second line of type, as the tile does — a
     ;; definition under every label would put more words on screen than numbers.
     [:span.pd-label {:title tip} label]
     [:span.pd-value (fmt v)
      (when-let [s (and sub (sub p))] [:span.pd-sub s])]]))

(defn band
  "One band's rows with the empty ones dropped, or nil when none survive.

  Same restraint the tile applies, in its one-column form: a metric the board
  cannot say anything about is dropped rather than dashed. A dash reads as \"the
  board cannot say\", which is fine once and is punctuation a dozen times over —
  and in preseason most of the evidence band is genuinely absent."
  [k p]
  (when-let [rs (seq (filter #(some? ((:f %) p)) (metrics/rows-by-band k)))]
    ;; No `:key`: `into [:<>]` makes these positional children, not a seq. The
    ;; rows inside are a seq and carry their own.
    [:div.pd-band
     [:h4.pd-band-label (band-labels k)]
     (for [r rs] ^{:key (:label r)} [metric-row r p])]))

(defn face
  "The large headshot. Silhouette underneath, image on top, so a missing *or
  broken* image hides itself and falls through — the arrangement
  `compare/face` and `controls/face` both use, and for its reason: there is no
  load state to track."
  [p]
  [:div.pd-face
   [controls/silhouette 88]
   (when-let [src (util/headshot-url p :full)]
     [:img {:src src :alt ""
            :on-error #(set! (.. % -target -style -display) "none")}])])

(defn meta-segments
  "The head's one line of context, as the parts that have something to say.

  Joined rather than concatenated because the matchup is routinely absent — in
  preseason `waivers/week-matchup` has no week and no opponent and answers with
  a dash, and a dash sitting between the team and the bye reads as a value that
  failed to load rather than as a schedule that does not exist yet. The board's
  Opp column can print that dash because it sits under a header naming it; a
  run-on line has no such excuse.

  Position and team always survive: a player the board could rank has both."
  [p week]
  (let [matchup (waivers/week-matchup p week)]
    (cond-> [(util/pos-label p) (or (:team p) "FA")]
      (not= "–" matchup) (conj matchup)
      (:bye p)           (conj (str "Bye " (:bye p))))))

(defn head
  "Name, position, team, and the one line of context a claim is decided on.

  `universe` may be nil — the face falls back to the silhouette and the rest of
  the head is unaffected, because every field here is on the board row."
  [p universe week]
  [:div.pd-head
   [face (or universe p)]
   [:div.pd-who
    [:h2#pd-title.pd-name (:player-name p) [compare/status-chip p]]
    [:p.pd-meta (str/join " · " (meta-segments p week))]]])

(defn player-detail-modal
  "The modal for `id`, or nothing when the board has no row for him.

  Nothing rather than an empty shell: the only way to open this is to click a
  name the board rendered, so a missing row means the board was replaced while
  the modal was open — a league switch drops `:waivers` entirely. An empty
  modal over a board that has moved on is worse than no modal."
  [id]
  (let [p        (get @(rf/subscribe [:comparable-by-id]) id)
        universe (get @(rf/subscribe [:universe-by-id]) id)
        week     (:week @(rf/subscribe [:waiver-meta]))
        season   (:season @(rf/subscribe [:universe]))]
    (when p
      [:div.modal-overlay
       {:on-click #(when (= (.-target %) (.-currentTarget %))
                     (rf/dispatch [:close-modal]))}
       [:div.modal.modal-wide.pd-modal
        {:role "dialog" :aria-modal "true" :aria-labelledby "pd-title"}
        [:button.pd-close {:on-click #(rf/dispatch [:close-modal])
                           :title "Close (Esc)"
                           :aria-label "Close"} "✕"]
        [head p universe week]
        [:div.pd-body
         (into [:<>] (keep #(band % p)) metrics/bands)
         ;; The universe half. `stat-table` returns nil on its own for a kicker,
         ;; a defense, or a player with no numbers, so there is no second guard
         ;; here — only the one for the universe not having arrived yet.
         (when universe
           [:div.pd-band
            [:h4.pd-band-label "Season trend"]
            [player-stats/stat-table universe season]])]]])))
