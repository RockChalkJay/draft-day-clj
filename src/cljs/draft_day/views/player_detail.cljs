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

  ORDER: identity, then the bid, then what he has done (season stats, week by
  week), then what he is projected for and the evidence behind it. A manager
  opens a card to decide a claim, and in season the projection is the least of
  what he reads; the season table drops its projection column then too.

  IT IS MOUNTED FROM `core.cljs`, not from the waivers view. This namespace
  requires `views.compare`, which requires `views.waivers`; a board surface that
  required this back would be a cycle ClojureScript will not load. The boards
  reach it by dispatching `:show-modal` — an event, not a require."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [draft-day.bio :as bio]
            [draft-day.db :as db]
            [draft-day.game-log :as game-log]
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

(def bid-labels
  "The claim band's rows `bidding` already states, left out of the card's claim
  band so a bid is not read twice."
  #{"Typical winning bid" "Suggested bid" "Rivals"})

(defn bidding-rows
  "`[[label value]]` for the card's Bidding section, the Bid column's popover
  kept open: what it usually takes, what to bid, the sure bid against what he is
  worth to you, and who is likely to bid. Empty where the league does not bid."
  [{:keys [typical-bid typical-win bid win-prob bid-sure walk-away rivals competition]}]
  (let [pct #(some->> % waivers/win-pct (str " · "))]
    (cond-> []
      (number? typical-bid) (conj ["Typical winning bid" (str "$" typical-bid (pct typical-win))])
      (number? bid)         (conj ["Suggested bid" (str "$" bid (pct win-prob))])
      (number? walk-away)   (conj ["90% sure" (str (if (number? bid-sure) (str "$" bid-sure) "out of reach")
                                                   " · worth $" walk-away " to you")])
      (number? rivals)      (conj ["Rivals likely to bid"
                                   (str (.toFixed rivals 1)
                                        (when-let [ts (seq (:threats competition))]
                                          (str " — " (str/join ", " (map #(str (:name %) " " (waivers/win-pct (:p %))) ts)))))]))))

(defn bidding
  [p]
  (when-let [rows (and db/bid-predictions? (seq (bidding-rows p)))]
    [:div.pd-band
     [:h4.pd-band-label "Bidding"]
     (map (fn [[label v]] ^{:key label} [:div.pd-row [:span.pd-label label] [:span.pd-value v]]) rows)]))

(defn band
  "One band's rows with the empty ones dropped, or nil when none survive.

  Same restraint the tile applies, in its one-column form: a metric the board
  cannot say anything about is dropped rather than dashed. A dash reads as \"the
  board cannot say\", which is fine once and is punctuation a dozen times over —
  and in preseason most of the evidence band is genuinely absent."
  ([k p] (band k p #{}))
  ([k p skip]
   (when-let [rs (seq (filter #(and (some? ((:f %) p)) (not (skip (:label %))))
                              (metrics/rows-by-band k)))]
     ;; No `:key`: `into [:<>]` makes these positional children, not a seq. The
     ;; rows inside are a seq and carry their own.
     [:div.pd-band
      [:h4.pd-band-label (band-labels k)]
      (map (fn [r] ^{:key (:label r)} [metric-row r p]) rs)])))

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

;; The head's context line is built from the parts that have something to say.
;; The matchup is routinely absent — in preseason there is no week and no
;; opponent — and a dash between the team and the bye reads as a value that
;; failed to load rather than as a schedule that does not exist yet. The board's
;; Opp column can print that dash because a header names it; a run-on line
;; cannot. The kickoff, its status and the venue drop out the same way.
;;
;; The venue only on a neutral site: "vs SF" says nothing about a game in
;; Melbourne, which is what the flag is for, and on the other 270 games a
;; stadium name is the longest string on the line and the least useful.

(defn meta-segments
  "Position and team always survive: a player the board could rank has both."
  [p week]
  (let [matchup (waivers/week-matchup p week)
        at      (util/kickoff-label (:kickoff/at p))
        done    (util/kickoff-status-label (:kickoff/status p) (:kickoff/detail p))]
    (cond-> [(util/pos-label (assoc p :pos-rank (db/season-rank p))) (or (:team p) "FA")]
      (not= "–" matchup)   (conj matchup)
      at                   (conj at)
      done                 (conj done)
      (and (:kickoff/neutral? p)
           (:kickoff/venue p)) (conj (:kickoff/venue p))
      (:bye p)             (conj (str "Bye " (:bye p))))))

(defn head
  "Name, position, team, and the context a claim is decided on. A nil `universe`
  — `/api/players` still in flight — drops the face and the bio line; every
  other field here is on the board row."
  [p universe week season]
  [:div.pd-head
   ;; `universe` alone, never the board row as a fallback: a waiver row carries
   ;; no `:ids`, so `headshot-url` would build a URL from the GSIS id and ask the
   ;; CDN for a player it has never heard of. Both paths end at the silhouette;
   ;; only one of them makes a request first.
   [face universe]
   [:div.pd-who
    [:h2#pd-title.pd-name (:player-name p) [compare/status-chip p]]
    [:p.pd-meta (str/join " · " (meta-segments p week))]
    ;; From the universe, like the face: `:bio` is a static fact and this is the
    ;; document that carries them. nil for a player nobody knows anything about.
    (when-let [l (bio/line universe season)]
      [:p.pd-bio l])]])

;; A week he did not play is a struck row rather than a line of dashes: four
;; dashes and a blank Pts cell read as data that failed to load, where "Out" is
;; the fact. `game-log/table` is what decides which weeks those are.

(defn game-log-table
  "The week-by-week table, or nil when there is nothing to draw."
  [player through-week scoring]
  (when-let [{:keys [columns rows]}
             (game-log/table player through-week scoring)]
    [:table.pd-log
     [:thead
      [:tr [:th.lbl "Wk"] [:th.lbl "Opp"]
       (map (fn [[label _]] ^{:key label} [:th.num label]) columns)
       [:th.num.pts "Pts"]]]
     [:tbody
      (map (fn [{:keys [week opponent played? values points]}]
             ^{:key week}
             [:tr {:class (when-not played? "out")}
              [:th.lbl week]
              [:td.opp (or opponent "–")]
              (if played?
                [:<>
                 (map-indexed (fn [i v] ^{:key i} [:td.num (player-stats/cell v)]) values)
                 [:td.num.pts (util/week-points points)]]
                [:td.num.out-note {:col-span (inc (count columns))} "Out"])])
           rows)]]))

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
        {:keys [season through-week]} @(rf/subscribe [:universe])
        scoring  @(rf/subscribe [:scoring-weights])
        season?  (= :in-season @(rf/subscribe [:season-phase]))]
    (when p
      [:div.modal-overlay
       {:on-click #(when (= (.-target %) (.-currentTarget %))
                     (rf/dispatch [:close-modal]))}
       [:div.modal.modal-wide.pd-modal
        ;; `role`, but deliberately not `aria-modal`. Nothing here traps focus —
        ;; Tab walks out of the dialog and into the board behind the scrim — and
        ;; an attribute telling a screen reader the rest of the page is inert
        ;; while it is not is the same kind of half-answer the board refuses to
        ;; give elsewhere. `docs/TODO.md` owns the keyboard story for both
        ;; boards; this describes what the thing actually does until then.
        {:role "dialog" :aria-labelledby "pd-title"}
        [:button.pd-close {:on-click #(rf/dispatch [:close-modal])
                           :title "Close (Esc)"
                           :aria-label "Close"} "✕"]
        [head p universe week season]
        ;; What to bid first, then what he has done, then what he is projected
        ;; for: a manager opens this card to decide a claim, and in season a
        ;; projection is the least of what he wants to read.
        [:div.pd-body
         [bidding p]
         (band :claim p bid-labels)
         ;; The universe half. `stat-table` returns nil on its own for a kicker,
         ;; a defense, or a player with no numbers, so there is no second guard
         ;; here — only the one for the universe not having arrived yet.
         (when universe
           [:<>
            [:div.pd-band
             [:h4.pd-band-label "Season stats"]
             [player-stats/stat-table universe season {:in-season? season?}]]
            (when-let [log (game-log-table universe through-week scoring)]
              [:div.pd-band
               [:h4.pd-band-label "Week by week"]
               log])])
         (band :horizon p)
         (band :evidence p)]]])))
