(ns draft-day.views.metrics
  "What the waiver board can say about a player, as data: one ordered list of
  metrics, each with the key it reads, how it formats and what its label means.

  It lives here rather than in `views.compare` because there are now two
  renderers over it. The tile draws each metric twice, anchored at a centre line
  and leaning toward whoever leads; the player-detail modal draws it once, for
  one player, with no lean to draw. Those are different pictures of the same
  list, and the list is the part that must not fork — `db/waiver-column-catalog`
  is the same information a third time, and `metrics-test` is what stops the
  three drifting apart.

  A row carries some keys only the tile reads — `:bar?`, `:better`,
  `:calibrated?` are all statements about a *comparison*, and a one-column
  readout ignores them. That is deliberate and cheaper than two lists: an
  ignored key costs nothing, while a second list costs a second guard and rots
  the moment somebody adds a column and only remembers one of them.

  `:band` on a row rather than slicing one flat list by index — the boundaries
  used to be `subvec`s, so inserting a metric anywhere above the last one
  silently moved a row into the wrong band and still rendered."
  (:require [draft-day.views.board :as board]
            [draft-day.views.util :as util]
            [draft-day.views.waivers :as waivers]))

;; ---- derived metrics ----
;; Numbers no board column ships, computed here from ones it does.

(defn opportunity-per-game
  "Targets plus carries per game, or nil. Volume rather than points, and the
  same measure `waiver/trend` is a ratio of — a role is what a claim is buying."
  [{:nflverse/keys [season-to-date]}]
  (let [{:keys [games usage]} season-to-date]
    (when (and games (pos? games))
      (/ (+ (or (:targets usage) 0) (or (:carries usage) 0)) games))))

(defn week-rank-label
  "\"WR8\" under a weekly number, or nil. The ordinal carries its own scale for
  the reason the board's Wk# column does — and it is what keeps the row legible
  on exactly the comparisons where the bar deliberately says nothing."
  [p]
  (when-let [n (:week-pos-rank p)]
    (str (:position p) n)))

;; ---- formatters local to these rows ----

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

(defn number-or-dash
  "A number as itself, for a rank or a count that neither rounds nor scales."
  [n]
  (if (number? n) n "–"))

;; ---- the list ----

(def bands
  "The three bands, in reading order: the question, what a claim would cost and
  gain, then the evidence for both.

  Both renderers draw them in this order, so the vector is the order rather than
  a note about it. It used to be neither: the tile emitted its three bands by
  hand and this was a constant nothing read, which a test asserting its value
  could not tell apart from a guarantee."
  [:horizon :claim :evidence])

(def rows
  "What the two surfaces show — everything the board can say about a player.

  A metric a waiver row carries and this list omits is one the manager has to
  find by scrolling a table sideways. `db/waiver-column-catalog` is the same
  information for the board and the two drift apart silently: Lineup became the
  column the board *sorts by* while the tile still did not mention it.
  `metrics-test` fails on a catalog key that has not been given a decision here.

  Three deliberate omissions. Targets and carries, because
  `opportunity-per-game` is both of them over games played and the raw counts
  add a scale rather than a fact. Bye and Opp, because a schedule has no winner
  and both are in the head of either surface. And the injury designation, which
  is a word among tabular numbers — it is a chip on the name, where the board
  puts it too.

  `:bar?` false where neither side can be better, `:better :lower` where the
  metric inverts, `:big?` on the two horizons, and `:tip` on a row whose label
  cannot carry its own definition. The first three are read only by the tile."
  [{:band :horizon  :label "This week"      :f :week-points :big? true
    :fmt board/format-whole :sub week-rank-label :calibrated? true}
   {:band :horizon  :label "Rest of season" :f :ros-points  :big? true
    :fmt board/format-whole}
   ;; Rest of season restated in cross-position units — see `views.compare`'s
   ;; ns docstring and `db/vorp-sort-key`.
   {:band :horizon  :label "Over replacement" :f :ros-vorp :fmt board/format-whole
    :tip (str "Rest-of-season points above a replacement player at his position"
              " — the one number that compares a QB to a TE")}
   ;; Lineup leads; Upgrade under it is the bench question — see
   ;; `db/waiver-rank-key`.
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
   ;; Realized production, so the tile keeps a directional bar here that the
   ;; weekly row does not — see `views.compare`'s ns docstring.
   {:band :evidence :label "Form / game"    :f :form-points
    :fmt board/format-one-decimal
    :tip "Points per game over the last three weeks, under your league's rules"}
   {:band :evidence :label "Opportunity / game" :f opportunity-per-game
    :fmt #(if (number? %) (.toFixed % 1) "–")
    :tip "Targets plus carries per game this season"}
   {:band :evidence :label "Games played"   :bar? false
    :f #(get-in % [:nflverse/season-to-date :games])
    :fmt number-or-dash}
   ;; Both preseason, so neither is evidence about now — they are what the
   ;; season so far is disagreeing with, which is the whole waiver-wire case.
   {:band :evidence :label "Preseason"      :f :points :fmt board/format-whole
    :tip (str "What he was projected for before the season — the number the"
              " rest-of-season line is correcting")}
   {:band :evidence :label "Expert rank"    :f :fantasypros/ecr :better :lower
    :fmt number-or-dash
    :tip "FantasyPros expert consensus rank, preseason. Lower is better"}
   {:band :evidence :label "Injury risk"    :f :injury-risk :better :lower
    :fmt number-or-dash
    :tip (str "Games missed per season over the last three, 1 (durable) to"
              " 5 (fragile)")}])

(def rows-by-band (group-by :band rows))
