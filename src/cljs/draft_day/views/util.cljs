(ns draft-day.views.util
  (:require [draft-day.team-names :as team-names]))

(defn money [n]
  (if (and (number? n) (pos? n)) 
    (str "$" n) 
    "–"))

(defn faab
  "A FAAB balance, or a dash when the host did not report one. Not `money`,
  which dashes $0: a team that has spent its budget has $0, and saying it does
  not know would be as wrong as saying $0 about a balance nobody reported."
  [n]
  (if (number? n) (str "$" n) "–"))

(defn pos-label
  "\"RB7\", or the bare position for a player the engine could not rank.

  One copy, because three surfaces render it — the board's Pos cell, the watch
  list and the On-the-block tile — and a manager reading `RB7` in one and `RB`
  in another has to work out whether that means something."
  [p]
  (str (:position p) (:pos-rank p)))

(defn points
  "A points figure to one decimal, or a dash.

  VORP rather than money, so deliberately not `money`: it is nil for K and DST
  (see `db/vorp-sort-key`) and it can be negative, which is real and load-bearing
  — a below-replacement player is exactly who the $1 bids come from."
  [n]
  (if (number? n)
    (.toFixed n 1)
    "-"))

(defn hundredths
  "`n` rounded to the hundredth on its decimal value, half away from zero.

  Not `.toFixed` alone, which rounds the binary double: 18.345 is stored a hair
  under, so it prints 18.34 where a host prints 18.35, and -0.004 prints as
  -0.00. Reading the product back at twelve significant digits puts it on the
  decimal value first. That also swallows the ~1e-14 a lineup gain picks up from
  summing one set of players in two orders, so a guard that tests this and the
  digits printed from it cannot disagree."
  [n]
  (let [x (js/parseFloat (.toPrecision (* n 100) 12))]
    (/ (* (js/Math.sign x) (js/Math.round (js/Math.abs x))) 100)))

(defn week-points
  "A week's points, to two decimals, or a dash: projected, scored, or the gain
  between two lineups. Two because that is the precision the hosts report
  points in, so an actual reads as the league's own scoreboard does. A
  projection is this app's scoring of Sleeper's weekly line and can differ from
  the host's own by more than a cent: its second place is precision, not
  agreement.

  A dash is not a zero: `:actual` is nil until the player's game starts, and
  0.00 means he played and did nothing."
  [n]
  (if (number? n) (.toFixed (hundredths n) 2) "-"))

(defn money-rnd [n]
  (if (and (number? n) (pos? n))
    (str "$" (js/Math.round n))
    "–"))

;; ---- signed differences ----
;; Barg and Edge are differences, not prices: the sign is the whole message, and
;; `money` is the wrong formatter for them because it dashes out anything not
;; positive — here a negative is the most interesting value there is.
;;
;; The board and the tile differ only in whether the unit is printed. The board's
;; are narrow numeric columns under their own headers, where a repeated "$" down
;; two hundred rows is noise; the tile shows them once, in a strip where every
;; neighbour carries a unit. So both go through `difference`, and both take their
;; colour from `sign-class`. Nothing here is stated twice.

(defn- difference
  "A signed difference with `unit` in front of the digits, or a dash.

  Zero dashes out along with nil and with anything that is not a number: a
  difference of exactly nothing is not a verdict, and it would sit in a place
  where colour carries meaning while having no colour to take.

  The sign is an ASCII \"-\" rather than a typographic minus. The board has
  always rendered these with one, and two surfaces spelling the same difference
  with two different glyphs is the drift `pos-label` exists to prevent."
  [n unit]
  (if (and (number? n) (not (zero? n)))
    (str (if (pos? n) "+" "-") unit (js/Math.abs n))
    "–"))

(defn signed
  "A signed difference for a board column: \"+4\", \"-4\", or a dash."
  [n]
  (difference n ""))

(defn signed-money
  "A signed dollar difference for the on-the-block strip: \"+$4\", \"-$4\", or a
  dash."
  [n]
  (difference n "$"))

(defn sign-class
  "\"good\" above zero, \"warn\" below, nil at zero or for a non-number.

  The board colours Edge and Barg by sign and so does the tile; this is that one
  rule, rather than a third and fourth copy of the same `cond`."
  [n]
  (cond
    (not (number? n)) nil
    (pos? n) "good"
    (neg? n) "warn"))

;; ---- column drag ----
;; Both places a column can be dragged from — the board header and the ⚙ Columns
;; picker — start the drag through `column-drag-start!`, so neither can drift
;; from the other. They already share the reorder event; this is the other half.

(def column-mime
  "Private drag type for a column. Deliberately not text/plain: a drag carrying
  text can be dropped into any text input, and the board's search box sits
  directly above the header row — a header dropped there would type the column
  key in and filter the board away. Firefox only requires that *some* data be
  set for a drag to begin, not that it be a standard type."
  "application/x-draft-day-column")

(defn column-drag-start! [e k]
  (let [dt (.-dataTransfer e)]
    (set! (.-effectAllowed dt) "move")
    (.setData dt column-mime (name k))))

(def watch-mime
  "Private drag type for a watch-list row, distinct from `column-mime` so the two
  drags cannot be dropped on each other: a column landing in the watch list, or a
  watched player landing in the board header, would each reorder against a key
  that does not exist there — a silent no-op that reads as a broken drag."
  "application/x-draft-day-watch")

(defn watch-drag-start! [e id]
  (let [dt (.-dataTransfer e)]
    (set! (.-effectAllowed dt) "move")
    (.setData dt watch-mime (str id))))

(defn left-element?
  "Did a dragleave actually leave `currentTarget`, or just cross into a child of
  it? The event fires on the parent either way, so without this a pointer moving
  onto a header's sort arrow reads as leaving the header. A null relatedTarget —
  leaving for nothing at all — counts as having left."
  [e]
  (not (.contains (.-currentTarget e) (.-relatedTarget e))))

(defn team-logo-url
  "Sleeper's CDN keys its team logos by the lowercase abbreviation."
  [team]
  (str "https://sleepercdn.com/images/team_logos/nfl/" (.toLowerCase team) ".png"))

(defn team-logo
  "A small logo for an NFL team, its full name on hover, or nil for a player
  with no team. It stands in for a Tm column."
  [team]
  (when team
    [:img.team-logo {:src     (team-logo-url team)
                     :alt     team
                     :title   (team-names/full-name team)
                     :loading "lazy"}]))

(defn headshot-url
  "Sleeper's CDN keys headshots by Sleeper id; `:player-id` is GSIS for most
  players, so read `[:ids :sleeper]` and fall back to `:player-id` for legacy
  rows. Team defenses have no headshot — their id is the team abbreviation, so
  they get the team logo instead.

  `size` is `:thumb` (the default, what every board surface wants) or `:full`.
  The CDN serves both off the same id and the same path, so this is one arity
  rather than a sibling function — a second copy would be a second place the
  DST branch has to be remembered. The detail modal is the only surface with
  room for the large one; a logo has no sizes and ignores the argument."
  ([p] (headshot-url p :thumb))
  ([{:keys [player-id position ids]} size]
   (let [sleeper-id (or (:sleeper ids) player-id)
         team-id    (or (:team ids) player-id)]
     (when sleeper-id
       (if (#{"DEF" "DST"} position)
         (team-logo-url team-id)
         (str "https://sleepercdn.com/content/nfl/players/"
              (when (= size :thumb) "thumb/")
              sleeper-id ".jpg"))))))

(defn ago
  "How long before `now-ms` a moment was, as `\"just now\"`, `\"12m ago\"`,
  `\"3h ago\"` or `\"2d ago\"`, or nil for a moment that cannot be read, or one
  older than `max-days` when that is given.

  `then` is an ISO string or epoch milliseconds, since the news feed sends one
  and Sleeper's injury list the other. A moment in the future reads as now:
  clocks disagree by a minute, and \"in 40s\" is not news."
  ([now-ms then] (ago now-ms then nil))
  ([now-ms then max-days]
   (let [t (cond (number? then) then
                 (string? then) (let [ms (js/Date.parse then)] (when-not (js/isNaN ms) ms)))]
     (when t
       (let [mins (js/Math.floor (/ (- now-ms t) 60000))]
         (cond (and max-days (> mins (* max-days 1440))) nil
               (< mins 1)    "just now"
               (< mins 60)   (str mins "m ago")
               (< mins 1440) (str (js/Math.floor (/ mins 60)) "h ago")
               :else         (str (js/Math.floor (/ mins 1440)) "d ago")))))))

(defn fetched-at-label
  "An ISO timestamp as a local wall-clock time, dated once it is not today.

  Deliberately absolute rather than \"8 minutes ago\": this element is only here
  to expose staleness, it is rendered once and not on a timer, and a relative age
  computed at render silently rots in exactly the case it exists for — a tab left
  open on a Sunday morning. A clock time cannot go stale, and it is also what the
  question actually compares against, since inactives drop at a time of day."
  [iso]
  (when iso
    (let [d     (js/Date. iso)
          today (= (.toDateString d) (.toDateString (js/Date.)))
          time  (.toLocaleTimeString d js/undefined
                                     #js {:hour "numeric" :minute "2-digit"})]
      (if today
        time
        (str (.toLocaleDateString d js/undefined #js {:month "short" :day "numeric"})
             ", " time)))))

;; ---- kickoff times ----
;; Rendered here and not on the server, which has no idea what timezone the
;; manager is in — `fetched-at-label` settles that convention above. The weekday
;; is load-bearing rather than decoration: Thursday, Sunday early, Sunday late
;; and Monday night are the whole decision content of a claim.
;;
;; `kickoff-status-label` prints ESPN's words and never one of ours. Its status
;; set includes IN_PROGRESS, HALFTIME, POSTPONED and DELAYED as well as FINAL,
;; so a fallback like "Final" is eventually printed over a game still being
;; played — and a manager who reads Final stops considering the claim.

(defn kickoff-label
  "An ISO kickoff stamp as the manager's own wall clock: \"Sun 1:00 PM\". nil
  for a missing or unparseable stamp, so a caller can drop the segment."
  [iso]
  (when iso
    (let [d (js/Date. iso)]
      (when-not (js/isNaN (.getTime d))
        (.toLocaleString d js/undefined
                         #js {:weekday "short" :hour "numeric" :minute "2-digit"})))))

(defn kickoff-status-label
  "ESPN's own word for a game that is not still ahead of us, or nil."
  [status detail]
  (when (and status (not= "STATUS_SCHEDULED" status))
    detail))
