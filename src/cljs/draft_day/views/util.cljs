(ns draft-day.views.util)

(defn money [n]
  (if (and (number? n) (pos? n)) 
    (str "$" n) 
    "–"))

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
    "\u2013"))

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
         (str "https://sleepercdn.com/images/team_logos/nfl/" (.toLowerCase team-id) ".png")
         (str "https://sleepercdn.com/content/nfl/players/"
              (when (= size :thumb) "thumb/")
              sleeper-id ".jpg"))))))

(defn kickoff-label
  "An ISO kickoff stamp as the manager's own wall clock: \"Sun 1:00 PM\".

  Rendered here rather than on the server for the reason `fetched-at-label`
  states: the server has no idea what timezone the manager is in, and a string
  formatted there reads in whatever zone the host happens to be set to. The
  2026 season opens at the Melbourne Cricket Ground, so assuming Eastern is
  wrong for more than the pedantic reason.

  The weekday is load-bearing and not decoration. Thursday, Sunday early,
  Sunday late and Monday night are the whole decision content of a waiver
  claim — a time with no day attached cannot answer \"is my claim in before he
  plays\".

  nil for a missing or unparseable stamp, so a caller can drop the segment
  rather than print a dash where there may simply be no game."
  [iso]
  (when iso
    (let [d (js/Date. iso)]
      (when-not (js/isNaN (.getTime d))
        (.toLocaleString d js/undefined
                         #js {:weekday "short" :hour "numeric" :minute "2-digit"})))))

(defn kickoff-status-label
  "What to say beside the time when the game is not still ahead of us.

  nil while a game is scheduled, because the kickoff time already says that. A
  Sunday-evening modal reading \"Sun 1:00 PM\" over a game that finished two
  hours ago is the same class of lie `fetched-at-label` exists to prevent, so
  anything other than SCHEDULED prints ESPN's own words for it."
  [status detail]
  (when (and status (not= "STATUS_SCHEDULED" status))
    (or detail "Final")))
