(ns draft-day.views.util)

(defn money [n]
  (if (and (number? n) (pos? n)) 
    (str "$" n) 
    "–"))

(defn money-rnd [n]
  (if (and (number? n) (pos? n)) 
    (str "$" (js/Math.round n)) 
    "–"))

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
  they get the team logo instead."
  [{:keys [player-id position ids]}]
  (let [sleeper-id (or (:sleeper ids) player-id)
        team-id    (or (:team ids) player-id)]
    (when sleeper-id
      (if (#{"DEF" "DST"} position)
        (str "https://sleepercdn.com/images/team_logos/nfl/" (.toLowerCase team-id) ".png")
        (str "https://sleepercdn.com/content/nfl/players/thumb/" sleeper-id ".jpg")))))
