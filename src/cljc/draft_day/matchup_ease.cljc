(ns draft-day.matchup-ease
  "How soft this week's opponent is at a player's position, read off
  `rankings.opponent/points-allowed`, and how the label naming that opponent says
  so: a color running green (allowed the most) through amber to red (allowed
  the fewest), and a title stating the rank in words.

  Shared because the server words the rank for Sensei and the browser words the
  same rank for the hover. Display only, like the table it reads.")

(def min-games
  "Games a defense must have played before its rank colors a label. One or two
  games rank a team on a single opponent's day."
  3)

(defn ordinal
  "1st, 2nd, 3rd, 4th, 11th, 12th, 13th, 21st."
  [n]
  (str n (if (<= 11 (mod n 100) 13)
           "th"
           (case (mod n 10) 1 "st" 2 "nd" 3 "rd" "th"))))

(defn rank-label
  "A rank with its direction in the words: `8th most` in the top half, `3rd
  fewest` in the bottom, counted from that end."
  [rank of]
  (if (<= rank (quot (inc of) 2))
    (str (ordinal rank) " most")
    (str (ordinal (- (inc of) rank)) " fewest")))

(defn ease-line
  "`opp`'s line at `position` in `table`, or nil on a bye, for a defense with
  fewer than `min-games` games, or for a line with no rank. The table's keys are
  strings on the server and keywords once JSON has crossed to the browser."
  [table opp position]
  (when (and opp position)
    (let [line (or (get-in table [opp position])
                   (get-in table [(keyword opp) (keyword position)]))]
      (when (and (:rank line) (:of line) (<= min-games (or (:games line) 0)))
        line))))

(defn ease-share
  "0.0 for the defense that allowed the most, 1.0 for the one that allowed the
  fewest."
  [{:keys [rank of]}]
  (if (> of 1) (/ (dec rank) (double (dec of))) 0.5))

(defn ease-color
  "A CSS color for `share`: the app's green, through amber, to its red. Mixed in
  OKLCH, whose hue runs the short way round through yellow; every step holds at
  least 5.7:1 on the panel."
  [share]
  (str "color-mix(in oklch, var(--good), var(--warn) " (Math/round (* 100.0 share)) "%)"))

(def position-plurals
  {"DST" "defenses" "DEF" "defenses" "K" "kickers"})

(defn ease-title
  "`BUF has allowed the 8th most points to RBs: 14.2 a game`."
  [{:keys [rank of per-game]} opp position]
  (str opp " has allowed the " (rank-label rank of) " points to "
       (get position-plurals position (str position "s"))
       (when (number? per-game)
         (str ": " #?(:clj (format "%.1f" (double per-game)) :cljs (.toFixed per-game 1)) " a game"))))
