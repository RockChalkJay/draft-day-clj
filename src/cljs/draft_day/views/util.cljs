(ns draft-day.views.util)

(defn money [n]
  (if (and (number? n) (pos? n)) 
    (str "$" n) 
    "–"))

(defn money-rnd [n]
  (if (and (number? n) (pos? n)) 
    (str "$" (js/Math.round n)) 
    "–"))

(defn headshot-url
  "Sleeper's CDN keys headshots by player-id; team defenses have no headshot,
  and their player-id *is* the team abbreviation, so they get the team logo."
  [{:keys [player-id position]}]
  (when player-id
    (if (#{"DEF" "DST"} position)
      (str "https://sleepercdn.com/images/team_logos/nfl/" (.toLowerCase player-id) ".png")
      (str "https://sleepercdn.com/content/nfl/players/thumb/" player-id ".jpg"))))
