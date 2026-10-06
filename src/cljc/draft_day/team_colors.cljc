(ns draft-day.team-colors
  "Each team's two colours, and the header gradient the player card draws from
  them.

  Keyed by the app's own abbreviations (`ingestion.teams/app-teams`), so a
  player's `:team` is the lookup. The darker colour leads the gradient: the name
  is white text on the left, and a team whose lead colour is pale (Pittsburgh's
  gold, New Orleans' old gold) would otherwise put it on a colour it cannot be
  read against.")

(def colors
  {"ARI" ["#97233F" "#FFB612"] "ATL" ["#A71930" "#000000"]
   "BAL" ["#241773" "#9E7C0C"] "BUF" ["#00338D" "#C60C30"]
   "CAR" ["#0085CA" "#101820"] "CHI" ["#0B162A" "#C83803"]
   "CIN" ["#FB4F14" "#000000"] "CLE" ["#311D00" "#FF3C00"]
   "DAL" ["#003594" "#869397"] "DEN" ["#FB4F14" "#002244"]
   "DET" ["#0076B6" "#B0B7BC"] "GB"  ["#203731" "#FFB612"]
   "HOU" ["#03202F" "#A71930"] "IND" ["#002C5F" "#A2AAAD"]
   "JAX" ["#006778" "#9F792C"] "KC"  ["#E31837" "#FFB81C"]
   "LAC" ["#0080C6" "#FFC20E"] "LAR" ["#003594" "#FFA300"]
   "LV"  ["#000000" "#A5ACAF"] "MIA" ["#008E97" "#FC4C02"]
   "MIN" ["#4F2683" "#FFC62F"] "NE"  ["#002244" "#C60C30"]
   "NO"  ["#D3BC8D" "#101820"] "NYG" ["#0B2265" "#A71930"]
   "NYJ" ["#125740" "#000000"] "PHI" ["#004C54" "#A5ACAF"]
   "PIT" ["#FFB612" "#101820"] "SEA" ["#002244" "#69BE28"]
   "SF"  ["#AA0000" "#B3995D"] "TB"  ["#D50A0A" "#FF7900"]
   "TEN" ["#0C2340" "#4B92DB"] "WAS" ["#5A1414" "#FFB612"]})

(defn luminance
  "Relative luminance of a `#RRGGBB` colour, 0 (black) to 1 (white)."
  [hex]
  (let [parse  #?(:clj #(Integer/parseInt % 16) :cljs #(js/parseInt % 16))
        linear (fn [c]
                 (if (<= c 0.03928)
                   (/ c 12.92)
                   (Math/pow (/ (+ c 0.055) 1.055) 2.4)))]
    (->> (re-seq #"[0-9a-fA-F]{2}" hex)
         (map #(linear (/ (parse %) 255.0)))
         (map * [0.2126 0.7152 0.0722])
         (reduce +))))

(defn pair
  "`[lead accent]` for a team, darker first, or nil for one this table does not
  know (a free agent, a typo)."
  [team]
  (when-let [cs (get colors team)]
    (vec (sort-by luminance cs))))

(defn gradient
  "A CSS `background` for a team's header, or nil when the team is unknown."
  [team]
  (when-let [[lead accent] (pair team)]
    (str "linear-gradient(110deg, " lead " 0%, " lead " 40%, " accent " 150%)")))
