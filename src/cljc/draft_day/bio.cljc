(ns draft-day.bio
  "Who a player is, for the line under his name in the detail modal.

  Pure and in cljc for `stat-lines`' reason: every rule here is a judgment about
  data rather than about markup, and `lein test` reaches cljc.

  AGE IS DERIVED IN THE BROWSER, AGAINST THE SEASON. The snapshot carries a
  birth *year* and nothing finer, so an age computed against today would be
  spuriously precise and wrong for roughly half the league. Computing it against
  the season also keeps it stable: a number that ticks over mid-October, with no
  new data behind the change, is worse than one that means \"his age this
  season\" and stays put. The universe is cached for a day and held in a server
  atom besides, so an age shipped from ingestion would be stale by construction.

  NOTHING HERE EVER PRINTS \"UNDRAFTED\". `player-ids/snapshot-row` strips nil
  values, so a genuinely undrafted player and one the crosswalk has no row for
  are indistinguishable in the pinned data. Inventing a verdict from an absent
  field is the failure BLANK IS NOT ZERO names; the segment is omitted instead."
  (:require [clojure.string :as str]))

(defn age
  "Age in `season`, or nil. See the ns docstring for why not against today."
  [{:keys [birth-year]} season]
  (when (and birth-year season (> season birth-year))
    (- season birth-year)))

(defn experience-label
  "\"Rookie\" / \"1 yr\" / \"5 yrs\", or nil. Sleeper counts a rookie as 0."
  [years-exp]
  (when (int? years-exp)
    (case years-exp
      0 "Rookie"
      1 "1 yr"
      (str years-exp " yrs"))))

(defn draft-label
  "\"2023 Rd 5, #177\", or nil. The round and the pick each drop out on their
  own — an old row can carry a year and nothing else."
  [{:keys [draft-year draft-round draft-overall]}]
  (when draft-year
    (str draft-year
         (when draft-round (str " Rd " draft-round))
         (when draft-overall (str ", #" draft-overall)))))

(defn segments
  "The parts of the line that have something to say, in reading order: how old
  he is, how long he has been here, where he came from."
  [{:keys [bio sleeper/years-exp]} season]
  (into [] (remove nil?)
        [(when-let [a (age bio season)] (str "Age " a))
         (experience-label years-exp)
         (draft-label bio)]))

(defn line
  "The bio line as rendered, or nil when nothing is known about him."
  [player season]
  (let [segs (segments player season)]
    (when (seq segs) (str/join " · " segs))))
