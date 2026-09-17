(ns draft-day.ingestion.season
  "Which NFL season the app means when nobody says.

  Its own namespace rather than a function on a vendor, because the league
  dispatchers need it and they are forbidden from requiring a provider — and
  because a year is a fact about the calendar, not about Sleeper.

  It answers with the *calendar* year, which is the league year for ten months
  of twelve. From January until the new league year it names the season after
  the one being played, so it is a default and never an override: a caller that
  holds a real season — a stored league entry, a request field — passes it
  rather than asking. `current` is what you reach for when there is genuinely
  nothing to go on.")

(defn current
  "This season's year."
  []
  (.getValue (java.time.Year/now)))

(defn resolve-season
  "`season` when the caller has one, this season otherwise.

  Blank counts as absent: a season crosses the wire as a string and an empty
  field is a caller with no opinion, not a request for year zero. So does
  anything that is not a four-digit year — this value is interpolated into a
  URL *path segment* (`league-import.espn/league-url`,
  `league-sync.sleeper/list-leagues`), which is the same reason
  `providers/league-id-error` pins a league id to `#\"\\d+\"`. A request body
  carrying `\"2026/../../..\"` would otherwise point a cookie-bearing fetch at
  a path nobody named."
  [season]
  (if (re-matches #"\d{4}" (str season)) season (current)))
