(ns draft-day.ingestion.espn-schedule
  "This week's kickoff times, from ESPN's public scoreboard. Keyless, ~40KB.

  WHY THIS IS NOT IN `ingestion.espn`. That namespace exists to survive one
  specific problem — a ~37MB read-mirror response that http-kit's client chokes
  on, which is why it carries its own `java.net.http` client. This is sixteen
  events of JSON on a different endpoint, answering a different question, on a
  different cadence, with a different consequence when it fails. Sharing a
  namespace would put a season-long auction scrape and a per-week schedule under
  one `:sources` label and one TTL, and the label is what makes a thin column
  diagnosable. The precedent is `nflverse` and `nflverse-weekly`: same host, same
  release, two namespaces, because they answer different questions.

  WHY THE TIME IS NOT FORMATTED HERE. The server does not know what timezone the
  manager is in, and `views.waivers/fetched-at-label` already settles the
  convention — ship the ISO stamp, render the wall clock in the browser. The
  2026 season opens with a game at the Melbourne Cricket Ground; assuming
  Eastern is wrong for more than the pedantic reason.

  A KICKOFF IS A FACT ABOUT A TEAM, not about a projection, which is why this is
  keyed by team and joined separately from the weekly line. Sleeper projects
  only ~14% of the board, and those are not the players a manager is deciding
  about on a Sunday morning."
  (:require [clojure.tools.logging :as log]
            [draft-day.ingestion.teams :as teams]
            [draft-day.json :refer [mapper]]
            [jsonista.core :as json]
            [org.httpkit.client :as http]))

(def ^:private base
  "https://site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard")

(defn scoreboard-url
  "`seasontype=2` is the regular season. The `dates` parameter is the season
  year, not a calendar date, which is why it is named for one and takes the
  other."
  [season week]
  (str base "?dates=" season "&seasontype=2&week=" week))

(defn- competitors
  "The two sides of a game as `{home-away abbrev}`, in the app's vocabulary."
  [competition]
  (into {} (keep (fn [{:keys [homeAway team]}]
                   (when-let [t (teams/normalize :espn (:abbreviation team))]
                     [homeAway t])))
        (:competitors competition)))

(defn parse-event
  "One scoreboard event -> the two `[team entry]` pairs it is worth, or nil.

  Both sides, because a kickoff is a fact about a game and every player on
  either roster needs it. nil when either side is missing — a half-parsed game
  would give one team a kickoff and leave the other looking like a bye.

  `:neutral?` rides along because `:home?` is a lie without it: the 2026 opener
  lists LAR as home at the Melbourne Cricket Ground, and ESPN itself writes that
  matchup `SF VS LAR` rather than `SF @ LAR`."
  [{:keys [date status competitions]}]
  (let [competition (first competitions)
        {home "home" away "away"} (competitors competition)
        {:keys [name shortDetail]} (:type status)]
    (when (and date home away)
      (let [entry {:kickoff  date
                   :status   name
                   :detail   shortDetail
                   :venue    (get-in competition [:venue :fullName])
                   :neutral? (boolean (:neutralSite competition))}]
        [[home (assoc entry :opponent away :home? true)]
         [away (assoc entry :opponent home :home? false)]]))))

(defn by-team
  "Pure: decoded scoreboard -> `{team entry}`, one entry per team playing.

  A team on bye is simply absent, and that absence is the answer — pre-filling
  every team would make 'no game this week' indistinguishable from 'the fetch
  came back empty'."
  [payload]
  (into {} (mapcat parse-event) (:events payload)))

(defn fetch
  "Network: `{team entry}` for one week, or nil.

  Every failure shape collapses to nil, including a 200 whose body is not a
  scoreboard: the caller trades a kickoff time for it, never the weekly
  projection it rides beside."
  [season week]
  (try
    (let [{:keys [status body error]} @(http/get (scoreboard-url season week)
                                                 {:timeout 15000})]
      (cond
        error (do (log/warn error "espn scoreboard fetch failed") nil)
        (not= 200 status) (do (log/warn "espn scoreboard non-200:" status) nil)
        :else (let [m (by-team (json/read-value body mapper))]
                (when (seq m)
                  (log/info "espn scoreboard: week" week "-" (count m) "teams")
                  m))))
    (catch Exception e
      (log/warn e "espn scoreboard unreadable:" (ex-message e))
      nil)))
