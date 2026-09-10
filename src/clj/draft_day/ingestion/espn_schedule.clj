(ns draft-day.ingestion.espn-schedule
  "This week's kickoff times, from ESPN's public scoreboard. Keyless, ~40KB.

  WHY THIS IS NOT IN `ingestion.espn`. That namespace exists to survive one
  specific problem — a ~37MB read-mirror response http-kit's client chokes on,
  which is why it carries its own `java.net.http` client. This is sixteen events
  of JSON on a different endpoint, a different cadence and a different
  consequence when it fails. Sharing a namespace would put a season-long auction
  scrape and a per-week schedule under one `:sources` label and one TTL, and the
  label is what makes a thin column diagnosable. The precedent is `nflverse` and
  `nflverse-weekly`: same host, same release, two namespaces, two questions.

  WHY THE TIME IS NOT FORMATTED HERE. The server does not know what timezone the
  manager is in, and `views.waivers/fetched-at-label` already settles the
  convention — ship the ISO stamp, render the wall clock in the browser. The
  2026 season opens at the Melbourne Cricket Ground; assuming Eastern is wrong
  for more than the pedantic reason.

  A KICKOFF IS A FACT ABOUT A TEAM, not about a projection, which is why every
  entry here is keyed by team and joined separately from the weekly line —
  Sleeper projects a fraction of the board, and not the fraction a manager is
  deciding about on a Sunday morning. See `pipeline/assoc-kickoffs`.

  Both sides of a game get an entry. A half-parsed game is dropped whole rather
  than giving one team a kickoff and leaving the other looking like a bye, and a
  team on bye is simply absent — pre-filling all thirty-two would make 'no game'
  indistinguishable from 'the fetch came back empty'.

  `:neutral?` rides on every entry because `:home?` is a lie without it: the
  2026 opener lists LAR as home in Melbourne, and ESPN writes it `SF VS LAR`."
  (:require [clojure.tools.logging :as log]
            [draft-day.ingestion.teams :as teams]
            [draft-day.json :refer [mapper]]
            [jsonista.core :as json]
            [org.httpkit.client :as http]))

(def ^:private base
  "https://site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard")

(defn scoreboard-url
  "`dates` is the season year, not a calendar date; `seasontype=2` is the
  regular season."
  [season week]
  (str base "?dates=" season "&seasontype=2&week=" week))

(defn- competitors
  "`{\"home\" abbrev \"away\" abbrev}`, in the app's vocabulary."
  [competition]
  (into {} (keep (fn [{:keys [homeAway team]}]
                   (when-let [t (teams/normalize :espn (:abbreviation team))]
                     [homeAway t])))
        (:competitors competition)))

(defn parse-event
  "One event -> its two `[team entry]` pairs, or nil. See the ns docstring."
  [{:keys [date status competitions]}]
  (let [competition (first competitions)
        {home "home" away "away"} (competitors competition)
        ;; Not `{:keys [name ...]}` — that shadows `clojure.core/name`.
        status-name (get-in status [:type :name])
        detail      (get-in status [:type :shortDetail])]
    (when (and date home away)
      (let [entry {:kickoff  date
                   :status   status-name
                   :detail   detail
                   :venue    (get-in competition [:venue :fullName])
                   :neutral? (boolean (:neutralSite competition))}]
        [[home (assoc entry :opponent away :home? true)]
         [away (assoc entry :opponent home :home? false)]]))))

(defn by-team
  "Decoded scoreboard -> `{team entry}`, one per team playing."
  [payload]
  (into {} (mapcat parse-event) (:events payload)))

(defn fetch
  "Network. Every failure shape collapses to nil, including a 200 whose body is
  not a scoreboard: the caller trades a kickoff for it, never the projection."
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
