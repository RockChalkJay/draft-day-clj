(ns draft-day.sensei.context
  "Which league Sensei is asked about, rebuilt on the server as the body the
  browser would post to /api/waivers.

  The browser owns a league's state, but a Claude app calling Sensei's tools has
  no browser behind it. For a connected league none is needed: everything the
  waiver board takes is either the league's own rules, which an import supplies
  (`db/league-owned-keys` is why — a connected league's settings are not
  editable), or its state, which a sync supplies. So the context is the import
  and the sync, re-fetched per call, plus the one fact neither knows: which team
  is the manager's.

  The league comes from `DRAFTDAY_SENSEI_PROVIDER`, `DRAFTDAY_SENSEI_LEAGUE_ID`
  and `DRAFTDAY_SENSEI_ROSTER_ID`. An ESPN league reads its cookies from
  `DRAFTDAY_ESPN_SWID` and `DRAFTDAY_ESPN_S2`, the integration tests' names;
  none of them is ever passed to a tool, so no credential reaches the model."
  (:require [clojure.string :as str]
            [draft-day.db :as db]
            [draft-day.ingestion.league-import :as league-import]
            [draft-day.ingestion.league-import.espn :as import-espn]
            [draft-day.ingestion.league-import.sleeper]
            [draft-day.ingestion.league-sync :as league-sync]
            [draft-day.ingestion.league-sync.espn]
            [draft-day.ingestion.league-sync.sleeper]
            [draft-day.json :refer [mapper]]
            [draft-day.providers :as providers]
            [jsonista.core :as json]))

(defn env-league
  "The league the `DRAFTDAY_SENSEI_*` env vars name, or nil unless all three
  are set."
  []
  (let [provider  (System/getenv "DRAFTDAY_SENSEI_PROVIDER")
        league-id (System/getenv "DRAFTDAY_SENSEI_LEAGUE_ID")
        roster-id (System/getenv "DRAFTDAY_SENSEI_ROSTER_ID")]
    (when (and provider league-id roster-id)
      {:provider provider :league-id league-id :roster-id roster-id})))

(defn env-credentials
  "What reading this provider's leagues takes, off the environment. Nil for
  Sleeper, which serves a league to anyone with its id."
  [provider]
  (when (= "espn" (name provider))
    (let [swid (System/getenv "DRAFTDAY_ESPN_SWID")
          s2   (System/getenv "DRAFTDAY_ESPN_S2")]
      (when (and swid s2) {:swid swid :espn-s2 s2}))))

(defn secrets-in
  "Every secret value in one provider's `credentials`, in each spelling an error
  might echo: ESPN's SWID is kept as pasted and read back with its braces."
  [provider credentials]
  (let [raw (map #(get credentials %) (providers/secret-keys provider))]
    (into [] (filter (every-pred string? seq))
          (cons (import-espn/normalize-swid (:swid credentials)) raw))))

(defn secrets
  "Every credential value this server holds."
  []
  (secrets-in :espn (env-credentials :espn)))

(defn mask
  "`text` with each of `secrets` replaced by what a secret reads as once it is
  somewhere it could be read back. The longest goes first, so a SWID with its
  braces is not left as `{••••}` by the same SWID without them."
  [secrets text]
  (reduce #(str/replace %1 %2 providers/redacted) text (sort-by (comp - count) secrets)))

(defn waivers-body
  "The /api/waivers body for an imported `config`, a synced `league` and the
  manager's `roster-id`, or `{:error msg}` when no team carries that id.

  The id is matched as a string, since an env var cannot say whether the host's
  ids are numbers, and the team's own id is what goes in the body."
  [config league roster-id]
  (let [roster (:roster config)
        team   (some #(when (= (str roster-id) (str (:roster-id %))) %) (:teams league))]
    (if-not team
      {:error (str "no team in this league has roster id " roster-id "; its teams are "
                   (->> (:teams league)
                        (map #(str (:roster-id %) " (" (:name %) ")"))
                        (interpose ", ")
                        (apply str)))}
      {:scoring            (:scoring config)
       :num-teams          (:num-teams config)
       :replacement-config (db/replacement-config roster)
       :league             league
       :my-roster-id       (:roster-id team)
       :roster             roster
       :roster-size        (count (db/roster-template roster))})))

(defn as-posted
  "`body` as the server would read it back from the browser.

  An import and a sync reach /api/waivers through the browser, so a keyword
  there arrives as a string and a provider's ids as whatever JSON made of them.
  A round trip gives Sensei exactly the body the Waivers tab sends, rather than
  one the board was never tested on."
  [body]
  (json/read-value (json/write-value-as-string body) mapper))

(defn failure
  "A provider's error, phrased for the model, with any secret the message
  echoes masked: a tool result is read by a third party, and an `espn_s2` is a
  live session."
  [step provider credentials error]
  (str step " failed: " (mask (secrets-in provider credentials) (str error))))

(defn league-context
  "The /api/waivers body for `league` (`{:provider :league-id :roster-id
  :season}`, defaulting to `env-league`), or `{:error msg}`. Imports and syncs
  concurrently: they are two hosts' worth of round trips, and neither needs the
  other."
  ([] (league-context (env-league)))
  ([{:keys [provider league-id roster-id season] :as league}]
   (if-not league
     {:error (str "Sensei has no league: set DRAFTDAY_SENSEI_PROVIDER, "
                  "DRAFTDAY_SENSEI_LEAGUE_ID and DRAFTDAY_SENSEI_ROSTER_ID")}
     (let [p        (keyword provider)
           creds    (env-credentials p)
           req      {:provider p :league-id (str league-id) :season season
                     :credentials creds}
           imported (future (league-import/import-league req))
           synced   (future (league-sync/sync-league req))]
       (cond
         (not (:ok @imported)) {:error (failure "import" p creds (:error @imported))}
         (not (:ok @synced))   {:error (failure "sync" p creds (:error @synced))}
         :else (let [body (waivers-body (:config @imported) (:league @synced) roster-id)]
                 (if (:error body) body (as-posted body))))))))
