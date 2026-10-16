(ns draft-day.sensei.context
  "Which leagues Sensei can be asked about, each rebuilt on the server as the
  body the browser would post to /api/waivers.

  The browser owns a league's state, but a Claude app calling Sensei's tools has
  no browser behind it. For a connected league none is needed: everything the
  waiver board takes is either the league's own rules, which an import supplies
  (`db/league-owned-keys` is why — a connected league's settings are not
  editable), or its state, which a sync supplies. So the context is the import
  and the sync, plus the one fact neither knows: which team is the manager's.

  The env holds accounts, not leagues. A Sleeper username is
  `DRAFTDAY_SENSEI_SLEEPER_USER`; an ESPN account reads its cookies from
  `DRAFTDAY_ESPN_SWID` and `DRAFTDAY_ESPN_S2`, the integration tests' names.
  `discover` lists each account's leagues with `league-sync/find-leagues`, the
  call Settings' Connect makes, and the manager's team is the one that account
  owns (`db/my-roster-id-for`), as in the browser. So a league joined tomorrow is
  there tomorrow, and nobody looks up a roster id.

  Credentials ride on a league's entry and go no further: none is ever passed to
  a tool, and `mask` is what keeps one out of a result that echoes it."
  (:require [clojure.string :as str]
            [draft-day.db :as db]
            [draft-day.ingestion.league-import :as league-import]
            [draft-day.ingestion.league-import.espn :as import-espn]
            [draft-day.ingestion.league-import.sleeper]
            [draft-day.ingestion.league-sync :as league-sync]
            [draft-day.ingestion.league-sync.espn]
            [draft-day.ingestion.league-sync.sleeper]
            [draft-day.ingestion.season :as season]
            [draft-day.json :refer [mapper]]
            [draft-day.providers :as providers]
            [jsonista.core :as json]))

(def env-fields
  "provider -> credential field -> the env var it is read from."
  {:sleeper {:username "DRAFTDAY_SENSEI_SLEEPER_USER"}
   :espn    {:swid "DRAFTDAY_ESPN_SWID" :espn-s2 "DRAFTDAY_ESPN_S2"}})

(defn accounts
  "`[{:provider :credentials}]` for each host `env` (a map of variable to value)
  holds credentials for, in the order the picker shows hosts. An account needs
  every field of its host: half an ESPN pair is no account."
  [env]
  (into []
        (keep (fn [p]
                (let [creds (update-vals (get env-fields p) #(not-empty (get env %)))]
                  (when (and (seq creds) (every? some? (vals creds)))
                    {:provider p :credentials creds}))))
        (providers/providers)))

(defn secrets-in
  "Every secret value in one provider's `credentials`, in each spelling an error
  might echo: ESPN's SWID is kept as pasted and read back with its braces."
  [provider credentials]
  (let [raw (map #(get credentials %) (providers/secret-keys provider))]
    (into [] (filter (every-pred string? seq))
          (cons (import-espn/normalize-swid (:swid credentials)) raw))))

(defn secrets
  "Every credential value in `accounts`."
  [accounts]
  (into [] (mapcat #(secrets-in (:provider %) (:credentials %))) accounts))

(defn mask
  "`text` with each of `secrets` replaced by what a secret reads as once it is
  somewhere it could be read back. The longest goes first, so a SWID with its
  braces is not left as `{••••}` by the same SWID without them."
  [secrets text]
  (reduce #(str/replace %1 %2 providers/redacted) text (sort-by (comp - count) secrets)))

(defn failure
  "A provider's error, phrased for the model, with any secret the message
  echoes masked: a tool result is read by a third party, and an `espn_s2` is a
  live session."
  [step provider credentials error]
  (str step " failed: " (mask (secrets-in provider credentials) (str error))))

;; ---- the clock and what it keeps ----

(defn now-ms [] (System/currentTimeMillis))

(def league-ttl-ms
  "How long a league's import and sync are reused. Claude calls tools in
  parallel, so a turn asks for the same league several times over; a claim the
  manager makes between turns shows within this."
  30000)

(def discovery-ttl-ms
  "How long an account's league list is reused. A league is joined once a year."
  600000)

(defn memo-get
  "The value of `(f)` for `k` in `cache`, shared by every caller within `ttl-ms`
  of the first, including one that arrives while it is still being fetched. A
  value carrying `:error` or `:errors`, or a throw, is dropped so that the next
  call tries again rather than pinning a failure for the whole TTL."
  [cache ttl-ms k f]
  (let [now    (now-ms)
        fresh? (fn [{:keys [at]}] (and at (< (- now at) ttl-ms)))
        entry  (-> (swap! cache #(if (fresh? (get % k)) % (assoc % k {:at now :value (delay (f))})))
                   (get k))
        evict! #(swap! cache (fn [m] (cond-> m (identical? entry (get m k)) (dissoc k))))
        v      (try @(:value entry) (catch Exception e (evict!) (throw e)))]
    (when (or (:error v) (seq (:errors v))) (evict!))
    v))

(defonce ^:private contexts (atom {}))

(defonce ^:private discovered (atom {}))

(defn reset-caches! [] (reset! contexts {}) (reset! discovered {}))

;; ---- the leagues ----

(defn league-entry
  "One discovered league, as every later step addresses it."
  [{:keys [provider credentials]} {:keys [user-id]} row]
  {:key         (db/league-key provider (:league-id row))
   :provider    provider
   :league-id   (str (:league-id row))
   :name        (:name row)
   :season      (:season row)
   :num-teams   (:num-teams row)
   :user-id     user-id
   :credentials credentials})

(defn discover-account
  "`{:leagues [entry] :errors [msg]}` for one account. A league from another
  season is dropped: ESPN's fan document names every season the manager has
  played, and nothing on a league that is over is worth a claim."
  [{:keys [provider credentials] :as account} season]
  (let [{:keys [ok user leagues leagues-error error]}
        (league-sync/find-leagues {:provider provider :credentials credentials :season season})
        step (str (providers/label provider) " league lookup")]
    (cond
      (not ok)       {:leagues [] :errors [(failure step provider credentials error)]}
      leagues-error  {:leagues [] :errors [(failure step provider credentials leagues-error)]}
      :else          {:leagues (into [] (comp (filter #(or (nil? (:season %))
                                                           (= (str season) (str (:season %)))))
                                              (map #(league-entry account user %)))
                                     leagues)
                      :errors  []})))

(defn discover
  "`{:leagues [entry] :errors [msg]}` across `accounts`, asked concurrently. One
  host being down does not hide the leagues on the other, and says so."
  [accounts]
  (let [season  (season/resolve-season nil)
        pending (mapv #(future (discover-account % season)) accounts)]
    (apply merge-with into {:leagues [] :errors []} (map deref pending))))

(defn leagues
  "`discover`, reused for `discovery-ttl-ms`."
  [accounts]
  (memo-get discovered discovery-ttl-ms (mapv :provider accounts) #(discover accounts)))

(defn no-leagues
  "Why there is nothing to read, for a model that can only relay it."
  [errors]
  (str "Sensei has no league to read. "
       (if (seq errors)
         (str/join "; " errors)
         "Set DRAFTDAY_SENSEI_SLEEPER_USER, or DRAFTDAY_ESPN_SWID and DRAFTDAY_ESPN_S2.")))

(defn choices
  "The leagues to choose from, as a tool error lists them."
  [leagues]
  (str/join "; " (map #(str (:key %) " (" (:name %) ")") leagues)))

(defn pick
  "The entry `league-arg` names among `found` (`{:leagues :errors}`): by key,
  else by name, case-folded. With none named, the only league there is — unless
  an account could not be listed, since then the only league is not known to be
  the only one. Anything else is `{:error msg}` listing the choices: a guess
  here prices one league's board under another's name."
  [{:keys [leagues errors]} league-arg]
  (let [arg (some-> league-arg str str/trim not-empty)]
    (cond
      (empty? leagues) {:error (no-leagues errors)}

      (nil? arg)
      (if (and (= 1 (count leagues)) (empty? errors))
        (first leagues)
        {:error (str "Name the league with the league argument. Choices: " (choices leagues)
                     (when (seq errors) (str ". Not listed: " (str/join "; " errors))))})

      :else
      (or (some #(when (= arg (:key %)) %) leagues)
          (let [named (filter #(= (str/lower-case arg) (str/lower-case (str (:name %)))) leagues)]
            (when (= 1 (count named)) (first named)))
          {:error (str "No league matches " (pr-str arg) ". Choices: " (choices leagues))}))))

;; ---- the board's body ----

(defn waivers-body
  "The /api/waivers body for an imported `config`, a synced `league` and the
  `user-id` of the account that plays in it, or `{:error msg}` when that account
  owns no team there.

  The manager's team is the one `db/my-roster-id-for` finds, as in the browser.
  The team's own id is what goes in the body."
  [config league user-id]
  (let [roster (:roster config)
        id     (db/my-roster-id-for (:teams league) user-id)
        team   (some #(when (= (str id) (str (:roster-id %))) %) (:teams league))]
    (if-not team
      {:error (str "this account owns no team in this league; its teams are "
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

(defn fetch-context
  "The /api/waivers body for `entry`, or `{:error msg}`. Imports and syncs
  concurrently: they are two hosts' worth of round trips, and neither needs the
  other."
  [{:keys [provider league-id season credentials user-id]}]
  (let [req      {:provider provider :league-id league-id :season season :credentials credentials}
        imported (future (league-import/import-league req))
        synced   (future (league-sync/sync-league req))]
    (cond
      (not (:ok @imported)) {:error (failure "import" provider credentials (:error @imported))}
      (not (:ok @synced))   {:error (failure "sync" provider credentials (:error @synced))}
      :else (let [body (waivers-body (:config @imported) (:league @synced) user-id)]
              (if (:error body) body (as-posted body))))))

(defn league-context
  "`fetch-context`, reused for `league-ttl-ms` and shared by callers that arrive
  while it is being fetched."
  [entry]
  (memo-get contexts league-ttl-ms (:key entry) #(fetch-context entry)))
