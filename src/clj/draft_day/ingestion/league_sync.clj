(ns draft-day.ingestion.league-sync
  "Provider-agnostic league *sync*: who is on whose roster right now, and what
  each manager has left to bid with.

  The in-season sibling of `league-import`, and deliberately a separate pair of
  multimethods rather than more keys on that one. They differ in the only way
  that matters for a cache: an import is a league's *rules*, which change maybe
  once a year, while a sync is its *state*, which changes every time anyone
  makes a claim. Folding the second into the first would mean either re-fetching
  the rules on every waiver refresh or serving month-old rosters.

  Same registration convention as `league-import`: a provider namespace
  defmethods onto both multimethods and this namespace never requires one, so
  adding Yahoo or ESPN is a new file plus a `:require` in `routes`, not a change
  here. The network multimethods take one request map for the reason
  `league-import`'s docstring gives — a host that needs a season or a cookie
  has somewhere to read it from — while the normalizers stay pure and
  positional.

  Three guarantees are `sync-league`'s rather than a provider's, for
  `unwrap-execution`'s reason: the season is defaulted, the reply names the
  provider it came from, and every roster id is a string. That last one is not
  cosmetic. `db/provider->player-id` builds a string-keyed crosswalk from the
  id files, and `db/held-ids` maps an id it cannot find to itself — so a
  host that publishes integer ids would resolve none of them, every rostered
  player would read as a free agent, and no drop would ever be named."
  (:require [draft-day.db :as db]
            [draft-day.ingestion.season :as season]
            [draft-day.providers :as providers]))

(defmulti fetch-raw-rosters
  "Network: raw provider-specific roster payload, from a request map of
  `{:league-id :season :credentials}`. Throws ex-info with :status on failure,
  exactly as `league-import/fetch-raw-league` does."
  (fn [provider _req] provider))

(defmethod fetch-raw-rosters :default
  [provider _req]
  (throw (ex-info "Unknown league provider" {:status 400 :provider provider})))

(defmulti normalize-rosters
  "Pure: a provider's raw roster payload ->

    {:teams  [{:roster-id :owner-id :name :player-ids :active-ids :starter-ids
               :faab-used :faab-left :waiver-position :wins :losses}]
     :waiver {:type :faab|:rolling|:reverse-standings :budget n :min-bid n}}

  `:min-bid` is optional: a host whose floor is not read leaves it out (ESPN's
  is not), and absent reads as $0.

  `:active-ids` is required, not optional, and it is `:player-ids` minus anyone
  the provider parks — IR, taxi, whatever the host calls it. Both readers of a
  roster ask a different question of the two: `:player-ids` is who is
  unavailable to everyone else, `:active-ids` is who occupies a seat a claim
  would have to free.

  A provider that omits it does not degrade gracefully. `waiver/drop-candidate`
  reads an empty roster, never finds it full and so names no drop at all — the
  upgrade floor falls to 0 and every row on the board is overstated by the
  dropped player's whole rest-of-season line — while `db/team-roster` puts
  every player on the roster under IR / Taxi. Spelled out here because that first
  failure is silent, and it is the same one `waiver/held-ids` was written to
  memorialise."
  (fn [provider _raw] provider))

(defmulti find-user
  "Network: a manager's account on this provider, by the name he types.

  -> `{:user-id :display-name :avatar}`, and nothing else. Providers hand back
  far more than that — Sleeper's user document carries `email`, `phone` and
  `token` keys — and none of it has any business reaching the browser, so an
  implementation builds the map it returns rather than passing one through.

  Takes `{:credentials}` — whatever the catalog says identifies a manager to
  this host, which is a username on one and a session cookie on another.

  Throws ex-info with `:status` on failure; an unknown name is a 404."
  (fn [provider _req] provider))

(defmethod find-user :default
  [provider _req]
  (throw (ex-info "Unknown league provider" {:status 400 :provider provider})))

(defmulti list-leagues
  "Network: the leagues that account plays in, for one season.

  -> `[{:league-id :name :season :num-teams :status :avatar}]`.

  Takes `{:user-id :season :credentials}`.

  An empty vector is a real answer — a manager who plays no fantasy football
  this year — and must not be reported as a missing account."
  (fn [provider _req] provider))

(defmethod list-leagues :default
  [provider _req]
  (throw (ex-info "Unknown league provider" {:status 400 :provider provider})))

(defn unwrap-execution
  "An `ExecutionException`'s cause in its place; anything else unchanged.

  A provider is free to fetch its documents concurrently, and a future's deref
  wraps whatever the thunk threw in a `java.util.concurrent.ExecutionException`,
  which carries no ex-data of its own. So an `ex-info` saying `{:status 404}`
  arrives looking like a generic failure, and an unknown league id reports as a
  502 upstream error.

  It lives here rather than in a provider because the contract it protects is
  `fetch-raw-rosters`' — the multimethod's, shared by every provider — and
  because `sync-league` is what reads the status back out. A provider that
  fetches concurrently should not have to know its own 404 needs rescuing; the
  first one that forgets would regress this silently.

  Not in `parallel/all` either: that namespace declares its thunks best-effort,
  so a throwing thunk is already outside its contract, and every other caller
  wraps in `pipeline/best-effort` and never throws at all."
  [e]
  (if (instance? java.util.concurrent.ExecutionException e)
    (or (.getCause e) e)
    e))

(defn string-ids
  "One team's roster id lists as strings.

  `:player-ids` and `:active-ids` drop their nils: `(str nil)` is `\"\"`, an id
  that resolves to nothing and occupies a seat. `:starter-ids` is read by
  position, so a nil there becomes `db/empty-seat` instead — see it."
  [team]
  (-> (reduce (fn [t k] (update t k #(into [] (comp (remove nil?) (map str)) %)))
              team
              [:player-ids :active-ids])
      (update :starter-ids
              #(into [] (map (fn [id] (if (nil? id) db/empty-seat (str id)))) %))))

(defn normalized
  "A provider's normalized league, made to keep this namespace's promises: it
  names its provider and its roster ids are strings. See the ns docstring."
  [provider league]
  ;; Guarded rather than unconditional: `(mapv f nil)` is `[]`, which would turn
  ;; a provider that answered with no teams at all into a league everybody has
  ;; left — exactly the shape `db/reconcile-league-sync` drops on arrival.
  (cond-> (assoc league :provider provider)
    (sequential? (:teams league)) (update :teams #(mapv string-ids %))))

(defn sync-league
  "{:provider :league-id :season :credentials} -> {:ok true :league {...}}
  or {:ok false :status :error}.

  The same envelope `league-import/import-league` returns, so `routes` handles
  both with one shape."
  [{:keys [provider league-id season credentials]}]
  (let [provider (keyword provider)]
    (if-let [bad (providers/league-access-error provider league-id credentials)]
      {:ok false :status 400 :error (:error bad)}
      (try
        (let [raw (fetch-raw-rosters provider {:league-id   league-id
                                               :season      (season/resolve-season season)
                                               :credentials credentials})]
          {:ok true :league (normalized provider (normalize-rosters provider raw))})
        ;; One catch rather than two: the unwrap has to happen before the status is
        ;; read, and `ex-data` is nil for anything that is not an ex-info, so the
        ;; 502 default already covers what the second clause used to.
        (catch Exception e
          (let [cause (unwrap-execution e)]
            {:ok false
             :status (or (:status (ex-data cause)) 502)
             :error  (ex-message cause)}))))))

(defn auth-failure?
  "Did this fail because the host refused the credentials, rather than because
  it could not answer? The two need opposite treatment in `find-leagues`."
  [status]
  (contains? #{400 401 403} status))

(defn find-leagues
  "{:provider :credentials :season} -> {:ok true :user {...} :leagues [...]}
  or {:ok false :status :error}.

  The same envelope `sync-league` and `league-import/import-league` return, so
  `routes` handles all three with one shape.

  A host that identifies the manager but cannot list his leagues answers
  `{:ok true :leagues [] :leagues-error msg}` rather than failing outright.
  The two are different facts and the card acts on them differently: a manager
  with no leagues is told so, while a listing that broke gets the paste-a-
  league-id row. Reporting the second as the first is how a discovery outage
  came to tell a manager his account does not exist.

  Sequential rather than concurrent, because the leagues call needs the id the
  user call returns — there is nothing to overlap."
  [{:keys [provider credentials season]}]
  (let [provider (keyword provider)]
    (if-let [bad (providers/credential-errors provider credentials)]
      {:ok false :status 400 :error (:error bad)}
      (try
        (let [user (find-user provider {:credentials credentials})
              req  {:user-id     (:user-id user)
                    :season      (season/resolve-season season)
                    :credentials credentials}]
          (try
            {:ok true :user user :leagues (list-leagues provider req)}
            (catch Exception e
              (let [cause (unwrap-execution e)]
                (if (auth-failure? (:status (ex-data cause)))
                  (throw cause)
                  {:ok true :user user :leagues []
                   :leagues-error (ex-message cause)})))))
        (catch Exception e
          (let [cause (unwrap-execution e)]
            {:ok false
             :status (or (:status (ex-data cause)) 502)
             :error  (ex-message cause)}))))))
