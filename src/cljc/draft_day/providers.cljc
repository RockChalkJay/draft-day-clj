(ns draft-day.providers
  "Every fantasy host the app can talk to, and what connecting to one needs.

  Shared cljc for `scoring.cljc`'s reason: the browser renders the connect form
  from this table *synchronously*, before any request exists, and the server
  validates the map that form produces against the same table. Two copies of
  what ESPN needs is how one end comes to accept what the other rejects — the
  drift `league-id-error`'s docstring already names for a rule split between
  two endpoints.

  A provider's `:fields` are what identifies a manager to that host, whether or
  not they are secret. Sleeper's username is public and ESPN's cookies are a
  live session, but both arrive as `:credentials` and both go through one door,
  because a second door is where the next provider's handling starts looking
  like a special case.

  Validation here is a typo guard, not a security boundary: the host decides
  whether a credential is real, and a pattern tight enough to reject a valid
  cookie is worse than one that lets the 401 do the talking. What it does
  enforce is that a required field is present, that a league id is safe to put
  in a URL path, and that no key the catalog does not name reaches a provider.

  Adding a host is an entry here plus its `defmethod`s. No form code, no
  validation branch, no second copy of the rule."
  (:require [clojure.string :as str]))

(def catalog
  "provider -> what it is called, what connecting needs, and how it is reached."
  {:sleeper
   {:label     "Sleeper"
    :order     0
    :fields    [{:key         :username
                 :label       "Sleeper username"
                 :placeholder "sleeperuser"
                 :secret?     false
                 :required?   true
                 ;; Real handles carry dots and hyphens, so the pattern has to
                 ;; admit them — and admitting a dot admits `..`, which is why
                 ;; the traversal guard is a separate clause and not a tighter
                 ;; regex.
                 :pattern     #"[A-Za-z0-9_][A-Za-z0-9_.-]{0,31}"
                 :forbidden   #{".."}}]
    :public?   true
    :league-id {:label "Sleeper league ID" :pattern #"\d+"}
    :help      nil}

   :espn
   {:label     "ESPN"
    :order     1
    :fields    [{:key         :swid
                 :label       "SWID"
                 :placeholder "{XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX}"
                 :secret?     true
                 :required?   true
                 ;; Braces optional on the way in; `league-sync.espn` puts them
                 ;; back. They are not decoration — ESPN's `owners` array
                 ;; carries them, and a SWID stripped of them matches no team.
                 :pattern     #"\{?[0-9A-Fa-f]{8}(-[0-9A-Fa-f]{4}){3}-[0-9A-Fa-f]{12}\}?"}
                {:key         :espn-s2
                 :label       "espn_s2"
                 :placeholder "AEB...%2F..."
                 :secret?     true
                 :required?   true
                 ;; Length and "no whitespace" only. The value is a long opaque
                 ;; token whose alphabet ESPN is free to change.
                 :pattern     #"\S{60,}"}]
    :public?   false
    :league-id {:label "ESPN league ID" :pattern #"\d+"}
    :help      {:text "Sign in at fantasy.espn.com, then open DevTools → Application → Cookies and copy the SWID and espn_s2 values."
                :url  "https://fantasy.espn.com/football/"}}})

(def redacted
  "What a secret reads as once it is somewhere it could be read back."
  "••••")

(defn provider-key
  "`p` as a catalog key, or nil when it names no host.

  Providers cross the wire as strings — `draft-day.json/mapper` keywordizes
  keys and not values — so every reader here coerces rather than trusting the
  spelling it was handed."
  [p]
  (cond
    (keyword? p)              (when (contains? catalog p) p)
    (and (string? p)
         (not (str/blank? p))) (let [k (keyword p)]
                                 (when (contains? catalog k) k))))

(defn known? [p] (some? (provider-key p)))

(defn- entry [p] (get catalog (provider-key p)))

(defn providers
  "Every host, in the order the picker shows them."
  []
  (mapv key (sort-by (comp :order val) catalog)))

(defn label
  "What a manager calls this host, or the raw spelling when it is not one."
  [p]
  (or (:label (entry p)) (some-> p name)))

(defn fields
  "The credential fields this host needs, in render order."
  [p]
  (:fields (entry p) []))

(defn field-keys [p] (into #{} (map :key) (fields p)))

(defn secret-keys
  "The fields whose values must never be rendered, logged or echoed."
  [p]
  (into #{} (comp (filter :secret?) (map :key)) (fields p)))

(defn public?
  "Can a league on this host be read with no account at all?"
  [p]
  (boolean (:public? (entry p))))

(defn help [p] (:help (entry p)))

(defn league-id-label [p]
  (or (get-in (entry p) [:league-id :label]) "League ID"))

(defn league-id-error
  "The 400 body for a league id this host cannot use, or nil when it is fine.

  Per host rather than one global rule, because the id goes into a URL path
  segment and the day a host uses a non-numeric id the guard should loosen for
  that host alone."
  [p league-id]
  (let [s (str league-id)]
    (cond
      (not (known? p))       {:error (str "unknown provider: " (pr-str p))}
      (str/blank? s)         {:error "league-id is required"}
      (not (re-matches (get-in (entry p) [:league-id :pattern]) s))
      {:error (str (league-id-label p) " must be numeric")})))

(defn- field-error
  "Why this one field is unusable, or nil. Names the field, never the value:
  the message reaches the browser and an `espn_s2` echoed into it is the leak
  `redact` exists to prevent."
  [{:keys [key label required? pattern forbidden]} creds]
  (let [v (get creds key)
        s (some-> v str str/trim)]
    (cond
      (str/blank? s)
      (when required? {:error (str label " is required")})

      (and pattern (not (re-matches pattern s)))
      {:error (str label " is not in the expected format")}

      (some #(str/includes? s %) forbidden)
      {:error (str label " cannot contain " (pr-str (first (filter #(str/includes? s %) forbidden))))})))

(defn credential-errors
  "The 400 body for a credential map this host cannot use, or nil when it is
  fine.

  An unnamed key is an error rather than ignored: a provider that one day reads
  a key the catalog never promised would do so on input no validator had ever
  looked at."
  [p credentials]
  (cond
    (not (known? p))
    {:error (str "unknown provider: " (pr-str p))}

    (not (map? credentials))
    {:error (str (label p) " credentials are required")}

    :else
    (or (some #(field-error % credentials) (fields p))
        (when-let [extra (seq (remove (field-keys p) (keys credentials)))]
          {:error (str "unexpected credential field: " (name (first (sort extra))))}))))

(defn redact
  "Credentials with every secret value replaced, the rest untouched.

  The one function that may put a credential map anywhere it can be read back —
  a log line, an `ex-data`, an error body. It leaves the public fields alone on
  purpose: a redactor that masked Sleeper's username too would turn every
  diagnosable failure into an unreadable one."
  [p credentials]
  (when (map? credentials)
    (let [secrets (secret-keys p)]
      (reduce-kv (fn [m k v] (assoc m k (if (secrets k) redacted v)))
                 {} credentials))))

(defn league-access-error
  "The 400 body for a league this host cannot be asked about, or nil.

  Credentials are demanded only of a host that cannot be read without them.
  Sleeper serves a league document to anyone with the id, so requiring a
  username to import one would refuse a case that works — which is what
  `:public?` is in the catalog to say."
  [p league-id credentials]
  (or (league-id-error p league-id)
      (when-not (public? p)
        (credential-errors p credentials))))
