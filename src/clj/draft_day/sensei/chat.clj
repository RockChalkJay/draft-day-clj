(ns draft-day.sensei.chat
  "The in-app chat: one question put to `claude -p`, answered as a stream.

  The process is isolated by hand, since `--bare` will not read a subscription
  login: no built-in tools but web search and a fetch limited to named hosts
  (`web-flags`), Sensei's own MCP server, no settings file (and so no hook or
  plugin) and no CLAUDE.md, no session kept, none of the app's
  secrets in its environment, and one owner-only working directory outside the
  repo so no new auto-memory folder appears per run. It runs on the owner's own
  plan, so it is off unless `DRAFTDAY_SENSEI=claude-cli`, answers this machine
  only and takes one question at a time. See docs/sensei.md for the terms.

  The answer is the model's, and the tools hand it text other people write:
  league-mates' team names, news, and now the pages it searches. A fetch is the
  one way out that carries a URL of the model's choosing, so it is allowed for
  named hosts only, and `--permission-mode dontAsk` makes every tool not
  allowed a refusal rather than a prompt, which is what gives that list teeth.
  A client renders it as text plus a safe
  markdown subset, never an image or raw HTML, or an instruction planted in a
  team name can make the browser fetch a URL of the planter's choosing."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [jsonista.core :as json]
            [org.httpkit.server :as http]
            [draft-day.json :refer [mapper]]
            [draft-day.sensei.context :as context]
            [draft-day.sensei.mcp :as mcp])
  (:import (java.nio.file FileAlreadyExistsException FileSystems Files LinkOption)
           (java.nio.file.attribute FileAttribute PosixFilePermissions)))

(def timeout-ms
  "How long a run may take, answered or not, before it is stopped. Measured
  questions took 17-26 s, 39-69 s when the model also searches and fetches, and
  every other question waits on the lock."
  180000)

(def max-message 4000)
(def max-history 20)
(def max-turn 8000)

(defn enabled?
  "Is the chat switched on in `env`?"
  [env]
  (= "claude-cli" (get env "DRAFTDAY_SENSEI")))

(defn private-dir
  "`dir`, made if missing, as a directory only this user can enter. A link, or
  a directory someone else made, is refused: `claude -p` trusts its working
  directory without asking, and in a shared temp dir another user could make
  this one first and leave a settings file in it. Windows keeps a temp dir per
  user, and has no POSIX permissions to set."
  [^java.io.File dir]
  (if-not (contains? (.supportedFileAttributeViews (FileSystems/getDefault)) "posix")
    (doto dir (.mkdirs))
    (let [path       (.toPath dir)
          nofollow   (into-array LinkOption [LinkOption/NOFOLLOW_LINKS])
          owner-only (PosixFilePermissions/fromString "rwx------")]
      (try (Files/createDirectory
            path (into-array FileAttribute [(PosixFilePermissions/asFileAttribute owner-only)]))
           (catch FileAlreadyExistsException _))
      (when-not (and (Files/isDirectory path nofollow)
                     (= (System/getProperty "user.name")
                        (.getName (Files/getOwner path nofollow))))
        (throw (ex-info (str "Sensei's working directory " dir " is not this user's own."
                             " Remove it and ask again.")
                        {:dir (str dir)})))
      (Files/setPosixFilePermissions path owner-only)
      dir)))

(defn work-dir
  "The one directory every run starts in."
  []
  (private-dir (io/file (System/getProperty "java.io.tmpdir") "draft-day-sensei")))

(defn chat-note
  "What the panel adds to the persona the MCP server already serves."
  []
  (str/trim (slurp (io/resource "sensei/chat.md"))))

(defn mcp-config
  "Sensei's own endpoint on this server, as `--mcp-config` takes it."
  [port]
  (json/write-value-as-string
   {:mcpServers {:sensei {:type "http"
                          :url  (str "http://127.0.0.1:" port "/api/sensei/mcp")}}}))

(defn session-flags
  "Where a later `--resume <id>` goes. Each question is a fresh process today:
  the browser keeps the transcript and sends it with every question."
  [_session-id]
  ["--no-session-persistence"])

(def default-web-hosts
  "The hosts the chat's WebFetch may read: outlets that publish weekly waiver,
  start/sit and sleeper articles. A permission rule matches one host exactly,
  so `www.` is its own entry."
  ["www.fantasypros.com" "www.rotoballer.com" "sports.yahoo.com" "bleacherreport.com"
   "www.espn.com" "www.cbssports.com" "www.nbcsports.com"])

(defn web?
  "Does the chat get web tools? On unless `DRAFTDAY_SENSEI_WEB` is `off`."
  [env]
  (not= "off" (get env "DRAFTDAY_SENSEI_WEB")))

(defn web-hosts
  "The hosts WebFetch may read: `DRAFTDAY_SENSEI_WEB_HOSTS`, comma-separated,
  when set, else `default-web-hosts`. A name that is not a plain host is
  dropped, since it is written into a permission rule. Set but empty means
  none, so only search remains."
  [env]
  (if-some [custom (get env "DRAFTDAY_SENSEI_WEB_HOSTS")]
    (->> (str/split custom #",")
         (map (comp str/lower-case str/trim))
         (filter #(re-matches #"[a-z0-9]([a-z0-9.-]*[a-z0-9])?" %))
         distinct
         vec)
    default-web-hosts))

(defn web-flags
  "The `--tools` and `--allowedTools` for the web access `env` allows: search,
  and a fetch for each of `web-hosts` (none exposed when there are none)."
  [env]
  (if (web? env)
    (let [hosts (web-hosts env)]
      ["--tools" (if (seq hosts) "WebSearch,WebFetch" "WebSearch")
       "--allowedTools" (str/join "," (concat ["mcp__sensei" "WebSearch"]
                                              (map #(str "WebFetch(domain:" % ")") hosts)))])
    ["--tools" "" "--allowedTools" "mcp__sensei"]))

(defn command
  "The `claude` argv for one question, with the web access `env` allows. The
  prompt goes in on stdin."
  [{:keys [port session-id env]}]
  (-> ["claude" "-p"
       "--output-format" "stream-json" "--verbose" "--include-partial-messages"]
      (into (web-flags env))
      (into ["--permission-mode" "dontAsk"
             "--setting-sources" ""
             "--strict-mcp-config" "--mcp-config" (mcp-config port)
             "--append-system-prompt" (chat-note)])
      (into (session-flags session-id))))

(defn history->prompt
  "One prompt from the transcript so far, the league open in the app and the
  new `message`. `history` is `[{:role \"user\"|\"assistant\" :text}]`."
  [{:keys [history league message]}]
  (str/join
   "\n\n"
   (cond-> []
     league  (conj (str "The league open in the app is `" league "`. Use it as the"
                        " `league` argument unless the question names another."))
     (seq history)
     (conj (str "The conversation so far:\n"
                (str/join "\n" (map (fn [{:keys [role text]}]
                                      (str (if (= role "user") "Manager: " "Sensei: ") text))
                                    history))))
     true    (conj (str "The manager's question:\n" message)))))

(defn parse-request
  "The chat body as `{:message :history :league :session-id}`, or `{:error}`.
  Over-long turns are cut and old turns dropped rather than refused: a long
  conversation should still be askable."
  [body]
  (let [{:keys [message history league session-id]} body
        turns (into [] (comp (filter #(and (#{"user" "assistant"} (:role %))
                                           (string? (:text %))))
                             (map (fn [{:keys [role text]}]
                                    {:role role :text (subs text 0 (min max-turn (count text)))})))
                    (when (sequential? history) history))]
    (cond
      (not (and (string? message) (not (str/blank? message))))
      {:error "message is required"}

      (> (count message) max-message)
      {:error (str "message is over " max-message " characters")}

      :else
      {:message    message
       :history    (vec (take-last max-history turns))
       :league     (when (and (string? league) (not (str/blank? league))) league)
       :session-id (when (string? session-id) session-id)})))

(defn tool-name
  "A tool as the manager knows it: `my_team`, not the `mcp__sensei__` the CLI
  prefixes."
  [n]
  (str/replace (str n) #"^mcp__sensei__" ""))

(defn parse-event
  "The chat events one `stream-json` message carries: `{:session-id}`,
  `{:tool-start name}`, `{:text-start true}`, `{:text delta}`, `{:done}` or
  `{:error message}`."
  [{:keys [type subtype session_id event is_error result]}]
  (case type
    "system" (if (= "init" subtype) [{:session-id session_id}] [])
    "stream_event"
    (let [{block :content_block delta :delta} event]
      (cond
        (and (= "content_block_start" (:type event)) (= "tool_use" (:type block)))
        [{:tool-start (tool-name (:name block))}]

        (and (= "content_block_start" (:type event)) (= "text" (:type block)))
        [{:text-start true}]

        (and (= "content_block_delta" (:type event)) (= "text_delta" (:type delta)))
        [{:text (:text delta)}]

        :else []))
    "result" [(if is_error
                {:error (if (string? result) result "Sensei could not answer")}
                {:done true})]
    []))

(defn parse-line
  "`parse-event` for one line of the CLI's output. A line that is not JSON says
  nothing."
  [line]
  (try (parse-event (json/read-value line mapper))
       (catch Exception _ [])))

(defn terminal?
  "Does this event end the answer?"
  [event]
  (or (:done event) (contains? event :error)))

(defn mask-event
  "`event` with `mask` run over the words in it."
  [mask event]
  (cond-> event
    (:text event)  (update :text mask)
    (:error event) (update :error mask)))

(defn unfinished-secret
  "How many of the last characters of `text` could be the start of one of
  `secrets` that a later delta finishes."
  [secrets ^String text]
  (transduce (map (fn [^String s]
                    (or (some #(when (.regionMatches text (- (count text) %) s 0 %) %)
                              (range (min (dec (count s)) (count text)) 0 -1))
                        0)))
             max 0 secrets))

(defn text-emitter
  "`emit` for one answer's events, its text masked against `secrets` as it
  builds up: masking each delta alone misses a secret the CLI splits across
  two. Text that could be the start of a secret waits for the next delta, and
  any other event sends what waited ahead of itself. Each text block after the
  first opens a paragraph with its first text, since the CLI streams the text
  either side of a tool call as two blocks with nothing between them."
  [secrets emit]
  (let [mask   (partial context/mask secrets)
        held   (volatile! "")
        said?  (volatile! false)
        break? (volatile! false)]
    (fn [event]
      (cond
        (:text-start event)
        (do (vreset! break? @said?) true)

        (= "" (:text event))
        true

        (:text event)
        (let [text (mask (str @held (when @break? "\n\n") (:text event)))
              n    (- (count text) (unfinished-secret secrets text))]
          (vreset! said? true)
          (vreset! break? false)
          (vreset! held (subs text n))
          (or (zero? n) (emit {:text (subs text 0 n)})))

        :else
        (let [text @held]
          (vreset! held "")
          (and (or (= "" text) (emit {:text text}))
               (emit (mask-event mask event))))))))

(def withheld-prefixes
  "What the child never gets from the server's environment: the app's own
  settings, which hold the accounts' secrets, and anything that would answer on
  an account other than the plan, since an API key wins over the login under
  `-p` and bills at API prices without a word."
  ["DRAFTDAY_" "ANTHROPIC_" "CLAUDE_CODE_USE_"])

(defn child-env
  "The `claude` child process's environment: the server's `env` less
  `withheld-prefixes`, with no CLAUDE.md read from anywhere, and with what the
  CLI needs that `env` may lack. It finds a subscription login in the macOS
  keychain under the user's name, and without `USER` it says \"Not logged in\"
  however good the login is; a server started from an IDE or a service manager
  often has none."
  [env]
  (let [user (System/getProperty "user.name")]
    (cond-> (into {"CLAUDE_CODE_DISABLE_CLAUDE_MDS" "1"}
                  (remove (fn [[k]] (some #(str/starts-with? k %) withheld-prefixes)))
                  env)
      (nil? (get env "USER"))    (assoc "USER" user)
      (nil? (get env "LOGNAME")) (assoc "LOGNAME" user)
      (nil? (get env "HOME"))    (assoc "HOME" (System/getProperty "user.home")))))

(defn spawn!
  "Start `command` in `work-dir` under `(child-env env)`:
  `{:stdin :stdout :stderr :stop}`. Stopping kills what the process started
  first, since a `claude` on the PATH may be a wrapper that forks the real CLI,
  which would outlive it."
  [command env]
  (let [pb (ProcessBuilder. ^java.util.List command)
        _  (.directory pb (work-dir))
        _  (doto (.environment pb) (.clear) (.putAll (child-env env)))
        p  (.start pb)]
    {:stdin  (.getOutputStream p)
     :stdout (io/reader (.getInputStream p))
     :stderr (io/reader (.getErrorStream p))
     :stop   #(do (run! (fn [^java.lang.ProcessHandle h] (.destroyForcibly h))
                        (iterator-seq (.iterator (.descendants p))))
                  (.destroyForcibly p))}))

(defn ended-early
  "What to tell the manager when the process ended without an answer: why, when
  the CLI said so on stderr."
  [timed-out? stderr]
  (if timed-out?
    "Sensei took too long to answer"
    (str "Sensei stopped before answering"
         (when-let [e (not-empty (str/trim stderr))]
           (str ": " (subs e (max 0 (- (count e) 300))))))))

(defn stream-events
  "Feed `emit` the events of one run, in order, until the answer ends, every
  word masked against `secrets`. `emit` answers whether the reader is still
  there; a false stops the run and kills the process, as does delivering
  `cancel`, which is how a reader who left in a silent stretch is noticed.
  Always ends with exactly one `{:done}` or `{:error}` unless the reader left
  first. `env` is the server's, which `spawn` makes the child's."
  [{:keys [command env prompt secrets emit timeout spawn cancel]
    :or   {spawn spawn! cancel (promise)}}]
  (let [mask  (partial context/mask secrets)
        emit* (text-emitter secrets emit)
        p     (try (spawn command env) (catch Exception e e))]
    (if (instance? Exception p)
      (do (log/warn "sensei chat could not start claude:" (mask (str p)))
          ;; Only a refused working directory says why in an ex-info.
          (emit* {:error (if (ex-data p)
                           (ex-message p)
                           "Sensei could not start the claude CLI. Is it on the server's PATH?")}))
      (let [timed-out? (atom false)
            _          (future (when (= ::timeout (deref cancel (long timeout) ::timeout))
                                 (reset! timed-out? true))
                               ((:stop p)))
            ;; Killing the process closes its stderr, perhaps under this read.
            stderr     (future (let [w (java.io.StringWriter.)]
                                 (try (io/copy (:stderr p) w)
                                      (catch java.io.IOException _))
                                 (str w)))
            ;; A process that died at once breaks the pipe before it reads the
            ;; prompt, so a write that throws is the same outcome as no answer.
            outcome    (try
                         (with-open [w (io/writer (:stdin p))] (.write w ^String prompt))
                         (with-open [r (:stdout p)]
                           (reduce (fn [_ line]
                                     (let [events (parse-line line)]
                                       (cond
                                         (not (every? emit* events)) (reduced :left)
                                         (some terminal? events)     (reduced :finished)
                                         :else                       nil)))
                                   nil (line-seq r)))
                         (catch Exception e
                           (log/warn "sensei chat failed:" (mask (str e)))
                           nil)
                         (finally
                           (deliver cancel true)
                           ((:stop p))))]
        (when-not outcome
          (emit* {:error (ended-early @timed-out? (deref stderr 1000 ""))}))))))

;; One question at a time: a second would share the plan's limits, and no one
;; panel needs two.
(defonce busy? (atom false))

(defn json-line [event]
  (str (json/write-value-as-string event) "\n"))

(defn stream-headers []
  {"Content-Type"      "application/x-ndjson"
   "Cache-Control"     "no-cache"
   "X-Accel-Buffering" "no"})

(defn handler
  "The Ring handler for POST /api/sensei/chat. `server` is the map the MCP
  endpoint reads (`routes/sensei-server`), whose `:env` holds the gate and the
  accounts whose secrets every word is masked against, and whose `:port` is
  where the child finds that endpoint."
  [server]
  (fn [req]
    (let [env (when-let [f (:env server)] (f))]
      (if-let [[status message] (mcp/refusal req)]
        (mcp/json-response status {:error message})
        (let [parsed (mcp/read-body req)
              q      (if (map? parsed) (parse-request parsed) {:error "invalid request body"})]
          (cond
            (not (enabled? env))
            (mcp/json-response 404 {:error "Sensei chat is off"})

            (:error q)
            (mcp/json-response 400 q)

            (not (compare-and-set! busy? false true))
            (mcp/json-response 429 {:error "Sensei is already answering a question"})

            :else
            (try
              (let [cancel  (promise)
                    secrets (context/secrets (context/accounts env))
                    command (command {:port ((:port server)) :session-id (:session-id q) :env env})]
                (or (http/as-channel
                     req
                     {:on-close (fn [_ _] (deliver cancel true))
                      :on-open
                      (fn [ch]
                        (http/send! ch {:status 200 :headers (stream-headers)} false)
                        (future
                          (try
                            (stream-events {:command command
                                            :env     env
                                            :prompt  (history->prompt q)
                                            :secrets secrets
                                            :timeout timeout-ms
                                            :cancel  cancel
                                            :emit    #(http/send! ch (json-line %) false)})
                            (finally
                              (reset! busy? false)
                              (http/close ch)))))})
                    ;; Only http-kit hands a request the channel `on-open` needs.
                    (do (reset! busy? false)
                        (mcp/json-response 500 {:error "Sensei chat needs http-kit's async channel"}))))
              (catch Throwable t
                (reset! busy? false)
                (throw t)))))))))

(defn status-handler
  "GET /api/sensei/status: whether this browser should offer the chat. A request
  from anywhere but this machine is told no, not refused, so the SPA behind a
  tunnel simply never shows it."
  [server]
  (fn [req]
    (mcp/json-response
     200 {:enabled (boolean (and (mcp/local-request? req)
                                 (enabled? (when-let [f (:env server)] (f)))))})))
