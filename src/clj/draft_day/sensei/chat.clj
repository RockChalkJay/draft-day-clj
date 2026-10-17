(ns draft-day.sensei.chat
  "The in-app chat: one question put to `claude -p`, answered as a stream.

  The process is isolated by hand, since `--bare` will not read a subscription
  login: no built-in tools, only Sensei's own MCP server, no session kept, and a
  fixed working directory outside the repo so no CLAUDE.md is read and no new
  auto-memory folder appears per run. It runs on the owner's own plan, so it is
  off unless `DRAFTDAY_SENSEI=claude-cli`, answers this machine only and takes
  one question at a time. See docs/sensei.md for the terms."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [jsonista.core :as json]
            [org.httpkit.server :as http]
            [draft-day.json :refer [mapper]]
            [draft-day.sensei.context :as context]
            [draft-day.sensei.mcp :as mcp]))

(def timeout-ms
  "Measured questions took 17-26 s. Three minutes is a stuck process."
  180000)

(def max-message 4000)
(def max-history 20)
(def max-turn 8000)

(defn enabled?
  "Is the chat switched on in `env`?"
  [env]
  (= "claude-cli" (get env "DRAFTDAY_SENSEI")))

(defn work-dir
  "The one directory every run starts in."
  []
  (doto (io/file (System/getProperty "java.io.tmpdir") "draft-day-sensei")
    (.mkdirs)))

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

(defn command
  "The `claude` argv for one question. The prompt goes in on stdin."
  [{:keys [port session-id]}]
  (into ["claude" "-p"
         "--output-format" "stream-json" "--verbose" "--include-partial-messages"
         "--tools" ""
         "--strict-mcp-config" "--mcp-config" (mcp-config port)
         "--allowedTools" "mcp__sensei"
         "--append-system-prompt" (chat-note)]
        (session-flags session-id)))

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
  `{:tool-start name}`, `{:text delta}`, `{:done}` or `{:error message}`."
  [{:keys [type subtype session_id event is_error result]}]
  (case type
    "system" (if (= "init" subtype) [{:session-id session_id}] [])
    "stream_event"
    (let [{block :content_block delta :delta} event]
      (cond
        (and (= "content_block_start" (:type event)) (= "tool_use" (:type block)))
        [{:tool-start (tool-name (:name block))}]

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

(defn spawn!
  "Start `command` in `work-dir`: `{:stdin :stdout :stderr :stop}`."
  [command]
  (let [pb (ProcessBuilder. ^java.util.List command)
        _  (.directory pb (work-dir))
        p  (.start pb)]
    {:stdin  (.getOutputStream p)
     :stdout (io/reader (.getInputStream p))
     :stderr (io/reader (.getErrorStream p))
     :stop   #(.destroyForcibly p)}))

(defn stream-events
  "Feed `emit` the events of one run, in order, until the answer ends. `emit`
  answers whether the reader is still there; a false stops the run and kills the
  process. Always ends with exactly one `{:done}` or `{:error}` unless the
  reader left first."
  [{:keys [command prompt mask emit timeout spawn] :or {spawn spawn!}}]
  (let [emit* (comp emit (partial mask-event mask))]
    (if-let [p (try (spawn command)
                    (catch Exception e
                      (log/warn "sensei chat could not start claude:" (mask (str e)))
                      nil))]
      (let [timed-out? (atom false)
            watchdog   (future (Thread/sleep (long timeout))
                               (reset! timed-out? true)
                               ((:stop p)))
            stderr     (future (slurp (:stderr p)))]
        (try
          (with-open [w (io/writer (:stdin p))] (.write w ^String prompt))
          (let [outcome (with-open [r (:stdout p)]
                          (reduce (fn [_ line]
                                    (let [events (parse-line line)]
                                      (cond
                                        (not (every? emit* events)) (reduced :left)
                                        (some terminal? events)     (reduced :finished)
                                        :else                       nil)))
                                  nil (line-seq r)))]
            (when-not outcome
              (emit* {:error (if @timed-out?
                               "Sensei took too long to answer"
                               (str "Sensei stopped before answering"
                                    (when-let [e (not-empty (str/trim @stderr))]
                                      (str ": " (subs e (max 0 (- (count e) 300)))))))})))
          (catch Exception e
            (log/warn "sensei chat failed:" (mask (str e)))
            (emit* {:error "Sensei could not answer"}))
          (finally
            (future-cancel watchdog)
            ((:stop p)))))
      (emit* {:error "Sensei could not start the claude CLI"}))))

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
  accounts whose secrets every word is masked against."
  [server]
  (fn [req]
    (let [env (when-let [f (:env server)] (f))]
      (cond
        (not (mcp/local-request? req))
        (mcp/json-response 403 {:error "Sensei answers requests from this machine only"})

        (not (enabled? env))
        (mcp/json-response 404 {:error "Sensei chat is off"})

        (not (some-> (get-in req [:headers "content-type"]) (str/starts-with? "application/json")))
        (mcp/json-response 415 {:error "Content-Type must be application/json"})

        :else
        (let [parsed (try (json/read-value (:body req) mapper) (catch Exception _ nil))
              q      (if (map? parsed) (parse-request parsed) {:error "invalid request body"})]
          (cond
            (:error q)
            (mcp/json-response 400 q)

            (not (compare-and-set! busy? false true))
            (mcp/json-response 429 {:error "Sensei is already answering a question"})

            :else
            (try
              (let [mask    (partial context/mask (context/secrets (context/accounts env)))
                    command (command {:port (:server-port req) :session-id (:session-id q)})]
                (http/as-channel
                 req
                 {:on-open
                  (fn [ch]
                    (http/send! ch {:status 200 :headers (stream-headers)} false)
                    (future
                      (try
                        (stream-events {:command command
                                        :prompt  (history->prompt q)
                                        :mask    mask
                                        :timeout timeout-ms
                                        :emit    #(http/send! ch (json-line %) false)})
                        (finally
                          (reset! busy? false)
                          (http/close ch)))))}))
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
