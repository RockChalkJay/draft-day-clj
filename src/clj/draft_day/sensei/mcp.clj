(ns draft-day.sensei.mcp
  "Sensei as a Model Context Protocol server, at POST /api/sensei/mcp: the
  Streamable HTTP transport in its JSON-only form, which is all a server of
  read-only tools needs. No session, no server-sent events — a GET is a 405 —
  and nothing held between calls but what the tools' providers cache anyway.

  Hand-rolled rather than built on the official Java SDK, whose server transport
  is a Servlet and does not mount on http-kit. A tools-only server speaks a
  handful of methods (`respond`), and a version the client asks for is echoed
  back when it is one of `protocol-versions`.

  It answers this machine only. The league it reads is the manager's, an ESPN
  one through the manager's own cookies, and http-kit listens on every
  interface; so a request from anywhere but loopback is refused, and so is one
  carrying a browser `Origin` other than localhost, which is how a web page
  would reach a loopback server by DNS rebinding. A tunnel (ngrok, cloudflared)
  connects from loopback too, so a `Host` that is not this machine's, or any
  header a proxy adds to say it is relaying, is refused as well."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [draft-day.json :refer [mapper]]
            [draft-day.sensei.context :as context]
            [draft-day.sensei.tools :as tools]
            [jsonista.core :as json])
  (:import (java.net InetAddress URI)))

(def server-info {:name "sensei" :title "Draft Day Sensei" :version "0.1.0"})

(def protocol-versions
  "Newest first. A client asking for any other gets the newest, and decides
  for itself whether to go on."
  ["2025-11-25" "2025-06-18" "2025-03-26" "2024-11-05"])

(defn instructions
  "Read per `initialize` rather than once, so an edit shows on the next connect."
  []
  (slurp (io/resource "sensei/instructions.md")))

(defn weekly-review
  "The prompt's text: one league when `league` names it, else every league the
  manager plays in, one at a time."
  [{:keys [league]}]
  (str (if (str/blank? league)
         (str "Review my team in every league I play in, one league at a time, naming the league "
              "before each section. In each, check")
         (str "Review my team in the league " (pr-str league) ". Check"))
       " my lineup for injuries, byes and anyone on my bench who should start instead. "
       "Then look at the waiver wire for claims worth making, naming the drop for each. "
       "Finish with my moves in priority order."))

(def prompts
  [{:name        "weekly_review"
    :title       "Weekly review"
    :description "Check this week's lineup, the waiver wire and the drop a claim would cost."
    :arguments   [{:name        "league"
                   :description "A league key or name from the leagues tool; leave empty to review every league."
                   :required    false}]
    :text        weekly-review}])

(defn rpc-result [id result] {:jsonrpc "2.0" :id id :result result})

(defn rpc-error [id code message] {:jsonrpc "2.0" :id id :error {:code code :message message}})

(defn initialize [{:keys [protocolVersion]}]
  {:protocolVersion (or (some #{protocolVersion} protocol-versions) (first protocol-versions))
   :capabilities    {:tools {:listChanged false} :prompts {:listChanged false}}
   :serverInfo      server-info
   :instructions    (instructions)})

(defn descriptor
  "A tool as `tools/list` states it."
  [tool]
  (-> (select-keys tool [:name :title :description])
      (assoc :inputSchema (:input-schema tool)
             :annotations {:readOnlyHint true})))

(defn call-tool
  "A `tools/call` reply. A failure the model can act on comes back as an
  `isError` result, as the spec asks, so the model reads it; only a tool that
  does not exist is a protocol error.

  Every word of the reply passes `context/mask` on its way out, whatever made
  it: a tool result is read by a third party, and a credential must not reach
  one by a path nobody thought to mask."
  [deps id {tool-name :name arguments :arguments}]
  (if-let [tool (tools/by-name tool-name)]
    (let [deps* @deps
          mask  (partial context/mask (context/secrets (:accounts deps*)))
          {:keys [result error]}
          (try (tools/run tool deps* arguments)
               (catch Exception e
                 ;; The message and the frame it came from, not the throwable: a
                 ;; stack trace prints every cause's message, unmasked.
                 (log/warn "sensei tool failed:" tool-name (mask (str e))
                           (some-> e .getStackTrace first str))
                 {:error (str "internal error: " (ex-message e))}))]
      (rpc-result id {:content [{:type "text"
                                 :text (mask (or error (json/write-value-as-string result)))}]
                      :isError (boolean error)}))
    (rpc-error id -32602 (str "unknown tool: " tool-name))))

(defn get-prompt [id {prompt-name :name arguments :arguments}]
  (if-let [p (some #(when (= prompt-name (:name %)) %) prompts)]
    (rpc-result id {:description (:description p)
                    :messages    [{:role "user" :content {:type "text" :text ((:text p) arguments)}}]})
    (rpc-error id -32602 (str "unknown prompt: " prompt-name))))

(defn respond
  "The JSON-RPC response to one message, or nil when none is owed: a
  notification, or a client's response to a request this server never makes."
  [deps {:keys [id method params] :as msg}]
  (cond
    (not (map? msg))           (rpc-error nil -32600 "invalid request")
    (nil? method)              nil
    (not (contains? msg :id))  nil
    :else
    (case method
      "initialize"   (rpc-result id (initialize params))
      "ping"         (rpc-result id {})
      "tools/list"   (rpc-result id {:tools (mapv descriptor tools/tools)})
      "tools/call"   (call-tool deps id params)
      "prompts/list" (rpc-result id {:prompts (mapv #(dissoc % :text) prompts)})
      "prompts/get"  (get-prompt id params)
      (rpc-error id -32601 (str "method not found: " method)))))

(defn loopback?
  [addr]
  (boolean (try (some-> addr not-empty InetAddress/getByName .isLoopbackAddress)
                (catch Exception _ false))))

(def local-names
  "How a URL names this machine."
  #{"localhost" "127.0.0.1" "[::1]"})

(defn local-origin?
  "No `Origin` at all (Claude Code sends none), or a page served from this
  machine."
  [origin]
  (or (nil? origin)
      (contains? local-names (try (.getHost (URI. origin)) (catch Exception _ nil)))))

(defn local-host?
  "No `Host` at all (HTTP/1.0), or one that names this machine. A rebound name
  and a tunnel's public name both fail it, with or without an `Origin`."
  [host]
  (or (nil? host)
      (contains? local-names (try (.getHost (URI. (str "//" host))) (catch Exception _ nil)))))

(def forwarding-headers
  "What a proxy or tunnel adds when it relays a request from somewhere else."
  ["x-forwarded-for" "forwarded" "x-real-ip"])

(defn local-request?
  "Does this request come from, and only through, this machine?"
  [{:keys [remote-addr headers]}]
  (and (loopback? remote-addr)
       (local-host? (get headers "host"))
       (local-origin? (get headers "origin"))
       (not-any? #(contains? headers %) forwarding-headers)))

(defn json-response [status body]
  {:status  status
   :headers {"Content-Type" "application/json"}
   :body    (json/write-value-as-string body)})

(defn refusal
  "Why a Sensei endpoint turns `req` away unread, as `[status message]`, or
  nil. The MCP server and the chat both ask it, so a gate added here guards
  both."
  [req]
  (cond
    (not (local-request? req))
    [403 "Sensei answers requests from this machine only"]

    ;; Said rather than left to the parse: the router's form middleware has
    ;; already read a form-encoded body, which then looks empty here.
    (not (some-> (get-in req [:headers "content-type"]) (str/starts-with? "application/json")))
    [415 "Content-Type must be application/json"]))

(defn read-body
  "The request's JSON body, or `::unreadable`."
  [req]
  (try (json/read-value (:body req) mapper) (catch Exception _ ::unreadable)))

(defn handler
  "The Ring handler for POST /api/sensei/mcp. `server` is what the tools read
  off it — see `tools/deps`. A batch (the 2025-03-26 transport's) is answered
  as one, its calls sharing one read of the board."
  [server]
  (fn [req]
    (if-let [[status message] (refusal req)]
      (json-response status (rpc-error nil (if (= 403 status) -32600 -32700) message))
      (let [msg (read-body req)]
        (if (= ::unreadable msg)
          (json-response 400 (rpc-error nil -32700 "parse error"))
          (let [deps    (delay (tools/deps server))
                replies (if (sequential? msg)
                          (not-empty (into [] (keep #(respond deps %)) msg))
                          (respond deps msg))]
            (if replies
              (json-response 200 replies)
              {:status 202 :headers {} :body ""})))))))
