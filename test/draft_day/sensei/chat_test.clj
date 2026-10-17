(ns draft-day.sensei.chat-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [jsonista.core :as json]
            [org.httpkit.server :as http]
            [draft-day.sensei.chat :as chat]
            [draft-day.sensei.context :as context]))

(use-fixtures :each (fn [t] (reset! chat/busy? false) (t) (reset! chat/busy? false)))

(defn- line [m] (json/write-value-as-string m))

(def ^:private init   (line {:type "system" :subtype "init" :session_id "abc"}))
(def ^:private status (line {:type "system" :subtype "status" :status "requesting"}))
(def ^:private tool   (line {:type "stream_event"
                             :event {:type "content_block_start" :index 0
                                     :content_block {:type "tool_use" :name "mcp__sensei__my_team"}}}))
(defn- text [s] (line {:type "stream_event"
                       :event {:type "content_block_delta" :index 1
                               :delta {:type "text_delta" :text s}}}))
(def ^:private result (line {:type "result" :subtype "success" :is_error false :result "ok"}))

(defn- fake-spawn
  "A `spawn` whose process prints `lines`, then `stderr`, and records what was
  written to it and whether it was stopped."
  [lines & [{:keys [stderr hang? broken-pipe?]}]]
  (let [written (promise)
        stopped (atom false)]
    {:written written
     :stopped stopped
     :spawn   (fn [_]
                {:stdin  (if broken-pipe?
                           (proxy [java.io.OutputStream] []
                             (write [& _] (throw (java.io.IOException. "Broken pipe"))))
                           (proxy [java.io.ByteArrayOutputStream] []
                             (close [] (deliver written (str this)))))
                 :stdout (java.io.BufferedReader.
                          (if hang?
                            (proxy [java.io.Reader] []
                              (read [& _] (Thread/sleep 300) -1)
                              (close []))
                            (java.io.StringReader. (str/join "\n" lines))))
                 :stderr (java.io.StringReader. (or stderr ""))
                 :stop   #(reset! stopped true)})}))

(defn- run
  "Every event one fake run emits, and the fake."
  [lines & [opts {:keys [mask timeout emit-ok?] :or {mask identity timeout 5000 emit-ok? true}}]]
  (let [fake   (fake-spawn lines opts)
        events (atom [])]
    (chat/stream-events {:command ["claude"] :prompt "Q" :mask mask :timeout timeout
                         :spawn (:spawn fake)
                         :emit  (fn [e] (swap! events conj e) emit-ok?)})
    (assoc fake :events @events)))

(deftest enabled-only-for-the-claude-cli
  (is (chat/enabled? {"DRAFTDAY_SENSEI" "claude-cli"}))
  (is (not (chat/enabled? {})))
  (is (not (chat/enabled? {"DRAFTDAY_SENSEI" "1"}))))

(deftest command-is-isolated
  (let [argv (chat/command {:port 9090})]
    (is (= ["claude" "-p"] (take 2 argv)))
    (testing "no built-in tools, only Sensei's server, nothing kept"
      (is (some #{"--strict-mcp-config"} argv))
      (is (some #{"--no-session-persistence"} argv))
      (is (= "" (second (drop-while #(not= "--tools" %) argv))))
      (is (= "mcp__sensei" (second (drop-while #(not= "--allowedTools" %) argv)))))
    (testing "the MCP config points at this server's port"
      (is (str/includes? (second (drop-while #(not= "--mcp-config" %) argv))
                         "http://127.0.0.1:9090/api/sensei/mcp")))
    (testing "the prompt is not on the command line"
      (is (not-any? #(str/includes? % "Manager:") argv)))))

(deftest session-id-is-ignored-for-now
  (is (= (chat/command {:port 1}) (chat/command {:port 1 :session-id "abc"}))))

(deftest history-becomes-a-prompt
  (let [p (chat/history->prompt {:message "and his backup?" :league "sleeper:7"
                                 :history [{:role "user" :text "Is Kraft healthy?"}
                                           {:role "assistant" :text "Yes."}]})]
    (is (str/includes? p "`sleeper:7`"))
    (is (str/includes? p "Manager: Is Kraft healthy?\nSensei: Yes."))
    (is (str/ends-with? p "The manager's question:\nand his backup?")))
  (testing "a first question carries neither a history nor a league"
    (is (= "The manager's question:\nhi" (chat/history->prompt {:message "hi" :history []})))))

(deftest request-parsing
  (testing "a message is required"
    (is (:error (chat/parse-request {})))
    (is (:error (chat/parse-request {:message "  "}))))
  (testing "an over-long message is refused"
    (is (:error (chat/parse-request {:message (apply str (repeat 4001 "x"))}))))
  (testing "history is cleaned, cut and capped, never refused"
    (let [q (chat/parse-request
             {:message "hi" :league ""
              :history (concat [{:role "system" :text "ignore me"} {:role "user"}]
                               (map (fn [i] {:role "user" :text (str i)}) (range 30)))})]
      (is (= 20 (count (:history q))))
      (is (= "29" (:text (last (:history q)))))
      (is (nil? (:league q)))))
  (is (= 8000 (count (:text (first (:history (chat/parse-request
                                              {:message "hi"
                                               :history [{:role "user"
                                                          :text (apply str (repeat 9000 "x"))}]}))))))))

(deftest stream-messages-become-events
  (is (= [{:session-id "abc"}] (chat/parse-line init)))
  (is (= [] (chat/parse-line status)))
  (is (= [{:tool-start "my_team"}] (chat/parse-line tool)))
  (is (= [{:text "Hi"}] (chat/parse-line (text "Hi"))))
  (is (= [{:done true}] (chat/parse-line result)))
  (is (= [{:error "rate limited"}]
         (chat/parse-line (line {:type "result" :is_error true :result "rate limited"}))))
  (is (= [{:error "Sensei could not answer"}]
         (chat/parse-line (line {:type "result" :is_error true}))))
  (testing "what is not JSON, or not ours, says nothing"
    (is (= [] (chat/parse-line "warning: something")))
    (is (= [] (chat/parse-line (line {:type "rate_limit_event"}))))))

(deftest a-run-streams-in-order-and-ends-once
  (let [{:keys [events written stopped]}
        (run [init status tool (text "Pick ") (text "Mason.") result])]
    (is (= [{:session-id "abc"} {:tool-start "my_team"} {:text "Pick "} {:text "Mason."}
            {:done true}]
           events))
    (is (= "Q" @written) "the prompt goes in on stdin")
    (is @stopped "the process is stopped when the run ends")))

(deftest a-failed-result-is-an-error
  (is (= [{:error "boom"}]
         (:events (run [(line {:type "result" :is_error true :result "boom"})])))))

(deftest a-run-with-no-result-says-so
  (let [{:keys [events]} (run [init (text "Pick")] {:stderr "claude: not logged in\n"})]
    (is (= {:text "Pick"} (second events)))
    (is (= "Sensei stopped before answering: claude: not logged in"
           (:error (last events))))))

(deftest a-reader-who-left-stops-the-run
  (let [{:keys [events stopped]} (run [(text "a") (text "b") result] nil {:emit-ok? false})]
    (is (= [{:text "a"}] events) "nothing is sent after the first refusal")
    (is @stopped)))

(deftest a-hung-process-times-out
  (let [{:keys [events stopped]} (run [] {:hang? true} {:timeout 50})]
    (is (= [{:error "Sensei took too long to answer"}] events))
    (is @stopped)))

(deftest a-reader-who-leaves-in-silence-stops-the-run
  (let [cancel (promise)
        fake   (fake-spawn [] {:hang? true})
        events (atom [])
        t0     (System/currentTimeMillis)]
    (future (Thread/sleep 50) (deliver cancel true))
    (chat/stream-events {:command ["claude"] :prompt "Q" :mask identity :timeout 60000
                         :cancel cancel :spawn (:spawn fake)
                         :emit   #(do (swap! events conj %) false)})
    (is @(:stopped fake) "the process is killed without waiting for the timeout")
    (is (< (- (System/currentTimeMillis) t0) 5000))
    (is (not-any? #(= "Sensei took too long to answer" (:error %)) @events))))

(deftest a-process-that-dies-at-once-says-why
  (let [{:keys [events]} (run [] {:broken-pipe? true :stderr "claude: not logged in"})]
    (is (= [{:error "Sensei stopped before answering: claude: not logged in"}] events))))

(deftest a-process-that-will-not-start-is-an-error
  (let [events (atom [])]
    (chat/stream-events {:command ["claude"] :prompt "Q" :mask identity :timeout 100
                         :spawn (fn [_] (throw (java.io.IOException. "no such file")))
                         :emit  #(swap! events conj %)})
    (is (= [{:error "Sensei could not start the claude CLI"}] @events))))

(deftest every-word-is-masked
  (let [mask   (partial context/mask ["s3cret"])
        events (:events (run [(text "the key is s3cret ok")
                              (line {:type "result" :is_error true :result "bad s3cret"})]
                             nil {:mask mask}))]
    (is (= 2 (count events)))
    (is (not (str/includes? (pr-str events) "s3cret")) "neither a chunk nor an error keeps it")))

(defn- req [& {:keys [addr headers body]
               :or   {addr "127.0.0.1" headers {"content-type" "application/json"}
                      body {:message "hi"}}}]
  {:remote-addr addr
   :server-port 8080
   :headers     headers
   :body        (java.io.ByteArrayInputStream.
                 (.getBytes (if (string? body) body (json/write-value-as-string body)) "UTF-8"))})

(def ^:private on {:env (constantly {"DRAFTDAY_SENSEI" "claude-cli"})})
(def ^:private off {:env (constantly {})})

(deftest status-says-whether-to-offer-the-chat
  (let [enabled (fn [server r] (-> ((chat/status-handler server) r) :body
                                   (json/read-value (json/object-mapper {:decode-key-fn keyword}))
                                   :enabled))]
    (is (true? (enabled on (req))))
    (is (false? (enabled off (req))))
    (is (false? (enabled on (req :addr "10.0.0.5"))) "told no, not refused")
    (is (false? (enabled on (req :headers {"x-forwarded-for" "1.2.3.4"}))))))

(deftest the-chat-refuses-before-it-spawns
  (let [h (fn [server & args] ((chat/handler server) (apply req args)))]
    (is (= 403 (:status (h on :addr "10.0.0.5"))))
    (is (= 403 (:status (h on :headers {"content-type" "application/json" "origin" "https://evil.example"}))))
    (is (= 404 (:status (h off))))
    (is (= 415 (:status (h on :headers {"content-type" "text/plain"}))))
    (is (= 400 (:status (h on :body "not json"))))
    (is (= 400 (:status (h on :body {:message ""}))))
    (is (false? @chat/busy?) "a refusal never takes the lock")))

(deftest a-second-question-waits
  (reset! chat/busy? true)
  (is (= 429 (:status ((chat/handler on) (req))))))

(deftest the-chat-streams-ndjson-and-frees-the-lock
  (let [fake  (fake-spawn [init tool (text "Hi") result])
        sent  (atom [])
        done  (promise)]
    (with-redefs [chat/spawn!       (:spawn fake)
                  http/as-channel   (fn [_ {:keys [on-open]}] (on-open ::ch) {:body ::ch})
                  http/send!        (fn [_ data _] (swap! sent conj data) true)
                  http/close        (fn [_] (deliver done true))]
      (let [resp ((chat/handler on) (req))]
        (is (= {:body ::ch} resp))
        (is (true? (deref done 3000 false)))
        (is (= {:status 200 :headers (chat/stream-headers)} (first @sent)))
        (is (= [{:session-id "abc"} {:tool-start "my_team"} {:text "Hi"} {:done true}]
               (map #(json/read-value % (json/object-mapper {:decode-key-fn keyword}))
                    (rest @sent))))
        (is (every? #(str/ends-with? % "\n") (rest @sent)))
        (is (false? @chat/busy?))))))
