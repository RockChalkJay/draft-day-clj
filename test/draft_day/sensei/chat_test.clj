(ns draft-day.sensei.chat-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [jsonista.core :as json]
            [org.httpkit.server :as http]
            [draft-day.providers :as providers]
            [draft-day.sensei.chat :as chat])
  (:import (java.nio.file Files LinkOption)
           (java.nio.file.attribute FileAttribute PosixFilePermissions)))

(use-fixtures :each (fn [t] (reset! chat/busy? false) (t) (reset! chat/busy? false)))

(defn- line [m] (json/write-value-as-string m))

(def ^:private init   (line {:type "system" :subtype "init" :session_id "abc"}))
(def ^:private status (line {:type "system" :subtype "status" :status "requesting"}))
(def ^:private tool   (line {:type "stream_event"
                             :event {:type "content_block_start" :index 0
                                     :content_block {:type "tool_use" :name "mcp__sensei__my_team"}}}))
(def ^:private text-start (line {:type "stream_event"
                                 :event {:type "content_block_start" :index 0
                                         :content_block {:type "text" :text ""}}}))
(defn- text [s] (line {:type "stream_event"
                       :event {:type "content_block_delta" :index 1
                               :delta {:type "text_delta" :text s}}}))
(def ^:private result (line {:type "result" :subtype "success" :is_error false :result "ok"}))

(defn- fake-spawn
  "A `spawn` whose process prints `lines`, then `stderr`, and records the command
  and environment it was started with, what was written to it and whether it
  was stopped."
  [lines & [{:keys [stderr stderr-closed? hang? broken-pipe?]}]]
  (let [command (promise)
        env     (promise)
        written (promise)
        stopped (atom false)]
    {:command command
     :env     env
     :written written
     :stopped stopped
     :spawn   (fn [cmd e]
                (deliver command cmd)
                (deliver env e)
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
                 :stderr (if stderr-closed?
                           (proxy [java.io.Reader] []
                             (read [& _] (throw (java.io.IOException. "Stream closed")))
                             (close []))
                           (java.io.StringReader. (or stderr "")))
                 :stop   #(reset! stopped true)})}))

(defn- run
  "Every event one fake run emits, and the fake."
  [lines & [opts {:keys [secrets timeout emit-ok?] :or {timeout 5000 emit-ok? true}}]]
  (let [fake   (fake-spawn lines opts)
        events (atom [])]
    (chat/stream-events {:command ["claude"] :prompt "Q" :secrets secrets :timeout timeout
                         :spawn (:spawn fake)
                         :emit  (fn [e] (swap! events conj e) emit-ok?)})
    (assoc fake :events @events)))

(deftest enabled-only-for-the-claude-cli
  (is (chat/enabled? {"DRAFTDAY_SENSEI" "claude-cli"}))
  (is (not (chat/enabled? {})))
  (is (not (chat/enabled? {"DRAFTDAY_SENSEI" "1"}))))

(deftest the-child-gets-the-user-name-the-cli-needs-to-find-its-login
  (let [user (System/getProperty "user.name")]
    (is (= {"USER" user "LOGNAME" user "HOME" (System/getProperty "user.home")}
           (select-keys (chat/child-env {}) ["USER" "LOGNAME" "HOME"]))
        "a server started without them is the \"Not logged in\" case")
    (is (= {"USER" "jay" "LOGNAME" "jay" "HOME" "/h"}
           (select-keys (chat/child-env {"USER" "jay" "LOGNAME" "jay" "HOME" "/h"})
                        ["USER" "LOGNAME" "HOME"]))
        "what the server already has is left alone")
    (is (= user (get (chat/child-env {"USER" "jay" "HOME" "/h"}) "LOGNAME")))))

(deftest the-child-gets-none-of-the-apps-settings
  (let [env (chat/child-env {"PATH"                         "/bin"
                             "DRAFTDAY_ESPN_S2"             "AEB"
                             "DRAFTDAY_ESPN_SWID"           "{X}"
                             "DRAFTDAY_SENSEI_SLEEPER_USER" "jay"
                             "DRAFTDAY_SENSEI"              "claude-cli"})]
    (is (= "/bin" (get env "PATH")))
    (is (not-any? #(str/starts-with? % "DRAFTDAY_") (keys env))
        "the accounts' secrets stay with the server")))

(deftest the-child-answers-on-the-plan-and-nothing-else
  (let [env (chat/child-env {"ANTHROPIC_API_KEY"       "sk-ant"
                             "ANTHROPIC_AUTH_TOKEN"    "tok"
                             "ANTHROPIC_BASE_URL"      "https://proxy.example"
                             "CLAUDE_CODE_USE_BEDROCK" "1"
                             "CLAUDE_CODE_OAUTH_TOKEN" "plan-token"})]
    (is (not-any? #(or (str/starts-with? % "ANTHROPIC_") (str/starts-with? % "CLAUDE_CODE_USE_"))
                  (keys env))
        "an API key wins over the login under -p and bills at API prices")
    (is (= "plan-token" (get env "CLAUDE_CODE_OAUTH_TOKEN")) "the plan's own token is a login")
    (is (= "1" (get env "CLAUDE_CODE_DISABLE_CLAUDE_MDS"))
        "no CLAUDE.md from the working directory up, nor the user's own")))

(deftest the-working-directory-is-the-owners-alone
  (let [base  (.toFile (Files/createTempDirectory "sensei-test" (make-array FileAttribute 0)))
        dir   (io/file base "work")
        perms #(PosixFilePermissions/toString
                (Files/getPosixFilePermissions (.toPath ^java.io.File %) (make-array LinkOption 0)))]
    (try
      (is (= dir (chat/private-dir dir)))
      (is (= "rwx------" (perms dir)))
      (testing "one already there is closed to everyone else"
        (Files/setPosixFilePermissions (.toPath dir) (PosixFilePermissions/fromString "rwxr-xr-x"))
        (chat/private-dir dir)
        (is (= "rwx------" (perms dir))))
      (testing "a link is refused, wherever it points"
        (let [link (io/file base "link")]
          (Files/createSymbolicLink (.toPath link) (.toPath dir) (make-array FileAttribute 0))
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not this user's own"
                                (chat/private-dir link)))))
      (finally
        (run! io/delete-file (reverse (file-seq base)))))))

(deftest command-is-isolated
  (let [argv (chat/command {:port 9090})]
    (is (= ["claude" "-p"] (take 2 argv)))
    (testing "no built-in tools, only Sensei's server, nothing kept"
      (is (some #{"--strict-mcp-config"} argv))
      (is (some #{"--no-session-persistence"} argv))
      (is (= "" (second (drop-while #(not= "--tools" %) argv))))
      (is (= "" (second (drop-while #(not= "--setting-sources" %) argv)))
          "no settings file, so no hook or plugin")
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
  (is (= [{:text-start true}] (chat/parse-line text-start)))
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

(deftest text-either-side-of-a-tool-call-is-two-paragraphs
  (is (= [{:text "Let me look."} {:tool-start "my_team"} {:text "\n\nPick Mason."} {:done true}]
         (:events (run [text-start (text "Let me look.") tool
                        text-start (text "Pick Mason.") result])))))

(deftest a-block-with-no-text-adds-no-paragraph
  (is (= [{:text "Let me look."} {:tool-start "my_team"} {:done true}]
         (:events (run [text-start (text "Let me look.") tool text-start (text "") result])))))

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
    (chat/stream-events {:command ["claude"] :prompt "Q" :timeout 60000
                         :cancel cancel :spawn (:spawn fake)
                         :emit   #(do (swap! events conj %) false)})
    (is @(:stopped fake) "the process is killed without waiting for the timeout")
    (is (< (- (System/currentTimeMillis) t0) 5000))
    (is (not-any? #(= "Sensei took too long to answer" (:error %)) @events))))

(deftest a-stderr-closed-under-the-read-still-ends-the-answer
  (is (= [{:error "Sensei stopped before answering"}]
         (:events (run [] {:stderr-closed? true})))))

(deftest a-process-that-dies-at-once-says-why
  (let [{:keys [events]} (run [] {:broken-pipe? true :stderr "claude: not logged in"})]
    (is (= [{:error "Sensei stopped before answering: claude: not logged in"}] events))))

(deftest a-process-that-will-not-start-is-an-error
  (let [events (atom [])]
    (chat/stream-events {:command ["claude"] :prompt "Q" :timeout 100
                         :spawn (fn [_ _] (throw (java.io.IOException. "no such file")))
                         :emit  #(swap! events conj %)})
    (is (= 1 (count @events)))
    (is (str/includes? (:error (first @events)) "PATH") "it names the likely cause"))
  (testing "a refused working directory says so"
    (let [events (atom [])]
      (chat/stream-events {:command ["claude"] :prompt "Q" :timeout 100
                           :spawn (fn [_ _] (throw (ex-info "the directory is not this user's own" {})))
                           :emit  #(swap! events conj %)})
      (is (= [{:error "the directory is not this user's own"}] @events)))))

(deftest stopping-a-run-stops-what-it-started
  (let [p     (chat/spawn! ["sh" "-c" "sleep 30 & echo $!; wait"] {"PATH" (System/getenv "PATH")})
        pid   (Long/parseLong (str/trim (.readLine ^java.io.BufferedReader (:stdout p))))
        alive #(.orElse (.map (java.lang.ProcessHandle/of pid)
                              (reify java.util.function.Function
                                (apply [_ h] (.isAlive ^java.lang.ProcessHandle h))))
                        false)]
    (is (alive))
    ((:stop p))
    (is (some (fn [_] (Thread/sleep 50) (not (alive))) (range 60))
        "a wrapper that forks the real CLI does not leave it running")))

(deftest every-word-is-masked
  (let [events (:events (run [(text "the key is s3cret ok")
                              (line {:type "result" :is_error true :result "bad s3cret"})]
                             nil {:secrets ["s3cret"]}))]
    (is (= 2 (count events)))
    (is (not (str/includes? (pr-str events) "s3cret")) "neither a chunk nor an error keeps it")))

(deftest a-secret-split-across-deltas-is-still-masked
  (let [events (:events (run [(text "the key is s3") (text "cr") (text "et, ok") result]
                             nil {:secrets ["s3cret"]}))]
    (is (= (str "the key is " providers/redacted ", ok") (apply str (keep :text events))))
    (is (not-any? #(str/includes? (str (:text %)) "s3") events) "no chunk carries a piece of it")
    (is (= {:done true} (last events))))
  (testing "a tail that only looked like the start of one goes out with the next delta"
    (is (= [{:text "the "} {:text "s3x"} {:done true}]
           (:events (run [(text "the s3") (text "x") result] nil {:secrets ["s3cret"]})))))
  (testing "what is held goes out before the answer ends"
    (is (= [{:text "the "} {:text "s3"} {:done true}]
           (:events (run [(text "the s3") result] nil {:secrets ["s3cret"]}))))))

(defn- req [& {:keys [addr headers body]
               :or   {addr "127.0.0.1" headers {"content-type" "application/json"}
                      body {:message "hi"}}}]
  {:remote-addr addr
   :server-port 8080
   :headers     headers
   :body        (java.io.ByteArrayInputStream.
                 (.getBytes (if (string? body) body (json/write-value-as-string body)) "UTF-8"))})

(def ^:private on {:env (constantly {"DRAFTDAY_SENSEI" "claude-cli"}) :port (constantly 9191)})
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

(deftest a-request-without-a-channel-frees-the-lock
  (let [resp ((chat/handler on) (req))]
    (is (= 500 (:status resp)) "only http-kit hands a request its channel")
    (is (false? @chat/busy?))))

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
        (is (false? @chat/busy?))
        (is (some #(str/includes? % "http://127.0.0.1:9191/api/sensei/mcp") @(:command fake))
            "the child is pointed at the port this server bound, not the one the request named")
        (is (= {"DRAFTDAY_SENSEI" "claude-cli"} @(:env fake))
            "the child's environment comes from the server's, as the gate and the masking do")))))
