(ns draft-day.sensei.mcp-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [jsonista.core :as json]
            [draft-day.sensei.context :as context]
            [draft-day.sensei.fixtures :as fx]
            [draft-day.sensei.mcp :as mcp]
            [draft-day.sensei.tools :as tools]))

(def ^:private mapper (json/object-mapper {:decode-key-fn keyword}))

(def ^:private server
  {:waivers-reply (constantly fx/reply)
   :players       (constantly fx/players)
   :news          fx/news})

(defn- post
  "POST one message, or a batch, from this machine unless `req` says
  otherwise, with the league resolving to `ctx`."
  ([msg] (post msg {}))
  ([msg req] (post msg req fx/body))
  ([msg req ctx]
   (with-redefs [context/league-context (constantly ctx)]
     ((mcp/handler server)
      (-> {:remote-addr "127.0.0.1"
           :headers     {"content-type" "application/json"}
           :body        (java.io.ByteArrayInputStream.
                         (.getBytes (if (string? msg) msg (json/write-value-as-string msg)) "UTF-8"))}
          (merge (dissoc req :headers))
          (update :headers merge (:headers req)))))))

(defn- rpc [& args]
  (let [resp (apply post args)]
    (assoc resp :json (some-> (not-empty (:body resp)) (json/read-value mapper)))))

(defn- request [id method & [params]]
  (cond-> {:jsonrpc "2.0" :id id :method method} params (assoc :params params)))

(deftest initialize-echoes-a-version-it-speaks
  (let [{:keys [status json]} (rpc (request 1 "initialize" {:protocolVersion "2025-06-18"}))]
    (is (= 200 status))
    (is (= "2025-06-18" (get-in json [:result :protocolVersion])))
    (is (= "sensei" (get-in json [:result :serverInfo :name])))
    (is (str/includes? (get-in json [:result :instructions]) "Sensei")))
  (is (= (first mcp/protocol-versions)
         (get-in (rpc (request 1 "initialize" {:protocolVersion "2099-01-01"})) [:json :result :protocolVersion]))
      "one it does not speak gets the newest it does"))

(deftest a-notification-owes-no-reply
  (let [{:keys [status body]} (post {:jsonrpc "2.0" :method "notifications/initialized"})]
    (is (= 202 status))
    (is (= "" body))))

(deftest tools-list-states-every-tool-read-only
  (let [tools (get-in (rpc (request 2 "tools/list")) [:json :result :tools])]
    (is (= ["my_team" "waiver_targets" "player_news"] (map :name tools)))
    (is (every? #(= "object" (get-in % [:inputSchema :type])) tools))
    (is (every? #(true? (get-in % [:annotations :readOnlyHint])) tools))))

(deftest a-tool-call-answers-in-json-text
  (let [{:keys [json]} (rpc (request 3 "tools/call" {:name "waiver_targets" :arguments {:limit 2}}))
        content (get-in json [:result :content 0])]
    (is (false? (get-in json [:result :isError])))
    (is (= "text" (:type content)))
    (is (= ["f3" "f2"] (map :player-id (:players (json/read-value (:text content) mapper)))))))

(deftest a-tool-failure-is-the-models-to-read
  (let [{:keys [json]} (rpc (request 4 "tools/call" {:name "my_team"}) {} {:error "sync failed: 502"})]
    (is (true? (get-in json [:result :isError])))
    (is (= "sync failed: 502" (get-in json [:result :content 0 :text])))))

(deftest protocol-errors
  (is (= -32602 (get-in (rpc (request 5 "tools/call" {:name "nope"})) [:json :error :code])) "unknown tool")
  (is (= -32601 (get-in (rpc (request 6 "resources/list")) [:json :error :code])) "unknown method")
  (let [{:keys [status json]} (rpc "{not json")]
    (is (= 400 status))
    (is (= -32700 (get-in json [:error :code])))))

(deftest a-batch-is-answered-as-one
  (let [{:keys [json]} (rpc [(request 7 "ping") {:jsonrpc "2.0" :method "notifications/initialized"}
                             (request 8 "tools/list")])]
    (is (= [7 8] (map :id json)) "the notification gets nothing"))
  (is (= 202 (:status (post [{:jsonrpc "2.0" :method "notifications/initialized"}])))))

(deftest prompts-carry-the-weekly-review
  (is (= ["weekly_review"] (map :name (get-in (rpc (request 9 "prompts/list")) [:json :result :prompts]))))
  (let [msg (get-in (rpc (request 10 "prompts/get" {:name "weekly_review"})) [:json :result :messages 0])]
    (is (= "user" (:role msg)))
    (is (str/includes? (get-in msg [:content :text]) "waiver wire"))))

(deftest only-this-machine-is-answered
  (testing "another host on the network"
    (is (= 403 (:status (post (request 1 "ping") {:remote-addr "192.168.1.20"})))))
  (testing "a web page reaching loopback by DNS rebinding"
    (is (= 403 (:status (post (request 1 "ping") {:headers {"origin" "https://evil.example"}}))))
    (is (= 403 (:status (post (request 1 "ping") {:headers {"origin" "null"}})))))
  (testing "this machine, by either address family, from a page or from a CLI"
    (is (= 200 (:status (post (request 1 "ping") {:remote-addr "0:0:0:0:0:0:0:1"}))))
    (is (= 200 (:status (post (request 1 "ping") {:headers {"origin" "http://localhost:8280"}}))))))

(deftest a-tunnel-from-this-machine-is-not-this-machine
  (testing "it relays the public name, with no Origin to give it away"
    (is (= 403 (:status (post (request 1 "ping") {:headers {"host" "sensei.example.ngrok.app"}}))))
    (is (= 403 (:status (post (request 1 "ping") {:headers {"host" "localhost.evil.example:8080"}})))))
  (testing "it says it is relaying"
    (doseq [h ["x-forwarded-for" "forwarded" "x-real-ip"]]
      (is (= 403 (:status (post (request 1 "ping") {:headers {"host" "127.0.0.1:8080" h "203.0.113.9"}})))
          h)))
  (testing "a client on this machine names it however it likes"
    (doseq [host ["localhost" "localhost:8080" "127.0.0.1:8080" "[::1]:8080"]]
      (is (= 200 (:status (post (request 1 "ping") {:headers {"host" host}}))) host))))

(deftest nothing-leaves-a-tool-call-unmasked
  (with-redefs [context/secrets (constantly ["S2SECRET" "{SW-ID}"])]
    (testing "a failure the tool reports"
      (let [{:keys [json]} (rpc (request 1 "tools/call" {:name "my_team"}) {} {:error "sync failed: S2SECRET"})]
        (is (= "sync failed: ••••" (get-in json [:result :content 0 :text])))))
    (testing "a bug, which no one thought to mask"
      (with-redefs [tools/run (fn [& _] (throw (ex-info "boom {SW-ID} S2SECRET" {})))]
        (let [{:keys [json]} (rpc (request 2 "tools/call" {:name "my_team"}))
              text (get-in json [:result :content 0 :text])]
          (is (true? (get-in json [:result :isError])))
          (is (= "internal error: boom •••• ••••" text)))))
    (testing "a result: here a news note stands in for a secret a feed echoed"
      (with-redefs [context/secrets (constantly ["note 101"])]
        (let [{:keys [json]} (rpc (request 3 "tools/call" {:name "player_news" :arguments {:player "p1"}}))
              text (get-in json [:result :content 0 :text])]
          (is (false? (get-in json [:result :isError])))
          (is (str/includes? text "••••"))
          (is (not (str/includes? text "note 101"))))))))

(deftest a-body-that-is-not-json-is-told-so
  (let [{:keys [status json]} (rpc (request 1 "ping")
                                   {:headers {"content-type" "application/x-www-form-urlencoded"}})]
    (is (= 415 status))
    (is (re-find #"application/json" (get-in json [:error :message])))))
