(ns draft-day.sensei-events-test
  (:require [cljs.test :refer [deftest is testing use-fixtures]]
            [re-frame.core :as rf]
            [re-frame.db :as rdb]
            [re-frame.registrar :as registrar]
            [draft-day.db :as db]
            [draft-day.events]
            [draft-day.fx :as fx]
            [draft-day.subs]))

(defonce captured (atom {}))

(def ^:private stubs
  {:http              (fn [r] (swap! captured update :http conj r))
   :http-stream       (fn [r] (swap! captured update :stream conj r))
   :http-stream-abort (fn [r] (swap! captured update :abort conj r))
   :persist!          (fn [r] (swap! captured update :persist conj r))})

(defonce ^:private real-fx
  (into {} (map (juxt identity #(registrar/get-handler :fx %))) (keys stubs)))

(defn- swap-fx! [m]
  (doseq [[id f] m]
    (rf/clear-fx id)
    (when f (rf/reg-fx id f))))

(defn- ask!
  "Dispatch an event and return the effects it produced."
  [& ev]
  (rf/dispatch-sync (vec ev))
  @captured)

(use-fixtures :each
  {:before (fn []
             (swap-fx! stubs)
             (reset! captured {:http [] :stream [] :abort [] :persist []})
             (reset! rdb/app-db (assoc (db/default-db) :sensei-enabled? true
                                       :active-league "sleeper:1"
                                       :leagues {"sleeper:1" {:provider "sleeper" :league-id "1"}})))
   :after  (fn [] (swap-fx! real-fx) (rf/clear-subscription-cache!))})

(defn- messages [] (:sensei-messages @rdb/app-db))

(deftest asking-adds-the-turn-and-streams-the-reply
  (let [{[req] :stream} (ask! :sensei-send "  Who do I add?  ")]
    (is (= "/api/sensei/chat" (:url req)))
    (is (= {:message "Who do I add?" :history [] :league "sleeper:1"} (:body req)))
    (is (= [:user :assistant] (map :role (messages))))
    (is (= :streaming (:status (peek (messages)))))
    (is (true? (:sensei-busy? @rdb/app-db)))
    (is (= [:sensei-event 1] (:on-event req)))))

(deftest a-blank-or-second-question-sends-nothing
  (ask! :sensei-send "   ")
  (is (empty? (:stream @captured)))
  (ask! :sensei-send "one")
  (ask! :sensei-send "two")
  (is (= 1 (count (:stream @captured))) "one at a time, as the server enforces")
  (is (= 2 (count (messages)))))

(deftest events-build-the-answer-and-the-last-one-frees-the-panel
  (ask! :sensei-send "q")
  (ask! :sensei-event 1 {:tool-start "my_team"})
  (ask! :sensei-event 1 {:text "Pick Mason."})
  (is (true? (:sensei-busy? @rdb/app-db)))
  (ask! :sensei-event 1 {:done true})
  (is (false? (:sensei-busy? @rdb/app-db)))
  (is (= {:text "Pick Mason." :tools ["my_team"] :status :done}
         (select-keys (peek (messages)) [:text :tools :status]))))

(deftest a-late-event-from-an-older-question-is-dropped
  (ask! :sensei-send "q")
  (ask! :sensei-stop)
  (ask! :sensei-event 1 {:text "too late"})
  (is (= "" (:text (peek (messages))))))

(deftest the-next-answer-is-asked-with-the-answered-history
  (ask! :sensei-send "q1")
  (ask! :sensei-event 1 {:text "a1"})
  (ask! :sensei-event 1 {:done true})
  (ask! :sensei-send "q2")
  (is (= [{:role "user" :text "q1"} {:role "assistant" :text "a1"}]
         (get-in @captured [:stream 1 :body :history]))))

(deftest a-failure-ends-the-answer-with-its-reason
  (ask! :sensei-send "q")
  (ask! :sensei-failed 1 "Sensei is already answering a question" 429)
  (is (= {:status :error :error "Sensei is already answering a question"}
         (select-keys (peek (messages)) [:status :error])))
  (is (false? (:sensei-busy? @rdb/app-db)))
  (is (true? (:sensei-enabled? @rdb/app-db)) "a 429 is not a withdrawn chat"))

(deftest a-server-that-will-not-chat-withdraws-the-panel
  (ask! :sensei-send "q")
  (ask! :sensei-failed 1 "Sensei chat is off" 404)
  (is (false? (:sensei-enabled? @rdb/app-db))))

(deftest stop-aborts-keeps-the-text-and-can-be-retried
  (ask! :sensei-send "q")
  (ask! :sensei-event 1 {:text "half"})
  (ask! :sensei-stop)
  (is (= [:sensei] (:abort @captured)))
  (is (= {:status :stopped :text "half"} (select-keys (peek (messages)) [:status :text])))
  (is (false? (:sensei-busy? @rdb/app-db)))
  (ask! :sensei-retry)
  (is (= 2 (count (messages))) "the stopped answer is replaced, not stacked")
  (is (= "q" (get-in @captured [:stream 1 :body :message]))))

(deftest clear-aborts-and-forgets
  (ask! :sensei-send "q")
  (ask! :sensei-clear)
  (is (= [] (messages)))
  (is (= [:sensei] (:abort @captured)))
  (is (false? (:sensei-busy? @rdb/app-db))))

(deftest switching-leagues-drops-the-conversation
  (swap! rdb/app-db assoc-in [:leagues "sleeper:2"] {:provider "sleeper" :league-id "2"})
  (ask! :sensei-send "q")
  (ask! :set-active-league "sleeper:2")
  (is (= [] (messages)) "an answer about the league you left is false under this one's name")
  (is (false? (:sensei-busy? @rdb/app-db)))
  (is (some #{:sensei} (:abort @captured)))
  (ask! :sensei-event 1 {:text "late"})
  (is (= [] (messages))))

(deftest the-width-is-kept-and-written-only-when-the-drag-ends
  (reset! captured {:http [] :stream [] :abort [] :persist []})
  (ask! :sensei-resize 9000)
  (is (= 720 (:sensei-width @rdb/app-db)))
  (is (empty? (:persist @captured)) "not on every pointer move")
  (ask! :sensei-resize-end)
  (is (= 720 (:sensei-width (last (:persist @captured))))))

(deftest opening-is-remembered
  (ask! :sensei-toggle)
  (is (true? (:sensei-open? (last (:persist @captured))))))

(deftest the-panel-shows-only-when-offered-and-open
  (let [visible #(deref (rf/subscribe [:sensei-visible?]))]
    (is (false? (visible)))
    (ask! :sensei-toggle)
    (rf/clear-subscription-cache!)
    (is (true? (visible)))
    (ask! :sensei-status-loaded {:enabled false})
    (rf/clear-subscription-cache!)
    (is (false? (visible)))))

(deftest an-unreachable-status-means-no-chat
  (ask! :sensei-status-failed)
  (is (false? (:sensei-enabled? @rdb/app-db))))

(deftest lines-are-whole-only-after-their-newline
  (is (= [[] "{\"te"] (fx/ndjson-split "" "{\"te")))
  (is (= [["{\"text\":\"a\"}"] "{\"te"] (fx/ndjson-split "{\"te" "xt\":\"a\"}\n{\"te") ) "ends"))

(deftest a-chunk-may-carry-several-lines-and-blank-ones
  (is (= [["a" "b"] ""] (fx/ndjson-split "" "a\n\nb\n")))
  (is (= {:done true} (fx/parse-event "{\"done\":true}")))
  (is (nil? (fx/parse-event "not json"))))
