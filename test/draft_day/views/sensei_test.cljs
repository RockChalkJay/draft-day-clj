(ns draft-day.views.sensei-test
  (:require [cljs.test :refer [deftest is use-fixtures]]
            [re-frame.core :as rf]
            [re-frame.db :as rdb]
            [draft-day.db :as db]
            [draft-day.events]
            [draft-day.subs]
            [draft-day.test-render :refer [press! render hiccup]]
            [draft-day.views.sensei :as sensei]))

(use-fixtures :each
  {:before (fn [] (rf/clear-subscription-cache!) (reset! rdb/app-db (db/default-db)))
   :after  (fn [] (rf/clear-subscription-cache!) (reset! rdb/app-db (db/default-db)))})

(deftest the-toggle-is-drawn-only-where-the-server-offers-a-chat
  (is (nil? (hiccup sensei/toggle)))
  (swap! rdb/app-db assoc :sensei-enabled? true)
  (let [html (render sensei/toggle)]
    (is (re-find #"Ask Sensei" html))
    (is (re-find #":aria-expanded false" html))
    (is (re-find #":aria-controls \"sensei-panel\"" html))
    (is (re-find #"\[:button#sensei-toggle.col-toggle" html)
        "Reagent reads an id before the classes; the other order throws on mount")))

(deftest the-panel-is-nothing-while-closed
  (swap! rdb/app-db assoc :sensei-enabled? true :sensei-open? false)
  (is (nil? (hiccup sensei/drawer))))

(deftest an-answer-in-progress-shows-the-tool-it-waits-on
  (let [html (render sensei/answer {:text "" :tools ["my_team" "waiver_targets"] :status :streaming})]
    (is (re-find #"my_team" html))
    (is (re-find #":class \"ok\"" html) "an earlier tool is done")
    (is (re-find #":class \"run\"" html) "the last one is still being read")))

(deftest a-thinking-answer-says-so
  (is (re-find #"Thinking" (render sensei/answer {:text "" :tools [] :status :streaming}))))

(deftest a-failed-answer-says-why-and-can-be-retried
  (let [m {:text "" :tools [] :status :error :error "claude: not logged in"}]
    (is (re-find #"claude: not logged in" (render sensei/answer m)))
    (is (= [[:sensei-retry]] (press! sensei/answer "Retry" m)))))

(deftest a-stopped-answer-keeps-its-text-and-can-be-retried
  (let [m {:text "Half an answer" :tools [] :status :stopped}
        html (render sensei/answer m)]
    (is (re-find #"Half an answer" html))
    (is (re-find #"Stopped" html))
    (is (= [[:sensei-retry]] (press! sensei/answer "Retry" m)))))

(deftest focus-follows-opening-the-panel-not-loading-the-page
  (swap! rdb/app-db assoc :sensei-enabled? true :sensei-open? false)
  (reset! sensei/focus-composer? false)
  (press! sensei/toggle "Ask Sensei")
  (is (true? @sensei/focus-composer?) "opening asks the composer for focus")
  (swap! rdb/app-db assoc :sensei-open? true)
  (rf/clear-subscription-cache!)
  (press! sensei/toggle "Ask Sensei")
  (is (false? @sensei/focus-composer?) "closing does not"))

(deftest a-table-lines-numbers-up-and-colours-only-signed-ones
  (let [html (render sensei/markdown-view
                     "| Player | Gain |\n|---|---|\n| Mason | +3.1 |\n| Kraft | -1.4 |\n| Coker | 8.8 |")]
    (is (re-find #"\+3.1" html))
    (is (re-find #":class \"num good\"" html))
    (is (re-find #":class \"num warn\"" html))
    (is (re-find #":class \"num\"" html))
    (is (re-find #"\[:th \{:class \"\"\} \(\"Player\"\)\]" html) "a word is not a number")))

(deftest nothing-the-model-writes-becomes-markup
  (let [h (hiccup sensei/markdown-view "<img src=x onerror=alert(1)> **ok**")]
    (is (re-find #"<img src=x onerror=alert\(1\)>" (pr-str h)) "kept as text")
    (is (not-any? #(and (vector? %) (= :img (first %))) (tree-seq coll? seq h)))
    (is (not (re-find #"dangerouslySetInnerHTML" (pr-str h))))))

(deftest the-composer-sends-nothing-it-has-not-been-given
  (swap! rdb/app-db assoc :sensei-busy? true)
  (is (re-find #"Stop" (render sensei/composer)))
  (is (= [[:sensei-stop]] (press! sensei/composer "Stop")))
  (swap! rdb/app-db assoc :sensei-busy? false)
  (is (re-find #":disabled true" (render sensei/composer)) "an empty draft cannot be sent"))

(deftest numbers-and-signs-are-told-apart
  (is (sensei/numeric? "+3.1"))
  (is (sensei/numeric? "$12"))
  (is (sensei/numeric? "41k"))
  (is (not (sensei/numeric? "RB5")))
  (is (= "good" (sensei/signed-class "+0.6")))
  (is (= "warn" (sensei/signed-class "−1.2")))
  (is (nil? (sensei/signed-class "8.8"))))
