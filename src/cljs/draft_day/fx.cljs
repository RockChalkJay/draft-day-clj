(ns draft-day.fx
  "Side-effect handlers: a small fetch-based :http effect (no extra deps) and
  localStorage persistence."
  (:require [re-frame.core :as rf]
            [cljs.reader :as reader]))

(def ^:private store-key "draft-day-state")

(rf/reg-fx
 :http
 (fn [{:keys [method url body on-success on-failure]}]
   (-> (js/fetch url
                 (clj->js (cond-> {:method  (name (or method :get))
                                   :headers {"Content-Type" "application/json"}}
                            body (assoc :body (js/JSON.stringify (clj->js body))))))
       ;; Carry resp.ok alongside the parsed body: a 4xx whose body is JSON used
       ;; to be dispatched as success, so an API error landed in :ranked and the
       ;; whole board rendered blank with nothing to explain it.
       (.then (fn [resp]
                (.then (.json resp)
                       (fn [j] [(.-ok resp) (js->clj j :keywordize-keys true)]))))
       (.then (fn [[ok? body]]
                (cond
                  (and ok? on-success)       (rf/dispatch (conj on-success body))
                  (and (not ok?) on-failure) (rf/dispatch (conj on-failure
                                                               (or (:error body) "request failed"))))))
       (.catch (fn [err] (when on-failure
                           (rf/dispatch (conj on-failure (str err)))))))))

;; Coalesce a burst of dispatches of the same event into one. Every keystroke in
;; the custom scoring editor changes a weight, and each change re-ranks the whole
;; universe server-side; without this a three-character edit fires three
;; full-board POSTs whose responses can land out of order.
(defonce ^:private debounce-timers (atom {}))

(rf/reg-fx
 :debounce
 (fn [{:keys [id event ms] :or {ms 250}}]
   (when-let [t (get @debounce-timers id)] (js/clearTimeout t))
   (swap! debounce-timers assoc id
          (js/setTimeout (fn []
                           (swap! debounce-timers dissoc id)
                           (rf/dispatch event))
                         ms))))

(rf/reg-fx
 :persist!
 (fn [slice]
   (try (.setItem js/localStorage store-key (pr-str slice))
        (catch :default _ nil))))

(defn load-persisted []
  (try (when-let [s (.getItem js/localStorage store-key)]
         (reader/read-string s))
       (catch :default _ nil)))
