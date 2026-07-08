(ns draftday.fx
  "Side-effect handlers: a small fetch-based :http effect (no extra deps) and
  localStorage persistence."
  (:require [re-frame.core :as rf]
            [cljs.reader :as reader]))

(def ^:private store-key "draftday-state")

(rf/reg-fx
 :http
 (fn [{:keys [method url body on-success on-failure]}]
   (-> (js/fetch url
                 (clj->js (cond-> {:method  (name (or method :get))
                                   :headers {"Content-Type" "application/json"}}
                            body (assoc :body (js/JSON.stringify (clj->js body))))))
       (.then (fn [resp] (.json resp)))
       (.then (fn [j] (when on-success
                        (rf/dispatch (conj on-success (js->clj j :keywordize-keys true))))))
       (.catch (fn [err] (when on-failure
                           (rf/dispatch (conj on-failure (str err)))))))))

(rf/reg-fx
 :persist!
 (fn [slice]
   (try (.setItem js/localStorage store-key (pr-str slice))
        (catch :default _ nil))))

(defn load-persisted []
  (try (when-let [s (.getItem js/localStorage store-key)]
         (reader/read-string s))
       (catch :default _ nil)))
