(ns draft-day.fx
  "re-frame side effects for fetches and localStorage persistence."
  (:require [re-frame.core :as rf]
            [cljs.reader :as reader]))

(def store-key "draft-day-state")

(def storage-version
  "Persisted-slice schema version.

  Bump this whenever a persisted shape changes; stale blobs are discarded and the
  app opens with defaults."
  13)

(def drafts-key "draft-day-drafts")

(def drafts-version
  "Archived-draft schema version.
  This is independent from the live app state because an archived draft should
  remain readable across UI migrations and layout changes."
  1)

(defn read-drafts
  "Read archived drafts, oldest first, or `[]` when unavailable or mismatched."
  []
  (try (if-let [s (.getItem js/localStorage drafts-key)]
         (let [{:keys [v drafts]} (reader/read-string s)]
           (if (= v drafts-version) (vec drafts) []))
         [])
       (catch :default _ [])))

(rf/reg-fx
 :archive-draft!
 (fn [entry]
   ;; Appends. A manager drafts in more than one league and more than one
   ;; season, and this is the only place those records go.
   (try (.setItem js/localStorage drafts-key
                  (pr-str {:v drafts-version :drafts (conj (read-drafts) entry)}))
        (catch :default _ nil))))

(defn failure-event
  "Append the HTTP status to an error event so callers can branch on 401 vs 502.
  The original event shape is preserved, and the status is only appended after the
  message to avoid breaking handlers that destructure the existing arity."
  [on-failure body status]
  (conj on-failure (or (:error body) "request failed") status))

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
                       (fn [j] [(.-ok resp) (.-status resp)
                                (js->clj j :keywordize-keys true)]))))
       (.then (fn [[ok? status body]]
                (cond
                  (and ok? on-success)       (rf/dispatch (conj on-success body))
                  (and (not ok?) on-failure) (rf/dispatch (failure-event on-failure body status)))))
       (.catch (fn [err] (when on-failure
                           (rf/dispatch (failure-event on-failure {:error (str err)} nil))))))))

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
   (try (.setItem js/localStorage store-key
                  (pr-str {:v storage-version :state slice}))
        (catch :default _ nil))))

(defn load-persisted
  "Load the saved app slice, or nil if missing, unreadable, or version-mismatched."
  []
  (try (when-let [s (.getItem js/localStorage store-key)]
         (let [{:keys [v state]} (reader/read-string s)]
           (when (= v storage-version) state)))
       (catch :default _ nil)))
