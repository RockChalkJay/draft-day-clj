(ns draft-day.fx
  "re-frame side effects for fetches and localStorage persistence.

  `:http` waits for a whole reply. `:http-stream` is for one that arrives as it is
  written, a JSON event per line: it ends in a failure when no terminal event
  went by, since a dropped connection and a finished answer otherwise look the
  same, and an abort is the manager's own Stop and reports nothing."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [cljs.reader :as reader]))

(def store-key "draft-day-state")

(def storage-version
  "Persisted-slice schema version.

  Bump this whenever a persisted shape changes; stale blobs are discarded and the
  app opens with defaults — all but the column layouts, which `load-persisted`
  keeps and `db/reconcile-columns` brings up to the catalog, so a catalog
  change no longer needs a bump."
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

(defn ndjson-split
  "`[complete-lines unfinished]` once `chunk` joins what was left of the last one.
  A network chunk ends wherever it likes, so a line is only whole after its
  newline."
  [unfinished chunk]
  (let [parts (str/split (str unfinished chunk) #"\n" -1)]
    [(filterv seq (butlast parts)) (last parts)]))

(defn parse-event
  "One NDJSON line as a keyword map, or nil when it is not JSON."
  [line]
  (try (js->clj (js/JSON.parse line) :keywordize-keys true)
       (catch :default _ nil)))

(defonce ^:private streams (atom {}))

(defn- pump
  "Read `reader` to its end, dispatching each whole line as `on-event` plus the
  event. Answers whether a terminal event (`done` or `error`) went by."
  [reader decoder on-event unfinished terminal?]
  (-> (.read reader)
      (.then (fn [r]
               (if (.-done r)
                 terminal?
                 (let [[lines rest] (ndjson-split unfinished
                                                  (.decode decoder (.-value r)
                                                           #js {:stream true}))
                       events       (keep parse-event lines)]
                   (run! #(rf/dispatch (conj on-event %)) events)
                   (pump reader decoder on-event rest
                         (or terminal?
                             (some #(or (:done %) (contains? % :error)) events)))))))))

;; Not `:http`, which would hold a chat answer back for its whole 20 seconds.
(rf/reg-fx
 :http-stream
 (fn [{:keys [id url body on-event on-failure]}]
   (let [ctl (js/AbortController.)]
     (swap! streams assoc id ctl)
     (-> (js/fetch url
                   (clj->js {:method  "POST"
                             :headers {"Content-Type" "application/json"}
                             :body    (js/JSON.stringify (clj->js body))
                             :signal  (.-signal ctl)}))
         (.then (fn [resp]
                  (if (.-ok resp)
                    (-> (pump (.getReader (.-body resp)) (js/TextDecoder.) on-event "" false)
                        (.then (fn [terminal?]
                                 (when-not terminal?
                                   (rf/dispatch (failure-event on-failure
                                                               {:error "The connection to Sensei ended before it answered"}
                                                               nil))))))
                    (-> (.json resp)
                        (.then (fn [j]
                                 (rf/dispatch (failure-event on-failure
                                                             (js->clj j :keywordize-keys true)
                                                             (.-status resp)))))))))
         (.catch (fn [err]
                   (when-not (= "AbortError" (.-name err))
                     (rf/dispatch (failure-event on-failure {:error (str err)} nil)))))
         (.finally (fn [] (when (identical? ctl (get @streams id))
                            (swap! streams dissoc id))))))))

(rf/reg-fx
 :http-stream-abort
 (fn [id]
   (when-let [ctl (get @streams id)]
     (.abort ctl)
     (swap! streams dissoc id))))

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

(def kept-across-versions
  "What a version bump does not discard: the manager's column layouts."
  [:columns :waiver-columns])

(defn load-persisted
  "Load the saved app slice, or nil if missing or unreadable. Under another
  version only `kept-across-versions` survives."
  []
  (try (when-let [s (.getItem js/localStorage store-key)]
         (let [{:keys [v state]} (reader/read-string s)]
           (if (= v storage-version)
             state
             (not-empty (select-keys state kept-across-versions)))))
       (catch :default _ nil)))
