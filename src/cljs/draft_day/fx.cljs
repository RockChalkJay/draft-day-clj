(ns draft-day.fx
  "Side-effect handlers: a small fetch-based :http effect (no extra deps) and
  localStorage persistence."
  (:require [re-frame.core :as rf]
            [cljs.reader :as reader]))

(def store-key "draft-day-state")

(def storage-version
  "The shape of the persisted slice (`db/persist-keys`).

  Saved state is stamped with this and read back only when the stamp matches;
  anything else is dropped and the app opens at defaults. **Bump it whenever a
  persisted shape changes** — a key added to or removed from the config, a
  column added to or removed from `db/column-catalog`, a value that changes
  type.

  This is deliberately all the migration there is. The app used to repair every
  shape it had ever written, in place, which meant a permanent record of its own
  history spread across three reconcile functions and an id crosswalk. Starting
  over costs a manager his column layout and, mid-draft, his picks; the version
  is only bumped by a deploy, and a rebuilt layout is worth less than the code
  that avoided rebuilding it."
  1)

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
   (try (.setItem js/localStorage store-key
                  (pr-str {:v storage-version :state slice}))
        (catch :default _ nil))))

(defn load-persisted
  "The saved slice, or nil if there is none, it is unreadable, or it was written
  by a different `storage-version`."
  []
  (try (when-let [s (.getItem js/localStorage store-key)]
         (let [{:keys [v state]} (reader/read-string s)]
           (when (= v storage-version) state)))
       (catch :default _ nil)))
