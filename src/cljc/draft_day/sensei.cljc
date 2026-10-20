(ns draft-day.sensei
  "The chat's state, as pure functions of the message list.

  Messages alternate `{:role :user :text}` and `{:role :assistant :text :tools
  :status}`, the assistant's added the moment a question is asked and filled in
  as events arrive. Shared with the JVM so the rules for what is resent, and what
  an event does to the last answer, are tested there.")

(def default-width 380)
(def min-width 300)
(def max-width 720)

(defn clamp-width
  "`w` held between the narrowest panel that still holds a table and the widest
  that still leaves the board room."
  [w]
  (-> (if (number? w) w default-width) (max min-width) (min max-width)))

(defn add-turn
  "`messages` with a question and the empty answer it is waiting on."
  [messages text]
  (conj (vec messages)
        {:role :user :text text}
        {:role :assistant :text "" :tools [] :status :streaming}))

(defn history
  "What the server is sent of the conversation so far: only questions that were
  answered in full. A failed or stopped answer is left out with its question, so
  the next question is not asked in the shadow of an answer that was never
  given."
  [messages]
  (->> (partition 2 messages)
       (filter (fn [[_ a]] (and (= :done (:status a)) (seq (:text a)))))
       (mapcat (fn [[u a]] [{:role "user" :text (:text u)}
                            {:role "assistant" :text (:text a)}]))
       vec))

(defn request-body
  "The `POST /api/sensei/chat` body for `text`, asked in `league-key`. The league
  is only a key: no credential is ever sent."
  [messages text league-key]
  (cond-> {:message text :history (history messages)}
    league-key (assoc :league league-key)))

(defn terminal?
  "Does this event end the answer?"
  [event]
  (or (:done event) (contains? event :error)))

(defn apply-event
  "`messages` with one stream event applied to the answer in progress."
  [messages event]
  (if (empty? messages)
    messages
    (update messages (dec (count messages))
            (fn [m]
              (cond
                (:tool-start event) (update m :tools conj (:tool-start event))
                (:text event)       (update m :text str (:text event))
                (:done event)       (assoc m :status :done)
                (contains? event :error) (assoc m :status :error :error (:error event))
                :else               m)))))

(defn fail
  "`messages` with the answer in progress ended as `status`, `:error` with its
  `message` or `:stopped` with none."
  [messages status message]
  (if (empty? messages)
    messages
    (update messages (dec (count messages))
            #(cond-> (assoc % :status status)
               message (assoc :error message)))))

(defn last-question
  "The text of the newest question, or nil."
  [messages]
  (->> messages (filter #(= :user (:role %))) last :text))

(defn retryable?
  "Can the newest answer be asked again: it ended in an error or was stopped."
  [messages]
  (contains? #{:error :stopped} (:status (peek (vec messages)))))
