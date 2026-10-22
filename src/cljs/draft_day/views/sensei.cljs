(ns draft-day.views.sensei
  "Sensei's chat, as a panel docked on the right of every tab.

  Docked, not floating: it pushes the board over instead of covering it, so the
  roster stays beside a recommendation that names a drop. The width is the
  manager's, dragged or moved by keyboard from the separator on its left edge.

  Its own namespace, like `header`, so the node test build can reach it. What an
  answer is made of is `draft-day.markdown`, drawn here as hiccup and never as
  HTML, so nothing the model writes becomes markup."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [draft-day.markdown :as md]
            [draft-day.sensei :as sensei]))

;; Set by the header button, read once by the composer as it mounts: focus follows
;; the manager opening the panel, not a page load that finds it already open.
(defonce focus-composer? (atom false))

(defn toggle
  "The header's button for the panel, drawn only where the server offers one."
  []
  (when @(rf/subscribe [:sensei-enabled?])
    (let [open? @(rf/subscribe [:sensei-open?])]
      [:button#sensei-toggle.col-toggle
       {:aria-expanded (boolean open?) :aria-controls "sensei-panel"
        :on-click (fn []
                    (reset! focus-composer? (not open?))
                    (rf/dispatch [:sensei-toggle]))}
       "Ask Sensei"])))

(defn signed-class
  "How a table cell that is a signed number reads: the sign stays in the text, so
  the color is a second signal rather than the only one."
  [s]
  (cond
    (re-matches #"\+\d.*" s)   "good"
    (re-matches #"[-−]\d.*" s) "warn"))

(defn numeric?
  "Is this cell a number, which a column should line up on its right edge?"
  [s]
  (boolean (re-matches #"[+\-−]?\$?\d[\d.,]*[%k]?" s)))

(defn inline-view
  "One inline run from `md/inline` as hiccup."
  [x]
  (if (string? x)
    x
    (case (first x)
      :b    [:strong (second x)]
      :code [:code (second x)])))

(defn inline-views [runs]
  (map-indexed (fn [i x] ^{:key i} [inline-view x]) runs))

(defn cell-view [tag runs]
  (let [text (apply str (map #(if (string? %) % (second %)) runs))]
    [tag {:class (str/join " " (remove nil? [(when (numeric? text) "num")
                                             (signed-class text)]))}
     (inline-views runs)]))

(defn block-view
  "One block from `md/parse` as hiccup."
  [{:keys [type inline items head rows]}]
  (case type
    :p     [:p (inline-views inline)]
    :ul    [:ul (map-indexed (fn [i it] ^{:key i} [:li (inline-views it)]) items)]
    :ol    [:ol (map-indexed (fn [i it] ^{:key i} [:li (inline-views it)]) items)]
    :table [:div.sn-table
            [:table
             [:thead [:tr (map-indexed (fn [i c] ^{:key i} [cell-view :th c]) head)]]
             [:tbody (map-indexed
                      (fn [i row]
                        ^{:key i}
                        [:tr (map-indexed (fn [j c] ^{:key j} [cell-view :td c]) row)])
                      rows)]]]))

(defn markdown-view [text]
  [:div.sn-md (map-indexed (fn [i b] ^{:key i} [block-view b]) (md/parse text))])

(defn tool-chip
  "A tool Sensei read, with a dot while it is the one still being waited on."
  [name running?]
  [:span.chip {:class (if running? "run" "ok")} name
   (when running? [:span.sr-only " (reading)"])])

(defn answer
  "One assistant message: the tools it read, its text, and how it ended."
  [{:keys [text tools status error]}]
  (let [streaming? (= :streaming status)]
    [:div.sn-bot
     (when (seq tools)
       [:div.chips
        (map-indexed (fn [i t]
                       ^{:key i}
                       [tool-chip t (and streaming? (= i (dec (count tools))) (str/blank? text))])
                     tools)])
     (when (seq text) [markdown-view text])
     (when (and streaming? (str/blank? text) (empty? tools))
       [:p.muted "Thinking…"])
     (when (and streaming? (seq text)) [:span.cursor {:aria-hidden true}])
     (when (= :stopped status)
       [:p.muted "Stopped. " [:button.link-btn {:on-click #(rf/dispatch [:sensei-retry])} "Retry"]])
     (when (= :error status)
       [:div.sn-err {:role "alert"}
        [:strong "Sensei couldn't answer."]
        [:div.muted error]
        [:button {:on-click #(rf/dispatch [:sensei-retry])} "Retry"]])]))

(defn message [{:keys [role] :as m}]
  (if (= :user role)
    [:div.sn-me (:text m)]
    [answer m]))

(defn near-bottom?
  "Is the log scrolled to within a line or two of its end?"
  [^js el]
  (< (- (.-scrollHeight el) (.-scrollTop el) (.-clientHeight el)) 48))

(defn log
  "The transcript. It follows an answer as it arrives only while the manager is
  already at the bottom: scrolling up to reread must not be undone by the next
  chunk."
  [_messages]
  (let [el    (atom nil)
        stick (atom true)]
    (r/create-class
     {:component-did-update
      (fn [] (when-let [e @el] (when @stick (set! (.-scrollTop e) (.-scrollHeight e)))))
      :reagent-render
      (fn [messages]
        ;; A live region so a screen reader hears the answer, `polite` so it waits
        ;; for the manager, and busy while it streams so it is read once whole
        ;; instead of again at every chunk.
        [:div.sn-log {:ref #(reset! el %)
                      :on-scroll #(reset! stick (near-bottom? (.-target %)))
                      :role "log" :aria-live "polite" :aria-busy @(rf/subscribe [:sensei-busy?])
                      :aria-label "Conversation with Sensei"}
         (map-indexed (fn [i m] ^{:key i} [message m]) messages)])})))

(defn send-key?
  "Is this keydown a plain Enter, which sends, as opposed to Shift+Enter or an
  Enter that commits an IME composition?"
  [^js e]
  (and (= "Enter" (.-key e)) (not (.-shiftKey e)) (not (.. e -nativeEvent -isComposing))))

(defn composer []
  (let [draft (r/atom "")
        send! (fn []
                (when-not (str/blank? @draft)
                  (rf/dispatch [:sensei-send @draft])
                  (reset! draft "")))]
    (fn []
      (let [busy? @(rf/subscribe [:sensei-busy?])]
        [:div.sn-comp
         [:div.row
          [:label.sr-only {:for "sensei-input"} "Ask Sensei"]
          [:textarea#sensei-input
           {:placeholder "Ask about your team, waivers, a rival…"
            :ref (fn [el] (when (and el @focus-composer?)
                            (reset! focus-composer? false)
                            (.focus el)))
            :value @draft
            :on-change #(reset! draft (.. % -target -value))
            :on-key-down (fn [e] (when (send-key? e) (.preventDefault e) (when-not busy? (send!))))}]
          (if busy?
            [:button.stop {:on-click #(rf/dispatch [:sensei-stop])} "Stop"]
            [:button.send {:disabled (str/blank? @draft) :on-click send!} "Send"])]
         [:small "Answers use your synced league. Enter sends, Shift+Enter adds a line."]]))))

(def key-step "How far an arrow key moves the edge, in pixels." 24)

(defn grip
  "The panel's left edge: a separator dragged with the pointer, or moved with the
  arrow keys, since a drag alone is not a way every manager can resize."
  [width]
  (let [drag! (fn [e]
                (.preventDefault e)
                (let [move (fn [ev] (rf/dispatch [:sensei-resize (- js/window.innerWidth (.-clientX ev))]))
                      up   (atom nil)]
                  (reset! up (fn [_]
                               (.removeEventListener js/document "pointermove" move)
                               (.removeEventListener js/document "pointerup" @up)
                               (rf/dispatch [:sensei-resize-end])))
                  (.addEventListener js/document "pointermove" move)
                  (.addEventListener js/document "pointerup" @up)))
        key!  (fn [e]
                (let [d (case (.-key e) "ArrowLeft" key-step "ArrowRight" (- key-step) nil)]
                  (when d
                    (.preventDefault e)
                    (rf/dispatch [:sensei-resize (+ width d)])
                    (rf/dispatch [:sensei-resize-end]))))]
    [:div.grip {:role "separator" :aria-orientation "vertical" :tabIndex 0
                :aria-label "Resize the Sensei panel"
                :aria-valuenow width :aria-valuemin sensei/min-width :aria-valuemax sensei/max-width
                :on-pointer-down drag! :on-key-down key!}]))

(defn header-offset
  "Publishes the header's height as `--header-h`, so the panel starts beneath it
  however many rows the header wraps to: a crowded header grows, and a panel
  pinned to a guess would cover the button that closes it."
  []
  (let [observer (atom nil)]
    (r/create-class
     {:component-did-mount
      (fn []
        (when-let [h (js/document.querySelector "header.top")]
          (let [publish! #(.setProperty (.-style js/document.documentElement) "--header-h"
                                        (str (.-offsetHeight h) "px"))]
            (publish!)
            (reset! observer (js/ResizeObserver. publish!))
            (.observe @observer h))))
      :component-will-unmount (fn [] (some-> @observer .disconnect))
      :reagent-render (fn [] nil)})))

(defn close! []
  (rf/dispatch [:sensei-toggle])
  (some-> (js/document.getElementById "sensei-toggle") .focus))

(defn drawer
  "The docked panel, or nothing while it is closed or the server offers none."
  []
  (when @(rf/subscribe [:sensei-visible?])
    (let [width    @(rf/subscribe [:sensei-width])
          messages @(rf/subscribe [:sensei-messages])
          {:keys [league-name]} @(rf/subscribe [:account])]
      [:aside#sensei-panel.sensei {:aria-label "Sensei chat"
                                   :style {"--sensei-w" (str width "px")}}
       [header-offset]
       [grip width]
       [:div.sn-head
        [:h2 "Sensei"]
        [:span.muted.sn-league league-name]
        [:span.sp]
        (when (seq messages)
          [:button {:on-click #(rf/dispatch [:sensei-clear])} "Clear"])
        [:button {:aria-label "Close Sensei" :on-click close!} "✕"]]
       [log messages]
       [composer]])))
