(ns draft-day.test-render
  "Rendering a component in the node test build, where there is no DOM.

  A component is a function of no arguments returning hiccup, so a test can
  call one and read what it says — but only the outermost one. Reagent expands
  a nested `[some-component arg]` when it mounts, and nothing mounts here, so
  an un-expanded tree shows a function object where the markup a test is
  asking about should be. `hiccup` does that expansion, including the form-2
  case where a component returns its render function rather than markup.

  Shared rather than private to one suite: two suites assert on views now, and
  a second copy is a second place for the reactive-context binding or the
  form-2 case to be forgotten."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [re-frame.core :as rf]
            [reagent.ratom]))

(def ^:private max-depth
  "A component that renders itself would otherwise hang the suite."
  50)

(defn- call
  "A component's markup, whether it is form-1 or form-2."
  [f args]
  (let [out (apply f args)]
    (if (fn? out) (apply out args) out)))

(defn- expand
  [x depth]
  (cond
    (> depth max-depth) x
    (and (vector? x) (fn? (first x))) (expand (call (first x) (rest x)) (inc depth))
    (vector? x) (mapv #(expand % (inc depth)) x)
    (seq? x)    (map #(expand % (inc depth)) x)
    :else x))

(defn hiccup
  "A component's fully expanded hiccup.

  The reactive-context binding is not optional: the component subscribes, and
  re-frame warns on every subscribe made outside one."
  [component & args]
  (binding [reagent.ratom/*ratom-context* #js {}]
    (expand (call component args) 0)))

(defn render
  "A component's hiccup as a string, for asserting on what it says."
  [component & args]
  (pr-str (apply hiccup component args)))

(defn- button?
  "A hiccup button, however its tag is spelled. `:button.link` and `:button.primary`
  are buttons too, and matching only `:button` finds neither."
  [x]
  (and (vector? x)
       (keyword? (first x))
       (str/starts-with? (name (first x)) "button")
       (map? (second x))))

(defn- own-text
  "The strings this element renders itself, ignoring its attributes."
  [x]
  (->> (flatten (vec (remove map? x)))
       (filter string?)
       (str/join " ")))

(defn buttons
  "Every button in a component's tree, as `[text on-click]` in document order."
  [component & args]
  (let [found (atom [])]
    (walk/prewalk
     (fn [x]
       (when (button? x) (swap! found conj [(own-text x) (:on-click (second x))]))
       x)
     (apply hiccup component args))
    @found))

(defn press!
  "Click the button labelled `label`, returning the events it dispatched.

  An exact label wins over a partial one, so pressing \"Add\" in a card that
  also holds \"Add league\" gets the one asked for rather than whichever the
  walk reached first.

  `render` stringifies, which is enough to assert what a view *says* but not
  what a button *does* — and a payload is exactly where the bugs have been.
  `rf/dispatch` is redefined rather than read off a captured effect: a view
  calls the dispatch *function*, which queues on the real router and never
  touches the `:dispatch` effect a fixture stubs."
  [component label & args]
  (let [bs (apply buttons component args)
        f  (or (some (fn [[t f]] (when (= label t) f)) bs)
               (some (fn [[t f]] (when (str/includes? t label) f)) bs))]
    (if f
      (let [seen (atom [])]
        (with-redefs [rf/dispatch (fn [ev] (swap! seen conj ev))] (f))
        @seen)
      (throw (ex-info (str "no button labelled " label)
                      {:labels (mapv first bs)})))))
