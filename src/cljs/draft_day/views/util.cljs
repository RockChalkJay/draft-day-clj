(ns draft-day.views.util)

(defn money [n]
  (if (and (number? n) (pos? n)) 
    (str "$" n) 
    "–"))

(defn money-rnd [n]
  (if (and (number? n) (pos? n)) 
    (str "$" (js/Math.round n)) 
    "–"))
