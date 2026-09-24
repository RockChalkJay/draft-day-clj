(ns draft-day.bid-history-format-test
  "`bid-history/style-line` formats in the browser through its own branch, which
  `lein test` never reaches."
  (:require [cljs.test :refer [deftest is]]
            [draft-day.bid-history :as bh]))

(deftest the-style-line-reads-the-same-in-the-browser
  (is (= "Sniper — 0.6 claims a week, 40% at $0, top bid $38"
         (bh/style-line {:style :sniper :bids 12 :per-week 0.6 :zero-share 0.4 :max-bid 38})))
  (is (= "$0 flyer — 3.0 claims a week, 90% at $0"
         (bh/style-line {:style :zero-flyer :bids 40 :per-week 3 :zero-share 0.9 :max-bid 0}))
      "a whole number keeps its decimal, as the JVM's format does"))
