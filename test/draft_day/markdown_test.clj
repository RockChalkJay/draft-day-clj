(ns draft-day.markdown-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.markdown :as md]))

(deftest inline-reads-bold-and-code-only
  (is (= ["Pick " [:b "Mason"] " now, " [:code "my_team"] "."]
         (md/inline "Pick **Mason** now, `my_team`.")))
  (testing "an unfinished construct stays text, never an error"
    (is (= ["a " "*" "*" "b"] (md/inline "a **b")))
    (is (= ["a " "`" "b"] (md/inline "a `b")))))

(deftest blocks-are-paragraphs-lists-and-tables
  (is (= [{:type :p :inline ["One"]}
          {:type :ul :items [["x"] [[:b "y"]]]}
          {:type :ol :items [["first"] ["second"]]}
          {:type :p :inline ["Two lines joined"]}]
         (md/parse "One\n\n- x\n* **y**\n\n1. first\n2) second\n\nTwo lines\njoined"))))

(deftest a-table-needs-its-separator-row
  (is (= [{:type :table
           :head [["Player"] ["Gain"]]
           :rows [[["Mason"] ["+3.1"]] [["Kraft"] ["+1.4"]]]}]
         (md/parse "| Player | Gain |\n|---|---:|\n| Mason | +3.1 |\n| Kraft | +1.4 |")))
  (testing "while it is still arriving it is a line of text"
    (is (= [{:type :p :inline ["| Player | Gain |"]}]
           (md/parse "| Player | Gain |")))))

(deftest nothing-the-model-writes-becomes-markup
  (let [blocks (md/parse "<script>alert(1)</script> [x](http://evil) ![i](u)")]
    (is (= [{:type :p :inline ["<script>alert(1)</script> [x](http://evil) ![i](u)"]}]
           blocks)
        "tags, links and images are text")))

(deftest headings-and-blank-input-are-safe
  (is (= [{:type :p :inline [[:b "Plan"]]}] (md/parse "## Plan")))
  (is (= [] (md/parse "")))
  (is (= [] (md/parse nil))))
