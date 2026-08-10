(ns draft-day.benchmark.fetch-test
  (:require [clojure.test :refer [deftest is testing]]
            [draft-day.benchmark.fetch :as fetch])
  (:import [java.io ByteArrayOutputStream]
           [java.util.zip GZIPOutputStream]))

(defn gzip-bytes [^String s]
  (let [out (ByteArrayOutputStream.)]
    (with-open [gz (GZIPOutputStream. out)]
      (.write gz (.getBytes s "UTF-8")))
    (.toByteArray out)))

(deftest gzip-bodies-are-decompressed
  ;; The JDK HTTP client does NOT transparently decompress, unlike curl
  ;; --compressed. archive.org gzips by default, and without this the bytes
  ;; reached Jsoup as mojibake: every table parsed to zero rows and the harness
  ;; reported "this season has no projections" — a transport bug that looked
  ;; exactly like a data finding.
  (let [html "<table><tr><td>Travis Kelce</td></tr></table>"]
    (is (= html (fetch/decode-body (gzip-bytes html) true)))))

(deftest plain-bodies-pass-through-as-utf8
  (let [s "Ja'Marr Chase — 1,708 yds"]
    (is (= s (fetch/decode-body (.getBytes s "UTF-8") false)))))

(deftest number-parsing-handles-nflverse-missing-markers
  ;; nflverse writes both "" and "NA" for missing; neither may become 0.0 by
  ;; accident in num-or-nil, though num0 deliberately floors them.
  (is (nil? (fetch/num-or-nil "NA")))
  (is (nil? (fetch/num-or-nil "")))
  (is (nil? (fetch/num-or-nil nil)))
  (is (= 12.5 (fetch/num-or-nil "12.5")))
  (is (= 0.0 (fetch/num0 "NA")))
  (is (= 12.5 (fetch/num0 "12.5"))))

(deftest csv-parsing-keys-by-header
  (let [rows (fetch/parse-csv "player_id,games,targets\n00-1,17,120\n00-2,9,55\n")]
    (is (= 2 (count rows)))
    (is (= "00-1" (get (first rows) "player_id")))
    (is (= "120" (get (first rows) "targets")))
    (testing "values stay strings; coercion is each source's business"
      (is (string? (get (first rows) "games"))))))

(deftest cache-path-includes-every-part
  (let [p (fetch/cache-path "sleeper" "proj" "v2" 2024)]
    (is (re-find #"sleeper-proj-v2-2024\.transit$" p))
    (testing "schema token is part of the key so a shape change invalidates"
      (is (not= (fetch/cache-path "sleeper" "proj" "v2" 2024)
                (fetch/cache-path "sleeper" "proj" "v3" 2024))))))
