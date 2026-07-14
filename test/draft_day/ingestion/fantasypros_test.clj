(ns draft-day.ingestion.fantasypros-test
  (:require [clojure.test :refer [deftest is]]
            [draft-day.ingestion.fantasypros :as fp]
            [draft-day.ingestion.match :as match]))

(def ^:private sample-html
  (str "<html><body><script>var ecrData = {\"sport\":\"NFL\",\"players\":["
       "{\"player_name\":\"Bijan Robinson\",\"player_team_id\":\"ATL\",\"player_position_id\":\"RB\","
       "\"player_bye_week\":\"11\",\"rank_ecr\":2,\"pos_rank\":\"RB1\",\"tier\":1,"
       "\"rank_min\":\"1\",\"rank_max\":\"5\",\"rank_ave\":\"2.98\",\"rank_std\":\"1.40\"},"
       "{\"player_name\":\"CeeDee Lamb\",\"player_team_id\":\"DAL\",\"player_position_id\":\"WR\","
       "\"player_bye_week\":\"7\",\"rank_ecr\":5,\"pos_rank\":\"WR2\",\"tier\":2,"
       "\"rank_min\":\"3\",\"rank_max\":\"9\",\"rank_ave\":\"5.5\",\"rank_std\":\"2.10\"}"
       "]};</script></body></html>"))

(deftest parse-extracts-and-types-fields
  (let [by-key (fp/ecr-by-key (fp/parse-ecr sample-html))
        bijan  (get by-key (match/key-for "Bijan Robinson" "RB"))]
    (is (= 2 (count by-key)))
    (is (= 2 (:fantasypros/ecr bijan)))
    (is (= 1 (:fantasypros/ecr-tier bijan)))
    (is (= 1.40 (:fantasypros/rank-std bijan)))     ; string -> double
    (is (= 11 (:bye bijan)))                         ; string -> long
    (is (= "RB1" (:fantasypros/pos-rank bijan)))))

(deftest balanced-object-respects-strings
  ;; a } inside a string value must not end the object early
  (is (= "{\"a\":\"x}y\"}" (fp/balanced-object "prefix {\"a\":\"x}y\"} trailing"))))

(deftest parse-returns-nil-without-blob
  (is (nil? (fp/parse-ecr "<html>no ecr here</html>"))))
