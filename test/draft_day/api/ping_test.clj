(ns draft-day.api.ping-test
  (:require [clojure.test :refer [deftest is]]
            [jsonista.core :as json]
            [draft-day.api.routes :as routes]))

(def ^:private mapper (json/object-mapper {:decode-key-fn keyword}))
(defn- parse [resp] (json/read-value (:body resp) mapper))

(deftest ping-endpoint-returns-ok
  (let [resp (routes/app {:request-method :get :uri "/api/ping"})
        body (parse resp)]
    (is (= 200 (:status resp)))
    (is (= true (:ok body)))))
