(ns draft-day.api.version-test
  (:require [clojure.test :refer [deftest is]]
            [jsonista.core :as json]
            [draft-day.api.routes :as routes]))

(def ^:private mapper (json/object-mapper {:decode-key-fn keyword}))
(defn- parse [resp] (json/read-value (:body resp) mapper))

(deftest version-endpoint-returns-version
  (let [resp (routes/app {:request-method :get :uri "/api/version"})
        body (parse resp)]
    (is (= 200 (:status resp)))
    (is (string? (:version body)))))
