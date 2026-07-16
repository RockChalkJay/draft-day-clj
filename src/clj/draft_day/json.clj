(ns draft-day.json
  (:require [jsonista.core :as json]))

(def mapper (json/object-mapper {:decode-key-fn keyword}))
