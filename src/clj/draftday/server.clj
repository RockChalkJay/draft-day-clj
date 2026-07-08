(ns draftday.server
  "http-kit server: serves the compiled SPA from resources/public and the JSON API.
  Stateless by design — the browser owns and re-sends draft state on every call."
  (:require [org.httpkit.server :as http]
            [reitit.ring :as ring]
            [jsonista.core :as json])
  (:gen-class))

(defn- json-response [status body]
  {:status  status
   :headers {"Content-Type" "application/json"}
   :body    (json/write-value-as-string body)})

(defn health-handler [_]
  (json-response 200 {:status "ok" :service "draft-day-clj"}))

(def app
  (ring/ring-handler
   (ring/router
    [["/api/health" {:get health-handler}]])
   ;; static SPA assets, then a 404 fallthrough
   (ring/routes
    (ring/create-resource-handler {:path "/" :root "public"})
    (ring/create-default-handler))))

(defonce ^:private server (atom nil))

(defn stop! []
  (when-let [s @server]
    (s)
    (reset! server nil)))

(defn -main [& _]
  (let [port (Integer/parseInt (or (System/getenv "PORT") "8080"))]
    (reset! server (http/run-server #'app {:port port}))
    (println (str "draft-day-clj server on http://localhost:" port))))
