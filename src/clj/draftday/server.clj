(ns draftday.server
  "http-kit server entry point: serves the compiled SPA + the JSON API
  (draftday.api.routes). Stateless — the browser owns and re-sends draft state."
  (:require [org.httpkit.server :as http]
            [draftday.api.routes :as routes])
  (:gen-class))

(defonce ^:private server (atom nil))

(defn stop! []
  (when-let [s @server]
    (s)
    (reset! server nil)))

(defn start! [port]
  (stop!)
  (reset! server (http/run-server #'routes/app {:port port})))

(defn -main [& _]
  (let [port (Integer/parseInt (or (System/getenv "PORT") "8080"))]
    (start! port)
    (println (str "draft-day-clj server on http://localhost:" port))))
