(ns draft-day.server
  "http-kit server entry point: serves the compiled SPA + the JSON API
  (draft-day.api.routes). Stateless — the browser owns and re-sends draft state."
  (:require [org.httpkit.server :as http]
            [nrepl.server :as nrepl]
            [draft-day.api.routes :as routes])
  (:gen-class))

(defonce ^:private server (atom nil))
(defonce ^:private nrepl-server (atom nil))

(defn stop! []
  (when-let [s @server]
    (s)
    (reset! server nil))
  (when-let [n @nrepl-server]
    (n)
    (reset! nrepl-server nil)))

(defn start! [port]
  (stop!)
  (reset! server (http/run-server #'routes/app {:port port})))

(defn -main [& _]
  (let [port (Integer/parseInt (or (System/getenv "PORT") "8080"))]
    (start! port)
    (println (str "draft-day-clj server on http://localhost:" port))
    (let [s (nrepl/start-server :port 0
                                :bind "127.0.0.1")]
      (reset! nrepl-server s)
      (spit ".nrepl-port" (:port s))
      (println (str "nREPL on port " (:port s))))))