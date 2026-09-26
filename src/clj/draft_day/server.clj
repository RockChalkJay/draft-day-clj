(ns draft-day.server
  "http-kit server entry point: serves the compiled SPA + the JSON API
  (draft-day.api.routes). Stateless — the browser owns and re-sends draft state.

  `-main` also starts an nREPL on a free localhost port for an editor to attach
  to. It lives as long as the JVM rather than the web server: `start!` is what a
  REPL session calls to restart http-kit, and stopping the nREPL there would end
  the session making the call."
  (:require [clojure.java.io :as io]
            [org.httpkit.server :as http]
            [nrepl.server :as nrepl]
            [draft-day.api.routes :as routes])
  (:gen-class))

(defonce ^:private server (atom nil))

(defn stop! []
  (when-let [s @server]
    (s)
    (reset! server nil)))

(defn start! [port]
  (stop!)
  (reset! server (http/run-server #'routes/app {:port port})))

(defn start-nrepl!
  "Start the nREPL and publish its port in `.nrepl-port`, removed again when the
  JVM exits so an editor never auto-connects to a dead port."
  []
  (let [s    (nrepl/start-server :port 0 :bind "127.0.0.1")
        file (io/file ".nrepl-port")]
    (spit file (:port s))
    (.addShutdownHook (Runtime/getRuntime) (Thread. #(io/delete-file file true)))
    s))

(defn -main [& _]
  (let [port (Integer/parseInt (or (System/getenv "PORT") "8080"))]
    (start! port)
    (println (str "draft-day-clj server on http://localhost:" port))
    (println (str "nREPL on port " (:port (start-nrepl!))))))
