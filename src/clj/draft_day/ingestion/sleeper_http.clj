(ns draft-day.ingestion.sleeper-http
  "Every request this server makes to api.sleeper.app, held to one limit.

  Politeness lives with the vendor, and Sleeper is one host read by five
  namespaces — the universe, the realized weeks, league import, sync and bid
  history. A throttle in any one of them limits that caller and not the host, so
  the limit is here, beneath all of them, and it is per server rather than per
  request: every sync in flight shares it, because Sleeper sees one IP."
  (:require [org.httpkit.client :as http])
  (:import [java.util.concurrent Semaphore]))

(def max-in-flight
  "How many requests Sleeper sees from this server at once. Its documented
  ceiling is 1000 a minute, so this is politeness rather than a limit: forty
  weekly logs fired together is a burst from one IP for no gain a handful of
  connections does not already give."
  4)

(defonce ^:private throttle (Semaphore. max-in-flight true))

(defn get!
  "Network: `http/get`'s response map, holding one of the host's permits until it
  arrives. Released however the request ends — a request that throws must not
  retire a permit for good."
  [url opts]
  (.acquire throttle)
  (try @(http/get url opts)
       (finally (.release throttle))))
