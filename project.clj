(defproject draft-day "0.1.0-SNAPSHOT"
  :description "Fantasy football auction draft assistant (auction-focused, VBD)"
  :url "https://github.com/RockChalkJay/draft-day-clj"
  :license {:name "MIT"}
  :min-lein-version "2.9.0"
  ;; src/cljs is on the classpath so shadow-cljs (:lein true) can find CLJS sources.
  :source-paths ["src/clj" "src/cljs"]
  :test-paths ["test"]
  :dependencies [[org.clojure/clojure "1.12.0"]
                 ;; --- backend web (http-kit serves + fetches) ---
                 [http-kit "2.8.0"]
                 [metosin/reitit "0.7.2"]
                 [ring/ring-core "1.12.2"]
                 [metosin/jsonista "0.3.11"]
                 ;; --- ingestion ---
                 [org.jsoup/jsoup "1.18.1"]         ; FantasyPros HTML scraping
                 [org.clojure/data.csv "1.1.0"]     ; nflverse / DynastyProcess CSVs
                 [com.cognitect/transit-clj "1.0.333"] ; disk cache
                 ;; --- frontend (compiled by shadow-cljs via :lein true) ---
                 [thheller/shadow-cljs "2.28.18"]
                 [reagent "1.2.0"]
                 [re-frame "1.4.3"]]
  :main ^:skip-aot draft-day.server
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
