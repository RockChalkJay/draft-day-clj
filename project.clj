(defproject draft-day "0.1.0-SNAPSHOT"
  :description "Fantasy football auction draft assistant (auction-focused, VBD)"
  :url "https://github.com/RockChalkJay/draft-day-clj"
  :license {:name "MIT"}
  :min-lein-version "2.9.0"
  ;; src/cljs is on the classpath so shadow-cljs (:lein true) can find CLJS sources.
  ;; src/cljc holds code both halves share — db shape and draft persistence logic —
  ;; which also makes it reachable from `lein test` on the JVM.
  :source-paths ["src/clj" "src/cljc" "src/cljs"]
  :test-paths ["test"]
  :dependencies [[org.clojure/clojure "1.12.0"]
                 [org.clojure/tools.logging "1.3.0"]
                 ;; SLF4J binding so tools.logging actually emits (slf4j-api
                 ;; arrives via shadow-cljs/directory-watcher; without a binding
                 ;; it NOPs). slf4j-simple logs WARN+ to stderr.
                 [org.slf4j/slf4j-simple "1.7.36"]
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
  ;; `dev/` holds research harnesses (auction replay, rankings benchmark) that are
  ;; not part of the shipped app. Leiningen activates :dev by default for
  ;; run/test/repl, so they are available without `with-profile`; :uberjar stays clean.
  ;; `lein test` reaches no third-party service. Tests that deliberately do live
  ;; under test/draft_day/integration, tagged ^:integration, and run only when
  ;; asked: `lein test :integration`. See draft-day.tools.offline-check for the
  ;; proof that the default suite stays clean.
  ;;
  ;; A selector rather than a :test profile with a JVM flag — `lein help
  ;; profiles` advises against the latter because it produces tests that behave
  ;; differently from the REPL, and a network rule that only holds under `lein
  ;; test` is worse than none.
  :test-selectors {:default     (complement :integration)
                   :integration :integration
                   :all         (constantly true)}
  :profiles {:dev     {:source-paths ["dev"]}
             :uberjar {:aot :all}})
