(defproject ju "0.1.0-SNAPSHOT"

  :description "FIXME: write description"
  :url "http://example.com/FIXME"

  :source-paths ["src" "src-cljc"]
  :repositories {"4thline.org-repo" "http://4thline.org/m2"}
  :dependencies [; Luminus
                 [org.clojure/clojure "1.7.0"]
                 [selmer "0.9.5"]
                 [markdown-clj "0.9.82"]
                 [environ "1.0.1"]
                 [metosin/ring-middleware-format "0.6.0"]
                 [metosin/ring-http-response "0.6.5"]
                 [bouncer "0.3.3"]
                 [org.clojure/tools.nrepl "0.2.12"]
                 [com.taoensso/tower "3.0.2"]
                 [com.taoensso/timbre "4.1.4"]
                 [com.fzakaria/slf4j-timbre "0.2.1"]
                 [compojure "1.4.0"]
                 ;not compatible with launch4j
                 ;[ring-webjars "0.1.1"]
                 ;[org.webjars/bootstrap "3.3.6"]
                 ;[org.webjars/jquery "2.1.4"]
                 [ring/ring-defaults "0.1.5"]
                 [ring "1.4.0" :exclusions [ring/ring-jetty-adapter]]
                 [mount "0.1.5"]
                 [buddy "0.8.2"]
                 ;[conman "0.2.7"]
                 [mysql/mysql-connector-java "5.1.34"]
                 [org.clojure/clojurescript "1.7.170" :scope "provided"]
                 [reagent "0.5.1"]
                 [reagent-forms "0.5.13"]
                 [reagent-utils "0.1.5"]
                 [secretary "1.2.3"]
                 [org.clojure/core.async "0.2.374"]
                 [cljs-ajax "0.5.1"]
                 [metosin/compojure-api "0.24.1"]
                 ;[metosin/ring-swagger-ui "2.1.3-4"]
                 ;[org.immutant/web "2.1.1" :exclusions [ch.qos.logback/logback-classic]]
                 [org.immutant/web "2.1.2" :exclusions [ch.qos.logback/logback-classic]]

                 ; Meriken
                 [com.h2database/h2 "1.4.191"]
                 [org.hsqldb/hsqldb "2.3.3"]
                 [clj-http "2.0.0"]
                 [korma "0.4.2"]
                 [org.clojure/java.jdbc "0.3.7"]
                 [pandect "0.5.4"]
                 ;[venantius/accountant "0.1.6"]
                 [jayq "2.5.4"]
                 [com.andrewmcveigh/cljs-time "0.3.14"]
                 [org.clojure/data.codec "0.1.0"]
                 [ring-middleware-format "0.7.0"]
                 [digest "1.4.4"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 ;[commons-lang/commons-lang "2.6"]
                 [org.apache.commons/commons-lang3 "3.1"]
                 [log4j "1.2.17" :exclusions [javax.mail/mail javax.jms/jms com.sun.jdmk/jmxtools com.sun.jmx/jmxri]]
                 [com.twelvemonkeys.imageio/imageio-core "3.1.1"]
                 [com.twelvemonkeys.imageio/imageio-jpeg "3.1.1"]
                 [cheshire "5.5.0"]
                 [clj-rss "0.2.3"]
                 [org.clojure/math.combinatorics "0.1.1"]
                 [org.fourthline.cling/cling-core "2.1.1-SNAPSHOT"]
                 [org.fourthline.cling/cling-support "2.1.1-SNAPSHOT"]
                 ;[org.bitlet/weupnp "0.1.4"]
                 ]

  :min-lein-version "2.0.0"
  :uberjar-name "ju.jar"
  :jvm-opts [;"-server"
             "-XX:ThreadStackSize=4096"
             "-XX:-OmitStackTraceInFastThrow"
             "-Xmx1g"
             "-XX:+UseParNewGC"
             "-XX:+UseConcMarkSweepGC"
             ;"-XX:+UseG1GC"
             "-XX:MaxGCPauseMillis=1000"
             ]
  :resource-paths ["resources" "target/cljsbuild"]

  :main ju.core

  :plugins [[lein-environ "1.0.1"]
            [lein-cljsbuild "1.1.1"]
            [lein-uberwar "0.1.0"]]
  :uberwar
  {:handler ju.handler/app
   :init ju.handler/init
   :destroy ju.handler/destroy
   :name "ju.war"}
  
  :clean-targets ^{:protect false} [:target-path [:cljsbuild :builds :app :compiler :output-dir] [:cljsbuild :builds :app :compiler :output-to]]
  :cljsbuild
  {:builds
   {:app
    {:source-paths ["src-cljs" "src-cljc"]
     :compiler
     {:output-to "target/cljsbuild/public/js/app.js"
      :output-dir "target/cljsbuild/public/js/out"
      :externs ["react/externs/react.js"
                "externs/gpt.js"
                "externs/jquery.js"
                "externs/hoverIntent.js"
                "externs/twitter-bootstrap.js"
                "externs/blueimp.js"
                "externs/highlight.js"
                "externs/emojione.js"
                "externs/bootstrap-dialog.js"]
      :pretty-print true}}}}
  
  :profiles
  {:uberjar {:omit-source true
             :env {:production true}
              :prep-tasks ["compile" ["cljsbuild" "once"]]
              :cljsbuild
              {:builds
               {:app
                {:source-paths ["env/prod/cljs"]
                 :compiler
                 {:optimizations :advanced
                  :pretty-print false
                  :closure-warnings
                  {:externs-validation :off :non-standard-jsdoc :off}}}}} 
             
             :aot :all
             :source-paths ["env/prod/clj"]}
   :dev           [:project/dev :profiles/dev]
   :test          [:project/test :profiles/test]
   :project/dev  {:dependencies [[prone "0.8.2"]
                                 [ring/ring-mock "0.3.0"]
                                 [ring/ring-devel "1.4.0"]
                                 [pjstadig/humane-test-output "0.7.1"]
                                 [com.cemerick/piggieback "0.2.2-SNAPSHOT"]
                                 [lein-figwheel "0.5.0-2"]
                                 [mvxcvi/puget "1.0.0"]]
                  :plugins [[lein-figwheel "0.5.0-2"]]
                   :cljsbuild
                   {:builds
                    {:app
                     {:source-paths ["env/dev/cljs"]
                      :compiler
                      {:main "ju.app"
                       :asset-path "/js/out"
                       :optimizations :none
                       :source-map true}}}}
                  
                  :figwheel
                  {:http-server-root "public"
                   :server-port 3449
                   :nrepl-port 7002
                   :nrepl-middleware ["cemerick.piggieback/wrap-cljs-repl"]
                   :css-dirs ["resources/public/css"]
                   :ring-handler ju.handler/app}
                  
                  :source-paths ["env/dev/clj"]
                  :repl-options {:init-ns ju.core}
                  :injections [(require 'pjstadig.humane-test-output)
                               (pjstadig.humane-test-output/activate!)]
                  ;;when :nrepl-port is set the application starts the nREPL server on load
                  :env {:dev        true
                        :port       8888
                        :nrepl-port 7000
                        :log-level  :trace}}
   :project/test {:env {:test       true
                        :port       3001
                        :nrepl-port 7001
                        :log-level  :trace}}
   :profiles/dev {}
   :profiles/test {}})
