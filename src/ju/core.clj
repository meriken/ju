(ns ju.core
  (:require [ju.handler :refer [app init destroy]]
            [immutant.web :as immutant]
            [ju.db.migrations :as migrations]
            [clojure.tools.nrepl.server :as nrepl]
            [taoensso.timbre :as timbre]
            [taoensso.timbre.appenders.3rd-party.rotor :as rotor]
            [environ.core :refer [env]]

    ; Meriken
            [ju.param :as param]
            [ju.db.core :as db]
            [ju.db.schema :as schema]
            [ju.routes.shingetsu :as shingetsu])
  (:gen-class)
  (:import (java.awt Desktop)
           (java.net URI)))

(defonce nrepl-server (atom nil))

(defn parse-port [port]
  (when port
    (cond
      (string? port) (Integer/parseInt port)
      (number? port) port
      :else          (throw (Exception. (str "invalid port value: " port))))))

(defn stop-nrepl []
  (when-let [server @nrepl-server]
    (nrepl/stop-server server)))

(defn start-nrepl
  "Start a network repl for debugging when the :nrepl-port is set in the environment."
  []
  (if @nrepl-server
    (timbre/error "nREPL is already running!")
    (when-let [port (env :nrepl-port)]
      (try
        (->> port
             (parse-port)
             (nrepl/start-server :port)
             (reset! nrepl-server))
        (timbre/info "nREPL server started on port" port)
        (catch Throwable t
          (timbre/error t "failed to start nREPL"))))))

(defn http-port [port]
  (parse-port (or port (env :port) param/default-http-port)))

(defonce http-server (atom nil))

(defn start-http-server
  []
  (init)
  (reset! http-server
          (immutant/run
            app
             (immutant.web.undertow/options
               :host "0.0.0.0"
               :port @shingetsu/http-server-port
               :io-threads param/io-threads
               :worker-threads param/worker-threads))))

(defn stop-http-server []
  (when @http-server
    (destroy)
    (immutant/stop @http-server)
    (reset! http-server nil)))

(defn stop-app []
  (stop-nrepl)
  (stop-http-server)
  (db/shutdown)
  (shutdown-agents))

(def config-file-loaded? (atom false)) ; TODO: Check to see if this is necessary.
(def config-file-path "ju.clj")

(defn load-config-file-if-necessary
  []
  (when (not @config-file-loaded?)
    (try
      (timbre/info "Loading configuration file...")
      (load-file config-file-path)
      (catch Throwable t
        (timbre/info "Failed to load configuration file:" t)))
    (reset! config-file-loaded? (atom true))))

(defn open-web-browser
  []
  (if (Desktop/isDesktopSupported)
    (.browse (Desktop/getDesktop) (URI. (str "http://localhost:"  @shingetsu/http-server-port)))))

(defn ju-output-fn
  ([data] (ju-output-fn nil data))
  ([{:keys [no-stacktrace? stacktrace-fonts] :as opts} data]
   (let [{:keys [level ?err_ vargs_ msg_ ?ns-str hostname_ timestamp_]} data]
     (str
       (force timestamp_) " "
       ; (force hostname_) " "
       ; (clojure.string/upper-case (name level))  " "
       ; "[" (or ?ns-str "?ns") "] "
       (force msg_) " "
       (if (force ?err_)
         (org.apache.commons.lang3.exception.ExceptionUtils/getStackTrace (force ?err_)))
       (comment when-not no-stacktrace?
                (when-let [err (force ?err_)]
                  (str "\n" (stacktrace err opts))))
       ))))

(defn configure-timbre
  []
  (let [filename-base  "ju"]
    (timbre/merge-config!
      {:output-fn ju-output-fn})
    (timbre/merge-config!
      {:ns-blacklist ["slf4j-timbre.adapter"]})))

(defn start-app [[port]]
  (reset! shingetsu/http-server-port (http-port port))
  (configure-timbre)
  (.addShutdownHook (Runtime/getRuntime) (Thread. stop-app))
  (load-config-file-if-necessary)
  ; Initialize the database if needed
  (if-not (schema/initialized?)
    (do
      (timbre/info "Initializing the database...")
      (timbre/info "Tables are being created...")
      (schema/create-tables schema/db-spec)))
  (start-nrepl)
  (db/start-database-monitor)
  (shingetsu/start-node-monitor)
  (shingetsu/start-crawler)
  (shingetsu/start-api-cache-manager)
  (start-http-server)
  (open-web-browser)
  (timbre/info "HTTP server started on port:" @shingetsu/http-server-port))

(defn -main [& args]
  (start-app args))
