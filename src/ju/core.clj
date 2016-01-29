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

(defn start-http-server [port]
  (timbre/debug "start-http-server:" port)
  (init)
  (reset! shingetsu/http-server-port port)
  (reset! http-server (immutant/run app :host "0.0.0.0" :port port :io-threads param/io-threads :worker-threads param/worker-threads)))

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
  [port]
  (if (Desktop/isDesktopSupported)
    (.browse (Desktop/getDesktop) (URI. (str "http://localhost:" port)))))

(defn ju-output-fn
  ([data] (ju-output-fn nil data))
  ([{:keys [no-stacktrace? stacktrace-fonts] :as opts} data]
   (let [{:keys [level ?err_ vargs_ msg_ ?ns-str hostname_ timestamp_]} data]
     (str
      (force timestamp_) " "
       ; (force hostname_) " "
       ; (clojure.string/upper-case (name level))  " "
       ; "[" (or ?ns-str "?ns") "] - "
       (force msg_)
       (comment when-not no-stacktrace?
         (when-let [err (force ?err_)]
           (str "\n" (stacktrace err opts))))
       ))))

(defn configure-timbre
  []
  (let [filename-base  "ju"]
    (timbre/merge-config!
      {:output-fn ju-output-fn})))

(defn start-app [[port]]
  (configure-timbre)
  (timbre/debug "start-app:" port)
  (.addShutdownHook (Runtime/getRuntime) (Thread. stop-app))
  (load-config-file-if-necessary)
  ; Initialize the database if needed
  (if-not (schema/initialized?)
    (do
      (timbre/info "Initializing the database...")
      (timbre/info "Tables are being created...")
      (schema/create-tables schema/db-spec)))
  ;(timbre/info "Updating files...")
  ;(db/update-all-files)
  (start-nrepl)
  (shingetsu/start-node-monitor)
  (shingetsu/start-crawler)
  (start-http-server (http-port port))
  (open-web-browser port)
  (timbre/info "HTTP server started on port:" (:port @http-server)))

(defn -main [& args]
  (configure-timbre)
  (timbre/debug "main:" (str args))
  (cond
    (some #{"migrate" "rollback"} args)
    (do (migrations/migrate args) (System/exit 0))
    :else
    (start-app args)))
