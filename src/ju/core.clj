(ns ju.core
  (:require [ju.handler :refer [app init destroy]]
            [immutant.web :as immutant]
            [clojure.tools.nrepl.server :as nrepl]
            [taoensso.timbre :as timbre]
            [environ.core :refer [env]]

    ; Meriken
            [ju.util :refer :all]
            [ju.param :as param]
            [ju.db.core :as db]
            [ju.db.schema :as schema]
            [ju.routes.shingetsu :as shingetsu])
  (:gen-class)
  (:import (java.awt Desktop)
           (java.net URI InetAddress)
           (java.util.logging Logger Level)))

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

(defonce upnp-service (atom nil))

(defn disable-upnp-port-forwading
  []
  (when @upnp-service
    (.shutdown @upnp-service)
    (reset! upnp-service nil)))

(defn enable-upnp-port-forwading
  []
  (disable-upnp-port-forwading)
  (.setLevel (Logger/getLogger "org.fourthline.cling") Level/FINEST)
  (let [port-mapping-array (comment into-array
                             [(org.fourthline.cling.support.model.PortMapping.
                                @shingetsu/http-server-port
                                (.getHostAddress (InetAddress/getLocalHost))
                                org.fourthline.cling.support.model.PortMapping$Protocol/TCP
                                "shingetsu-ju-tcp")
                              (org.fourthline.cling.support.model.PortMapping.
                                @shingetsu/http-server-port
                                (.getHostAddress (InetAddress/getLocalHost))
                                org.fourthline.cling.support.model.PortMapping$Protocol/UDP
                                "shingetsu-ju-udp")])]
    (reset! upnp-service (org.fourthline.cling.UpnpServiceImpl.
                           (into-array
                             [(org.fourthline.cling.support.igd.PortMappingListener.
                                (org.fourthline.cling.support.model.PortMapping.
                                  @shingetsu/http-server-port
                                  (.getHostAddress (InetAddress/getLocalHost))
                                  org.fourthline.cling.support.model.PortMapping$Protocol/TCP
                                  "shingetsu-ju-tcp"))])))
    (.search
      (.getControlPoint @upnp-service)
      (org.fourthline.cling.model.message.header.DeviceTypeHeader.
        (org.fourthline.cling.model.types.DeviceType. "schemas-upnp-org" "InternetGatewayDevice" 1)))))

(defn stop-app []
  (if param/enable-upnp
    (disable-upnp-port-forwading))
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
  (shingetsu/start-api-cache-manager)
  (if param/enable-upnp
    (enable-upnp-port-forwading))
  (start-http-server)
  (open-web-browser)
  (shingetsu/start-node-monitor)
  (shingetsu/start-crawler)
  (timbre/info "HTTP server started on port:" @shingetsu/http-server-port))

(defn -main [& args]
  (start-app args))
