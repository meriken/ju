(ns ju.handler
  (:require [compojure.core :refer [defroutes routes wrap-routes]]
            [ju.layout :refer [error-page]]
            [ju.routes.home :refer [home-routes]]
            [ju.routes.shingetsu :refer [shingetsu-routes]]
            [ju.middleware :as middleware]
            [ju.db.core :as db]
            [compojure.route :as route]
            [taoensso.timbre :as timbre]
            [taoensso.timbre.appenders.3rd-party.rotor :as rotor]
            [selmer.parser :as parser]
            [environ.core :refer [env]]
            [ju.config :refer [defaults]]
            [mount.core :as mount]))

(defn init
  "init will be called once when
   app is deployed as a servlet on
   an app server such as Tomcat
   put any initialization code here"
  []

  (timbre/merge-config!
    {:level     ((fnil keyword :info) (env :log-level))
     :appenders {:rotor (rotor/rotor-appender
                          {:path (or (env :log-path) "ju.log")
                           :max-size (* 512 1024)
                           :backlog 10})}})
  (doseq [component (:started (mount/start))]
    (timbre/info component "started"))
  ((:init defaults)))

(defn destroy
  "destroy will be called when your application
   shuts down, put any clean up code here"
  []
  (timbre/info "ju is shutting down...")
  (doseq [component (:stopped (mount/stop))]
    (timbre/info component "stopped"))
  (timbre/info "shutdown complete!"))

(def app-routes
  (routes
    (var shingetsu-routes)
    (wrap-routes #'home-routes middleware/wrap-csrf)
    (route/not-found
      (:body
        (error-page {:status 404
                     :message "Page Not Found ページが見つかりません。"})))))

(def app (middleware/wrap-base #'app-routes))
