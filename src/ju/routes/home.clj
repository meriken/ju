(ns ju.routes.home
  (:require [ju.layout :as layout]
            [compojure.core :refer [defroutes GET]]
            [ring.util.http-response :refer [ok]]
            [ring.util.response :refer [content-type]]
            [clojure.java.io :as io]

            ; Meriken
            [taoensso.timbre :as timbre]))

(defn home-page []
  (layout/render "home.html"))

(defroutes home-routes
           (GET "/" [] (home-page))
           (GET "/threads" [] (home-page))
           (GET "/thread/:title" [] (home-page))
           (GET "/thread/:title/:qualifier" [] (home-page))
           (GET "/new-posts" [] (home-page))
           (GET "/create-new-thread" [] (home-page))
           ; (GET "/status" [] (home-page))
           (GET "/help" [] (home-page))

           (GET "/docs" [] (ok (-> "docs/docs.md" io/resource slurp))))
