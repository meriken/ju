(ns ju.routes.home
  (:require [ju.layout :as layout]
            [compojure.core :refer [defroutes GET]]
            [ring.util.http-response :refer [ok]]
            [ring.util.response :refer [content-type]]
            [clojure.java.io :as io]

    ; Meriken
            [taoensso.timbre :as timbre]
            [clojure.data.codec.base64 :as base64]
            [ju.db.core :as db]
            [ring.util.mime-type])
  (:import (java.io ByteArrayInputStream)))

(defn home-page []
  (layout/render "home.html"))

(defroutes home-routes
           (GET "/" [] (home-page))
           (GET "/threads" [] (home-page))
           (GET "/recent-threads" [] (home-page))
           (GET "/thread/:thread-title" [] (home-page))
           (GET "/thread/:thread-title/:qualifier"
                [thread-title qualifier]
             (if (not (re-find #"^[a-f0-9]{32}\.[a-zA-Z0-9]+$" qualifier))
               (home-page)
               (let [file-id (db/get-file-id-by-thread-title thread-title)
                     [_ record-id suffix] (re-find #"^([a-f0-9]{32})\.([a-zA-Z0-9]+)$" qualifier)
                     record (db/get-record-in-file-by-record-id file-id record-id)
                     body (String. (:body record) "UTF-8")
                     elements (->> (clojure.string/split body #"<>")
                                   (map #(re-find #"^([a-zA-Z0-9]+):(.*)$" %))
                                   (map #(do {(keyword (nth % 1)) (nth % 2)}))
                                   (apply merge))]
                 ;(timbre/debug (:suffix elements))
                 (if (= suffix (:suffix elements))
                   {:status  200
                    :headers {"Content-Type" (ring.util.mime-type/ext-mime-type qualifier)}
                    :body    (ByteArrayInputStream. (base64/decode (.getBytes (:attach elements))))}))))
           (GET "/new-posts" [] (home-page))
           (GET "/create-new-thread" [] (home-page))
           (GET "/status" [] (home-page))
           (GET "/help" [] (home-page))
           (GET "/terms" [] (home-page))

           (GET "/docs" [] (ok (-> "docs/docs.md" io/resource slurp))))
