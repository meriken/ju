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
            [ring.util.mime-type]
            [ju.param :as param])
  (:import (java.io ByteArrayInputStream)))

(defn home-page
  [title]
  (layout/render "home.html" {:title (str title " - " param/service-name)}))

(defroutes home-routes
           (GET "/" [] (home-page "目次"))
           (GET "/threads" [] (home-page "全てのスレッド"))
           (GET "/recent-threads" [] (home-page "最近更新されたスレッド"))
           (GET "/thread/:thread-title" [thread-title] (home-page thread-title))
           (GET "/thread/:thread-title/:qualifier"
                [thread-title qualifier]
             (timbre/debug "/thread/:thread-title/:qualifier")
             (cond
               (re-find #"^[a-f0-9]{32}\.[a-zA-Z0-9]+$" qualifier)
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
                    :headers {"Content-Type" (ring.util.mime-type/ext-mime-type suffix)}
                    :body    (ByteArrayInputStream. (base64/decode (.getBytes (:attach elements))))}))

               (re-find #"^thumbnail-[a-f0-9]{32}\.[a-zA-Z0-9]+$" qualifier)
               (let [file-id (db/get-file-id-by-thread-title thread-title)
                     [_ record-id suffix] (re-find #"^thumbnail-([a-f0-9]{32})\.([a-zA-Z0-9]+)$" qualifier)
                     image (db/get-image file-id record-id)]
                 ;(timbre/debug (:suffix elements))
                 (if (and image (= suffix "jpg"))
                   {:status  200
                    :headers {"Content-Type" (ring.util.mime-type/ext-mime-type suffix)}
                    :body    (ByteArrayInputStream. (:thumbnail image))}))

               :else
               (home-page (home-page "スレッド一覧"))
               ))
           (GET "/new-posts" [] (home-page "新着レスまとめ読み"))
           (GET "/create-new-thread" [] (home-page "新規スレッド作成"))
           (GET "/status" [] (home-page "状態"))
           (GET "/help" [] (home-page "使い方"))
           (GET "/terms" [] (home-page "新月ネットワーク利用規約"))

           (GET "/docs" [] (ok (-> "docs/docs.md" io/resource slurp))))
