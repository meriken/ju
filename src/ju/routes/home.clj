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
           (GET "/" [] (home-page "�ڎ�"))
           (GET "/threads" [] (home-page "�S�ẴX���b�h"))
           (GET "/recent-threads" [] (home-page "�ŋߍX�V���ꂽ�X���b�h"))
           (GET "/thread/:thread-title" [thread-title] (home-page thread-title))
           (GET "/thread/:thread-title/:qualifier"
                [thread-title qualifier]
             (if (not (re-find #"^[a-f0-9]{32}\.[a-zA-Z0-9]+$" qualifier))
               (home-page (home-page "�X���b�h�ꗗ"))
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
           (GET "/new-posts" [] (home-page "�V�����X�܂Ƃߓǂ�"))
           (GET "/create-new-thread" [] (home-page "�V�K�X���b�h�쐬"))
           (GET "/status" [] (home-page "���"))
           (GET "/help" [] (home-page "�g����"))
           (GET "/terms" [] (home-page "�V���l�b�g���[�N���p�K��"))

           (GET "/docs" [] (ok (-> "docs/docs.md" io/resource slurp))))
