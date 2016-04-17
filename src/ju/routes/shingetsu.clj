(ns ju.routes.shingetsu
  (:require [ju.layout :as layout]
            [compojure.core :refer [defroutes GET POST]]
            [compojure.coercions :refer [as-int]]
            [ring.util.http-response :refer [ok internal-server-error]]
            [ring.util.response :refer [content-type redirect]]
            [ring.util.request :refer [body-string]]
            [ring.util.codec :refer [percent-encode]]
            [clojure.java.io :as io]

    ; Meriken
            [taoensso.timbre :as timbre]
            [clj-http.client :as client]
            [ju.param :as param]
            [ju.util :refer :all]
            [ju.db.core :as db]
            [ju.routes.home :as home]
            [clojure.math.numeric-tower :refer [expt]]
            [clojure.data.codec.base64]
            [clj-time.core]
            [clj-time.coerce]
            [clj-time.format]
            [clj-time.predicates]
            [cheshire.core]
            [clj-rss.core]
            [clojure.math.combinatorics])
  (:import (java.net URLEncoder)
           (java.nio.file Files)
           (java.security MessageDigest)
           (javax.imageio ImageIO ImageWriter ImageWriteParam IIOImage)
           (java.io ByteArrayInputStream ByteArrayOutputStream)
           (java.awt.image BufferedImage)
           (java.awt Image)
           (javax.imageio.stream MemoryCacheImageOutputStream)))



; (def http-params { :headers {"Accept-Encoding" ""} :socket-timeout (* 5 60 1000) :conn-timeout (* 30 60 1000)})
(def http-params
  {:socket-timeout (* 5 60 1000)
   :conn-timeout (* 30 60 1000)
   :retry-handler (fn [ex try-count http-context]
                    ;(timbre/info "HTTP(S) connection failed:" try-count http-context)
                    ;(Thread/sleep 10000)
                    (if (> try-count 4) false true))})

(def http-params-for-quick-commands
  {:socket-timeout 5000
   :conn-timeout 5000
   :retry-handler (fn [ex try-count http-context]
                    ;(timbre/info "HTTP(S) connection failed:" try-count http-context)
                    ;(Thread/sleep 10000)
                    (if (> try-count 0) false true))})

(defonce http-server-port (atom nil))
(defonce active-nodes (atom (hash-set)))
(defonce search-nodes (atom (hash-set)))
(defonce server-node-name (atom nil))
(defonce update-command-history (atom (hash-set)))
(def crawl-node-interval (* 24 60 60 1000))
(def crawl-nodes-interval (* 60 60 1000))
(def check-nodes-interval (* 60 1000))
(def max-num-retries 5)
(def max-num-active-nodes 8)
(def max-num-search-nodes 128)



(defn is-post-spam?
  [file-name stamp record-id elements]
  (let  [thread-title (and (re-find #"^thread_(.*)$" file-name)
                           (unhexify (second (re-find #"^thread_(.*)$" file-name))))]
  (or (and thread-title
           (:name elements) (pos? (count (:name elements)))
           (:mail elements) (pos? (count (:mail elements))) (not (re-find #"^(?i)s?age$" (:mail elements)))
           (:body elements)
           (or (re-find #"^[^ぁ-ゞァ-ヶ]+$" (:body elements))
               (re-find #"http://|href=" (:body elements))))
      (and thread-title
           (:name elements) (pos? (count (:name elements)))
           (:body elements)
           (re-find #"ジミーチュウ|ダンヒル|ゴヤール|クリスチャンディオール|[Aa]mbien|ケイト・スペード|シドニーハミルトン|スーパーコピー|ルミノックス|LUMINOX|kate spade|スント|SUUNTO|ドルチェ| ガッバーナ|diesel|ディーゼル|キッチン調理用機器|スマートウォッチ|スマートフォン|スマートガジェット|人気商品|トリーバーチ|レイバン|オークリー|パーカー|[Vv]iagra|[Gg]ucci" (:body elements)))
      (and file-name
           (some #{file-name} param/known-corrupt-files))
      (and (:name elements) (= "bvd*mfs}@gmail.com" (:name elements)))
      (some #{record-id} #{"1ee4305022f5734d1b519142b5c1234a"
                           "1487684b816147a7a5a610bc53289dec"
                           "a7d2b82c3dc4370879c4f8619c1eb0e8"
                           "e49fea10d5c9fddf41246d54b4f5a11b"
                           "2534c8c95f0497f8e75cc1da0113edcb"})
      (and thread-title
           (= thread-title "雑談")
           stamp
           (<= 1368229462 stamp) (<= stamp 1368259427))
      (and thread-title
           stamp
           (<= 1368259441 stamp) (<= stamp 1368334184)
           (some #{thread-title}
                 #{"新月を広める方法を考えよう～♪"
                   "\"2ちゃんねるに代わる新たな新天地\"\"新月\"\"\""
                   "【2ch】難民キャンプ【書き込み規制】"
                   "SPAM"
                   "雑談しながらリンクを貼るスレ"
                   "新月の開発"
                   "【ただひたすら書き込むスレ】"
                   "雑談"
                   "初心者用質問スレッド"
                   "Twitterの真似事"
                   "ここが新月かぁ～"
                   "今日の天気を報告するスレ"
                   "メニュー"
                   "朔の「状態」を晒す"})
           (not (some #{[stamp thread-title]}
                      #{[1368269164 "朔の「状態」を晒す"]
                        [1368288797 "朔の「状態」を晒す"]
                        [1368322356 "朔の「状態」を晒す"]
                        [1368269568 "雑談しながらリンクを貼るスレ"]
                        [1368291561 "新月の開発"]
                        [1368292773 "新月の開発"]
                        [1368324146 "新月の開発"]
                        [1368287598 "新月を広める方法を考えよう～♪"]
                        [1368321534 "新月を広める方法を考えよう～♪"]
                        [1368321715 "新月を広める方法を考えよう～♪"]
                        [1368323070 "【2ch】難民キャンプ【書き込み規制】"]
                        [1368334122 "【2ch】難民キャンプ【書き込み規制】"]
                        [1368289606 "ここが新月かぁ～"]
                        [1368289928 "ここが新月かぁ～"]
                        [1368287843 "初心者用質問スレッド"]
                        [1368294225 "初心者用質問スレッド"]
                        [1368272890 "初心者用質問スレッド"]
                        [1368274600 "初心者用質問スレッド"]
                        [1368278013 "初心者用質問スレッド"]
                        [1368278588 "初心者用質問スレッド"]
                        [1368278882 "初心者用質問スレッド"]
                        [1368280229 "初心者用質問スレッド"]
                        [1368281312 "初心者用質問スレッド"]
                        [1368282234 "初心者用質問スレッド"]
                        [1368286488 "初心者用質問スレッド"]}))))))

(defn dat-timestamp-formatter
  [local-time]
  (str
    (clj-time.format/unparse
      (clj-time.format/formatter "yyyy/MM/dd")
      local-time)
    (cond
      (clj-time.predicates/sunday? local-time) "(日)"
      (clj-time.predicates/monday? local-time) "(月)"
      (clj-time.predicates/tuesday? local-time) "(火)"
      (clj-time.predicates/wednesday? local-time) "(水)"
      (clj-time.predicates/thursday? local-time) "(木)"
      (clj-time.predicates/friday? local-time) "(金)"
      (clj-time.predicates/saturday? local-time) "(土)")
    (clj-time.format/unparse
      (clj-time.format/formatter " HH:mm:ss")
      local-time)))

(defn convert-record-into-dat-file-line
  [record]
  (let [name (:name record)
        name (if name
               (if (re-find #"◆" name)
                 (let [valid-tripcode? (= (:pubkey record) param/tripcode-public-key)]
                   (if valid-tripcode?
                     (clojure.string/replace name #"(◆.*)$" " </b>$1<b>")
                     (clojure.string/replace name #"(◆.*)$" "")))
                 name))
        name (if (nil? name) param/anonymous-users-handle name)
        mail (if (nil? (:mail record)) "" (:mail record))
        local-time (clj-time.coerce/from-long
                       (*
                         (+
                           (if (string? (:stamp record)) (Long/parseLong (:stamp record)) (:stamp record))
                           (* 9 60 60))
                          1000))
        ts (dat-timestamp-formatter local-time)
        record-short-id (second (re-find #"^(.{8})" (:record-id record)))]
    (str name
         "<>"
         mail
         "<>"
         ts " ID:" record-short-id
         "<>"
         (if (:body record)
           (:body record)))))

(defn get-server-url-base
  []
  (if @server-node-name
    (clojure.string/replace
      (if param/static-server-url-base
        param/static-server-url-base
        (str "http://" (clojure.string/replace @server-node-name (re-pattern (str param/server-path "$")) "")))
      #"/$" "")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Shingetsu Protocol Commands ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare crawl-node)

(defn ping [node-name]
  (try
    (if-not (valid-node-name? node-name)
      (throw (IllegalArgumentException. "Invalid node name.")))
    (if-not (valid-node? node-name)
      (throw (IllegalArgumentException. "Node was blocked.")))

    (let [response-body (:body (client/get (str "http://" node-name "/ping") http-params-for-quick-commands))
          new-server-node-name (second (re-find #"^PONG\n([^\n]+)\n?$" response-body))
          new-server-node-name (str new-server-node-name ":" @http-server-port param/server-path)]
      (if-not (valid-node-name? new-server-node-name)
        (throw (Exception. (str "Invalid node name: " new-server-node-name))))
      (if (nil? param/static-server-node-name)
        (reset! server-node-name new-server-node-name))
      (when (not (db/known-node? node-name))
        (db/add-node node-name)
        (db/mark-node-as-active node-name))
      true)

    (catch Throwable t
      ;(timbre/debug "ping:" t node-name)
      (swap! active-nodes #(clojure.set/difference % #{node-name}))
      (swap! search-nodes #(clojure.set/difference % #{node-name}))
      false)))

(defn join [node-name]
  ; (timbre/debug "join:" node-name)
  (if-not (valid-node-name? node-name)
    (throw (IllegalArgumentException. "Invalid node name.")))
  (if-not (db/known-node? node-name)
    (throw (IllegalArgumentException. "Invalid node.")))
  (if (= node-name @server-node-name)
    (throw (Exception. "Invalid node.")))
  (if-not (valid-node? node-name)
    (throw (IllegalArgumentException. "Node was blocked.")))

  (client/get (str "http://" node-name "/join/" (clojure.string/replace @server-node-name #"/" "+")) http-params-for-quick-commands)
  (swap! active-nodes conj node-name))

(defn bye [node-name]
  ; (timbre/debug "bye:" node-name)
  (if-not (valid-node-name? node-name)
    (throw (IllegalArgumentException. "Invalid node name.")))
  (if-not (valid-node? node-name)
    (throw (IllegalArgumentException. "Node was blocked.")))

  (swap! active-nodes #(clojure.set/difference % #{node-name}))
  (client/get (str "http://" node-name "/bye/" (clojure.string/replace @server-node-name #"/" "+")) http-params-for-quick-commands))

(defn node [node-name]
  (if-not (valid-node-name? node-name)
    (throw (IllegalArgumentException. "Invalid node name.")))
  (if-not (valid-node? node-name)
    (throw (IllegalArgumentException. "Node was blocked.")))

  (let [response (client/get (str "http://" node-name "/node") http-params-for-quick-commands)
        new-node-name (clojure.string/replace (:body response) #"\n$" "")]
    (if-not (valid-node-name? new-node-name)
      (throw (Exception. "Invalid node name.")))
    new-node-name))

(defn recent [node-name range]
  (if-not (valid-node-name? node-name)
    (throw (IllegalArgumentException. "Invalid node name.")))
  (if-not (valid-range? range)
    (throw (IllegalArgumentException. "Invalid range.")))
  (if-not (valid-node? node-name)
    (throw (IllegalArgumentException. "Node was blocked.")))

  (let [file (:body (client/get (str "http://" node-name "/recent/" range) http-params))
        file (clojure.string/replace file #"(?m)^(?![0-9]+<>[0-9a-f]{32}<>[a-z]+_).*$" "")
        file (clojure.string/replace file #"(?m)^.*tag:tag:.*$" "")
        file (clojure.string/replace file #"\r" "")
        file (clojure.string/replace file #"\n+" "\n")]
    file))

(defn update
  ([node-name file-name stamp record-id]
   (update node-name file-name stamp record-id @server-node-name))

  ([node-name file-name stamp record-id sender-node-name]
   (if-not (valid-file-name? file-name)
     (throw (IllegalArgumentException. "Invalid file name.")))
   (if-not (valid-node? node-name)
     (throw (IllegalArgumentException. "Node was blocked.")))

   (client/get
     (str "http://" node-name "/update/" file-name "/" stamp "/" record-id "/" (clojure.string/replace sender-node-name #"/" "+"))
     http-params)))



;;;;;;;;;;;
; Daemons ;
;;;;;;;;;;;

(defn check-node
  [node-name]
  ;(timbre/info "Checking node:" node-name)
  (if-not (valid-node-name? node-name)
    (throw (IllegalArgumentException. "Invalid node name.")))
  (if (= node-name @server-node-name)
    (throw (Exception. "Invalid node name.")))
  (if-not (valid-node? node-name)
    (throw (IllegalArgumentException. "Node was blocked.")))

  (when (ping node-name)
    ; Add as an active node if appropriate.
    (try
      (if (and
            (not (some #{node-name} @active-nodes))
            (< (count @active-nodes) max-num-active-nodes))
        (join node-name))
      (catch Throwable t
        (timbre/error t)
        nil))

    ; Add as a search node if appropriate.
    (try
      (if (and
            (not (some #{node-name} @search-nodes))
            (< (count @search-nodes) max-num-search-nodes))
        (swap! search-nodes conj node-name))
      (catch Throwable t
        (timbre/error t)
        nil))

    ; Get a new node.
    (try
      (dotimes [n 1]
        (let [new-node-name (node node-name)]
          (if (not (= new-node-name @server-node-name))
            (ping new-node-name))))
      (catch Throwable t
        (timbre/error t)
        nil))

   ))

(defn check-nodes
  ([]
   (check-nodes false))
  ([burst-mode]
   ;(timbre/info "Checking nodes..")
   (try
     (if (pos? (count @active-nodes))
       (bye (first (shuffle @active-nodes))))
     (if (pos? (count @search-nodes))
       (swap! search-nodes #(clojure.set/difference % #{(first (shuffle @search-nodes))})))

     (let [nodes (db/get-all-nodes)]
       (if (zero? (count nodes))
         (dorun ((if burst-mode pmap map) #(try (check-node %1) (catch Throwable t)) param/initial-nodes))
         (dorun ((if burst-mode pmap map) #(try (check-node %1) (catch Throwable t)) (shuffle (map :node-name nodes))))))

     (while (> (count @active-nodes) max-num-active-nodes)
       (bye (first (shuffle @active-nodes))))

     (catch Throwable t
       (timbre/error "check-nodes:" t)
       nil))))

(defn start-node-monitor []
  (if param/static-server-node-name
    (reset! server-node-name param/static-server-node-name))
  (dorun (map db/delete-node param/blocked-nodes))
  (try
    (check-nodes true)
    (catch Throwable t
      (timbre/error "Node Monitor:" t)))
  (do
    (future
      (timbre/info "Node monitor started." )
      (while true
        (Thread/sleep check-nodes-interval)
        (try
          (check-nodes)
          (catch Throwable t
            (timbre/error "Node Monitor:" t)))))))

(defn get-files-with-recent-command
  ([]
    (get-files-with-recent-command false))
  ([add-tags]
    (let [records (clojure.string/split (apply str (pmap #(try (recent %1 "0-") (catch Throwable _)) @search-nodes)) #"\n")
          records (remove #(not (re-find #"^[0-9]+<>[0-9a-f]{32}<>thread_[0-9A-F]+(<>.*)?$" %)) records)
          file-names (map
                       (fn [record]
                         (try
                           (let [match (re-find #"^[0-9]+<>[0-9a-f]{32}<>(thread_[0-9A-F]+)(<>.*)?$" record)
                                 file-name (nth match 1 nil)
                                 tags-string (if (nth match 2 "") (nth (re-find #"^<>tag:(.+)$" (nth match 2 "")) 1 nil))
                                 tags (if tags-string
                                        (clojure.string/split tags-string #" +")
                                        '())
                                 tags (remove #(re-find #"[ 　<>&]" %) tags )]
                             (when (and file-name (not (some #{file-name} param/known-corrupt-files)))
                               (db/add-file file-name)
                               (if (pos? (count tags))
                               (let [file-id (:id (db/get-file file-name))]
                                 (db/add-suggested-tags file-id tags)
                                 (if add-tags
                                   (dorun (map #(db/add-file-tag file-id %) tags)))))
                               file-name))
                           (catch Throwable _)))
                       records)
          file-names (remove nil? file-names)]
      (doall file-names))))

(defn download-file-from-node
  ([node-name file-name]
   (download-file-from-node node-name file-name "0-"))

  ([node-name file-name range]
   (download-file-from-node node-name file-name range nil))

  ([node-name file-name range record-id]
   (if-not (valid-node? node-name)
     (throw (IllegalArgumentException. "Node was blocked.")))

   (if-not (valid-node-name? node-name)
     (throw (IllegalArgumentException. "Invalid node name.")))
   (if-not (valid-file-name? file-name)
     (throw (IllegalArgumentException. "Invalid file name.")))
   (if-not (valid-range? range)
     (throw (IllegalArgumentException. "Invalid range.")))
   ;TODO: Check record-id

   (db/add-file file-name)
   (let [file-id (db/get-file-id file-name)
         existing-records (if file-id (db/get-all-active-and-deleted-records-in-file-without-bodies file-id))
         blocked-records (if file-id (db/get-all-blocked-records-in-file file-name node-name))]
     (cond
       (and record-id (db/is-record-blocked? file-name (Long/parseLong range) record-id node-name))
       ;(timbre/info (str "Blocked record on blacklist: " node-name " " file-name " " range " " record-id))
       nil

       (re-find #"-$" range) ; Use /head to identify missing records.
       (let [file (:body (try-times max-num-retries (client/get (str "http://" node-name "/head/" file-name "/" range) http-params)))
             file (if (nil? file) "" file)
             file (clojure.string/replace file #"(?m)^(?![0-9]+<>[0-9a-f]{32}).*$" "")
             file (clojure.string/replace file #"\r" "")
             file (clojure.string/replace file #"\n+" "\n")
             records (remove #(zero? (count %)) (clojure.string/split-lines file))
             records (map #(let [match (re-find #"^([0-9]+)<>([0-9a-f]{32})" %)]
                            {:stamp (Long/parseLong (nth match 1)) :record-id (nth match 2)})
                          records)
             existing-records (map #(do {:stamp (:stamp %) :record-id (:record-id %)}) existing-records)
             records (clojure.set/difference (into #{} records) (into #{} existing-records))
             blocked-records (map #(do {:stamp (:stamp %) :record-id (:record-id %)}) blocked-records)
             records (clojure.set/difference (into #{} records) (into #{} blocked-records))]
         (if (<= (count records) 1) ;TODO: Use an intelligent guess to predict the size of response.
           (dorun (map #(download-file-from-node node-name file-name (str (:stamp %)) (:record-id %)) records))
           (let [stamps (map :stamp records)
                 oldest (apply min stamps)
                 newest (apply max stamps)]
             (try
               (download-file-from-node node-name file-name (str oldest "-" newest))
               (catch Throwable t))
             (let [existing-records (map #(identity {:stamp (:stamp %) :record-id (:record-id %)}) (db/get-all-active-and-deleted-records-in-file-without-bodies file-id))
                   records (clojure.set/difference (into #{} records) (into #{} existing-records))]
               (dorun (map #(download-file-from-node node-name file-name (str (:stamp %)) (:record-id %)) records))))))

       ; Use the supplied range.
       :else
       (let [_ (timbre/info "Downloading file:" node-name file-name (file-name-to-thread-title file-name) range record-id)
             url (if record-id
                   (str "http://" node-name "/get/" file-name "/" range "/" record-id)
                   (str "http://" node-name "/get/" file-name "/" range))
             file (:body (try-times max-num-retries (client/get url http-params)))
             file (clojure.string/replace file #"(?m)^(?![0-9]+<>[0-9a-f]{32}<>).*$" "")
             file (clojure.string/replace file #"\r" "")
             file (clojure.string/replace file #"\n+" "\n")
             records (remove #(zero? (count %)) (clojure.string/split-lines file))]
         (dorun
           (map
             (fn [record]
               (try
                 (let [match (re-find #"^([0-9]+)<>([0-9a-f]{32})<>(.*)$" record)
                       stamp (Long/parseLong (nth match 1))
                       record-id (nth match 2)
                       body (.getBytes (nth match 3) "UTF-8")
                       elements (->> (clojure.string/split (nth match 3) #"<>")
                                     (map #(re-find #"^([a-zA-Z0-9_]+):(.*)$" %))
                                     (map #(do {(keyword (nth % 1)) (nth % 2)}))
                                     (apply merge))]
                   (cond
                     (db/is-record-blocked? file-name stamp record-id node-name)
                     (timbre/info (str "Blocked record on blacklist: " node-name " " file-name " " (nth (re-find #"^([0-9]+<>[0-9a-f]+)<>" record) 1 "")))

                     (not (= (md5 body) record-id))
                     (do
                       (timbre/info (str "Blocked record with invalid ID: " node-name " " file-name " " (nth (re-find #"^([0-9]+<>[0-9a-f]+)<>" record) 1 "")))
                       (db/add-blocked-record-with-origin file-name stamp record-id node-name))

                     (and (:stamp elements) (not (= (:stamp elements) (str stamp))))
                     (do
                       (timbre/info (str "Blocked record with invalid stamp: " node-name " " file-name " " (nth (re-find #"^([0-9]+<>[0-9a-f]+)<>" record) 1 "")))
                       (db/add-blocked-record-with-origin file-name stamp record-id node-name))

                     (and (:file_name elements) (not (= (:file_name elements) file-name)))
                     (do
                       (timbre/info (str "Blocked record with invalid file name: " node-name " " file-name " " (nth (re-find #"^([0-9]+<>[0-9a-f]+)<>" record) 1 "")))
                       (db/add-blocked-record-with-origin file-name stamp record-id node-name))

                     :else
                     (let [deleted (is-post-spam? file-name stamp record-id elements)]
                       (db/add-record
                         file-id
                         stamp
                         record-id
                         body
                         deleted
                         (convert-record-into-dat-file-line
                           (-> elements
                               (assoc :stamp stamp)
                               (assoc :record-id record-id)))
                         (:suffix elements)
                         node-name
                         nil)
                       ;(db/mark-file-as-dirty file-id)
                       (if (some #{(:suffix elements)} param/image-suffixes)
                         (db/create-image file-id stamp record-id elements deleted))
                       )))
                 (catch Throwable t
                   (db/update-file file-id)
                   ;(clojure.stacktrace/print-stack-trace t)
                   (timbre/info (str "Skipped record: " (str t) " " node-name " " file-name " " (nth (re-find #"^([0-9]+<>[0-9a-f]+)<>" record) 1 ""))))))
             records))
         (db/update-file file-id)
         (count records))))))

(defn download-file
  ([file-name]
   (download-file file-name "0-"))

  ([file-name range]
   (if-not (valid-file-name? file-name)
     (throw (IllegalArgumentException. "Invalid file name.")))
   (if-not (valid-range? range)
     (throw (IllegalArgumentException. "Invalid range.")))
   (dorun
     (pmap
       #(try (download-file-from-node % file-name range) (catch Throwable t (timbre/error t)))
       (shuffle @search-nodes)))
   true))

(defn crawl-node
  [node-name
   & {:keys [randomize recent]
      :or {randomize true
           recent false }}]
  (timbre/info "Crawler: Crawling node:" node-name)
  (if-not (valid-node? node-name)
    (throw (IllegalArgumentException. "Node was blocked.")))
  (try
    ; Try to download a list of files.
    (let [file-names (concat
                       ; Saku
                       (try
                         (clojure.string/split-lines
                           (:body (client/get (str "http://" (nth (re-find #"^(.*)/server\.cgi$" node-name) 1) "/gateway.cgi/csv/changes/file") http-params)))
                         (catch Throwable _ '()))
                       ; Ju
                       (try
                         (clojure.string/split-lines
                           (:body (client/get (str "http://" node-name "/files") http-params)))
                         (catch Throwable _ '())))
          file-names (remove #(not (re-find #"^thread_[0-9A-F]+$" %)) file-names)

          file-list (if (and file-names (pos? (count file-names)))
                      (map (fn [file-name]
                             (do {:file-name file-name
                                  :time-updated (let [file (db/get-file file-name)]
                                                  (if file (:time-updated file) nil))}))
                           file-names)
                      (db/get-all-files))
          file-list (if randomize (shuffle file-list) file-list)
          ;file-list (reverse (sort-by #(clj-time.coerce/to-long (clj-time.coerce/from-sql-time (:time-updated %))) file-list))
          ]
      (if (zero? (count file-names))
        (throw (Exception.)))
      ;(timbre/debug "crawl-node: Downloaded a list of files:" node-name)
      (dorun
        (map
          #(do
            (db/add-file (:file-name %))
            (try
              (download-file-from-node
                node-name
                (:file-name %)
                (if recent
                  (str
                    (- (long (/ (clj-time.coerce/to-long (clj-time.core/now)) 1000))
                       (* 30 24 60 60))
                    "-")
                  "0-"))
              (catch Throwable t
                (timbre/info "Crawler: Failed to download file:" node-name (:file-name %)))))
          file-list))
      (db/mark-node-as-crawled node-name)
      (timbre/info "Crawler: Done crawling node:" node-name)
      true)

    (catch Throwable t
      ; A list of files was not available.
      ;(timbre/debug "crawl-node: Failed to download a list of files:" node-name)
      (try
        (dorun
          (map
            #(when (some #{node-name} @search-nodes)
              (try
                (download-file-from-node node-name %)
                (catch Throwable t
                  (timbre/info "Crawler: Done crawling node:" node-name %))))
            (shuffle (map :file-name (db/get-all-files)))))
        (catch Throwable t
          (timbre/error t)
          nil)))))

(defn crawl-nodes
  [& {:keys [force-crawling]
      :or {force-crawling false}}]
  (timbre/info "Crawler: Crawling nodes...")
  (try
    (timbre/info "Crawler: Downloading lists of recently updated files...")
    (let [file-names (get-files-with-recent-command)
          designated-super-nodes (nth
                                   (shuffle (into () (clojure.set/intersection @search-nodes (into #{} param/initial-super-nodes))))
                                   0 nil)]
      (when designated-super-nodes
        (timbre/info "Crawler: Crawling" designated-super-nodes "first...")
        (crawl-node designated-super-nodes :randomize false :recent true)
        (crawl-node designated-super-nodes :randomize false))
      (dorun
        ((if param/enable-parallel-crawling pmap map)
          #(let [time-crawled (:time-crawled (db/get-node %))
                 time-elapsed (and time-crawled (- (clj-time.coerce/to-long (clj-time.core/now)) (.getTime time-crawled)))]
            (if (or force-crawling
                    (nil? time-crawled)
                    (>= time-elapsed crawl-node-interval))
              (crawl-node %)
              (timbre/info "Crawler: Skipped node:" % time-elapsed)))
          (shuffle
            (if designated-super-nodes
              (clojure.set/difference @search-nodes #{designated-super-nodes})
              @search-nodes)))))
    (timbre/info "Crawler: Done crawling nodes.")
    (catch Throwable t
      (timbre/error "Crawler:" t)
      nil)))

(defn start-crawler
  []
  (if param/enable-crawler
    (do
      (future
        (configure-timbre)
        (timbre/info "Crawler started." @active-nodes @search-nodes)
        (crawl-nodes :force-crawling true)
        (while true
          (Thread/sleep crawl-nodes-interval)
          (try
            (crawl-nodes :force-crawling false)
            (catch Throwable t
              (timbre/error "Crawler:" t))))))))



;;;;;;;;;;;;;;
; Signatures ;
;;;;;;;;;;;;;;

(def int-to-base64-char-table
  {0 "A" 16 "Q" 32 "g" 48 "w"
   1 "B" 17 "R" 33 "h" 49 "x"
   2 "C" 18 "S" 34 "i" 50 "y"
   3 "D" 19 "T" 35 "j" 51 "z"
   4 "E" 20 "U" 36 "k" 52 "0"
   5 "F" 21 "V" 37 "l" 53 "1"
   6 "G" 22 "W" 38 "m" 54 "2"
   7 "H" 23 "X" 39 "n" 55 "3"
   8 "I" 24 "Y" 40 "o" 56 "4"
   9 "J" 25 "Z" 41 "p" 57 "5"
   10 "K" 26 "a" 42 "q" 58 "6"
   11 "L" 27 "b" 43 "r" 59 "7"
   12 "M" 28 "c" 44 "s" 60 "8"
   13 "N" 29 "d" 45 "t" 61 "9"
   14 "O" 30 "e" 46 "u" 62 "+"
   15 "P" 31 "f" 47 "v" 63 "/" })

(def base64-char-to-int-table (clojure.set/map-invert int-to-base64-char-table))

(defn- expt-mod [b e n] (bigint (.modPow (biginteger b) (biginteger e) (biginteger n))))

(defn- hex-string-to-bigint
  [s]
  (apply +
         (map
           (fn [[x y] n] (* (bigint (Integer/parseInt (str x y) 16)) (expt 256 n)))
           (partition 2 s)
           (range (/ (count s) 2)))))

(defn- is-nth-bit-on? [num n] (pos? (rem (quot num (expt 2 (dec n))) 2)))

(defn- bigint-to-base64-
  [n]
  (if (>= n 64)
    (concat (list (get int-to-base64-char-table (rem n 64))) (bigint-to-base64- (quot n 64)))
    (list (get int-to-base64-char-table n))))

(defn- add-padding [s] (str s (apply str (repeat (- 86 (count s)) "A"))))

(defn- bigint-to-base64
  [n]
  (add-padding (apply str (bigint-to-base64- n))))

(defn- base64-to-bigint
  [s]
  (apply +
         (map (fn [c n] (* (bigint (get base64-char-to-int-table (str c))) (expt 64 n)))
              s
              (range (count s)))))

(defn get-prime-numbers-for-signature
  [password]
  (let [hashs (str (md5 password)
                   (md5 (str password "pad1"))
                   (md5 (str password "pad2"))
                   (md5 (str password "pad3")))
        match (re-find #"^(.{56})(.{72})$" hashs)
        p (hex-string-to-bigint (nth match 1))
        q (hex-string-to-bigint (nth match 2))
        p (if (is-nth-bit-on? p 216) p (+ p (expt 2 215)))
        q (if (is-nth-bit-on? p 280) q (+ q (expt 2 279)))
        p (bigint (.nextProbablePrime (biginteger p)))
        q (bigint (.nextProbablePrime (biginteger q)))
        t 30531
        e 65537]
    (loop [p p
           q q]
      (let [n (* p q)
            d (bigint (.modInverse (biginteger e) (biginteger (* (dec p) (dec q)))))]
        (if (= (expt-mod t (* e d) n) t)
          [n d]
          (recur (+ p 2) (+ q 2)))))))

(defn- md5-string-to-bigint
  [s]
  (apply +
         (map
           (fn [c n] (* (bigint (int c)) (expt 256 n)))
           s
           (range (count s)))))

(defn sign-post
  [target password]
  (let [[n d] (get-prime-numbers-for-signature password)
        public-key (bigint-to-base64 n)
        secret-key (bigint-to-base64 d)
        target-bigint (md5-string-to-bigint (md5 target))
        c (expt-mod target-bigint d n)
        signature (add-padding (apply str (bigint-to-base64 c)))]
    {:public-key public-key :signature signature}))

(defn verify-signature
  [target public-key signature]
  ;(timbre/debug "verify-signature:" target public-key signature)
  (try
    (= (md5-string-to-bigint (md5 target))
     (expt-mod (base64-to-bigint signature) 65537 (base64-to-bigint public-key)))
    (catch Throwable _
      false)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Shingetsu Protocol Commands ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-remote-address
  [request]
  (clojure.string/replace
    (or
      (get-in request [:headers "x-forwarded-for"])
      (:remote-addr request))
    #",.*$"
    ""))

(defn process-get-command
  [request without-bodies]
  (let [{:keys [headers params body server-name]} request]
    (let [{file-name :file-name range :range} params
          file-id (db/get-file-id file-name)
          match (re-find #"^([0-9]*)-([0-9]*)$" range)
          start (try (Long/parseLong (if match (nth match 1) (nth (re-find #"^([0-9]*)$" range) 1))) (catch Throwable t nil))
          end (try (Long/parseLong (if match (nth match 2) (nth (re-find #"^([0-9]*)$" range) 1))) (catch Throwable t nil))
          record-id (:record-id params)]
      (if (and
            (valid-file-name? file-name)
            (valid-range? range)
            (or
              without-bodies
              (>= param/max-get-command-rersponse-size
                  (reduce + (map :size (db/get-records-in-file-with-range-without-bodies file-id start end))))))
        (->
          (ok (apply
                str
                (map
                  #(do
                    (if (and (not without-bodies) (not (= (:record-id %) (md5 (:body %)))))
                      (throw (Exception. "Invalid record ID")))
                    (if (and record-id (not (= record-id (:record-id %))))
                      ""
                      (str
                        (:stamp %) "<>"
                        (:record-id %)
                        (if-not without-bodies
                          (str "<>" (String. (:body %) "UTF-8"))
                          "")
                        "\n")))
                  ((if without-bodies
                     db/get-records-in-file-with-range-without-bodies
                     db/get-records-in-file-with-range)
                    file-id start end))))
          (content-type "text/plain; charset=UTF-8"))))))

(defn process-record-body
  [record]
  (let [body (String. (:body record) "UTF-8")
        elements (->> (clojure.string/split body #"<>")
                      (map #(re-find #"^([a-zA-Z0-9_]+):(.*)$" %))
                      (map #(do {(keyword (nth % 1)) (nth % 2)}))
                      (apply merge))
        target (and (:target elements)
                    (clojure.string/join "<>"
                                         (map #(str % ":" ((keyword %) elements))
                                              (clojure.string/split (:target elements) #","))))
        elements (if (or (nil? target)
                         (and (verify-signature target (:pubkey elements) (:sign elements))
                              (= (into #{} (map keyword (clojure.string/split (:target elements) #",")))
                                 (clojure.set/difference
                                   (into #{} (keys elements))
                                   #{:sign :pubkey :target}))))
                   elements
                   (-> elements
                       (assoc :sign nil)
                       (assoc :pubkey nil)
                       (assoc :target nil)))
        elements (assoc elements :attach (if (:attach elements) true false))]
    (-> record
        (assoc :body nil)
        (dissoc :origin)
        (dissoc :remote-address)
        (merge elements))))

(defn update-dat-file-line-in-record
  [id]
  (try
    (let [record (process-record-body (db/get-record-by-id id))
          dat-file-line (convert-record-into-dat-file-line record)]
      (if (zero? (mod id 100))
        (timbre/info "update-dat-file-line-in-all-record:" id))
      (db/update-dat-file-line
        id
        dat-file-line
        (:suffix record)))
    (catch Throwable t
      (clojure.stacktrace/print-stack-trace t)
      (timbre/info "update-dat-file-line-in-record:" t id))))

(defn update-dat-file-lines-in-all-records
  []
  (dorun (map #(do
                (if (zero? (mod (:id %) 100))
                  (timbre/info "update-dat-file-line-in-all-record:" (:id %)))
                (update-dat-file-line-in-record (:id %)))
              (sort-by :id (db/get-all-records-with-ids-only)))))


(defn
  bracket-link-to-2ch-style-link
  [s]
  (if (re-find #"\[\[([^\]]+)/([a-f0-9]{8})\]\]" s)
    ; with ID
    (let [[_ thread-title record-short-id] (re-find #"\[\[([^\]]+)/([a-f0-9]{8})\]\]" s)
          file-name (thread-title-to-file-name thread-title)
          file (db/get-file file-name)]
      (if (or (nil? file) (nil? (:time-first-post file)))
        s
        (let [records (db/get-all-records-in-file-with-record-short-ids-only (:id file))
              post-numbers-map (apply merge (map
                                              (fn [record post-number]
                                                {(:record-short-id record) post-number})
                                              records
                                              (range 1 (inc (count records)))))
              post-number (get post-numbers-map record-short-id nil)]
          (str
            "[[" thread-title "/" record-short-id "( "
            (get-server-url-base) "/test/read.cgi/2ch/"
            (+ (long (/ (clj-time.coerce/to-long (clj-time.coerce/from-sql-time (:time-first-post file))) 1000))
               (* 9 60 60))
            "/" (if post-number (str post-number)) " )"))))
    ; without ID
    (let [thread-title (second (re-find #"\[\[([^\]]+)\]\]" s))
          file-name (thread-title-to-file-name thread-title)
          file (db/get-file file-name)]
      (if (or (nil? file) (nil? (:time-first-post file)))
        s
        (str
          "[[" thread-title "( "
          (get-server-url-base) "/test/read.cgi/2ch/"
          (+ (long (/ (clj-time.coerce/to-long (clj-time.coerce/from-sql-time (:time-first-post file))) 1000))
             (* 9 60 60))
          "/ )]]")))))

(defn update-dat-file-lines-in-all-records
  []
  (dorun (map #(try
                (let [record (process-record-body (db/get-record-by-id (:id %)))
                      dat-file-line (convert-record-into-dat-file-line record)]
                  (if (zero? (mod (:id %) 100))
                    (timbre/info "update-dat-file-lines-in-all-records:" (:id %)))
                  (db/update-dat-file-line
                    (:id %)
                    dat-file-line
                    (:suffix record)))
                (catch Throwable t
                  (timbre/info "update-dat-file-lines-in-all-records:" (str t) (:id %))))
              (sort-by :id (db/get-all-records-with-ids-only)))))



(defn process-post
  [thread-title name mail password body attachment remote-address]
  (if (or
        (not (re-find #"^[^\[\]\<\>\/]{1,30}$" thread-title))
        (re-find #"^[ ]" thread-title)
        (re-find #"[ ]$" thread-title)
        (re-find #"^[ 　]*$" thread-title))
    (throw (ex-info "スレッドの題名が不正です。\n題名の長さは30文字までです。\n題名に次の文字は使えません: /<>[]\nまた先頭、もしくは末尾の半角の空白も不可です。" {})))
  (let [file-name (thread-title-to-file-name thread-title)
        file (db/get-file file-name)
        file (if file
               file
               (db/add-file file-name))
        file-id (:id file)
        stamp (long (/ (clj-time.coerce/to-long (clj-time.core/now)) 1000))
        escape-special-characters (fn [s]
                                    (-> s
                                        (clojure.string/replace #"&" "&amp;")
                                        (clojure.string/replace #"<" "&lt;")
                                        (clojure.string/replace #">" "&gt;")))
        name? (and name (pos? (count name)))
        password? (and password (pos? (count password)))
        body? (and body (pos? (count body)))
        tripcode? (and name? (re-find #"#" name))
        _ (if (and tripcode? password?)
            (throw (ex-info "署名とトリップは同時に使うことは出来ません。" {})))
        _ (if (and tripcode? (nil? param/tripcode-password))
            (throw (ex-info "この掲示版ではトリップは使えません。" {})))
        password (if tripcode?
                   param/tripcode-password
                   password)
        password? (if tripcode?
                   true
                   password?)
        tripcode-key (if tripcode? (second (re-find #"#(.*)$" name)))
        tripcode (if tripcode-key (generate-tripcode tripcode-key))
        name (if tripcode
               (str (second (re-find #"^([^#]*)#" name)) "◆" tripcode)
               name)
        attachment? (and attachment
                         (:filename attachment)
                         (pos? (count (:filename attachment)))
                         (:size attachment)
                         (pos? (:size attachment))
                         (:tempfile attachment))
        _ (if (and (not body?) (not attachment?))
            (throw (ex-info "空の書き込みはできません。"  {})))
        _ (if (and attachment? (> (:size attachment) param/max-attachment-size))
            (throw (ex-info (str "添付ファイルが大きすぎです(最大サイズ: " (quot param/max-attachment-size (* 1024 1024)) "MB)。") {})))
        record-body (str
                      (if body?
                        (str
                          "body:"
                          (clojure.string/replace (escape-special-characters body) #"\r?\n" "<br>")
                          ))
                      (if attachment?
                        (str
                          (if body?
                            "<>")
                          "attach:"
                          (String. (clojure.data.codec.base64/encode (Files/readAllBytes (.toPath (:tempfile attachment)))) "ASCII")
                          "<>suffix:"
                          (let [match (re-find #"\.([a-zA-Z0-9]+)$" (:filename attachment))]
                            (if match (second match) "bin"))))
                      (if (and name (pos? (count name))) (str "<>name:" (escape-special-characters name)))
                      (if (and mail (pos? (count mail))) (str "<>mail:" (escape-special-characters mail)))
                      "<>stamp:" stamp
                      "<>file_name:" (:file-name file))
        record-body (if-not password?
                      record-body
                      (let [{:keys [public-key signature]} (sign-post record-body password)]
                        (str
                          record-body
                          "<>pubkey:" public-key
                          "<>sign:" signature
                          "<>target:body"
                          (if attachment? ",attach,suffix")
                          (if (and name (pos? (count name))) ",name")
                          (if (and mail (pos? (count mail))) ",mail")
                          ",stamp,file_name")))
        record-id (md5 record-body)
        elements (->> (clojure.string/split record-body #"<>")
                      (map #(re-find #"^([a-zA-Z0-9_]+):(.*)$" %))
                      (map #(do {(keyword (nth % 1)) (nth % 2)}))
                      (apply merge))
        entry {:file-name (:file-name file) :stamp stamp :record-id record-id}]
    ;(timbre/debug record-body)
    (db/add-record
      file-id
      stamp
      record-id
      (.getBytes record-body "UTF-8")
      false
      (convert-record-into-dat-file-line
        (-> elements
            (assoc :stamp stamp)
            (assoc :record-id record-id)))
      (:suffix elements)
      @server-node-name
      remote-address)
    (if (some #{(:suffix elements)} param/image-suffixes)
      (db/create-image file-id stamp record-id elements false))
    (db/update-num-records-in-file file-id)
    (db/process-update-command (:file-name file) stamp record-id)
    (do (future
          (swap! update-command-history conj entry)
          (dorun (pmap
                   #(try
                     (update % (:file-name file) stamp record-id)
                     (catch Throwable t
                       (timbre/error t)))
                   @active-nodes))
          (db/update-file file-id)))
    (Thread/sleep param/wait-time-after-post)))

(defn create-thread-list
  [n tag]
  (let [file-list (if (and tag (pos? (count tag)))
                    (db/get-files-with-tag tag)
                    (db/get-all-files))

        file-list (remove #(or
                            ;(not (= (:application %) "thread")) ; TODO
                            (some #{(:file-name %)} param/known-corrupt-files)
                            (zero? (:num-records %)))
                          file-list)
        file-list (if n (take n file-list) file-list)
        file-list (map #(-> %
                            (assoc :time-updated (try (long (/ (clj-time.coerce/to-long (:time-updated %)) 1000)) (catch Throwable _ nil)))
                            (assoc :tags (into [] (map :tag-string (db/get-tags-for-file (:id %)))))
                            (assoc :thread-title (file-name-to-thread-title (:file-name %)))
                            (dissoc :dirty :deleted :time-first-post :application :size :time-created :suggested-tags :id :num-deleted-records))
                       file-list)]
    (reverse (sort-by :time-updated file-list))))

(defn create-recommended-thread-list
  [n]
  (try
    (let [file-list (shuffle (db/get-files-with-tag "おすすめ"))
          file-list (remove #(or
                              ;(not (= (:application %) "thread")) ; TODO
                              (some #{(:file-name %)} param/known-corrupt-files)
                              (nil? (:num-records %))
                              (zero? (:num-records %)))
                            file-list)
          file-list (if n (take n file-list) file-list)

          ;file-list (concat file-list [(db/get-file-by-thread-title "質問スレッド")])
          ;file-list (concat file-list [(db/get-file-by-thread-title "【公開ゲートウェイ】ゆぐちゃんねる")])
          ;_ (timbre/debug (pr-str file-list))
          file-list (map #(-> %
                              (assoc :time-updated (try (long (/ (clj-time.coerce/to-long (:time-updated %)) 1000)) (catch Throwable _ nil)))
                              (assoc :tags (into [] (map :tag-string (db/get-tags-for-file (:id %)))))
                              (assoc :thread-title (file-name-to-thread-title (:file-name %)))
                              (dissoc :dirty :deleted :time-first-post :application :size :time-created :suggested-tags :id :num-deleted-records))
                         file-list)]
      file-list)
    (catch Throwable t
      (clojure.stacktrace/print-stack-trace t))))

(defn create-related-thread-list
  [thread-title n]
  (try
    (let [file-id (db/get-file-id-by-thread-title thread-title)
          tags (map :tag-string (db/get-tags-for-file file-id))
          tags (clojure.set/difference (into #{} tags) #{"きれいな新月"})
          tags (clojure.set/union tags #{"おすすめ"})
          file-ids (map :id (apply concat (map db/get-files-with-tag tags)))
          file-ids (shuffle (into () (clojure.set/difference (into #{} file-ids) #{file-id})))
          file-list (map db/get-file-by-id file-ids)

          file-list (remove #(or
                              ;(not (= (:application %) "thread")) ; TODO
                              (some #{(:file-name %)} param/known-corrupt-files)
                              (nil? (:num-records %))
                              (zero? (:num-records %)))
                            file-list)
          file-list (if n (take n file-list) file-list)
          file-list (map #(-> %
                              (assoc :time-updated (try (long (/ (clj-time.coerce/to-long (:time-updated %)) 1000)) (catch Throwable _ nil)))
                              (assoc :tags (into [] (map :tag-string (db/get-tags-for-file (:id %)))))
                              (assoc :thread-title (file-name-to-thread-title (:file-name %)))
                              (dissoc :dirty :deleted :time-first-post :application :size :time-created :suggested-tags :id :num-deleted-records))
                         file-list)]
      file-list)
    (catch Throwable t
      (clojure.stacktrace/print-stack-trace t))))

(defn expand-record-short-ids
  [file-id record-short-ids]
  ;(timbre/info "expand-record-short-ids:" file-id (pr-str record-short-ids))
  (let [anchors (distinct (concat
                    (apply concat (map (fn [destnation]
                                         (db/get-anchors file-id destnation))
                                       record-short-ids))
                    (apply concat (map (fn [source]
                                         (db/get-anchors-for-source file-id source))
                                       record-short-ids))))
        new-record-short-ids (into #{} (concat
                                         (map :source anchors)
                                         (map :destination anchors)))]
    (if (= record-short-ids new-record-short-ids)
      record-short-ids
      (expand-record-short-ids file-id new-record-short-ids))))

(defn create-popup-cache
  [file-id record-short-ids]
  (apply merge (remove nil? (map
                              #(try
                                {%
                                 (process-record-body
                                   (db/get-record-in-file-by-short-id file-id %))}
                                (catch Throwable _ nil))
                              record-short-ids))))

(defn process-api-thread-command*
  [thread-title page-num page-size record-short-id download]
  (let [ file-id (db/get-file-id-by-thread-title thread-title)
        file (db/get-file-by-id file-id)
        _ (when (and file download)
            (download-file (:file-name file))
            (db/remove-duplicate-records-in-file (:id file)))
        results (map
                  process-record-body
                  (if (and record-short-id (pos? (count record-short-id)))
                    (db/get-records-in-file-by-short-id file-id record-short-id)
                    (db/get-records-on-page file-id page-size page-num)))
        ;_ (timbre/debug (str (count results)))
        record-short-ids (map :record-short-id results)
        record-short-ids (expand-record-short-ids file-id (into #{} record-short-ids))
        anchors (into [] (distinct (concat
                                     (apply concat (map (fn [destnation]
                                                          (db/get-anchors file-id destnation))
                                                        record-short-ids))
                                     (apply concat (map (fn [source]
                                                          (db/get-anchors-for-source file-id source))
                                                        record-short-ids)))))
        tags (into [] (map :tag-string (db/get-tags-for-file file-id)))
        suggested-tags (if (:suggested-tags file)
                         (into [] (clojure.string/split (:suggested-tags file) #" +"))
                         [])]
    ;(timbre/info "anchors:"(pr-str anchors))
    {:num-posts (:num-records file)
     :posts     results
     :anchors   anchors
     :popup-cache (create-popup-cache file-id record-short-ids)
     :tags      tags
     :suggested-tags suggested-tags
     :ads (if (and page-size (not (= page-size "")))
            (into [] (map #(try (param/ad-code-for-thread thread-title tags % false) (catch Throwable t nil)) (range (inc page-size))))
            [])
     :mobile-ads (if (and page-size (not (= page-size "")))
            (into [] (map #(try (param/ad-code-for-thread thread-title tags % true) (catch Throwable t nil)) (range (inc page-size))))
            [])
     :related-threads (create-related-thread-list thread-title 5)}))

(defn process-api-thread-command
  [thread-title page-num page-size record-short-id download]
  {:status 200
   :headers {"Content-Type" "application/json; charset=utf-8"}
   :body (cheshire.core/generate-string
           (process-api-thread-command* thread-title page-num page-size record-short-id download))})

(def api-thread-response-cache (atom {}))
(def api-threads-cache (atom nil))
(def api-threads-response-cache (atom nil))
(def api-threads-100-response-cache (atom nil))
(def api-new-posts-rss-response-cache (atom nil))
(def dat-file-response-cache (atom {}))

(defn create-dat-file-response
  [thread-number]
  (let [file (db/get-file-by-thread-number thread-number)
        results (db/get-all-records-in-file-without-bodies (:id file))
        anchor-map (apply merge
                          (remove
                            nil?
                            (map
                              #(try
                                {(str "&gt;&gt;" (second (re-find #"^(.{8})" (:record-id %1)))) (str "&gt;&gt;" %2)}
                                (catch Throwable _
                                  nil))
                              results
                              (range 1 (inc (count results))))))
        thread-title (file-name-to-thread-title (:file-name file))
        posts-as-strings (doall (remove
                                  nil?
                                  (map
                                    #(try
                                      (str
                                        (cond
                                          (nil? (:dat-file-line %1))
                                          (str "?<>?<>????/?/?(?) ??:??:?? ID:" (:record-short-id %1) "<>")

                                          (not (or (re-find #"&gt;&gt;" (:dat-file-line %1))
                                                   (re-find #"\[\[" (:dat-file-line %1))))
                                          (:dat-file-line %1)

                                          :else ; This is costly.
                                          (-> (:dat-file-line %1)
                                              (clojure.string/replace
                                                #"&gt;&gt;[a-f0-9]{8}"
                                                (fn [s] (get anchor-map s s)))
                                              (clojure.string/replace
                                                #"\[\[[^\]]+\]\]"
                                                bracket-link-to-2ch-style-link)))
                                        (if (:suffix %1)
                                          (str
                                            (if-not (re-find #"<>$" (:dat-file-line %1))
                                              "<br>")
                                            (get-server-url-base)
                                            "/thread"
                                            "/" (java.net.URLEncoder/encode thread-title "UTF-8")
                                            "/" (:record-id %1) "." (:suffix %1)))
                                        "<>"
                                        (if (= %2 1)
                                          (str (org.apache.commons.lang3.StringEscapeUtils/escapeHtml4 thread-title)))
                                        "\n")
                                      (catch Throwable t
                                        (timbre/debug (str t))
                                        (str (:dat-file-line %1) "<>\n")))
                                    results
                                    (range 1 (inc (count results))))))
        response {:status 200
                  :headers {"Content-Type" "text/plain; charset=windows-31j"}
                  :body (apply str posts-as-strings)}]
    (swap! dat-file-response-cache assoc thread-number
           {:thread-number thread-number
            :num-records (:num-records file)
            :time-updated (:time-updated file)
            :response response})
    response))

(defn update-dat-file-response-cache
  [thread-number request]
  (timbre/debug "update-dat-file-response-cache:" request)
  (let [file (db/get-file-by-thread-number thread-number)
        _ (if (nil? file) (throw (Exception.)))
        cache-entry (get @dat-file-response-cache thread-number nil)
        response (if (and cache-entry
                           (= (:num-records cache-entry) (:num-records file))
                           (= (:time-updated cache-entry) (:time-updated file)))
                    (:response cache-entry)
                    (create-dat-file-response thread-number))
        headers (if request (:headers request))
        range (if headers (get headers "range"))
        range-start (if (and
                          range
                          (re-find #"^bytes=([0-9]+)-$" range))
                      (Long/parseLong (second (re-find #"^bytes=([0-9]+)-$" range))))
        body-in-bytes (if range-start (.getBytes (:body response) "windows-31j"))
        response (cond
                   (and range-start (= range-start (count body-in-bytes)))
                   {:status 304
                    :headers {"Content-Type" "text/plain; charset=windows-31j"}
                    :body ""}

                   (and range-start (< range-start (count body-in-bytes)))
                   (let [body-in-bytes (byte-array (drop range-start body-in-bytes))
                         body (String. body-in-bytes "windows-31j")]
                     {:status 206
                      :headers {"Content-Type" "text/plain; charset=windows-31j"
                                "Accept-Ranges" "bytes"
                                "Content-Range" (str "bytes " range-start "-" (+ range-start (dec (count body-in-bytes))) "/" (+ range-start (count body-in-bytes)))}
                      :body body})

                   range-start
                   {:status 416 ;RANGE_NOT_SATISFIABLE
                    :headers {"Content-Type" "text/plain; charset=windows-31j"}
                    :body ""}

                   :else
                   response)]
    (timbre/debug "update-dat-file-response-cache:" (pr-str response))
    response
    ))

(defn update-api-thread-cache
  [thread-title page-num page-size]
  ;(timbre/debug "update-api-thread-cache:" thread-title page-num page-size)
  (let [page-num (cond
                   (or (= page-num "") (nil? page-num)) 0
                   (string? page-num) (Integer/parseInt page-num)
                   :else page-num)
        page-size (if (string? page-size) (Integer/parseInt page-size) page-size)
        cache-entry (get @api-thread-response-cache [thread-title page-num page-size] nil)
        file (db/get-file (thread-title-to-file-name thread-title))
        tags (into #{} (map :tag-string (db/get-tags-for-file (:id file))))
        use-cache? (and
                     cache-entry
                     (= (:num-records file) (:num-records cache-entry))
                     (= (:time-updated file) (:time-updated cache-entry))
                     (= tags (:tags cache-entry)))
        response (if use-cache?
                   (:response cache-entry)
                   (process-api-thread-command thread-title page-num page-size nil false))]
    ;(timbre/debug "use-cache?" (pr-str use-cache?))
    (if-not use-cache?
      (swap! api-thread-response-cache
             assoc
             [thread-title page-num page-size]
             {:response response
              :num-records (:num-records file)
              :time-updated (:time-updated file)
              :tags tags}))
    response))

(defn update-thread-cache-for-all-files
  ([]
   (update-thread-cache-for-all-files 0))
  ([wait-time]
   (try
     (dorun
       (map
         (fn [file]
           (when (and
                   (:num-records file)
                   (pos? (:num-records file))
                   (not (some #{(:file-name file)} param/known-corrupt-files)))
             (update-dat-file-response-cache
               (long (/ (clj-time.coerce/to-long (:time-first-post file)) 1000))
               nil)
             (dorun (map
                      (fn [page-num]
                        (update-api-thread-cache (file-name-to-thread-title (:file-name file)) page-num param/page-size)
                        (Thread/sleep wait-time))
                      (range
                        (quot
                          (+ (:num-records file) (dec param/page-size))
                          param/page-size))))))
         (db/get-all-files)))
     (catch Throwable t
       ;(clojure.stacktrace/print-stack-trace t)
       (timbre/error "update-api-thread-cache-for-all-files:" t)))))

(defn create-new-posts-rss-response
  []
  {:status 200
   :headers {"Content-Type" "application/json; charset=utf-8"}
   :body (cheshire.core/generate-string
           {:threads
            (into []
                  (doall (map
                           (fn [threads]
                             (let [posts (into [] (apply concat (map :posts threads)))
                                   record-short-ids (map :record-short-id posts)]
                               {:thread-title  (file-name-to-thread-title (:file-name (db/get-file-by-id (:file-id (first threads)))))
                                :posts posts
                                :anchors (into [] (distinct (apply concat (map :anchors threads))))
                                :popup-cache (create-popup-cache (:file-id (first threads)) record-short-ids)}))
                           (partition-by
                             :file-id
                             (map (fn [record]
                                    (let []
                                      {:file-id (:file-id record)
                                       :posts [(process-record-body record)]
                                       :anchors (into [] (db/get-anchors (:file-id record)  (:record-short-id record)))}))
                                  (db/get-recent-records 100))))))})})

(defn start-api-cache-manager
  []
  (if param/enable-api-cache-manager
    (do
      (future
        (timbre/info "API Cache Manager started.")
        (while true
          (try
            (reset! api-threads-cache (doall (create-thread-list nil nil)))
            (reset! api-threads-100-response-cache
                    {:status 200
                     :headers {"Content-Type" "application/json; charset=utf-8"}
                     :body (cheshire.core/generate-string (take 100 @api-threads-cache))})
            (reset! api-threads-response-cache
                    {:status 200
                     :headers {"Content-Type" "application/json; charset=utf-8"}
                     :body (cheshire.core/generate-string @api-threads-cache)})
            (reset! api-new-posts-rss-response-cache (create-new-posts-rss-response))
            (catch Throwable t
              (timbre/error "API Cache Manager:" t)))
          (Thread/sleep 500)))))

  (if param/enable-thread-api-cache-manager
    (do
      (future
        (timbre/info "Thread API Cache Manager started.")
        (try
          (update-thread-cache-for-all-files)
          (catch Throwable t
            (timbre/error "API Cache Manager:" t)))
        (timbre/info "Thread API Cache Manager: Initial caching completed.")
        (while true
          (try
            (update-thread-cache-for-all-files 100)
            (catch Throwable t
              (timbre/error "Thread API Cache Manager:" t))))))))

(defn create-2ch-subject-txt
  [sorted-files]
  (->> sorted-files
       (remove #(or
                 ;(not (= (:application "thread") %))
                 (some #{(:file-name %)} param/known-corrupt-files)
                 (zero? (:num-records %))))
       (map #(try
              (str
                (long (/ (clj-time.coerce/to-long (:time-first-post %)) 1000))
                ".dat<>"
                (org.apache.commons.lang3.StringEscapeUtils/escapeHtml4
                  (unhexify (second (re-find #"^thread_(.*)$" (:file-name %)))))
                " (" (:num-records %) ")\n")
              (catch Throwable _ "")))
       (apply str)))

(defroutes shingetsu-routes
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ; Shingetsu Protocol Commands ;
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

           (GET (str param/server-path "/ping") request
             ;(timbre/info "/ping" (get-remote-address request))
             (->
               (ok
                 (str
                   "PONG\n"
                   (or (get-in request [:headers "x-forwarded-for"]) (:remote-addr request))))
               (content-type "text/plain; charset=UTF-8")))

           (GET (str param/server-path "/node") request
             ;(timbre/info "/node" (get-remote-address request))
             (->
               (ok (try (rand-nth (into [] @search-nodes)) (catch Throwable _ "")))
               (content-type "text/plain; charset=UTF-8")))

           (GET (str param/server-path "/join/:node-name")
                {:keys [headers params body server-name] :as request}
             (let [{node-name :node-name} params
                   node-name (clojure.string/replace node-name #"\+" "/")
                   remote-addr (get-remote-address request)
                   node-name (if (re-find #"^:" node-name)
                               (str remote-addr node-name)
                               node-name)]
               (when (and (valid-node-name? node-name)
                          (not (= node-name @server-node-name))
                          (ping node-name))
                 (when (and (not (some #{node-name} @active-nodes))
                            (>= (count @active-nodes) max-num-active-nodes))
                   (bye (first (shuffle @active-nodes))))
                 (swap! active-nodes conj node-name)
                 (->
                   (ok "WELCOME")
                   (content-type "text/plain; charset=UTF-8")))))

           (GET (str param/server-path "/bye/:node-name")
                {:keys [headers params body server-name] :as request}
             (let [{node-name :node-name} params
                   node-name (clojure.string/replace node-name #"\+" "/")]
               (timbre/info "/bye" (get-remote-address request) node-name)
               (when (and (valid-node-name? node-name)
                          (not (= node-name @server-node-name))
                          (some #{node-name} @active-nodes))
                 (swap! active-nodes #(clojure.set/difference % #{node-name}))
                 (->
                   (ok "BYEBYE")
                   (content-type "text/plain; charset=UTF-8")))))

           (GET (str param/server-path "/update/:file-name/:stamp/:record-id/:node-name")
                {:keys [headers params body server-name] :as request}
             (try
               (let [{node-name :node-name
                      file-name :file-name
                      stamp     :stamp
                      record-id :record-id} params
                     node-name (clojure.string/replace node-name #"\+" "/")
                     remote-addr (get-remote-address request)
                     node-name (if (re-find #"^:" node-name)
                                 (str remote-addr node-name)
                                 node-name)
                     stamp (Long/parseLong stamp)
                     entry {:file-name file-name :stamp stamp :record-id record-id}
                     file (db/get-file file-name)]
                 (timbre/info "/update" remote-addr file-name (file-name-to-thread-title file-name) stamp record-id node-name)
                 ;(if (and
                 ;      (not (db/file-deleted? file-id))
                 ;      (not (db/record-exists file-id stamp record-id)))
                 (db/process-update-command file-name stamp record-id)
                 (cond
                   (and file (:deleted file) (not (= node-name @server-node-name)) (not (some #{entry} @update-command-history)))
                   (try
                     (dorun (pmap
                              #(try
                                (update % file-name stamp record-id node-name)
                                (catch Throwable t
                                  (timbre/error t)))
                              (clojure.set/difference @active-nodes #{node-name})))
                     (swap! update-command-history conj entry)
                     (catch Throwable t
                       (timbre/error t)))

                   (and (not (= node-name @server-node-name)) (not (some #{entry} @update-command-history)))
                   (try
                     (Thread/sleep param/wait-time-for-update-command)
                     (download-file-from-node node-name file-name (str stamp))
                     (if-not (db/get-record-without-body (db/get-file-id file-name) stamp record-id)
                       (timbre/info "Record not found for /update:" file-name (file-name-to-thread-title file-name) stamp record-id node-name))
                     (when (db/get-record-without-body (db/get-file-id file-name) stamp record-id)
                       (dorun (pmap
                                #(try
                                  (update % file-name stamp record-id)
                                  (catch Throwable t
                                    (timbre/error t)))
                                (clojure.set/difference @active-nodes #{node-name})))
                       (swap! update-command-history conj entry))
                     (catch Throwable t
                       (timbre/error t))))
                 (->
                   (ok "OK")
                   (content-type "text/plain; charset=UTF-8")))
               (catch Throwable t
                 (timbre/error t)
                 nil)))

           (GET (str param/server-path "/have/:file-name")
                {:keys [headers params body server-name] :as request}
             ;(timbre/info "/have" (get-remote-address request) (:file-name params))
             (let [{file-name :file-name} params
                   file (db/get-file file-name)]
               (->
                 (ok (if (and file (not (:deleted file)) (pos? (:num-records file))) "YES" "NO"))
                 (content-type "text/plain; charset=UTF-8"))))

           (GET (str param/server-path "/head/:file-name/:range")
                {:keys [headers params body server-name] :as request}
             ;(timbre/info "/head" (get-remote-address request) (:file-name params) (:range params))
             (process-get-command request true))

           (GET (str param/server-path "/get/:file-name/:range")
                {:keys [headers params body server-name] :as request}
             (timbre/info "/get" (get-remote-address request) (:file-name params) (:range params))
             (process-get-command request false))

           (GET (str param/server-path "/get/:file-name/:range/:record-id")
                {:keys [headers params body server-name] :as request}
             (timbre/info "/get" (get-remote-address request) (:file-name params) (:range params) (:record-id params))
             (process-get-command request false))

           (GET (str param/server-path "/recent/:range")
                {:keys [headers params body server-name] :as request}
             (timbre/info "/recent" (get-remote-address request) (:range params))
             (let [{:keys [range]} params]
               (if (valid-range? range)
                 (let [{range :range} params
                       match (re-find #"^([0-9]*)-([0-9]*)$" range)
                       start (try (Long/parseLong (if match (nth match 1) (nth (re-find #"^([0-9]*)$" range) 1))) (catch Throwable t nil))
                       end (try (Long/parseLong (if match (nth match 2) (nth (re-find #"^([0-9]*)$" range) 1))) (catch Throwable t nil))
                       commands (db/get-update-commands-with-range start end)
                       commands (map #(first (second %)) (group-by :file-name commands))]
                   (->
                     (ok (apply str (map
                                      #(str (:stamp %) "<>" (:record-id %) "<>" (:file-name %) "\n")
                                      commands)))
                     (content-type "text/plain; charset=UTF-8"))))))



           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ; Extended Shingetsu Protocol Commands ;
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

           (GET (str param/server-path "/active-nodes") request
             (timbre/info "/active-nodes" (get-remote-address request))
             (->
               (ok (apply str (sort (map #(str % "\n") @active-nodes))))
               (content-type "text/plain; charset=UTF-8")))

           (GET (str param/server-path "/search-nodes") request
             (timbre/info "/search-nodes" (get-remote-address request))
             (->
               (ok (apply str (sort (map #(str % "\n") @search-nodes))))
               (content-type "text/plain; charset=UTF-8")))

           (GET (str param/server-path "/known-nodes") request
             (timbre/info "/known-nodes" (get-remote-address request))
             (->
               (ok (apply str (sort (map #(str % "\n") (map :node-name (db/get-all-nodes))))))
               (content-type "text/plain; charset=UTF-8")))

           (GET (str param/server-path "/files") request
             (timbre/info "/files" (get-remote-address request))
             (->
               (ok (apply str (map #(str % "\n")
                                         (map :file-name
                                              (remove #(or
                                                        (some #{(:file-name %)} param/known-corrupt-files)
                                                        (zero? (:num-records %)))
                                                      (db/get-all-files))
                                              ))))
               (content-type "text/plain; charset=UTF-8")))



           ;;;;;;;;;;;
           ; Gateway ;
           ;;;;;;;;;;;

           (POST "/api/thread"
                 request
             (let [{:keys [thread-title page-num page-size record-short-id download]} (:params request)]
               (timbre/info "/api/thread" (get-remote-address request) thread-title page-num page-size record-short-id download)
               (let [check-cache? (and
                                  (not download)
                                  (not (and record-short-id (pos? (count record-short-id)))))]
                 (if check-cache?
                   (update-api-thread-cache thread-title page-num page-size)
                   (process-api-thread-command thread-title page-num page-size record-short-id download)))))

           (POST "/api/new-posts"
                 request
             (try
               (timbre/info "/api/new-posts" (get-remote-address request))
               (let [{:keys [threads rss]} (:params request)]
                 (cond
                   (and param/enable-api-cache-manager rss)
                   @api-new-posts-rss-response-cache

                   rss
                   (create-new-posts-rss-response)

                   :else
                   {:body
                    {:threads
                     (into []
                           (remove nil?
                                   (map (fn [thread]
                                          (let [file-id (db/get-file-id-by-thread-title (:thread-title thread))
                                                posts (map process-record-body
                                                           (db/get-new-records-in-file file-id (:time-last-accessed thread)))
                                                ;_ (timbre/debug thread (count posts))
                                                record-short-ids (map :record-short-id posts)
                                                anchors (distinct (into [] (apply concat (map (fn [destnation]
                                                                                                (db/get-anchors file-id destnation))
                                                                                              record-short-ids))))]
                                            (if (zero? (count posts))
                                              nil
                                              {:thread-title (:thread-title thread)
                                               :posts posts
                                               :anchors anchors
                                               :popup-cache (create-popup-cache file-id record-short-ids)
                                               })))
                                        threads)))}}))
               (catch Throwable t
                 ;(clojure.stacktrace/print-stack-trace t)
                 (timbre/error "/api/new-posts:" t))))

           (POST "/api/new-post-notification"
                 request
             ;(timbre/info "/api/new-post-notification" (get-remote-address request))
             (let [{:keys [threads]} (:params request)
                   result (remove zero?
                                   (map (fn [thread]
                                           (let [file-id (db/get-file-id-by-thread-title (:thread-title thread))]
                                             (db/count-new-records-in-file file-id (:time-last-accessed thread))))
                                         threads))]
               {:body {:result (if (pos? (count result)) true false)}}))

           (GET "/api/threads"
                {:keys [headers params body server-name] :as request}
             (let [{:keys [n tag]} params
                   n (if (zero? (count n)) nil (Integer/parseInt n))
                   tag (if (zero? (count tag)) nil tag)
                   _ (timbre/info "/api/threads" (get-remote-address request) n tag)]
               (if (or (not param/enable-api-cache-manager) tag)
                 (create-thread-list n tag)
                 (do
                   ;(timbre/debug "@api-threads-cache")
                   (case n
                     nil @api-threads-response-cache
                     100 @api-threads-100-response-cache
                     (take n @api-threads-cache))))))

           (GET "/api/recommended-threads"
                {:keys [headers params body server-name] :as request}
             (let [{:keys [n]} params
                   n (if (zero? (count n)) nil (Integer/parseInt n))
                   _ (timbre/info "/api/recommended-threads" (get-remote-address request) n)]
               (create-recommended-thread-list n)))

           ; Not used
           (GET "/api/related-threads"
                {:keys [headers params body server-name] :as request}
             (let [{:keys [thread-title n]} params
                   n (if (zero? (count n)) nil (Integer/parseInt n))
                   _ (timbre/info "/api/related-threads" (get-remote-address request) n)]
               (create-related-thread-list thread-title n)))

           (POST "/api/images-in-thread"
                 request
             (let [{:keys [thread-title]} (:params request)
                   _ (timbre/info "/api/images-in-thread" (get-remote-address request) thread-title)
                   file (db/get-file (thread-title-to-file-name thread-title))
                   images (db/get-all-images-in-thread-with-record-ids-and-suffixes-only (:id file))]
               {:body {:images images}}))

           (POST "/api/post"
                 request
             (try
               (let [{:keys [thread-title name mail password body attachment g-recaptcha-response]} (:params request)
                     _ (timbre/debug "/api/post" (get-remote-address request) thread-title)
                     remote-address (get-remote-address request)
                     recaptcha-result (if-not param/enable-recaptcha
                                        true
                                        (:success (:body
                                                    (clj-http.client/post "https://www.google.com/recaptcha/api/siteverify"
                                                                          {:as :json
                                                                           :form-params
                                                                               {:secret param/recaptcha-secret-key
                                                                                :response g-recaptcha-response
                                                                                :remoteip remote-address}}))))]
                 (if-not recaptcha-result
                   (throw (ex-info "reCAPTCHAの認証に失敗しました。\n「書き込む」ボタンの下にあるチェックボックスをクリックして、ロボットでないことを証明してください。" {})))
                 (process-post thread-title name mail password body attachment remote-address)
                 (ok "OK"))
               (catch clojure.lang.ExceptionInfo e
                 (timbre/error e)
                 {:status 400
                  :headers {"Content-Type" "text/plain; charset=utf-8"}
                  :body (.getMessage e)})
               (catch Throwable t
                 (timbre/error t)
                 {:status 500
                  :headers {"Content-Type" "text/plain; charset=utf-8"}
                  :body (str "内部エラーが発生しました。\n" t)})))

           (POST "/api/update-thread-tags"
                 request
             (if (re-find param/admin-remote-address (:remote-addr request))
               (try
                 (let [{:keys [thread-title tags]} (:params request)
                       _ (timbre/debug "/api/update-thread-tags" (get-remote-address request) thread-title (pr-str tags))
                       file (db/get-file (thread-title-to-file-name thread-title))]
                   (db/update-tags-for-file (:id file) tags)
                   {:body {:success true}})
                 (catch Throwable t
                   (timbre/error t)
                   {:body {:success false}}))))

           (GET "/api/status" request
             ;(timbre/info "/api/status" (get-remote-address request))
             (let [cache-size (reduce + (map :size (db/get-all-files)))]
               {:body
                {:status
                 {:num-files (db/count-all-files)
                  :num-records (db/count-all-records)
                  :num-deleted-records (db/count-all-deleted-records)
                  :cache-size cache-size
                  :service-name param/service-name
                  :active-nodes (into [] (sort @active-nodes))
                  :search-nodes (into [] (sort @search-nodes))

                  :admin (if (re-find param/admin-remote-address (:remote-addr request)) true false)
                  :server-node-name @server-node-name
                  :server-url-base (get-server-url-base)
                  :enable-recaptcha param/enable-recaptcha
                  :recaptcha-site-key param/recaptcha-site-key
                  :enable-google-analytics param/enable-google-analytics
                  :google-analytics-tracking-id param/google-analytics-tracking-id
                  :thumbnail-height param/thumbnail-height
                  :allow-tripcode (if param/tripcode-password true false)
                  :admin-name param/admin-name
                  :admin-website param/admin-website
                  :admin-email param/admin-email
                  }}}))

           (POST "/api/generate-tripcode"
                 request
             (try
               (let [{:keys [key]} (:params request)
                     _ (timbre/debug "/api/generate-tripcode" (get-remote-address request) key)]
                 {:body
                  {:success true
                   :tripcode (generate-tripcode key)}})
               (catch clojure.lang.ExceptionInfo e
                 (timbre/error e)
                 {:status 400
                  :headers {"Content-Type" "text/plain; charset=utf-8"}
                  :body (.getMessage e)})
               (catch Throwable t
                 (timbre/error t)
                 {:status 500
                  :headers {"Content-Type" "text/plain; charset=utf-8"}
                  :body (str "内部エラーが発生しました。\n" t)})))

           (GET "/api/gist/:id"
                request
             (try
               (let [{:keys [id]} (:params request)
                     _ (timbre/debug "/api/gist/:id" (get-remote-address request) id)]
                 (if (re-find #"^[a-f0-9]+$" id)
                   {:status 200
                    :headers {"Content-Type" "text/html; charset=utf-8"}
                    :body (str
                            "<html>"
                            "<base target=\"_blank\" />"
                            "<style>"
                            "body { margin: 0; overflow: hidden; padding: 0; }"
                            "</style>"
                            "<body>"
                            "<script type=\"text/javascript\" src=\"https://gist.github.com/" id ".js\"></script>"
                            "</body>"
                            "</html>"
                            )}))
               (catch clojure.lang.ExceptionInfo e
                 (timbre/error e)
                 {:status 400
                  :headers {"Content-Type" "text/plain; charset=utf-8"}
                  :body (.getMessage e)})
               (catch Throwable t
                 (timbre/error t)
                 {:status 500
                  :headers {"Content-Type" "text/plain; charset=utf-8"}
                  :body (str "内部エラーが発生しました。\n" t)})))

           (GET "/api/i-mobile-ad"
                request
             (try
               (let [{:keys [pid asid width height]} (:params request)
                     _ (timbre/debug "/api/i-mobile-ad" (get-remote-address request))]
                 {:status 200
                  :headers {"Content-Type" "text/html; charset=utf-8"}
                  :body (str
                          "<html>"
                          "<head>"
                          "</head>"
                          "<style>"
                          "body { margin: 0; overflow: hidden; padding: 0; }"
                          "</style>"
                          "<body>"
                          "<script type=\"text/javascript\">\n\t\timobile_pid = \"" pid "\"; \n\t\timobile_asid = \"" asid "\"; \n\t\timobile_width = " width "; \n\t\timobile_height = " height ";\n\t</script>\n\t<script type=\"text/javascript\" src=\"https://spdeliver.i-mobile.co.jp/script/ads.js?20101001\"></script>\n\t"
                          "</body>"
                          "</html>"
                          )})
               (catch clojure.lang.ExceptionInfo e
                 (timbre/error e)
                 {:status 400
                  :headers {"Content-Type" "text/plain; charset=utf-8"}
                  :body (.getMessage e)})
               (catch Throwable t
                 (timbre/error t)
                 {:status 500
                  :headers {"Content-Type" "text/plain; charset=utf-8"}
                  :body (str "内部エラーが発生しました。\n" t)})))

           (GET "/api/i-mobile-ad-mobile"
                request
             (try
               (let [{:keys [pid asid type]} (:params request)
                     _ (timbre/debug "/api/i-mobile-ad-mobile" (get-remote-address request))]
                 {:status 200
                  :headers {"Content-Type" "text/html; charset=utf-8"}
                  :body (str
                          "<html>"
                          "<head>"
                          "</head>"
                          "<style>"
                          "body { margin: 0; overflow: hidden; padding: 0; }"
                          "</style>"
                          "<body>"
                          "<script type=\"text/javascript\">\n\t\timobile_tag_ver = \"0.3\"; \n\t\timobile_pid = \"" pid "\"; \n\t\timobile_asid = \"" asid "\"; \n\t\timobile_type = \"" type "\";\n\t</script>\n\t<script type=\"text/javascript\" src=\"https://spad.i-mobile.co.jp/script/adssp.js?20110215\"></script>\n\t"
                          "</body>"
                          "</html>"
                          )})
               (catch clojure.lang.ExceptionInfo e
                 (timbre/error e)
                 {:status 400
                  :headers {"Content-Type" "text/plain; charset=utf-8"}
                  :body (.getMessage e)})
               (catch Throwable t
                 (timbre/error t)
                 {:status 500
                  :headers {"Content-Type" "text/plain; charset=utf-8"}
                  :body (str "内部エラーが発生しました。\n" t)})))

           (GET "/api/nend-ad"
                request
             (try
               (let [{:keys [media site spot site oriented]} (:params request)
                     _ (timbre/debug "/api/i-mobile-ad-mobile" (get-remote-address request))]
                 {:status 200
                  :headers {"Content-Type" "text/html; charset=utf-8"}
                  :body (str
                          "<html>"
                          "<head>"
                          "</head>"
                          "<style>"
                          "body { margin: 0; overflow: hidden; padding: 0; }"
                          "</style>"
                          "<body>"
                          "<script type=\"text/javascript\">\nvar nend_params = {\"media\":" media ",\"site\":" site ",\"spot\":" spot ",\"type\":" type ",\"oriented\":" oriented "};\n</script>\n<script type=\"text/javascript\" src=\"https://js1.nend.net/js/nendAdLoader.js\"></script>"
                          "</body>"
                          "</html>"
                          )})
               (catch clojure.lang.ExceptionInfo e
                 (timbre/error e)
                 {:status 400
                  :headers {"Content-Type" "text/plain; charset=utf-8"}
                  :body (.getMessage e)})
               (catch Throwable t
                 (timbre/error t)
                 {:status 500
                  :headers {"Content-Type" "text/plain; charset=utf-8"}
                  :body (str "内部エラーが発生しました。\n" t)})))


           (GET "/api/ninja-ad/:id"
                request
             (try
               (let [{:keys [id]} (:params request)
                     _ (timbre/debug "/api/ninja-ad/:id" (get-remote-address request) id)]
                 (if (re-find #"^[a-z0-9]+$" id)
                   {:status 200
                    :headers {"Content-Type" "text/html; charset=utf-8"}
                    :body (str
                            "<html>"
                            "<head>"
                            "</head>"
                            "<style>"
                            "body { margin: 0; overflow: hidden; padding: 0; }"
                            "</style>"
                            "<body>"
                            "<script src=\"http://adm.shinobi.jp/s/" id "\"></script>"
                            "</body>"
                            "</html>"
                            )}))
               (catch clojure.lang.ExceptionInfo e
                 (timbre/error e)
                 {:status 400
                  :headers {"Content-Type" "text/plain; charset=utf-8"}
                  :body (.getMessage e)})
               (catch Throwable t
                 (timbre/error t)
                 {:status 500
                  :headers {"Content-Type" "text/plain; charset=utf-8"}
                  :body (str "内部エラーが発生しました。\n" t)})))

           (GET "/api/nicovideo/:id"
                request
             (try
               (let [{:keys [id]} (:params request)
                     _ (timbre/debug "/api/nicovideo/:id" (get-remote-address request) id)]
                 (if (re-find #"^[a-z0-9]+$" id)
                   {:status 200
                    :headers {"Content-Type" "text/html; charset=utf-8"}
                    :body (str
                            "<html>"
                            "<style>"
                            "body { margin: 0; overflow: hidden; padding: 0; }"
                            "</style>"
                            "<body>"
                            "<script type=\"text/javascript\" src=\"http://ext.nicovideo.jp/thumb_watch/" id "\"></script>"
                            "</body>"
                            "</html>"
                            )}))
               (catch clojure.lang.ExceptionInfo e
                 (timbre/error e)
                 {:status 400
                  :headers {"Content-Type" "text/plain; charset=utf-8"}
                  :body (.getMessage e)})
               (catch Throwable t
                 (timbre/error t)
                 {:status 500
                  :headers {"Content-Type" "text/plain; charset=utf-8"}
                  :body (str "内部エラーが発生しました。\n" t)})))

           (GET "/api/twitter"
                request
             (try
               (let [{:keys [url]} (:params request)
                     _ (timbre/debug "/api/twitter" (get-remote-address request) url (pr-str (:body (client/get (str "https://api.twitter.com/1/statuses/oembed.json?url=" url)))))]
                 {:status 200
                  :headers {"Content-Type" "application/json; charset=utf-8"}
                  :body (:body (client/get (str "https://api.twitter.com/1/statuses/oembed.json?url=" url)))}
                 )
               (catch clojure.lang.ExceptionInfo e
                 (timbre/error e)
                 {:status 400
                  :headers {"Content-Type" "text/plain; charset=utf-8"}
                  :body (.getMessage e)})
               (catch Throwable t
                 (timbre/error t)
                 {:status 500
                  :headers {"Content-Type" "text/plain; charset=utf-8"}
                  :body (str "内部エラーが発生しました。\n" t)})))

           (GET "/api/image-proxy"
                request
             (try
               (let [{:keys [url]} (:params request)
                     _ (timbre/debug "/api/image-proxy" (get-remote-address request) url)
                     options  {:as      :byte-array
                               :headers {"Cache-Control" "no-cache"}
                               :socket-timeout 60000
                               :conn-timeout   60000}
                     response (clj-http.client/get url options)
                     headers  (into {} (for [[k v] (:headers response)] [(keyword k) v]))]
                 (if (not (= (:status response) 200))
                   (throw (Exception. (str "status " (:status response)))))
                 (when (or (nil? (:Content-Type headers))
                           (not (re-find #"^image(/|%2F)" (:Content-Type headers)))) ; "%2F" for http://i.minus.com/
                   ; (log :debug "Wrong content type: " (:Content-Type headers))
                   (throw (Exception. (str "Wrong content type: " (:Content-Type headers)))))

                 {:status  200
                  :headers {"Content-Type"  (:Content-Type headers)
                            "Cache-Control" "private"}
                  :body    (ByteArrayInputStream. (:body response))})
               (catch clojure.lang.ExceptionInfo e
                 (timbre/error e)
                 {:status 400
                  :headers {"Content-Type" "text/plain; charset=utf-8"}
                  :body (.getMessage e)})
               (catch Throwable t
                 (timbre/error t)
                 {:status 500
                  :headers {"Content-Type" "text/plain; charset=utf-8"}
                  :body (str "内部エラーが発生しました。\n" t)})))




           (GET "/bbsmenu.html"
                {:keys [headers params body server-name] :as request}
             ;(timbre/info "/2ch/subject.txt" (get-remote-address request))
             (->
               (ok
                 (str
                   "<HTML>\n"
                   "<HEAD>\n"
                   "<META http-equiv=\"Content-Type\" content=\"text/html; charset=Shift_JIS\">\n"
                   "<TITLE>BBS MENU for " param/service-name "</TITLE>\n"
                   "<BASE TARGET=\"cont\">\n"
                   "</HEAD>\n"
                   "<BODY TEXT=\"#CC3300\" BGCOLOR=\"#FFFFFF\" link=\"#0000FF\" alink=\"#ff0000\" vlink=\"#660099\">\n"
                   "<font size=2>\n"
                   "<br><br><B>" param/service-name "</B><br>\n"
                   "<A HREF=" (get-server-url-base) "/2ch/>全てのスレッド</A>\n"
                   (apply str
                          (map (fn [heading items]
                                 (clojure.string/replace
                                   (str
                                     "<br><br><B>" heading "</B><br>\n"
                                     (apply str
                                            (map (fn [item]
                                                   (str "<A HREF=" (get-server-url-base) "/2ch_" (hexify item) "/>" item"</A><br>\n"))
                                                 items)))
                                   #"<br>$"
                                   ""))
                               param/standard-tag-headings
                               param/standard-tags))
                   "</FONT>\n"
                   "</BODY>\n"
                   "</HTML>"))
               (content-type "text/html; charset=Shift_JIS")))

           ; For JaneStyle
           (GET "/2ch/"
                {:keys [headers params body server-name] :as request}
             (->
               (ok
                 (str
                   "<HTML>\n"
                   "<HEAD>\n"
                   "<META http-equiv=\"Content-Type\" content=\"text/html; charset=Shift_JIS\">\n"
                   "<meta http-equiv=\"refresh\" content=\"0; URL='/threads'\" />\n"
                   "<TITLE>" (org.apache.commons.lang3.StringEscapeUtils/escapeHtml4 param/service-name) "</TITLE>\n"
                   "</HEAD>\n"
                   "<BODY>\n"
                   "</BODY>\n"
                   "</HTML>"))
               (content-type "text/html; charset=Shift_JIS")))

           (GET "/:board-name/"
                {:keys [headers params body server-name] :as request}
             (timbre/info "/:board-name/" (get-remote-address request))
             (let [board-name (:board-name params)
                   match (re-find #"^2ch_([A-F0-9]+)$" board-name)
                   decoded-board-name (if match (unhexify (second match)))
                   redirect-url (if match (str "/threads?tag=" (percent-encode decoded-board-name)))]
               (when match
                 (->
                   (ok
                     (str
                       "<HTML>\n"
                       "<HEAD>\n"
                       "<META http-equiv=\"Content-Type\" content=\"text/html; charset=Shift_JIS\">\n"
                       "<meta http-equiv=\"refresh\" content=\"0; URL='" redirect-url "'\" />\n"
                       "<TITLE>" (org.apache.commons.lang3.StringEscapeUtils/escapeHtml4 decoded-board-name) "</TITLE>\n"
                       "</HEAD>\n"
                       "<BODY>\n"
                       "</BODY>\n"
                       "</HTML>"))
                   (content-type "text/html; charset=Shift_JIS"))
                 )))

           (GET "/2ch/subject.txt"
                {:keys [headers params body server-name] :as request}
             ;(timbre/info "/2ch/subject.txt" (get-remote-address request))
             (->
               (ok (create-2ch-subject-txt (db/get-all-files)))
               (content-type "text/plain; charset=windows-31j")))

           (GET "/:board-name/subject.txt"
                {:keys [headers params body server-name] :as request}
             ;(timbre/info "/:board-name/subject.txt" (get-remote-address request))
             (let [board-name (:board-name params)
                   match (re-find #"^2ch_([A-F0-9]+)$" board-name)]
               (when match
                 (->
                   (ok (create-2ch-subject-txt (sort-by :time-updated #(.after %1 %2) (db/get-files-with-tag (unhexify (second match))))))
                   (content-type "text/plain; charset=windows-31j")))))

           (GET "/:board-name/head.txt"
                {:keys [headers params body server-name] :as request}
             (let [board-name (:board-name params)]
               (when (re-find #"^2ch(_[A-F0-9]+)?$" board-name)
                 (->
                   (ok (str
                         "新月 - P2P anonymous BBS<br>\n"
                         "<br>\n"
                         "http://shingetsu.info/<br>\n"
                         "<br>\n"
                         "次の利用規約に同意した方のみ新月ネットワークに参加できます。<br>\n"
                         "<br>\n"
                         "(投稿者の責任または免責)<br>\n"
                         "1. 投稿者は投稿した記事に使用、改変または再配布の条件を記述しなければならない。<br>\n"
                         "   条件は新月の仕組みに矛盾するものであってはならない。<br>\n"
                         "2. 第1項の条件の記述がない場合には、利用者は投稿者が使用、<br>\n"
                         "   改変または再配布を制限しないことに同意したものとみなすことができる。<br>\n"
                         "3. 投稿者は第1項の条件または第2項の同意が正しいことに責任を持つ。<br>\n"
                         "4. 投稿者は法律に定めのない限り、個別の記事で宣言しない限り、<br>\n"
                         "   かつ第3項に反しない限り、記事の内容が正しいこと、役に立つこと、<br>\n"
                         "   または不愉快でないことなどについて保証しない。<br>\n"
                         "<br>\n"
                         "(ノード管理者の責任または免責)<br>\n"
                         "5. ノード管理者は記事または掲示板を自由に編集または削除できる。<br>\n"
                         "6. ノード管理者は法律に定めのない限り、<br>\n"
                         "   ノードを管理・運営することで知った情報についての守秘義務を負わない。<br>\n"
                         "7. ノード管理者は法律に定めのない限り、記事の内容が正しいこと、役に立つこと、<br>\n"
                         "   または不愉快でないことなどについて保証しない。<br>\n"
                         "8. ノード管理者は自分の管理するノードに対して、<br>\n"
                         "   特定のユーザ、特定のノード、全ての利用者または全てのノードが<br>\n"
                         "   一時的または永続的に接続できることを保証しない。<br>"))
                   (content-type "text/plain; charset=windows-31j")))))

           (GET "/2ch/SETTING.TXT"
                {:keys [headers params body server-name] :as request}
             (->
               (ok (str
                     "BBS_TITLE=" param/service-name "\n"
                     "BBS_NONAME_NAME=" param/anonymous-users-handle "\n"))
               (content-type "text/plain; charset=windows-31j")))

           (GET "/:board-name/SETTING.TXT"
                {:keys [headers params body server-name] :as request}
             (let [board-name (:board-name params)
                   match (re-find #"^2ch_([A-F0-9]+)$" board-name)]
               (when match
                 (->
                   (ok (str
                         "BBS_TITLE=" (unhexify (second match)) "@" param/service-name "\n"
                         "BBS_NONAME_NAME=" param/anonymous-users-handle "\n"))
                   (content-type "text/plain; charset=windows-31j")))))

           (GET "/test/read.cgi/:board-name/:thread-number"
                {:keys [headers params body server-name] :as request}
             (let [board-name (:board-name params)]
               (when (re-find #"^2ch(_[A-F0-9]+)?$" board-name)
                 (let [thread-number (:thread-number (:params request))
                       _ (timbre/info "/test/read.cgi/:board-name/:thread-number" thread-number)
                       file (db/get-file-by-thread-number thread-number)
                       _ (if (nil? file) (throw (Exception.)))]
                   (redirect (str "/thread/" (percent-encode (file-name-to-thread-title (:file-name file)))))))))

           (GET "/test/read.cgi/:board-name/:thread-number/"
                {:keys [headers params body server-name] :as request}
             (let [board-name (:board-name params)]
               (when (re-find #"^2ch(_[A-F0-9]+)?$" board-name)
                 (let [thread-number (:thread-number (:params request))
                       _ (timbre/info "/test/read.cgi/:board-name/:thread-number/" thread-number)
                       file (db/get-file-by-thread-number thread-number)
                       _ (if (nil? file) (throw (Exception.)))]
                   (if file
                     (do
                       (timbre/info "/test/read.cgi/:board-name/:thread-number/" thread-number)
                       (redirect (str "/thread/" (percent-encode (file-name-to-thread-title (:file-name file))))))
                     (do
                       (timbre/info "/test/read.cgi/:board-name/:thread-number/" thread-number "Thread not found.")
                       (redirect "/")))))))

           (GET "/test/read.cgi/:board-name/:thread-number/:qualifier"
                {:keys [headers params body server-name] :as request}
             (let [board-name (:board-name params)]
               (when (re-find #"^2ch(_[A-F0-9]+)?$" board-name)
                 (let [{:keys [thread-number qualifier]} (:params request)
                       _ (timbre/info "/test/read.cgi/:board-name/:thread-number/:qualifier" thread-number qualifier)
                       file (db/get-file-by-thread-number thread-number)
                       _ (if (nil? file) (throw (Exception.)))
                       records (db/get-all-records-in-file-without-bodies (:id file))
                       post-numbers-map (apply merge
                                               (remove
                                                 nil?
                                                 (map
                                                   #(try
                                                     {%2 (second (re-find #"^(.{8})" (:record-id %1)))}
                                                     (catch Throwable _
                                                       nil))
                                                   records
                                                   (range 1 (inc (count records))))))
                       _ (timbre/debug (str post-numbers-map))
                       record-short-id (try (get post-numbers-map (Integer/parseInt qualifier)) (catch Throwable _ nil))]
                   (redirect
                     (str
                       "/thread/"
                       (percent-encode (file-name-to-thread-title (:file-name file)))
                       (if record-short-id
                         (str "/" record-short-id))))))))

           (GET "/:board-name/dat/:dat-file-name"
                {:keys [headers params body server-name] :as request}
             (let [board-name (:board-name params)]
               (when (re-find #"^2ch(_[A-F0-9]+)?$" board-name)
                 (let [{:keys [dat-file-name]} (:params request)
                       _ (timbre/info "/:board-name/dat/:dat-file-name" (get-remote-address request) dat-file-name headers)
                       [_ thread-number] (re-find #"^([0-9]+)\.dat$" dat-file-name)
                       thread-number (if thread-number (Long/parseLong thread-number))]
                   (cond
                     (and thread-number (db/get-file-by-thread-number thread-number))
                     (update-dat-file-response-cache thread-number request)

                     (and thread-number (db/get-file-by-thread-number-plus-9 thread-number) (get headers "range"))
                     {:status 416 ;RANGE_NOT_SATISFIABLE
                      :headers {"Content-Type" "text/plain; charset=windows-31j"}
                      :body ""}

                     (and thread-number (db/get-file-by-thread-number-plus-9 thread-number))
                     {:status 200
                      :headers {"Content-Type" "text/plain; charset=windows-31j"}
                      :body (str
                              "新月名無しさん<><>"
                              (dat-timestamp-formatter
                                (clj-time.coerce/from-long
                                  (+
                                    (clj-time.coerce/to-long (clj-time.core/now))
                                    (* 9 60 60 1000))))
                              "<>"
                              "このスレッドは移動しました。新しいアドレスは次の通りです。<br>"
                              (get-server-url-base) "/test/read.cgi/" board-name "/" (- thread-number (* 9 60 60)) "/"
                              "<>このスレッドは移動しました\n")}
                     )))))



           (POST "/test/bbs.cgi"
                 request
             (try
               (let [{:keys [subject bbs key FROM mail password MESSAGE attachment submit]} (:form-params request)
                     _ (timbre/debug "/test/bbs.cgi" (get-remote-address request) bbs key)
                     remote-address (get-remote-address request)
                     file (if (and subject
                                   (pos? (count subject))
                                   (= submit "新規スレッド作成"))
                            (db/add-file (thread-title-to-file-name subject))
                            (db/get-file-by-thread-number key))
                     thread-title (file-name-to-thread-title (:file-name file))
                     results (db/get-all-records-in-file-without-bodies (:id file))
                     anchor-map (apply merge
                                       (remove
                                         nil?
                                         (map
                                           #(try
                                             {(str ">>" %2) (str ">>" (second (re-find #"^(.{8})" (:record-id %1))))}
                                             (catch Throwable _
                                               nil))
                                           results
                                           (range 1 (inc (count results))))))]
                 (process-post
                   thread-title
                   FROM
                   mail
                   nil
                   (-> MESSAGE
                     (clojure.string/replace #">>[0-9]+" (fn [s] (get anchor-map s s))))
                   nil
                   remote-address)
                 (->
                   (ok (str
                         "<html lang=\"ja\">\n"
                         "<head>\n<title>書きこみました。</title>\n"
                         "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=shift_jis\">\n"
                         "<meta content=5;URL=../namazuplus/index.html http-equiv=refresh>\n"
                         "</head>\n<body>書きこみが終わりました。<br><br>\n"
                         "画面を切り替えるまでしばらくお待ち下さい。<br><br>\n"
                         "<br><br><br><br><br>\n"
                         "<center>\n"
                         "</center>\n"
                         "</body>\n"
                         "</html>"))
                   (content-type "text/html; charset=windows-31j")))
               (catch Throwable t
                 (timbre/error (str t))
                 (->
                   (ok (str "<html><head><title>ＥＲＲＯＲ！</title><meta http-equiv=\"Content-Type\" content=\"text/html; charset=shift_jis\"></head>\n"
                            "<body><!-- 2ch_X:error -->\n"
                            "ＥＲＲＯＲ - エラーが発生しました。\n"
                            "<hr>(Ju 2ch Interface)</body></html>"))
                   (content-type "text/html; charset=windows-31j")))))



           (GET "/rss"
                {:keys [headers params body server-name] :as request}
             (timbre/info "/rss" (get-remote-address request))
             (if-not (or (nil? (get headers "accept" nil)) (re-find #"^(.*,)?(application|\*)/(rss\+xml|\*)(,.*)?$" (get headers "accept")))
               (home/home-page "RSSフィード")
               (->
                 (ok
                   (clj-rss.core/channel-xml
                     {:title param/service-name :link (str (get-server-url-base) "/") :description "新月ネットワークの匿名掲示板"}
                     (map
                       (fn [record]
                         (let [record (ju.db.core/get-record-by-id (:id record))
                               body (String. (:body record) "UTF-8")
                               elements (->> (clojure.string/split body #"<>")
                                             (map #(re-find #"^([a-zA-Z0-9_]+):(.*)$" %))
                                             (map #(do {(keyword (nth % 1)) (nth % 2)}))
                                             (apply merge))
                               thread-title (file-name-to-thread-title (:file-name (db/get-file-by-id (:file-id record))))]
                           {:title (str
                                     "["
                                     (org.apache.commons.lang3.StringEscapeUtils/unescapeHtml4 thread-title)
                                     "]"
                                     (if (:body elements)
                                       (str
                                         " "
                                         (-> (org.apache.commons.lang3.StringEscapeUtils/unescapeHtml4 (:body elements))
                                             (clojure.string/replace #"<br>" "")
                                             (clojure.string/replace #"^(.{40}).+$" "$1…")
                                             ))))
                            :link (str (get-server-url-base) "/thread/" (percent-encode thread-title) "/" (:record-short-id record))
                            :pubDate (clj-time.coerce/to-date (clj-time.coerce/from-long (* (:stamp record) 1000)))
                            }))
                       (db/get-recent-records 50))))
                 (content-type "application/rss+xml; charset=UTF-8"))))
              )



(defn remove-spam
  []
  (dorun (map (fn [record]
                 (let [record (ju.db.core/get-record-by-id (:id record))
                       body (String. (:body record) "UTF-8")
                       elements (->> (clojure.string/split body #"<>")
                                     (map #(re-find #"^([a-zA-Z0-9_]+):(.*)$" %))
                                     (map #(do {(keyword (nth % 1)) (nth % 2)}))
                                     (apply merge))
                       file-name (:file-name (ju.db.core/get-file-by-id (:file-id record)))
                       thread-title (and (re-find #"^thread_(.*)$" file-name)
                                         (unhexify (second (re-find #"^thread_(.*)$" file-name))))]
                   (when (and thread-title (is-post-spam? file-name (:stamp record) (:record-id record) elements))
                     (ju.db.core/mark-record-as-deleted (:id record))
                     (taoensso.timbre/info
                       thread-title
                       (:id record)
                       (:stamp record)
                       (:name elements)
                       (:mail elements)
                       (:body elements)))
                   (if (zero? (mod (:id record) 100))
                     (taoensso.timbre/info "Processed" (:id record) "records."))))
               (sort #(< (:id %1) (:id %2)) (ju.db.core/get-all-records-with-ids-only))))
  (ju.db.core/update-all-files))

(defn resurrect-archived-posts
  []
  (doall
    (apply
      concat
      (remove
        nil?
        (pmap
          (fn [thread-title-hash]
            (try
              (let [thread-top (:body (clj-http.client/get (str "http://archive.shingetsu.info/" thread-title-hash "/")))
                    file-names (distinct (re-seq #"[0-9a-f]{8}\.html" thread-top))
                    short-ids (doall (map #(re-find #"^[0-9a-f]{8}" %) file-names))]
                (if-not (empty? file-names)
                  (let [first-post-url (str "http://archive.shingetsu.info/" thread-title-hash "/" (first file-names))
                        first-post (:body (clj-http.client/get first-post-url))
                        thread-title (second (re-find #"<h1><a href=\"./\">([^<]+)</a></h1>" first-post))
                        _
                        (timbre/debug "resurrect-archived-posts:" thread-title)
                        file-name (thread-title-to-file-name thread-title)
                        file (db/get-file file-name)
                        file (cond
                               file file
                               :else (db/add-file file-name))
                        existing-records (db/get-all-records-in-file (:id file))
                        short-ids (clojure.set/difference (into #{} short-ids) (into #{} (map :record-short-id existing-records)))
                        results (doall
                                  (remove
                                    nil?
                                    (map
                                      (fn [short-id]
                                        (let [post-url (str "http://archive.shingetsu.info/" thread-title-hash "/" short-id ".html")
                                              post (:body (clj-http.client/get post-url))

                                              name (second (re-find #"<span class=\"name\">([^>]+)</span>" post))
                                              name (if (= name "Anonymous") nil name)

                                              stamp (nth (re-find #"<span class=\"stamp\" id=\"s([0-9]{10})\">" post) 1 nil)
                                              stamp (if stamp (Long/parseLong stamp) nil)

                                              sign (nth (re-find #"<span class=\"sign\"[^>]+>([^>]+)</span>" post) 1 nil)

                                              match (re-find #"\n    \[([^\]]+)\]\n" post)
                                              mail (if match (str (nth match 1)))

                                              match (re-find #"<a href=\"([a-f0-9]{8})x\.([a-z0-9]+)\">" post)
                                              suffix (if (and match (= short-id (second match))) (nth match 2))

                                              attach-url (str "http://archive.shingetsu.info/" thread-title-hash "/" short-id "x." suffix)
                                              attach (if suffix (String. (clojure.data.codec.base64/encode (:body (clj-http.client/get attach-url {:as :byte-array}))) "ASCII"))

                                              ;suffix (if (and (nil? suffix) (re-find #"<a href=\"([a-f0-9]{8})x\.\">" post)) "txt" suffix)
                                              ;attach (if (and suffix (nil? attach)) (String. (clojure.data.codec.base64/encode (byte-array 0)) "ASCII") attach)

                                              body (second (re-find #"(?s)<dd[^>]+>\n(.*)\n  </dd>" post))
                                              body (clojure.string/replace body #"(?m)^    " "")
                                              body (clojure.string/replace body #"<br />\n" "<br>")
                                              body (clojure.string/replace body #"<a[^>]*>" "")
                                              body (clojure.string/replace body #"</a>" "")
                                              body (if (= body "") nil body)

                                              record-bodies (map #(clojure.string/replace (apply str %) #"^<>" "")
                                                                 (concat
                                                                   (clojure.math.combinatorics/permutations
                                                                     [(if name (str "<>name:" name) "")
                                                                      (if mail (str "<>mail:" mail) "")
                                                                      (if body (str "<>body:" body) "")
                                                                      (if attach (str "<>attach:" attach) "")
                                                                      (if suffix (str "<>suffix:" suffix) "")
                                                                      ])
                                                                   (clojure.math.combinatorics/permutations
                                                                     [(str "<>name:" name)
                                                                      (str "<>mail:" mail)
                                                                      (str "<>body:" body)
                                                                      (if attach (str "<>attach:" attach) "")
                                                                      (if suffix (str "<>suffix:" suffix) "")
                                                                      ])))
                                              record-body (nth (remove #(not (= short-id (apply str (take 8 (ju.util/md5 %)))))
                                                                       record-bodies)
                                                               0 nil)
                                              record-id (md5 record-body)
                                              success (and stamp (= short-id (apply str (take 8 record-id))))
                                              record (if file (db/get-record-in-file-by-short-id (:id file) short-id))
                                              result (cond
                                                       (and success record (= stamp (:stamp record))) :existing
                                                       (and success record) :wrong-stamp
                                                       success :new
                                                       :else :invalid)
                                              elements (if record-body
                                                         (->> (clojure.string/split record-body #"<>")
                                                            (map #(re-find #"^([a-zA-Z0-9_]+):(.*)$" %))
                                                            (map #(do {(keyword (nth % 1)) (nth % 2)}))
                                                            (apply merge)))
                                              ]
                                          (when (= result :new)
                                            (db/add-record
                                              (:id file)
                                              stamp
                                              record-id
                                              (.getBytes record-body "UTF-8")
                                              false
                                              (convert-record-into-dat-file-line
                                                (-> elements
                                                    (assoc :stamp stamp)
                                                    (assoc :record-id record-id)))
                                              (:suffix elements)
                                              "http://archive.shingetsu.info/"
                                              nil)
                                            (if (some #{(:suffix elements)} param/image-suffixes)
                                              (db/create-image (:id file) stamp record-id elements false))
                                            (db/update-file (:id file)))
                                          {:thread-title thread-title
                                           :thread-title-hash thread-title-hash
                                           :short-id short-id
                                           :success success
                                           :result result
                                           ;:record-body record-body
                                           :name name
                                           :sign sign
                                           :mail mail
                                           :suffix suffix
                                           :body body
                                           :stamp stamp
                                           }))
                                      short-ids ; (take 1 short-ids)
                                      )))]
                    results)))
              (catch Throwable t
                (clojure.stacktrace/print-stack-trace t))))
          (distinct (re-seq #"[0-9a-f]{32}" (:body (clj-http.client/get "http://archive.shingetsu.info/")))))))))