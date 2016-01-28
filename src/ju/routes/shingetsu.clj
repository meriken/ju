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
            [clojure.math.numeric-tower :refer [expt]]
            [clojure.data.codec.base64]
            [clj-time.core]
            [clj-time.coerce]
            [clj-time.format]
            [clj-time.predicates])
  (:import (java.net URLEncoder)
           (java.nio.file Files)
           (java.security MessageDigest)))



; (def http-params { :headers {"Accept-Encoding" ""} :socket-timeout (* 5 60 1000) :conn-timeout (* 30 60 1000)})
(def http-params
  {:socket-timeout (* 5 60 1000)
   :conn-timeout (* 30 60 1000)
   :retry-handler (fn [ex try-count http-context]
                    ;(timbre/info "HTTP(S) connection failed:" try-count http-context)
                    ;(Thread/sleep 10000)
                    (if (> try-count 4) false true))})

(def http-params-for-quick-commands
  {:socket-timeout 60000
   :conn-timeout 60000
   :retry-handler (fn [ex try-count http-context]
                    ;(timbre/info "HTTP(S) connection failed:" try-count http-context)
                    ;(Thread/sleep 10000)
                    (if (> try-count 4) false true))})

(defonce http-server-port (atom nil))
(def initial-nodes ["node.shingetsu.info:8000/server.cgi" "node.fuktommy.com:8000/server.cgi"])
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



(defn valid-node-name? [node-name]
  (and
    (re-find #"^((([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])|(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\-]*[a-zA-Z0-9])\.)*([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9\-]*[A-Za-z0-9])):[0-9]+(/[a-zA-Z0-9._]+)+$" node-name)
    (not (re-find #"^192\." node-name))
    (not (re-find #"^128\." node-name))
    (not (re-find #"^localhost:" node-name))))

(defn valid-file? [file]
  (re-find #"^([0-9]+<>[0-9a-f]{32}<>.*\n)+$" file))

(defn valid-file-name? [file-name]
  (re-find #"^[0-9a-zA-Z]+_[0-9a-zA-Z_]+$" file-name))

(defn valid-range? [range]
  (if (or (re-find #"^[0-9]+$" range)
          (re-find #"^-[0-9]+$" range)
          (re-find #"^[0-9]+-$" range))
    true
    (let [match (re-find #"^([0-9]+)-([0-9]+)$" range)]
      (and match (<= (Long/parseLong (nth match 1)) (Long/parseLong (nth match 2)))))))

(defn unhexify [s]
  (let [bytes (into-array Byte/TYPE
                          (map (fn [[x y]]
                                 (unchecked-byte (Integer/parseInt (str x y) 16)))
                               (partition 2 s)))]
    (String. bytes "UTF-8")))

(defn file-name-to-thread-title
  [file-name]
  (and
    (re-find #"^thread_(.*)$" file-name)
    (unhexify (second (re-find #"^thread_(.*)$" file-name)))))

(defn thread-title-to-file-name
  [thread-title]
  (str "thread_" (apply str (map #(format "%02X" %) (.getBytes thread-title "UTF-8")))))

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
      (and file-name
           (some #{file-name} param/known-corrupt-files))
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

(defn convert-record-into-dat-file-line
  [record]
  (let [name (if (nil? (:name record)) "新月名無しさん" (:name record))
        mail (if (nil? (:mail record)) "" (:mail record))
        local-time (clj-time.core/to-time-zone (clj-time.coerce/from-long (* (:stamp record) 1000)) (clj-time.core/time-zone-for-offset +9))
        ts (str
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
               local-time))
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
  (if-not (valid-node-name? node-name)
    (throw (IllegalArgumentException. "Invalid node name.")))
  (try
    (let [response-body (:body (client/get (str "http://" node-name "/ping") http-params-for-quick-commands))
          new-server-node-name (second (re-find #"^PONG\n([^\n]+)\n?$" response-body))
          new-server-node-name (str new-server-node-name ":" @http-server-port param/server-path)]
      (if-not (valid-node-name? new-server-node-name)
        (throw (Exception. "Invalid node name.")))
      (if (nil? param/static-server-node-name)
        (reset! server-node-name new-server-node-name))
      (when (not (db/known-node? node-name))
        (db/add-node node-name)
        (db/mark-node-as-active node-name)
        (do (future (crawl-node node-name))))
      true)
    (catch Throwable t
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
    (throw (Exception. "Invalid node name.")))

  (client/get (str "http://" node-name "/join/" (clojure.string/replace @server-node-name #"/" "+")) http-params-for-quick-commands)
  (swap! active-nodes conj node-name))

(defn bye [node-name]
  ; (timbre/debug "bye:" node-name)
  (if-not (valid-node-name? node-name)
    (throw (IllegalArgumentException. "Invalid node name.")))

  (swap! active-nodes #(clojure.set/difference % #{node-name}))
  (client/get (str "http://" node-name "/bye/" (clojure.string/replace @server-node-name #"/" "+")) http-params-for-quick-commands))

(defn node [node-name]
  (if-not (valid-node-name? node-name)
    (throw (IllegalArgumentException. "Invalid node name.")))

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

  (when (ping node-name)
    ; Add as an active node if appropriate.
    (if (and
          (not (some #{node-name} @active-nodes))
          (< (count @active-nodes) max-num-active-nodes))
      (join node-name))

    ; Add as a search node if appropriate.
    (if (and
          (not (some #{node-name} @search-nodes))
          (< (count @search-nodes) max-num-search-nodes))
      (swap! search-nodes conj node-name))

    ; Get a new node.
    (try
      (dotimes [n 1]
        (let [new-node-name (node node-name)]
          (if (not (= new-node-name @server-node-name))
            (ping new-node-name))))
      (catch Exception e
        nil))))

(defn check-nodes
  ([]
   (check-nodes false))
  ([burst-mode]
   (timbre/info "Checking nodes..")
   (try
     (if (pos? (count @active-nodes))
       (bye (first (shuffle @active-nodes))))
     (if (pos? (count @search-nodes))
       (swap! search-nodes #(clojure.set/difference % #{(first (shuffle @search-nodes))})))

     (let [nodes (db/get-all-nodes)]
       (if (zero? (count nodes))
         (dorun ((if burst-mode pmap map) #(try (check-node %1) (catch Throwable t)) initial-nodes))
         (dorun ((if burst-mode pmap map) #(try (check-node %1) (catch Throwable t)) (shuffle (map :node-name nodes))))))

     (while (> (count @active-nodes) max-num-active-nodes)
       (bye (first (shuffle @active-nodes))))

     (catch Exception e
       ;(timbre/error e)
       nil))))

(defn start-node-monitor []
  (if param/static-server-node-name
    (reset! server-node-name param/static-server-node-name))
  (do
    (future
      (timbre/info "Node monitor started.")
      (try
        (dorun (map #(db/add-file %) param/known-files))
        (check-nodes true)
        (catch Throwable t))
      (while true
        (Thread/sleep check-nodes-interval)
        (try
          (check-nodes)
          (catch Throwable t))))))

(defn get-files-with-recent-command []
  (let [records (clojure.string/split (apply str (pmap #(try (recent %1 "0-") (catch Throwable _)) @search-nodes)) #"\n")
        records (remove #(not (re-find #"^[0-9]+<>[0-9a-f]{32}<>thread_[0-9A-F]+(<>.*)?$" %)) records)
        file-names (map #(second (re-find #"^[0-9]+<>[0-9a-f]{32}<>(thread_[0-9A-F]+)(<>.*)?$" %)) records)
        file-names (clojure.set/difference (into #{} file-names) param/known-corrupt-files)]
    file-names))

(defn download-file-from-node
  ([node-name file-name]
   (download-file-from-node node-name file-name "0-"))

  ([node-name file-name range]
   (if-not (= range "0-")
     (timbre/info "Downloading file:" node-name file-name range))
   (if-not (valid-node-name? node-name)
     (throw (IllegalArgumentException. "Invalid node name.")))
   (if-not (valid-file-name? file-name)
     (throw (IllegalArgumentException. "Invalid file name.")))
   (if-not (valid-range? range)
     (throw (IllegalArgumentException. "Invalid range.")))

   (db/add-file file-name)
   (let [file-id (db/get-file-id file-name)
         existing-records (and file-id (db/get-all-active-and-deleted-records-in-file-without-bodies file-id))]
     (if (= range "0-")
       ; Use /head to find missing records.
       (let [file (:body (try-times max-num-retries (client/get (str "http://" node-name "/head/" file-name "/" range) http-params)))
             file (if (nil? file) "" file)
             file (clojure.string/replace file #"(?m)^(?![0-9]+<>[0-9a-f]{32}).*$" "")
             file (clojure.string/replace file #"\r" "")
             file (clojure.string/replace file #"\n+" "\n")
             records (remove #(zero? (count %)) (clojure.string/split-lines file))
             records (map #(let [match (re-find #"^([0-9]+)<>([0-9a-f]{32})" %)]
                            {:stamp (Long/parseLong (nth match 1)) :record-id (nth match 2)})
                          records)
             existing-records (map #(identity {:stamp (:stamp %) :record-id (:record-id %)}) existing-records)
             records (clojure.set/difference (into #{} records) (into #{} existing-records))]
         (if (empty? records)
           0
           (let [stamps (map :stamp records)
                 oldest (apply min stamps)
                 newest (apply max stamps)]
             (try
               (download-file-from-node node-name file-name (str oldest "-" newest))
               (catch Throwable t))
             (let [existing-records (map #(identity {:stamp (:stamp %) :record-id (:record-id %)}) (db/get-all-active-and-deleted-records-in-file-without-bodies file-id))
                   records (clojure.set/difference (into #{} records) (into #{} existing-records))
                   stamps (map :stamp records)]
               (dorun (map #(download-file-from-node node-name file-name (str %)) stamps))))))

       ; Use the supplied range.
       (let [file (:body (try-times max-num-retries (client/get (str "http://" node-name "/get/" file-name "/" range) http-params)))
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
                                     (map #(re-find #"^([a-zA-Z0-9]+):(.*)$" %))
                                     (map #(do {(keyword (nth % 1)) (nth % 2)}))
                                     (apply merge))]
                   (db/add-record
                     file-id
                     stamp
                     record-id
                     body
                     (is-post-spam? file-name stamp record-id elements)
                     (convert-record-into-dat-file-line
                       (-> elements
                           (assoc :stamp stamp)
                           (assoc :record-id record-id)))
                     (:suffix elements)
                     node-name
                     nil
                     ))
                 (catch Throwable t  (timbre/info (str "Skipped record: " (str t) " " node-name " " file-name " " (nth (re-find #"^([0-9]+<>[0-9a-f]+)<>" record) 1 ""))))))
             records))
         (db/update-file file-id)
         ;(if-not (valid-file? file)
         ;  (throw (Exception. "Invalid file.")))
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

(defn crawl-node [node-name]
  (timbre/info "Crawler: Crawling node:" node-name)
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
          file-list (reverse (sort-by #(clj-time.coerce/to-long (clj-time.coerce/from-sql-time (:time-updated %))) file-list))]
      (if (zero? (count file-names))
        (throw (Exception.)))
      ;(timbre/debug "crawl-node: Downloaded a list of files:" node-name)
      (dorun
        (map
          #(do
            (db/add-file (:file-name %))
            (try
              (download-file-from-node node-name (:file-name %))
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
  [force-crawling]
  (timbre/info "Crawler: Crawling nodes...")
  (try
    (timbre/info "Crawler: Downloading lists of recently updated files...")
    (let [file-names (get-files-with-recent-command)]
      (dorun (map #(db/add-file %) file-names)))
    (dorun
      (pmap
        #(let [time-crawled (:time-crawled (db/get-node %))
               time-elapsed (and time-crawled (- (clj-time.coerce/to-long (clj-time.core/now)) (.getTime time-crawled)))]
          (if (or force-crawling
                  (nil? time-crawled)
                  (>= time-elapsed crawl-node-interval))
            (crawl-node %)
            (timbre/info "Crawler: Skipped node:" % time-elapsed)))
        (shuffle @search-nodes)))
    (timbre/info "Crawler: Done crawling nodes.")
    (catch Throwable t
      (timbre/error t)
      nil)))

(defn start-crawler
  []
  (do
    (future
      (Thread/sleep 60000)
      (timbre/info "Crawler started.")
      (crawl-nodes true)
      (while true
        (Thread/sleep crawl-nodes-interval)
        (try
          (crawl-nodes false)
          (catch Throwable t))))))



;;;;;;;;;;;;;;
; Signatures ;
;;;;;;;;;;;;;;

(defn md5
  [s]
  (let [md (MessageDigest/getInstance "MD5")]
    (.update md (cond
                  (nil? s) (.getBytes "" "UTF-8")
                  (string? s) (.getBytes s "UTF-8")
                  :else s))
    (apply str (map #(format "%02x" %) (.digest md)))))

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
      (if (and (valid-file-name? file-name) (valid-range? range))
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
                      (map #(re-find #"^([a-zA-Z0-9]+):(.*)$" %))
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
        (merge elements))))

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
  (let [file-id (db/get-file-id-by-thread-title thread-title)
        file (db/get-file-by-id file-id)
        stamp (long (/ (clj-time.coerce/to-long (clj-time.core/now)) 1000))
        escape-special-characters (fn [s]
                                    (-> s
                                        (clojure.string/replace #"&" "&amp;")
                                        (clojure.string/replace #"<" "&lt;")
                                        (clojure.string/replace #">" "&gt;")))
        body? (and body (pos? (count body)))
        attachment? (and attachment
                         (:filename attachment)
                         (pos? (count (:filename attachment)))
                         (:size attachment)
                         (pos? (:size attachment))
                         (:tempfile attachment))
        _ (if (and (not body?) (not attachment?))
            (throw (Exception. "空の書き込みはできません。")))
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
                      (if (and mail (pos? (count mail))) (str "<>mail:" (escape-special-characters mail))))
        record-body (if (or (nil? password) (zero? (count password)))
                      record-body
                      (let [{:keys [public-key signature]} (sign-post record-body password)]
                        (str
                          record-body
                          "<>pubkey:" public-key
                          "<>sign:" signature
                          "<>target:body"
                          (if (and name (pos? (count name))) ",name")
                          (if (and mail (pos? (count mail))) ",mail")
                          (if attachment? ",attach,suffix"))))
        record-id (md5 record-body)
        elements (->> (clojure.string/split record-body #"<>")
                      (map #(re-find #"^([a-zA-Z0-9]+):(.*)$" %))
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
    (db/update-file file-id)
    (db/process-update-command (:file-name file) stamp record-id)
    (do (future
          (swap! update-command-history conj entry)
          (dorun (pmap
                   #(try
                     (update % (:file-name file) stamp record-id)
                     (catch Throwable t
                       (timbre/error t)))
                   @active-nodes))))))

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
               (timbre/info "/join" (get-remote-address request) node-name)
               (when (and (valid-node-name? node-name)
                          (not (= node-name @server-node-name))
                          (not (some #{node-name} @active-nodes))
                          (re-find (re-pattern (str "^" (java.util.regex.Pattern/quote server-name) ":")) node-name)
                          (ping node-name))
                 (if (>= (count @active-nodes) max-num-active-nodes)
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
                          (some #{node-name} @active-nodes)
                          (re-find (re-pattern (str "^" (java.util.regex.Pattern/quote server-name) ":")) node-name))
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
                 (timbre/info "/update" remote-addr file-name stamp record-id node-name)
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
                     (download-file-from-node node-name file-name (str stamp))
                     (if-not (db/get-record-without-body (db/get-file-id file-name) stamp record-id)
                       (timbre/info "INVALID /update COMMAND:" file-name stamp record-id node-name))
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
               (ok (apply str (sort (map #(str % "\n") (map :file-name (db/get-all-files))))))
               (content-type "text/plain; charset=UTF-8")))



           ;;;;;;;;;;;
           ; Gateway ;
           ;;;;;;;;;;;

           (POST "/api/thread"
                 request
             (timbre/info "/api/thread" (get-remote-address request))
             (let [{:keys [thread-title page-num page-size record-short-id download]} (:params request)
                   ;page-num (Integer/parseInt page-num)
                   ;page-size (Integer/parseInt page-size)
                   file-id (db/get-file-id-by-thread-title thread-title)
                   file (db/get-file-by-id file-id)
                   _ (if (and file download)
                       (download-file (:file-name file)))
                   results (map
                             process-record-body
                             (if (and record-short-id (pos? (count record-short-id)))
                               (db/get-records-in-file-by-short-id file-id record-short-id)
                               (db/get-records-on-page file-id page-size page-num)))
                   ;_ (timbre/debug (str (count results)))
                   anchors (into [] (apply concat (map (fn [destnation]
                                                         ;(timbre/info file-id destnation (apply str (db/get-anchors file-id destnation)))
                                                         (db/get-anchors file-id destnation))
                                                       (map :record-short-id results))))]
               ;(timbre/info "anchors:" (apply str anchors))
               {:body {:num-posts (:num-records file)
                       :posts     results
                       :anchors   anchors}}))

           (POST "/api/new-posts"
                 request
             (timbre/info "/api/new-posts" (get-remote-address request))
             (let [{:keys [threads]} (:params request)
                   ;_ (timbre/debug threads)
                   results (remove nil?
                                   (map (fn [thread]
                                           (let [file-id (db/get-file-id-by-thread-title (:thread-title thread))
                                                 posts (map process-record-body
                                                            (db/get-new-records-in-file file-id (:time-last-accessed thread)))
                                                 ;_ (timbre/debug thread (count posts))
                                                 anchors (into [] (apply concat (map (fn [destnation]
                                                                                       (db/get-anchors file-id destnation))
                                                                                     (map :record-short-id posts))))]
                                             (if (zero? (count posts))
                                               nil
                                               {:thread-title (:thread-title thread)
                                                :posts posts
                                                :anchors anchors})))
                                         threads))]
               {:body {:threads (into [] results)}}))

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
             (timbre/info "/api/threads" (get-remote-address request))
             (let [n (:n params)
                   n (if (zero? (count n)) nil n)]
               (->> (if n
                      (db/get-files-with-limit (Integer/parseInt n))
                      (db/get-all-files))
                    (remove #(or
                              (some #{(:file-name %)} param/known-corrupt-files)
                              (zero? (:num-records %))))
                    (map #(assoc % :time-updated (try (long (/ (clj-time.coerce/to-long (:time-updated %)) 1000)) (catch Throwable _ nil)))))))

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
                   (throw (Exception.)))
                 (process-post thread-title name mail password body attachment remote-address)
                 (ok "OK"))
               (catch Throwable t
                 (timbre/error t)
                 (internal-server-error "NG"))))

           (GET "/api/status" request
             ;(timbre/info "/api/status" (get-remote-address request))
             (let [cache-size (reduce + (map :size (db/get-all-files)))]
               {:body
                {:status
                 {:num-files (db/count-all-files)
                  :num-records (db/count-all-records)
                  :num-deleted-records (db/count-all-deleted-records)
                  :cache-size cache-size
                  :server-node-name @server-node-name
                  :server-url-base (get-server-url-base)
                  :enable-recaptcha param/enable-recaptcha
                  :recaptcha-site-key param/recaptcha-site-key
                  :recaptcha-secret-key param/recaptcha-secret-key
                  :service-name param/service-name
                  :active-nodes (into [] (sort @active-nodes))
                  :search-nodes (into [] (sort @search-nodes))}}}))



           (GET "/2ch/subject.txt"
                {:keys [headers params body server-name] :as request}
             ;(timbre/info "/2ch/subject.txt" (get-remote-address request))
             (let [files (db/get-all-files)
                   lines (->> files
                              (remove #(or
                                        (not (= (:application "thread")))
                                        (some #{(:file-name %)} param/known-corrupt-files)
                                        (zero? (:num-records %))))
                              (map #(try
                                     (str
                                       (+ (long (/ (clj-time.coerce/to-long (:time-first-post %)) 1000)) (* 9 60 60))
                                       ".dat<>"
                                       (org.apache.commons.lang3.StringEscapeUtils/escapeHtml4
                                         (unhexify (second (re-find #"^thread_(.*)$" (:file-name %)))))
                                       " (" (:num-records %) ")\n")
                                     (catch Throwable _ ""))))]
               (->
                 (ok (apply str lines))
                 (content-type "text/plain; charset=windows-31j"))
               ))

           (GET "/test/read.cgi/2ch/:thread-number"
                request
             (let [thread-number (:thread-number (:params request))
                   _ (timbre/info "/test/read.cgi/2ch/:thread-number" thread-number)
                   file (db/get-file-by-thread-number thread-number)
                   _ (if (nil? file) (throw (Exception.)))]
               (redirect (str "/thread/" (percent-encode (file-name-to-thread-title (:file-name file)))))))

           (GET "/test/read.cgi/2ch/:thread-number/"
                request
             (let [thread-number (:thread-number (:params request))
                   _ (timbre/info "/test/read.cgi/2ch/:thread-number/" thread-number)
                   file (db/get-file-by-thread-number thread-number)
                   _ (if (nil? file) (throw (Exception.)))]
               (redirect (str "/thread/" (percent-encode (file-name-to-thread-title (:file-name file)))))))

           (GET "/test/read.cgi/2ch/:thread-number/:qualifier"
                request
             (let [{:keys [thread-number qualifier]} (:params request)
                   _ (timbre/info "/test/read.cgi/2ch/:thread-number/:qualifier" thread-number qualifier)
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
                     (str "/" record-short-id))))))

           (GET "/2ch/dat/:dat-file-name"
                 request
             (let [{:keys [dat-file-name]} (:params request)
                   _ (timbre/info "/2ch/dat/:dat-file-name" (get-remote-address request) dat-file-name)
                   [_ thread-number] (re-find #"^([0-9]+)\.dat$" dat-file-name)
                   file (db/get-file-by-thread-number thread-number)
                   _ (if (nil? file) (throw (Exception.)))
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
                                                 (if (:dat-file-line %1)
                                                   (str
                                                     (-> (:dat-file-line %1)
                                                         (clojure.string/replace
                                                           #"&gt;&gt;[a-f0-9]{8}"
                                                           (fn [s] (get anchor-map s s)))
                                                         (clojure.string/replace
                                                           #"\[\[[^\]]+\]\]"
                                                           (fn [s]
                                                             (let [thread-title (second (re-find #"\[\[([^\]]+)\]\]" s))
                                                                   file-name (thread-title-to-file-name thread-title)
                                                                   file (db/get-file file-name)]
                                                               (if file
                                                                 (str
                                                                   "[["
                                                                   thread-title
                                                                   "( "
                                                                   (get-server-url-base)
                                                                   "/test/read.cgi/2ch/"
                                                                   (+ (long (/ (clj-time.coerce/to-long (clj-time.coerce/from-sql-time (:time-first-post file))) 1000))
                                                                      (* 9 60 60))
                                                                   "/"
                                                                   " )"
                                                                   "]]")
                                                                 s)
                                                               )))
                                                         (clojure.string/replace
                                                           #"\[\[[^\]]+/[a-f0-9]{8}\]\]"
                                                           (fn [s]
                                                             (let [[_ thread-title record-short-id] (re-find #"\[\[([^\]]+)/([a-f0-9]{8})\]\]" s)
                                                                   file-name (thread-title-to-file-name thread-title)
                                                                   file (db/get-file file-name)
                                                                   records (db/get-all-records-in-file-without-bodies (:id file))
                                                                   post-numbers-map (apply merge
                                                                                           (remove
                                                                                             nil?
                                                                                             (map
                                                                                               (fn [record post-number]
                                                                                                 (try
                                                                                                 {(second (re-find #"^(.{8})" (:record-id record))) post-number}
                                                                                                 (catch Throwable _
                                                                                                   nil)))
                                                                                               records
                                                                                               (range 1 (inc (count records))))))
                                                                   post-number (get post-numbers-map record-short-id nil)]
                                                               (if file
                                                                 (str
                                                                   "[["
                                                                   thread-title
                                                                   "( "
                                                                   (get-server-url-base)
                                                                   "/test/read.cgi/2ch/"
                                                                   (+ (long (/ (clj-time.coerce/to-long (clj-time.coerce/from-sql-time (:time-first-post file))) 1000))
                                                                      (* 9 60 60))
                                                                   "/"
                                                                   (if post-number
                                                                     (str post-number))
                                                                   " )")
                                                                 s)
                                                               ))))
                                                     (if (:suffix %1)
                                                       (str
                                                         (if-not (re-find #"<>$" (:dat-file-line %1))
                                                           "<br>")
                                                         (get-server-url-base)
                                                         "/thread"
                                                         "/" (java.net.URLEncoder/encode thread-title "UTF-8")
                                                         "/" (:record-id %1) "." (:suffix %1)
                                                         ))
                                                     "<>"
                                                     (if (= %2 1)
                                                       (str (org.apache.commons.lang3.StringEscapeUtils/escapeHtml4 thread-title)))
                                                     "\n"))
                                                 (catch Throwable _
                                                   nil))
                                             results
                                             (range 1 (inc (count results))))))]
               (->
                 (ok (apply str posts-as-strings))
                 (content-type "text/plain; charset=windows-31j"))))



           (POST "/test/bbs.cgi"
                 request
             (try
               (let [{:keys [bbs key FROM mail password MESSAGE attachment g-recaptcha-response]} (:form-params request)
                     _ (timbre/debug "/test/bbs.cgi" (get-remote-address request) bbs key)
                     remote-address (get-remote-address request)
                     file (db/get-file-by-thread-number key)
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
                   (content-type "text/html; charset=windows-31j"))))))



(defn remove-spam
  []
  (dorun (map (fn [record]
                 (let [record (ju.db.core/get-record-by-id (:id record))
                       body (String. (:body record) "UTF-8")
                       elements (->> (clojure.string/split body #"<>")
                                     (map #(re-find #"^([a-zA-Z0-9]+):(.*)$" %))
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

(defn delete-duplicate-posts
  "Returns a set of records in file table that are special cases of duplicates.
  e.g. NHK総合実況 -> nhk総合実況"
  []
  (dorun
    (into #{}
          (remove nil?
                  (map (fn [file]
                         (let [thread-title (ju.routes.shingetsu/file-name-to-thread-title (:file-name file))
                               thread-title-lower-case (clojure.string/lower-case thread-title)]
                           (if (and thread-title thread-title-lower-case (not (= thread-title thread-title-lower-case)))
                             (let [records-in-original-file (ju.db.core/get-all-active-and-deleted-records-in-file-without-bodies (:id file))
                                   file-lower-case (ju.db.core/get-file (ju.routes.shingetsu/thread-title-to-file-name thread-title-lower-case))
                                   records-in-duplicate-file (and file-lower-case (ju.db.core/get-all-active-and-deleted-records-in-file-without-bodies (:id file-lower-case)))
                                   duplicate-records (if file-lower-case (clojure.set/intersection
                                                                                   (into #{} (map #(:record-id %) records-in-original-file))
                                                                                   (into #{} (map #(:record-id %) records-in-duplicate-file)))
                                                                         #{})]
                               (when (and
                                       file-lower-case
                                       duplicate-records
                                       (pos? (count duplicate-records)))
                                 (timbre/debug
                                   (str
                                     thread-title
                                     " -> " thread-title-lower-case
                                     " " (count records-in-original-file)
                                     " " (count records-in-duplicate-file)
                                     " " (count duplicate-records)))
                                 (dorun
                                   (map #(db/mark-record-in-file-with-record-id-as-deleted (:id file-lower-case) %)
                                        duplicate-records))
                                 (db/update-file (:id file-lower-case))
                                 )))))
                       (ju.db.core/get-all-files))))))
