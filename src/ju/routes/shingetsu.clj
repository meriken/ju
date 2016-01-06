(ns ju.routes.shingetsu
  (:require [ju.layout :as layout]
            [compojure.core :refer [defroutes GET POST]]
            [compojure.coercions :refer [as-int]]
            [ring.util.http-response :refer [ok]]
            [ring.util.response :refer [content-type]]
            [ring.util.request :refer [body-string]]
            [clojure.java.io :as io]

    ; Meriken
            [taoensso.timbre :as timbre]
            [clj-http.client :as client]
            [ju.db.core :as db]
            [pandect.algo.md5 :refer :all])
  (:import (java.util.regex Pattern)))



; (def http-params { :headers {"Accept-Encoding" ""} :socket-timeout (* 5 60 1000) :conn-timeout (* 30 60 1000)})
(def http-params { :socket-timeout (* 5 60 1000) :conn-timeout (* 30 60 1000)})
(def http-params-for-quick-commands {:socket-timeout 60000 :conn-timeout 60000})
(defonce http-server-port (atom nil))
(def server-path "/server")
(def initial-nodes ["node.shingetsu.info:8000/server.cgi" "node.fuktommy.com:8000/server.cgi"])
(defonce active-nodes (atom (hash-set)))
(defonce search-nodes (atom (hash-set)))
(defonce server-node-name (atom nil))
(defonce update-command-history (atom (hash-set)))
(def known-corrupt-files
  #{"thread_E6BF82E5A790E4BAB6E98A87EE8182E581A8E98A89E69B98E58493E98A88E68E95E78DAEE98A88E5ACA8E581A3E98A89"
    "thread_254535254142253843254535253834254232"
    "thread_266E6273703B28EFBD80EFBDA5CF89EFBDA5C2B4EFBC89EFBDBCEFBDACEFBDB7EFBDB0EFBE9D"
    "thread_266E6273703BE38386E382B9E38388"
    "thread_2D45332D38312D38382D45332D38312D41332D45332D38312D41312D45332D38312D41412D45342D42412D38432D45362D41432D41312D45352D38352D3833"
    "thread_33EFBFBDEFBFBDEFBFBDEFBFBDEFBFBDEFBFBD70EFBFBD40EFBFBD47EFBFBDEFBFBDEFBFBDE6919C"
    "thread_383245333345333333383339414545373036"
    "thread_3F3F"
    "thread_3F3F3F3F3F"
    "thread_3F3F3F3F3F3F3F"
    "thread_3F3F3F3F3F3F3F3F"
    "thread_3F3F3F3F3F3F3F3F3F3F3F3F3F3F"
    "thread_3F3F3FE5838FE7BDAE"
    "thread_467265656E6574EFBFBDEFBFBDEFBFBDEFBFBD"
    "thread_4954EFBFBD47EFBFBD6B"
    "thread_4E454320E697A5E69CACE99BBBE6B097E381AEE8A38FE4BA8BE68385266E6273703B"
    "thread_4E616E61534849D0B8D196D084D0B5E280A2D08FD0B3E2809AE28496D0B3D193C2AC"
    "thread_506572666563744461726BEFBFBD56EFBFBDEFBFBDEFBFBD78EFBFBDEFBFBD"
    "thread_5261737062657272792532305069"
    "thread_52617370626572727925323532305069"
    "thread_544253D0B3D083D08A"
    "thread_544253E98A87"
    "thread_676966C3A5C28BC295C3A7C294C2BBC3A7C2B7C28FC3A5C290C288"
    "thread_784533783831783838784533783831784133784533783831784131784533783831784141784534784241783843784536784143784131784535783835783833"
    "thread_C383C2A3C382C692C382C2ADC383C2A3C382C692C382C2AAC383C2A7C382E2809DC382C2BBC383C2A5C382C692C382C28FC383C2A7C382C2BDC382C2AEC383C2A3C382C281C382C28DC383C2A5C382C2A0C382C2B4"
    "thread_C3A3C281C288C3A3C281C2A3C3A3C281C2A1C3A3C281C2AAC3A4C2BAC28CC3A6C2ACC2A1C3A5C285C28320C3A3C281C29DC3A3C281C2AE32"
    "thread_C3A3C281C288C3A3C282C28DC3A3C281C299C3A3C281C28CC3A8C2B6C2B3C3A3C282C28AC3A3C281C2AAC3A3C281C284"
    "thread_C3A3C281C2A8C3A3C281C2ABC3A3C281C28BC3A3C281C28FC3AFC2BEC28AC3AFC2BDC2A7C3AFC2BEC28AC3AFC2BDC2A7C3A3C281C299C3A3C282C28BC3A3C282C2B9C3A3C283C2AC\n"
    "thread_C3A3C283C2ADC3A3C283C2AAC3A7C294C2BBC3A5C283C28FC3A7C2BDC2AEC3A3C281C28DC3A5C2A0C2B4"
    "thread_C3A6C2B5C281C3A5C287C2BAC3A3C281C297C3A3C281C29FC3A5C2A5C2B3C3A3C282C2BFC3A3C283C281"
    "thread_D0B3D082D1924C4653D0B3D082E28098204C696E75782066726F6D2053637261746368"
    "thread_D0B3D193D08FD0B3D193D193D0B3E2809AC2ADD0B3D193D196D0B3E2809AC2B0"
    "thread_D0B3D193E280B9D0B3D193D290D0B3D193D198D0B3E2809AE28496"
    "thread_D0B3D193E282ACD0B3D193D19BD0B3D193E282ACD0B3D083C2A7D0B3D083E28094D0B3E2809AD089D0B3D083D081D0B3E2809AD089"
    "thread_D0B6E28093C2B0D0B6D19AE282ACD0B3D083C2AED0B9E28093E280B9D0B7E284A2D194"
    "thread_D0B9E280BAE28098D0B8C2ABE280A1"
    "thread_E2809CE8A5B2E69283E4BA88E5918AE2809DE69BB8E3818DE8BEBCE381BFE38081E8ADA6E8A696E5BA81EFBCA8EFBCB0E381ABE3808CE8A5B2E69283E38197E381BEE3819BE38293E3808D266E6273703B"
    "thread_E383ABE3838DE382B5E382B9E382A8E383ACE382AFE38388E383ADE3838BE382AFE382B9E381AEE8A38FE4BA8BE68385572331"
    "thread_E4B889E697A5E69C882026616D703B20E381A4E38194E38282E3828A"
    "thread_E6B2B9E583B9E8B68AE995B7E8B68AE58587"
    "thread_E6B5A0E5A8BFE4BCA8E98A87E69B98E4BC80E991B1E6B7ACE4BA9CE98A87EFB8BAE4BA9CE98A88E5A9A544E98A87EE8899E7B68DE98A87E78387E7B4B5"
    "thread_E98A88E5B9BFE584B8E7BB94E5ACA8E4BBB8E6B5A0EFBD88EE9491E6B89AE6BF8BE789B8"
    "thread_E98A89C2B0E58496E98A89E383A3E58597"
    "thread_E98A89EE859CE584B6E990A2E8AFB2E5849AE7BC83EE86BAE4BAB6E98DAB"
    "thread_E98A89EE8898E584B9E98A88E5B9BFE4BAB4E79392E782BDE5809EE98A87EE8182E4BA9C"
    "thread_E98EB6E69BA1E7A1A3E98A87E384A6E5A787E5A797"
    "thread_E98F83E383A6E6B9B0E98A87EE86BFEE9794E98D8B"
    "thread_E99097E9809BE7B0BB"
    "thread_E99782E5A09BE59EBBE5A885D187E5A6B5E98D95E29581E4BBA2E996B5E5A48BE69FA3E98D8EEFB982E5A6B5E98A8AEFBD84E7BD95"
    "thread_E99B91E8AB87236165633963616662"
    "thread_EFBFBD47EFBFBD6B"
    "thread_EFBFBD47EFBFBD6BEFBFBDEFBFBDEFBFBDC882EFBFBDEFBFBDE7838AEFBFBDEFBFBDEFBFBD4EEFBFBDEFBFBD5CEFBFBDEFBFBD58EFBFBDEFBFBD"
    "thread_EFBFBDEFBFBDEFBFBDCC8370EFBFBD60EFBFBDEFBFBDEFBFBD52EFBFBDCD88EFBFBD40EFBFBD71EFBFBDEFBFBDEFBFBDEFBFBDEFBFBDEFBFBDC882EFBFBDEFBFBDCC82EFBFBD"
    "thread_EFBFBDEFBFBDEFBFBDEFBFBD5043"
    "thread_EFBFBDEFBFBDEFBFBDEFBFBDEFBFBDE6919CEFBFBD75EFBFBDEFBFBDEFBFBDEFBFBD"
    "thread_C3A3C281C2A8C3A3C281C2ABC3A3C281C28BC3A3C281C28FC3AFC2BEC28AC3AFC2BDC2A7C3AFC2BEC28AC3AFC2BDC2A7C3A3C281C299C3A3C282C28BC3A3C282C2B9C3A3C283C2AC"
    "thread_333F3F3F3F3F3F3F3F3F3F3F313F3F"
    "thread_266E6273703BE383ADE383AAE794BBE5838FE7BDAEE3818DE5A0B4"
    "thread_E38080"
    "thread_506572666563744461726B3F3F3F"
    "thread_254533253833253842254533253833254135254533253833254243254533253832254239"
    "thread_3F"
    "thread_3F3F3F3F3263683F3F3F3F3F3F3F3F3F3F3F3F3F"
    "thread_3F3F3F3F3F3F"
    "thread_3F3F3F3F3F3F3F3F3F3F"
    "thread_3F3F3F3F3F3F3F3F3F43443F3F3F"
    "thread_3F5553442D4A50593F3F3F3F3F3F3F3F2B50617274313F24"
    "thread_E38080E38080E38080E38080E38080E38080E38080E38080E38080E38080E38080E38080E38080E38080E38080E38080E38080E38080E38080"
    "thread_E38193E381AEEFBFBD58E383ACE381ABE383ACEFBFBD58E38197E3819FE38289E4B880E7949FE38197E3828FE38182E3819BE381ABE381AAE3828CE3828BEFBFBD58E383AC"
    "thread_E98A89E59790E581A3E98A89"
    "thread_E98EB3E684A9E4BAB1E98A87E6A4BCE4BAB6E98A88EE8182E58597E98A89E59BA5E58185E98A88EE8182E4BC84E6B693E6A0ABE699AB"
    "thread_E99786E688A3E78F96"
    "thread_E99786E688A3E78F96E98A87E6A4BCE4BBBEE98A87E5B1BBE5809DE98A89EE8182E58582E98A88EE8898E580B0E792A8E7ACBAE580A0E98A88E5B9BFE584B8"
    "thread_EFBFBD56EFBFBD51E59091E38191EFBFBD56E69C88EFBFBD6EE383B3EFBFBD68EFBFBD75EFBFBD62EFBFBD4E"})
(def crawl-node-interval  (* 24 60 60 1000))
(def crawl-nodes-interval (*    60 60 1000))
(def check-nodes-interval (*       60 1000))
(def max-num-retries 5)
(def max-num-active-nodes 8)
(def max-num-search-nodes 128)


(defn valid-node-name? [node-name]
  (and
    (re-find #"^((([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])|(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\-]*[a-zA-Z0-9])\.)*([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9\-]*[A-Za-z0-9])):[0-9]+(/[a-zA-Z0-9.]+)+$" node-name)
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Shingetsu Protocol Commands ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ping [node-name]
  (if-not (valid-node-name? node-name)
    (throw (IllegalArgumentException. "Invalid node name.")))
  (try
    (let [response-body (:body (client/get (str "http://" node-name "/ping") http-params-for-quick-commands))
          new-server-node-name (second (re-find #"^PONG\n([^\n]+)\n?$" response-body))
          new-server-node-name (str new-server-node-name ":" @http-server-port server-path)]
      (if-not (valid-node-name? new-server-node-name)
        (throw (Exception. "Invalid node name.")))
      (reset! server-node-name  new-server-node-name)
      (db/add-node node-name)
      (db/mark-node-as-active node-name)
      true)
  (catch Throwable t
    (swap! active-nodes #(clojure.set/difference % #{node-name}))
    (swap! search-nodes #(clojure.set/difference % #{node-name}))
    false)))

(defn join [node-name]
  ; (timbre/debug "join:" node-name)
  (if-not (valid-node-name? node-name)
    (throw (IllegalArgumentException. "Invalid node name.")))
  (if (= node-name @server-node-name)
    (throw (Exception. "Invalid node name.")))

  (client/get (str "http://" node-name "/join/" (clojure.string/replace @server-node-name #"/" "+")) http-params-for-quick-commands)
  (swap! active-nodes conj node-name)
  (db/add-node node-name))

(defn bye [node-name]
  ; (timbre/debug "bye:" node-name)
  (if-not (valid-node-name? node-name)
    (throw (IllegalArgumentException. "Invalid node name.")))

  (swap! active-nodes #(clojure.set/difference % #{node-name}))
  (client/get (str "http://" node-name "/bye/" (clojure.string/replace @server-node-name #"/" "+")) http-params-for-quick-commands))

(defn node [node-name]
  (if-not (valid-node-name? node-name)
    (throw (IllegalArgumentException. "Invalid node name.")))

  (let [response (client/get  (str "http://" node-name "/node") http-params-for-quick-commands)
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

  ([node-name file-name stamp record-id node-name]
   (if-not (valid-file-name? file-name)
     (throw (IllegalArgumentException. "Invalid file name.")))

   (client/get
     (str "http://" node-name "/recent/" file-name "/" stamp "/" record-id "/" (clojure.string/replace node-name #"/" "+"))
     http-params)))



;;;;;;;;;;;
; Daemons ;
;;;;;;;;;;;

(defn check-node
  [node-name]
  ; (timbre/debug "check-node:" node-name)
  (if-not (valid-node-name? node-name)
    (throw (IllegalArgumentException. "Invalid node name.")))
  (if (= node-name @server-node-name)
    (throw (Exception. "Invalid node name.")))

  (when (ping node-name)
    ; Add as an active node if appropriate.
    (if (and
          (not (some #{ node-name } @active-nodes))
          (< (count @active-nodes) max-num-active-nodes))
      (join node-name))

    ; Add as a search node if appropriate.
    (if (and
          (not (some #{ node-name } @search-nodes))
          (< (count @search-nodes) max-num-search-nodes))
      (swap! search-nodes conj node-name))

    ; Get a new node.
    (try
      (dotimes [n 1]
        (let [new-node-name (node node-name)]
          (if (not  (= new-node-name @server-node-name))
            (ping new-node-name))))
        (catch Exception e
          nil))))

(defn check-nodes []
  ;(timbre/debug "check-nodes")
  (try
    (if (pos? (count @active-nodes))
      (bye (first (shuffle @active-nodes))))
    (if (pos? (count @search-nodes))
      (swap! search-nodes #(clojure.set/difference % #{(first (shuffle @search-nodes))})))

    (let [nodes (db/get-all-nodes)]
      (if (zero? (count nodes))
        (doall (map #(try (check-node %1) (catch Throwable t)) initial-nodes))
        (doall (map #(try (check-node %1) (catch Throwable t)) (shuffle (map :node-name nodes))))))

    (while (> (count @active-nodes) max-num-active-nodes)
      (bye (first (shuffle @active-nodes))))

    (catch Exception e
      ;(timbre/error e)
      nil)))

(defn start-node-monitor []
  (do
    (future
      (timbre/info "Node monitor started.")
      (try
        (doall (map #(try (ping %1) (catch Throwable t)) initial-nodes))
        (doall (map #(try (ping %1) (catch Throwable t)) (map :node-name (db/get-all-nodes))))
        (catch Throwable t))
      (while true
        (try
          (check-nodes)
          (catch Throwable t))
        (Thread/sleep check-nodes-interval)))))

(defn get-files-with-recent-command []
  (let [records (clojure.string/split (apply str (pmap #(try (recent %1 "0-") (catch Throwable _)) @search-nodes)) #"\n")
        records (remove #(not (re-find #"^[0-9]+<>[0-9a-f]{32}<>thread_[0-9A-F]+(<>.*)?$" %)) records)
        file-names (map #(second (re-find #"^[0-9]+<>[0-9a-f]{32}<>(thread_[0-9A-F]+)(<>.*)?$" %)) records)
        file-names (clojure.set/difference (into #{} file-names) known-corrupt-files)]
    file-names))

(defn try-times*
  "Executes thunk. If an exception is thrown, will retry. At most n retries
  are done. If still some exception is thrown it is bubbled upwards in
  the call chain."
  [n thunk]
  (loop [n n]
    (if-let [result (try
                      [(thunk)]
                      (catch Exception e
                        (when (zero? n)
                          (throw e))))]
      (result 0)
      (recur (dec n)))))

(defmacro try-times
  "Executes body. If an exception is thrown, will retry. At most n retries
  are done. If still some exception is thrown it is bubbled upwards in
  the call chain."
  [n & body]
  `(try-times* ~n (fn [] ~@body)))

(defn download-file-from-node
  ([node-name file-name]
   (download-file-from-node node-name file-name "0-"))

  ([node-name file-name range]
   (if-not (= range "0-")
     (timbre/debug "Downloading file:" node-name file-name range)
     )
   (if-not (valid-node-name? node-name)
     (throw (IllegalArgumentException. "Invalid node name.")))
   (if-not (valid-file-name? file-name)
     (throw (IllegalArgumentException. "Invalid file name.")))
   (if-not (valid-range? range)
     (throw (IllegalArgumentException. "Invalid range.")))

   (db/add-file file-name)
     (let [file-id (db/get-file-id file-name)
           existing-records (and file-id (db/get-all-records-in-file-without-bodies file-id))]
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
               (let [existing-records (map #(identity {:stamp (:stamp %) :record-id (:record-id %)}) (db/get-all-records-in-file-without-bodies file-id))
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
               #(try
                 (let [match (re-find #"^([0-9]+)<>([0-9a-f]{32})<>(.*)$" %)
                       stamp (Long/parseLong (nth match 1))
                       record-id (nth match 2)
                       body (.getBytes (nth match 3) "UTF-8")]
                   (db/add-record file-id stamp record-id body))
                 (catch Throwable _ (comment timbre/debug (str "download-file-from-node: Record skipped: " %))))
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
       #(download-file-from-node % file-name range)
       (shuffle @search-nodes)))
   true))

(defn crawl-node [node-name]
  (timbre/info "Crawler: Crawling node:" node-name)
  (try
    ; Try to download a list of files.
    (let [saku-api-url (str "http://" (nth (re-find #"^(.*)/server\.cgi$" node-name) 1) "/gateway.cgi/csv/changes/file")
          file-names (clojure.string/split-lines (:body (client/get saku-api-url http-params)))
          file-names (remove #(not (re-find #"^thread_[0-9A-F]+$" %)) file-names)]
      (if (zero? (count file-names))
        (throw (Exception.)))
      ;(timbre/debug "crawl-node: Downloaded a list of files:" node-name)
      (dorun
        (map
          #(when (some #{ node-name } @search-nodes)
            (db/add-file %)
            (try
              (download-file-from-node node-name %)
              (catch Throwable t
                (timbre/info "Crawler: Failed to download file:" node-name %))))
          (shuffle file-names)))
      (db/mark-node-as-crawled node-name)
      (timbre/info "Crawler: Done crawling node:" node-name)
      true)

    (catch Throwable t
      ; A list of files was not available.
      ;(timbre/debug "crawl-node: Failed to download a list of files:" node-name)
      (try
        (dorun
          (map
            #(when (some #{ node-name } @search-nodes)
              (try
                (download-file-from-node node-name %)
                (catch Throwable t
                  (timbre/info "Crawler: Done crawling node:" node-name %))))
            (shuffle (map :file-name (db/get-all-files)))))
        (catch Throwable t
          (timbre/error t)
          nil)))))

(defn crawl-nodes []
  (timbre/info "Crawler: Crawling nodes...")
  (try
    (timbre/info "Crawler: Downloading lists of recently updated files...")
    (let [file-names (get-files-with-recent-command)]
      (dorun (pmap #(db/add-file %) file-names)))
    (dorun
      (pmap
        #(let [time-crawled (:time-crawled (db/get-node %))
               time-elapsed (and time-crawled (- (clj-time.coerce/to-long (clj-time.core/now)) (.getTime time-crawled)))]
          (if (or (nil? time-crawled)
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
      (timbre/info "Crawler started.")
      (dotimes [n 8] (check-nodes))
      (while true
        (try
          (crawl-nodes)
          (catch Throwable t))
        (Thread/sleep crawl-nodes-interval)))))



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Shingetsu Protocol Commands ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn unhexify [s]
  (let [bytes (into-array Byte/TYPE
                          (map (fn [[x y]]
                                 (unchecked-byte (Integer/parseInt (str x y) 16)))
                               (partition 2 s)))]
    (String. bytes "UTF-8")))



(defn get-remote-address
  [request]
  (or (get-in request [:headers "x-forwarded-for"]) (:remote-addr request)))

(defn process-get-command
  [request without-bodies]
  (let [{:keys [headers params body server-name]} request]
    (let [{file-name :file-name range :range} params
          file-id (db/get-file-id file-name)
          match (re-find #"^([0-9]*)-([0-9]*)$" range)
          start (try (Long/parseLong (if match (nth match 1) (nth (re-find #"^([0-9]*)$" range) 1))) (catch Throwable t nil))
          end   (try (Long/parseLong (if match (nth match 2) (nth (re-find #"^([0-9]*)$" range) 1))) (catch Throwable t nil))
          record-id (:record-id params)]
      (if (and (valid-file-name? file-name) (valid-range? range))
        (->
          (ok (apply
                str
                (map
                  #(do
                    (if-not (= (:record-id %) (md5 (:body %)))
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
                  (db/get-records-in-file-with-range file-id start end))))
          (content-type "text/plain; charset=UTF-8"))))))

(defroutes shingetsu-routes
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ; Shingetsu Protocol Commands ;
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

           (GET (str server-path "/ping") request
             ;(timbre/info "/ping command received:" (get-remote-address request))
             (->
               (ok
                 (str
                   "PONG\n"
                   (or (get-in request [:headers "x-forwarded-for"]) (:remote-addr request))))
               (content-type "text/plain; charset=UTF-8")))

           (GET (str server-path "/node") request
             ;(timbre/info "/node command received:" (get-remote-address request))
             (->
               (ok (try (rand-nth (into [] @search-nodes)) (catch Throwable _ "")))
               (content-type "text/plain; charset=UTF-8")))

           (GET (str server-path "/join/:node-name")
                {:keys [headers params body server-name] :as request}
             (let [{node-name :node-name} params
                   node-name (clojure.string/replace node-name #"\+" "/")
                   remote-addr (get-remote-address request)
                   node-name (if (re-find #"^:" node-name)
                               (str remote-addr node-name)
                               node-name)]
               (timbre/info "/join command received:" node-name)
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

           (GET (str server-path "/bye/:node-name")
                {:keys [headers params body server-name] :as request}
             (let [{node-name :node-name} params
                   node-name (clojure.string/replace node-name #"\+" "/")]
               (timbre/info "/bye command received:" node-name)
               (when (and (valid-node-name? node-name)
                          (not (= node-name @server-node-name))
                          (some #{node-name} @active-nodes)
                          (re-find (re-pattern (str "^" (java.util.regex.Pattern/quote server-name) ":")) node-name))
                 (swap! active-nodes #(clojure.set/difference % #{node-name}))
                 (->
                   (ok "BYEBYE")
                   (content-type "text/plain; charset=UTF-8")))))

           (GET (str server-path "/update/:file-name/:stamp/:record-id/:node-name")
                {:keys [headers params body server-name] :as request}
             (try
               (let [{node-name :node-name
                    file-name :file-name
                    stamp :stamp
                    record-id :record-id} params
                   node-name (clojure.string/replace node-name #"\+" "/")
                   remote-addr (get-remote-address request)
                   node-name (if (re-find #"^:" node-name)
                               (str remote-addr node-name)
                               node-name)
                   stamp (Long/parseLong stamp)
                   entry {:file-name file-name :stamp stamp :record-id record-id}
                     file (db/get-file file-name)]
               (timbre/info "/update command received:" file-name stamp record-id node-name)
               ;(if (and
               ;      (not (db/file-deleted? file-id))
               ;      (not (db/record-exists file-id stamp record-id)))
               (db/process-update-command file-name stamp record-id)
               (cond
                 (and file (:deleted file) (not (some #{entry} @update-command-history)))
                 (try
                   (dorun (pmap
                            #(try
                              (catch Throwable t
                                (update % file-name stamp record-id node-name)
                                (timbre/error t)))
                            (clojure.set/difference @active-nodes #{node-name})))
                   (swap! update-command-history conj entry)
                   (catch Throwable t
                     (timbre/error t)))

                 (not (some #{entry} @update-command-history))
                 (try
                   (download-file-from-node node-name file-name (str stamp))
                   (if-not (db/get-record-without-body (db/get-file-id file-name) stamp record-id)
                     (timbre/info "INVALID /update COMMAND:" file-name stamp record-id node-name))
                   (when (db/get-record-without-body (db/get-file-id file-name) stamp record-id)
                     (dorun (pmap
                              #(try
                                (catch Throwable t
                                  (update % file-name stamp record-id)
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

           (GET (str server-path "/have/:file-name")
                {:keys [headers params body server-name] :as request}
             ;(timbre/info "/have command received:" (get-remote-address request) (:file-name params))
             (let [{file-name :file-name} params
                   file (db/get-file file-name)]
                 (->
                   (ok (if (and file (not (:deleted file)) (pos? (:num-records file))) "YES" "NO"))
                   (content-type "text/plain; charset=UTF-8"))))

           (GET (str server-path "/head/:file-name/:range")
                {:keys [headers params body server-name] :as request}
             ;(timbre/info "/head command received:" (get-remote-address request) (:file-name params) (:range params))
             (process-get-command request true))

           (GET (str server-path "/get/:file-name/:range")
                {:keys [headers params body server-name] :as request}
             (timbre/info "/get command received:" (get-remote-address request) (:file-name params) (:range params))
             (process-get-command request false))

           (GET (str server-path "/get/:file-name/:range/:record-id")
                {:keys [headers params body server-name] :as request}
             (timbre/info "/get command received:" (get-remote-address request) (:file-name params) (:range params) (:record-id params))
             (process-get-command request false))

           (GET (str server-path "/recent/:range")
                {:keys [headers params body server-name] :as request}
               (timbre/info "/recent command received:" (get-remote-address request) (:range params))
               (let [{:keys [range]} params]
                   (if (valid-range? range)
                       (let [{range :range} params
                             match (re-find #"^([0-9]*)-([0-9]*)$" range)
                             start (try (Long/parseLong (if match (nth match 1) (nth (re-find #"^([0-9]*)$" range) 1))) (catch Throwable t nil))
                             end   (try (Long/parseLong (if match (nth match 2) (nth (re-find #"^([0-9]*)$" range) 1))) (catch Throwable t nil))
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

           (GET (str server-path "/active-nodes") request
             (->
               (ok (apply str (sort (map #(str % "\n") @active-nodes))))
               (content-type "text/plain; charset=UTF-8")))

           (GET (str server-path "/search-nodes") request
             (->
               (ok (apply str (sort (map #(str % "\n") @search-nodes))))
               (content-type "text/plain; charset=UTF-8")))

           (GET (str server-path "/known-nodes") request
             (->
               (ok (apply str (sort (map #(str % "\n") (map :node-name (db/get-all-nodes))))))
               (content-type "text/plain; charset=UTF-8")))

           (GET (str server-path "/files") request
             (->
               (ok (apply str (sort (map #(str % "\n") (map :file-name (db/get-all-files))))))
               (content-type "text/plain; charset=UTF-8")))



           ;;;;;;;;;;;
           ; Gateway ;
           ;;;;;;;;;;;

           (POST "/api/thread"
                request
             (timbre/debug request)
             (let [{:keys [thread-title page-num page-size record-short-id]} (:params request)
                   ;page-num (Integer/parseInt page-num)
                   ;page-size (Integer/parseInt page-size)
                   file-id (db/get-file-id-by-thread-title thread-title)
                   file (db/get-file-by-id file-id)
                   results (doall (map
                             (fn [record]
                               (let [body (String. (:body record) "UTF-8")
                                     elements (->> (clojure.string/split body #"<>")
                                                   (map #(re-find #"^([a-zA-Z0-9]+):(.*)$" %))
                                                   (map #(do {(keyword (nth % 1)) (nth % 2)}))
                                                   (apply merge))
                                     elements (assoc elements :attach (true? (:attach elements)))]
                                 (-> record
                                     (assoc :body nil)
                                     (merge elements))))
                             (if (and record-short-id (pos? (count record-short-id)))
                               (db/get-records-in-file-by-short-id file-id record-short-id)
                               (db/get-records-on-page file-id page-size page-num))
                             ))
                   anchors (into [] (apply concat (map (fn [destnation]
                                  ;(timbre/info file-id destnation (apply str (db/get-anchors file-id destnation)))
                                  (db/get-anchors file-id destnation))
                                (map :record-short-id results))))]
               ;(timbre/info "anchors:" (apply str anchors))
               {:body {:num-posts (:num-records file)
                       :posts results
                       :anchors anchors}}))

           (GET "/api/threads"
                {:keys [headers params body server-name] :as request}
             (let [n (:n params)
                   n (if (zero? (count n)) nil n)]
               (remove #(or
                       (some #{(:file-name %)} known-corrupt-files)
                       (zero? (:num-records %)))
                       (if n
                         (db/get-files-with-limit (Integer/parseInt n))
                         (db/get-all-files)))))

           (GET "/status" request
             (let [total-size (reduce + (map :size (db/get-all-files)))]
               (->
                 (ok (str
                       "ファイルの数: " (db/count-all-files) "\n"
                       "レコードの数: " (db/count-all-records) "\n"
                       "削除されたレコードの数: " (db/count-all-deleted-records) "\n"
                       "キャッシュサイズ: " (int (/ total-size 1000000)) "MB\n\n"
                       "隣接ノード:\n"
                       (apply str (map #(str % "\n") (sort @active-nodes)))
                       "計" (count @active-nodes) "個" "\n"
                       "\n"
                       "探索ノード:\n"
                       (apply str (map #(str % "\n") (sort @search-nodes)))
                       "計" (count @search-nodes) "個"))
                 (content-type "text/plain; charset=UTF-8"))))

           (GET "/test/recent" request
             (do
               (->
                 (ok (apply str (pmap #(recent %1 "0-") @search-nodes)))
                 (content-type "text/plain; charset=UTF-8"))))

           (GET "/test/join" request
             (do
               (doall (pmap #(ping %1) initial-nodes))
               (doall (pmap #(join %1) initial-nodes))
               (doall (pmap #(join %1) @search-nodes))
               (->
                 (ok (str "#: " (count @active-nodes) "\n" @active-nodes))
                 (content-type "text/plain; charset=UTF-8"))))

           (GET "/test/bye" request
             (do
               (doall (pmap #(bye %1) @active-nodes))
               (->
                 (ok (str @active-nodes))
                 (content-type "text/plain; charset=UTF-8")))))
