(ns ju.util
  (:require [ju.param :as param])
  (:import (java.net URLEncoder)
           (java.nio.file Files)
           (java.security MessageDigest)))



(defn try-times*
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
  [n & body]
  `(try-times* ~n (fn [] ~@body)))



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

(defn hexify [s]
  (apply str (map #(format "%02X" %) (.getBytes s "UTF-8"))))

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



(defn md5
  [s]
  (let [md (MessageDigest/getInstance "MD5")]
    (.update md (cond
                  (nil? s) (.getBytes "" "UTF-8")
                  (string? s) (.getBytes s "UTF-8")
                  :else s))
    (apply str (map #(format "%02x" %) (.digest md)))))

(defn- nth-unsigned
  [a i]
  (let [value (int (nth a i 0))]
    (if (< value 0) (+ 256 value) value)))

(defn jane-md5
  [binary-array]
  (let [digest (let [m (java.security.MessageDigest/getInstance "MD5")]
                 (.reset m)
                 (.update m binary-array)
                 (.digest m))]
    (apply str (map #(.charAt
                      "0123456789ABCDEFGHIJKLMNOPQRSTUV"
                      (bit-and
                        (bit-shift-right
                          (bit-or
                            (bit-shift-left
                              (nth-unsigned digest (inc (quot (* % 5) 8))) 8)
                            (nth-unsigned digest (quot (* % 5) 8)))
                          (rem (* % 5) 8))
                        31))
                    (range 0 26)))))

(defn valid-node?
         [node-name]
         (not (some #{node-name} (into #{} param/blocked-nodes))))



(import 'org.apache.commons.codec.digest.Crypt)
(def tripcode-encoding "windows-31j")
(defn generate-tripcode
  [key]
  (let
    [modified-key (-> key
                      (clojure.string/replace "&r" "")
                      (clojure.string/replace "\"" "&quot;")
                      (clojure.string/replace "<" "&lt;")
                      (clojure.string/replace ">" "&gt;")
                      (clojure.string/replace "＃" "#"))
     key-byte-array (.getBytes modified-key tripcode-encoding)
     key-byte-array-original-length (count key-byte-array)
     key-byte-array (java.util.Arrays/copyOf key-byte-array (+ (* (count key-byte-array) 2) 16))
     _ (dorun (map (fn [[target replacement]]
                     (let [target-bytes (.getBytes target tripcode-encoding)
                           replacement-bytes (.getBytes replacement tripcode-encoding)]
                       (dorun (loop [[pos & tail] (range (- (count key-byte-array) (count target-bytes)))]
                                (cond
                                  (zero? (count (remove true? (map #(= (get key-byte-array (+ pos %)) (get target-bytes %)) (range (count target-bytes))))))
                                  (do
                                    (dorun (for [i (range (- key-byte-array-original-length (count target-bytes) pos))]
                                             (do
                                               ;(print pos " "  i " " (+ pos i (count replacement-bytes)) "\n")
                                               (aset-byte
                                                 key-byte-array
                                                 (+ pos i (count replacement-bytes))
                                                 (get key-byte-array (+ pos i (count target-bytes)))
                                                 ))))
                                    (dorun (for [j (range (count replacement-bytes))]
                                             (aset-byte key-byte-array (+ pos j) (get replacement-bytes j)))))

                                  (pos? (count tail))
                                  (recur tail))))))
                   [["●" "○"]
                    ["◆" "◇"]
                    ["★" "☆"]
                    ["管理" "”管理”"]
                    ["削除" "”削除”"]
                    ["復帰" "”復帰”"]
                    ["山崎渉" "fusianasan"]]))
     actual-key-byte-array-length ((fn count-length
                                     [pos]
                                     (if (or (= (get key-byte-array pos) (byte 0))
                                             (>= pos (count key-byte-array))
                                             )
                                       pos
                                       (count-length (inc pos))))
                                    0
                                    )
     modified-key (String. (java.util.Arrays/copyOf key-byte-array actual-key-byte-array-length) "windows-31j")
     modified-key-0x80 (clojure.string/replace modified-key #"([÷ムо〝園橿朽劇項死準逗操逐凍楳斧摩沃凰噫它怙捩梳麾烙痼窶縲艢蛟諤轢閠騾黴塚蕫兤祥]).*$" "$1")

     key-byte-array-for-salt (.getBytes (str modified-key "H.") "windows-31j")
     salt-chars "/0123456789ABCDEFGABCDEFGHIJKLMNOPQRSTUVWXYZabcdefabcdefghijklmnopqrstuvwxyz"
     salt-map (apply merge (map #(do {(+ 47 (int %2)) (str %1)}) salt-chars (range (count salt-chars))))
     salt0 (get salt-map (get key-byte-array-for-salt 1) ".")
     salt1 (get salt-map (get key-byte-array-for-salt 2) ".")
     salt (str salt0 salt1)]
    (cond
      (< (count (.getBytes key tripcode-encoding)) 12)
      (subs (Crypt/crypt (.getBytes modified-key-0x80 tripcode-encoding) salt) 3))

    ))

(defn check-tripcodes
  []
  (dorun (map (fn [[tripcode key]]
                (print (str "◆" tripcode " #" key)
                       (if (= tripcode (generate-tripcode key))
                         "[PASS]\n"
                         (str "[FAIL: ◆" (generate-tripcode key) "]\n"))))
              [["WBRXcNtpf." "12345678"]
               ["WBRXcNtpf." "ｱ23ｴｵｶｷｸ"]
               ["XlUiP8mHCU" "ｱｲｳｴｵｶｷｸ"]
               ["QH.zpPwVew" "ﾁﾂﾃﾄﾅﾆﾇﾈ"]
               ["QH.zpPwVew" "AﾂﾃDEFGH"]
               ["l.vPS4V11w" "ABCDEFGH"] ; This may be wrong."aOLjRoi1zs"

               ["RMZ/x4umbQ" "'()*+,-."]
               ["RMZ/x4umbQ" "ｧ()ｪｫｬｭｮ"]
               ["RMZ/x4umbQ" "ｧｨｩｪｫｬｭｮ"]
               ["5637936436" "'!+$#-.)"]
               ["5637936436" "ｧ!+､｣ｭｮｩ"]
               ["5637936436" "ｧ｡ｫ､｣ｭｮｩ"]

               ["IshBgEJs9E" "&rｷR;YFj"]
               ["IshBgEJs9E" "ｷR;YFj"]
               ["7778933162" "ｦrｷR;YFj"]
               ["7778933162" "&&rrｷR;YFj"]

               ["gt1azVccY2" "\""]
               ["gt1azVccY2" "&quot;"]
               ["1771265006" "`ｻ静<"]
               ["1771265006" "`ｻ静&lt;"]
               ["4097306092" "通:U>"]
               ["4097306092" "通:U&gt;"]

               ["lsQhRnmqAY" "+ﾅ檮尞彈"]
               ["lsQhRnmqAY" "+ﾅ栫h削除"]
               ["hbqILjsyyc" "虫R崎渉1"]
               ])))