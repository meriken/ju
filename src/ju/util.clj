(ns ju.util
  (:require [ju.param :as param]
            [taoensso.timbre :as timbre])
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

(defn hex-string-to-byte-array
  [s]
  (into-array Byte/TYPE
              (map (fn [[x y]]
                     (unchecked-byte (Integer/parseInt (str x y) 16)))
                   (partition 2 s))))

(defn unhexify [s]
  (let [bytes (hex-string-to-byte-array s)]
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

(def nec-character-map
  (apply merge (map (fn [pair] {(first pair) (hex-string-to-byte-array (second pair))})
       (apply concat (map (fn [[start char-string]]
                      (map (fn [c i] (list (str c) (format "%04x" i)))
                           char-string
                           (range start (+ start (count char-string)))))
              [[0xed40 "纊褜鍈銈蓜俉炻昱棈鋹曻彅丨仡仼伀伃伹佖侒侊侚侔俍偀倢俿倞偆偰偂傔僴僘兊兤冝冾凬刕劜劦勀勛匀匇匤卲厓厲叝﨎咜咊咩哿喆坙坥垬埈埇﨏"]
               [0xed80 "塚增墲夋奓奛奝奣妤妺孖寀甯寘寬尞岦岺峵崧嵓﨑嵂嵭嶸嶹巐弡弴彧德忞恝悅悊惞惕愠惲愑愷愰憘戓抦揵摠撝擎敎昀昕昻昉昮昞昤晥晗晙晴晳暙暠暲暿曺朎朗杦枻桒柀栁桄棏﨓楨﨔榘槢樰橫橆橳橾櫢櫤毖氿汜沆汯泚洄涇浯涖涬淏淸淲淼渹湜渧渼溿澈澵濵瀅瀇瀨炅炫焏焄煜煆煇凞燁燾犱"]
               [0xee40 "犾猤猪獷玽珉珖珣珒琇珵琦琪琩琮瑢璉璟甁畯皂皜皞皛皦益睆劯砡硎硤硺礰礼神祥禔福禛竑竧靖竫箞精絈絜綷綠緖繒罇羡羽茁荢荿菇菶葈蒴蕓蕙"]
               [0xee80 "蕫﨟薰蘒﨡蠇裵訒訷詹誧誾諟諸諶譓譿賰賴贒赶﨣軏﨤逸遧郞都鄕鄧釚釗釞釭釮釤釥鈆鈐鈊鈺鉀鈼鉎鉙鉑鈹鉧銧鉷鉸鋧鋗鋙鋐﨧鋕鋠鋓錥錡鋻﨨錞鋿錝錂鍰鍗鎤鏆鏞鏸鐱鑅鑈閒隆﨩隝隯霳霻靃靍靏靑靕顗顥飯飼餧館馞驎髙髜魵魲鮏鮱鮻鰀鵰鵫鶴鸙黑"]
               [0xeeef "ⅰⅱⅲⅳⅴⅵⅶⅷⅸⅹ¬¦＇＂"]
               ])))))

(defn get-byte-array-for-tripcode-key
  [key]
  (byte-array
    (mapcat
      seq
      (map
        #(cond
          (get nec-character-map % nil) (get nec-character-map %)
          :else (.getBytes % tripcode-encoding))
        (map str key)))))

(defn generate-tripcode
  [key]
  ;(timbre/debug "key:              " (apply str (map #(format "%02X " %) (get-byte-array-for-tripcode-key key))))
  (let
    [modified-key (-> key
                      (clojure.string/replace "&r" "")
                      (clojure.string/replace "\"" "&quot;")
                      (clojure.string/replace "<" "&lt;")
                      (clojure.string/replace ">" "&gt;")
                      (clojure.string/replace "＃" "#"))
     modified-key key
     key-byte-array (get-byte-array-for-tripcode-key modified-key )
     key-byte-array-original-length (count key-byte-array)
     key-byte-array (java.util.Arrays/copyOf key-byte-array (+ (* (count key-byte-array) 2) 16))
     _ (comment dorun (map (fn [[target replacement]]
                     (let [target-bytes (get-byte-array-for-tripcode-key target )
                           replacement-bytes (get-byte-array-for-tripcode-key replacement)]
                       (if (or
                             (not (re-find (re-pattern target) replacement))
                             (loop [[pos & tail] (range key-byte-array-original-length)]
                                (cond
                                  (every? true? (map
                                                  #(or (and (pos? %) (>= (+ pos %) key-byte-array-original-length))
                                                       (= (get key-byte-array (+ pos %)) (get replacement-bytes %)))
                                                  (range (count replacement-bytes))))
                                  false

                                  (pos? (count tail))
                                  (recur tail)

                                  :else
                                  true)))
                       (dorun (loop [[pos & tail] (range key-byte-array-original-length)]
                                (cond
                                  (zero? (count (remove true? (map #(= (get key-byte-array (+ pos %)) (get target-bytes %)) (range (count target-bytes))))))
                                  (do
                                    (dorun (for [i (range (- key-byte-array-original-length (count target-bytes) pos))]
                                             (do
                                               (aset-byte
                                                 key-byte-array
                                                 (+ pos i (count replacement-bytes))
                                                 (get key-byte-array (+ pos i (count target-bytes)))
                                                 ))))
                                    (dorun (for [j (range (count replacement-bytes))]
                                             (aset-byte key-byte-array (+ pos j) (get replacement-bytes j))
                                             )))

                                  (pos? (count tail))
                                  (recur tail)))))))
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
                                    0)
     key-byte-list (reverse (into () (java.util.Arrays/copyOf key-byte-array actual-key-byte-array-length)))
     result (split-with #(not (= % (byte -128))) key-byte-list)
     key-byte-list-0x80 (concat (first result) (if (empty? (second result)) '() (list (byte -128))))

     key-byte-array-for-salt (byte-array (mapcat seq (list key-byte-list (map byte "H."))))
     salt-chars "/0123456789ABCDEFGABCDEFGHIJKLMNOPQRSTUVWXYZabcdefabcdefghijklmnopqrstuvwxyz"
     salt-map (apply merge (map #(do {(+ 47 (int %2)) (str %1)}) salt-chars (range (count salt-chars))))
     salt (str
            (get salt-map (get key-byte-array-for-salt 1) ".")
            (get salt-map (get key-byte-array-for-salt 2) "."))]
    ;(timbre/debug "modified-key-0x80:" (apply str (map #(format "%02X " %) (get-byte-array-for-tripcode-key modified-key-0x80))))
    (cond
      (< (count (get-byte-array-for-tripcode-key key)) 12)
      (subs (Crypt/crypt (byte-array key-byte-list-0x80) salt) 3))

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
               ["aOLjRoi1zs" "ABCDEFGH"]

               ["RMZ/x4umbQ" "'()*+,-."]
               ["RMZ/x4umbQ" "ｧ()ｪｫｬｭｮ"]
               ["RMZ/x4umbQ" "ｧｨｩｪｫｬｭｮ"]
               ["5637936436" "'!+$#-.)"]
               ["5637936436" "ｧ!+､｣ｭｮｩ"]
               ["5637936436" "ｧ｡ｫ､｣ｭｮｩ"]

               ["7778933162" "&rｷR;YFj"];◆IshBgEJs9E ;◆7778933162 http://hanabi.2ch.net/test/read.cgi/qa/1449922222/536
               ["IshBgEJs9E" "ｷR;YFj"]
               ["7778933162" "ｦrｷR;YFj"]
               ["7778933162" "&&rrｷR;YFj"]

               ["4eqVTkonjE" "\""];◆gt1azVccY2 ;◆4eqVTkonjE http://hanabi.2ch.net/test/read.cgi/qa/1449922222/525
               ["gt1azVccY2" "&quot;"]
               ["S6SGQNQ5.s" "`ｻ静<"];◆1771265006 ;◆S6SGQNQ5.s http://hanabi.2ch.net/test/read.cgi/qa/1449922222/526
               ["1771265006" "`ｻ静&lt;"]
               ["4097306092" "通:U>"]
               ["4097306092" "通:U&gt;"]

               ["s8X90K5ceE" "●"];◆GwiVxeuT5c ;◆s8X90K5ceE http://hanabi.2ch.net/test/read.cgi/qa/1449922222/524
               ["GwiVxeuT5c" "○"];OK
               ["gqRrL0OhYE" "◆"]
               ["gqRrL0OhYE" "◇"]
               ["G8Jw4.nqFk" "★"]
               ["G8Jw4.nqFk" "☆"]
               ["u2YjtUz8MU" "＃"];OK
               ["u2YjtUz8MU" "#"]
               ["XKSnTxcTbA" "管理"];◆..M7CxRsnk http://hanabi.2ch.net/test/read.cgi/qa/1449922222/523
               ["XKSnTxcTbA" "”管理”"]
               ["172VC7723I" "削除"]
               ["172VC7723I" "”削除”"]
               ["F4vmrUK4pg" "復帰"]
               ["F4vmrUK4pg" "”復帰”"]
               ["M2TLe2H2No" "山崎渉"]
               ["M2TLe2H2No" "fusianasan"]

               ["6ZmSz0zwL2" "÷×＋－"];◆QQnk8rTtk6 ;http://hanabi.2ch.net/test/read.cgi/qa/1449922222/538
               ["6ZmSz0zwL2" "÷うんこ"]
               ["AOGu5v68Us" "ムスカ"]
               ["AOGu5v68Us" "ムーミン"]

               ["Kpoz.PjwKU" "krMfメ彗"]
               ["Kpoz.PjwKU" "krMfメ嫗"]
               ["d23M6xVMa." "=ﾑwT早浣"]
               ["d23M6xVMa." "=ﾑwT早椡"]
               ["aJx2.8B7ls" "ａ坩7CU|"]
               ["aJx2.8B7ls" "ａ勸7CU|"]
               ["vQ/eO6pbUM" "E0孖ﾇ理拭"];◆..M7CxRsnk ;http://hanabi.2ch.net/test/read.cgi/qa/1449922222/522
               ["vQ/eO6pbUM" "E0增h管理"]
               ["lsQhRnmqAY" "+ﾅ檮尞彈"]
               ["lsQhRnmqAY" "+ﾅ栫h削除"]
               ["uxO1D1zfT." "封恚A欲["]
               ["uxO1D1zfT." "普h復帰”"]
               ["hbqILjsyyc" "虫R崎渉1"]
               ["hbqILjsyyc" "断usiana"]

               ["siuX2W5X5Q" "a＃eＸ.o"]
               ["4297602876" "瘁覇Ｘ.o"]
               ["d6yXdw5r52" "d＃#ｼﾀu!"]
               ["6089931596" "艨肇ｼﾀu!"]


               ])))