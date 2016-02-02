(ns ju.util
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
