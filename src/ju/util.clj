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
