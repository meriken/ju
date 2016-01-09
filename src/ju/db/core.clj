(ns ju.db.core
  (:require
    [clojure.java.jdbc :as sql]
    ;[conman.core :as conman]
    [environ.core :refer [env]]
    [mount.core :refer [defstate]]

    ; Meriken
    [taoensso.timbre :as timbre]
    [korma.core :refer :all]
    [korma.db :refer [defdb transaction create-db default-connection]]
    [ju.db.schema :as schema]
    [pandect.algo.md5 :refer :all]
    [clj-time.coerce])
  (:import [java.sql
            BatchUpdateException
            PreparedStatement]))



(comment
  (def pool-spec
    {:adapter    :hsqldb ;:mysql
     :init-size  1
     :min-idle   1
     :max-idle   4
     :max-active 32})

  (defn connect! []
    (let [conn (atom nil)]
      (conman/connect!
        conn
        (assoc
          pool-spec
          :jdbc-url (env :database-url)))
      conn))

  (defn disconnect! [conn]
    (conman/disconnect! conn))

  (defstate ^:dynamic *db*
            :start (connect!)
            :stop (disconnect! *db*))

  (conman/bind-connection *db* "sql/queries.sql")

  (defn to-date [sql-date]
    (-> sql-date (.getTime) (java.util.Date.)))

  (extend-protocol jdbc/IResultSetReadColumn
    java.sql.Date
    (result-set-read-column [v _ _] (to-date v))

    java.sql.Timestamp
    (result-set-read-column [v _ _] (to-date v)))

  (extend-type java.util.Date
    jdbc/ISQLParameter
    (set-parameter [v ^PreparedStatement stmt idx]
      (.setTimestamp stmt idx (java.sql.Timestamp. (.getTime v)))))
  )



(defdb db schema/db-spec)

(defn java-pgobject-get-type  [obj] (.getType obj))
(defn java-pgobject-get-value  [obj] (.getValue obj))

(defn convert-citext-into-string
  [row]
  (let [k (keys row) v (vals row)]
    (zipmap k
            (map #(if (and (= (Class/forName "org.postgresql.util.PGobject") (type %1))
                           (= (java-pgobject-get-type %1) "citext"))
                   (java-pgobject-get-value %1)
                   %1)
                 v))))

(defn slurp-bytes
  "Slurp the bytes from a slurpable thing"
  [x]
  (with-open [out (java.io.ByteArrayOutputStream.)]
    (clojure.java.io/copy (clojure.java.io/input-stream x) out)
    (.toByteArray out)))

(defn convert-blobs
  [row]
  (let [k (keys row) v (vals row)]
    (zipmap k
            (map #(if (= (Class/forName "org.hsqldb.jdbc.JDBCBlobClient") (type %1))
                   (slurp-bytes (.getBinaryStream %1))
                   %1)
                 v))))

(defn- normalize-keys
  "Invoke in a `defentity` form to automatically translate between
  dash-separated keywords and underscore-separated DB column names."
  [ent]
  (let [map-keys (fn [f m] (reduce-kv #(assoc %1 (f %2) %3) {} m))
        underscore-kw (comp keyword #(clojure.string/replace % "-" "_") name)
        dasherize-kw  (comp keyword #(clojure.string/replace % "_" "-") name)]
    (-> ent
        (korma.core/prepare   (partial map-keys underscore-kw))
        (korma.core/transform (partial map-keys dasherize-kw)))))

;(defentity nodes (transform convert-citext-into-string) (normalize-keys))
(defentity nodes (normalize-keys))
(defentity files (normalize-keys))
(defentity records (transform convert-blobs) (normalize-keys))
(defentity update_commands (normalize-keys))
(defentity anchors (normalize-keys))



(defn shutdown
  []
  (try
    ; (clojure.java.jdbc/db-do-commands schema/db-spec "CHECKPOINT SYNC")
    ; (clojure.java.jdbc/db-do-commands schema/db-spec "SET TRACE_LEVEL_SYSTEM_OUT 0; SHUTDOWN")
    (clojure.java.jdbc/db-do-commands schema/db-spec "SHUTDOWN")

    (catch Throwable t
      (timbre/debug "shutdown: Unexpected exception:" (str t)))))

(def count-keyword
  (fn [result]
    (cond
      ((keyword "count(*)") result)
      ((keyword "count(*)") result)

      (:c1 result)
      (:c1 result)

      (:count result)
      (:count result)

      :else
      nil)))

(defn byte-array?
  [a]
  (= (Class/forName "[B") (type a)))



(defn known-node? [node-name]
  (pos? (count (select nodes (where {:node_name node-name})))))

(defn add-node [node-name]
  (transaction
    (if-not (known-node? node-name)
      (insert nodes
              (values {:node-name node-name
                       :time-created (clj-time.coerce/to-sql-time (clj-time.core/now))})))))

(defn mark-node-as-active
  [node-name]
  (update
    nodes
    (set-fields {:time_active (clj-time.coerce/to-sql-time (clj-time.core/now))})
    (where {:node_name node-name})))

(defn mark-node-as-crawled
  [node-name]
  (update
    nodes
    (set-fields {:time_crawled (clj-time.coerce/to-sql-time (clj-time.core/now))})
    (where {:node_name node-name})))

(defn get-all-nodes
  []
  (select nodes))

(defn get-node
  [node-name]
  (first (select nodes
                 (where { :node_name node-name }))))



(defn get-file-id
  [file-name]
  (let [files (select files (where {:file_name file-name}))]
    (and
      files
      (pos? (count files))
      (:id (first files)))))

(defn get-file
  [file-name]
  (let [files (select files (where {:file_name file-name}))]
    (and
      files
      (pos? (count files))
      (first files))))

(defn get-file-by-id
  [file-id]
  (let [files (select files (where {:id file-id}))]
    (and
      files
      (pos? (count files))
      (first files))))

(defn hexify [s]
  (apply str (map #(format "%02X" %) (.getBytes s "UTF-8"))))

(defn get-file-id-by-thread-title
  [thread-title]
  (let [file-name (str "thread_" (hexify thread-title))
        files (select files (where {:file_name file-name}))]
    (and
      files
      (pos? (count files))
      (:id (first files)))))

(defn add-file [file-name]
  (if-not (re-find #"^[a-zA-Z0-9]+_[a-zA-Z0-9_]+$" file-name)
    (throw (IllegalArgumentException. "Invalid file name.")))

  (transaction
    (if-not (get-file-id file-name)
      (insert files
              (values {:file-name file-name
                       :application (clojure.string/replace file-name "^[a-z]+_" "")
                       :time-created (clj-time.coerce/to-sql-time (clj-time.core/now))})))))

(defn get-all-files
  []
  (select files
          (order :time_updated :DESC)))

(defn get-files-with-limit
  [n]
  (select files
          (order :time_updated :DESC)
          (limit n)))

(defn count-all-files
  []
  (count-keyword (first (clojure.java.jdbc/query
                          ju.db.schema/db-spec
                          ["SELECT COUNT(*) FROM files WHERE deleted=FALSE AND num_records>0"]))))



(declare add-anchor)
(declare get-all-records)
(declare get-all-records-in-file)

(defn add-anchor-in-post
  [file-id body record-short-id]
  ;(timbre/debug "add-anchor-in-post" file-id (String. body "UTF-8") record-short-id)
  (let [elements (->> (clojure.string/split (String. body "UTF-8") #"<>")
                      (map #(re-find #"^([a-zA-Z0-9]+):(.*)$" %))
                      (map #(do {(keyword (nth % 1)) (nth % 2)}))
                      (apply merge))
        elements (assoc elements :attach (true? (:attach elements)))
        matches (and (:body elements) (re-seq #"&gt;&gt;[0-9a-f]{8}" (:body elements)))]
    ;(map #(do {:file-id (:file-id record) :source (:record-short-id record) :destination (clojure.string/replace % #"^&gt;&gt;" "")}) matches
    (doall (map #(add-anchor file-id record-short-id (clojure.string/replace % #"^&gt;&gt;" "")) matches))))

(defn search-for-anchors-in-file
  [file-id]
  (let [results (doall (apply concat (map
                                       (fn [record]
                                         (add-anchor-in-post (:file-id record) (:body record) (:record-short-id record)))
                                       (get-all-records-in-file file-id))))]
    results))

(defn search-for-anchors
  []
  (let [results (doall (apply concat (map
                                       (fn [record]
                                         (add-anchor-in-post (:file-id record) (:body record) (:record-short-id record)))
                                       (get-all-records))))]
    results))

(defn add-record [file-id stamp record-id body]
  (if (zero? (count (select files (where {:id file-id}))))
    (throw (IllegalArgumentException. "Invalid file ID.")))
  (if (< stamp 1000000000)
    (throw (IllegalArgumentException. "Invalid stamp.")))
  (if-not (= (md5 body) record-id)
    (throw (IllegalArgumentException. (str "Invalid record ID:" record-id))))

  (transaction
    (when (zero? (count (select records (where { :file_id file-id :stamp stamp :record_id record-id }))))
      (insert records
              (values {:file_id file-id
                       :stamp stamp
                       :record_id record-id
                       :record_short_id (second (re-find #"^([0-9a-f]{8})" record-id))
                       :body body
                       :time-created (clj-time.coerce/to-sql-time (clj-time.core/now))
                       :size (+ 10 2 32 2 (count body) 1)}))
      (add-anchor-in-post file-id body (second (re-find #"^([0-9a-f]{8})" record-id))))))

(defn get-all-records-in-file
  [file-id]
  (select records
          (where {:file_id file-id})
          (order :stamp :ASC)))

(defn get-records-in-file-by-short-id
  [file-id short-id]
  (select records
          (where {:file_id file-id
                  :record_short_id short-id})))

(defn get-record-in-file-by-short-id
  [file-id short-id]
  (first
    (select records
            (where {:file_id file-id
                    :record_short_id short-id})
            (limit 1))))

(defn get-records-on-page
  [file-id page-size page-num]
  (let [num-records (:num-records (get-file-by-id file-id))
        num-pages (+ (quot num-records page-size) (if (pos? (rem num-records page-size)) 1 0))
        record-offset (- num-records page-size (* page-num page-size))
        record-offset (if (neg? record-offset) 0 record-offset)]
    (select records
          (where {:file_id file-id})
          (order :stamp :ASC)
          (offset record-offset)
          (limit page-size))))

(defn get-records-in-file-with-range
  [file-id start end]
  (cond
    (nil? end)   (select records (where {:file_id file-id}) (where {:stamp [>= start]})                            (order :stamp :ASC))
    (nil? start) (select records (where {:file_id file-id})                              (where {:stamp [<= end]}) (order :stamp :ASC))
    :else         (select records (where {:file_id file-id}) (where {:stamp [>= start]}) (where {:stamp [<= end]}) (order :stamp :ASC))))

(defn count-records-in-file
  [file-id]
  (count-keyword (first (clojure.java.jdbc/query
                          ju.db.schema/db-spec
                          ["SELECT COUNT(*) FROM records WHERE file_id=? AND deleted=FALSE" file-id]))))

(defn count-all-records
  []
  (count-keyword (first (clojure.java.jdbc/query
                          ju.db.schema/db-spec
                          ["SELECT COUNT(*) FROM records WHERE deleted=FALSE"]))))

(defn count-deleted-records-in-file
  [file-id]
  (count-keyword (first (clojure.java.jdbc/query
                          ju.db.schema/db-spec
                          ["SELECT COUNT(*) FROM records WHERE file_id=? AND deleted=TRUE" file-id]))))

(defn count-all-deleted-records
  []
  (count-keyword (first (clojure.java.jdbc/query
                          ju.db.schema/db-spec
                          ["SELECT COUNT(*) FROM records WHERE deleted=TRUE"]))))

(defn get-record-without-body
  [file-id stamp record-id]
  (nth
    (select records
            (fields :id :file_id :stamp :record_id :record_short_id :time_created :size)
            (where {:file_id file-id :stamp stamp :record_id record-id}))
    0
    nil))

(defn get-all-records-in-file-without-bodies
  [file-id]
  (select records
          (fields :id :file_id :stamp :record_id :record_short_id :time_created :size)
          (where {:file_id file-id})))

(defn get-all-records-without-bodies
  []
  (select records
          (fields :id :file_id :stamp :record_id :record_short_id :time_created :size)))

(defn get-all-records
  []
  (select records))



(defn update-file
  [file-id]
  (update
    files
    (set-fields {:num_records (count-records-in-file file-id)})
    (where {:id file-id}))
  (update
    files
    (set-fields {:num_deleted_records (count-deleted-records-in-file file-id)})
    (where {:id file-id}))
  (update
    files
    (set-fields {:time_updated (try
                                 (java.sql.Timestamp. (* 1000 (:stamp
                                                        (nth
                                                          (select records
                                                                  (where {:file_id file-id :deleted false})
                                                                  (order :stamp :DESC)
                                                                  (limit 1))
                                                          0
                                                          nil))))
                                 (catch Throwable t nil))})
    (where {:id file-id}))
  (update
    files
    (set-fields {:size (reduce + (map :size (select records
                                               (where {:file_id file-id})
                                               (fields :size))))})
    (where {:id file-id})))

(defn update-all-files
    []
  (dorun
    (map
      #(update-file %)
      (map :id (select files (fields :id))))))



(defn process-update-command
  [file-name stamp record-id]
  (if-not (re-find #"^[a-zA-Z]+_[a-zA-Z0-9_]+$" file-name)
    (throw (IllegalArgumentException. "Invalid file name.")))
  (if (< stamp 1000000000)
    (throw (IllegalArgumentException. "Invalid stamp.")))
  (if-not (re-find #"^[a-f0-9]{32}$" record-id)
    (throw (IllegalArgumentException. (str "Invalid record ID: " record-id))))

  (transaction
    (let [existing-update-command (try (first (select update_commands (where {:file_name file-name :stamp stamp :record_id record-id}))) (catch Throwable _ nil))]
      (if (nil? existing-update-command)
      (insert update_commands
              (values {:file_name file-name
                       :stamp stamp
                       :record_id record-id}))))))

(defn get-all-update-commands
  []
  (select update_commands
          (order :stamp :DESC)))

(defn get-update-commands-with-range
  [start end]
  (cond
    (nil? end)   (select update_commands (where {:stamp [>= start]})                            (order :stamp :DESC))
    (nil? start) (select update_commands                              (where {:stamp [<= end]}) (order :stamp :DESC))
    :else         (select update_commands (where {:stamp [>= start]}) (where {:stamp [<= end]}) (order :stamp :DESC))))



(defn add-anchor
  [file-id source destination]
  ;(timbre/debug "add-anchor" file-id source destination)
  (transaction
    (when (zero? (count (select anchors (where { :file_id file-id :source source :destination destination}))))
      (insert anchors
              (values {:file_id file-id
                       :source source
                       :destination destination})))))

(defn get-anchors
  [file-id destination]
  (select anchors
          (where {:file_id file-id :destination destination})))

