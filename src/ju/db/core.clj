(ns ju.db.core
  (:require
    [clojure.java.jdbc :as sql]
    [clojure.stacktrace]
    ;[conman.core :as conman]
    [environ.core :refer [env]]
    [mount.core :refer [defstate]]

    ; Meriken
    [taoensso.timbre :as timbre]
    [korma.core :refer :all]
    [korma.db :refer [defdb transaction create-db default-connection]]
    [ju.util :refer :all]
    [ju.db.schema :as schema]
    [clj-time.coerce])
  (:import [java.sql
            ;BatchUpdateException
            PreparedStatement]))



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
(defentity images (transform convert-blobs) (normalize-keys))
(defentity blocked_records (normalize-keys))
(defentity update_commands (normalize-keys))
(defentity anchors (normalize-keys))
(defentity file_tags (normalize-keys))



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
  (pos? (count (select nodes (fields :id) (where {:node_name node-name})))))

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

(defn delete-node
  [node-name]
  (delete
    nodes
    (where {:node_name node-name})))


(defn get-file-id
  [file-name]
  (let [files (select files (fields :id) (where {:file_name file-name}))]
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

(defn get-file-by-thread-number
  [thread-number]
  (let [ts (java.sql.Timestamp. (* (- (Long/parseLong thread-number) (* 9 60 60)) 1000))
        files (select files (where {:time_first_post ts}))]
    (if (and
          files
          (pos? (count files)))
      (first files))))

(defn get-file-id-by-thread-title
  [thread-title]
  (let [file-name (str "thread_" (hexify thread-title))
        files (select files (fields :id) (where {:file_name file-name}))]
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

(defn really-delete-file
  [file-id]
  (transaction
    (delete files (where {:id file-id}))
    (delete records (where {:file_id file-id}))
    (delete anchors (where {:file_id file-id}))))

(defn add-suggested-tags
  [file-id tags]
  (let [tag-string (:suggested-tags (get-file-by-id file-id))
        tags (if tag-string
               (clojure.set/union
                 (into #{} (clojure.string/split tag-string #" +"))
                 (into #{} tags))
               tags)
        tag-string (clojure.string/replace
                     (apply str (map #(str % " ") tags))
                     #" +$" "")
        tag-string (if (= tag-string "") nil tag-string)]
    (update files
            (where {:id file-id})
            (set-fields {:suggested_tags tag-string}))))



(defn add-file-tag [file-id tag-string]
  (if (re-find #"[ 　<>&]" tag-string)
    (throw (IllegalArgumentException. "Invalid tag string")))

  (transaction
    (if (zero? (count (select file_tags (fields :id) (where {:file_id file-id :tag_string tag-string}))))
      (insert file_tags
              (values {:file_id file-id
                       :tag_string tag-string
                       :time-created (clj-time.coerce/to-sql-time (clj-time.core/now))})))))

(defn get-tags-for-file
  [file-id]
  (select file_tags
          (where {:file_id file-id})))

(defn update-tags-for-file
  [file-id new-tags]
  (transaction
    (delete file_tags
            (where {:file_id file-id}))
    (dorun (map #(add-file-tag file-id %)
                new-tags))))

(defn get-files-with-tag
  [tag-string]
  (apply concat
    (map (fn [file-tag]
           (select files
                   (where {:id (:file-id file-tag)})))
         (select file_tags
                 (where {:tag_string tag-string})))))



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

(defn add-record [file-id stamp record-id body deleted dat-file-line suffix origin remote-address]
  (if (zero? (count (select files (fields :id) (where {:id file-id}))))
    (throw (IllegalArgumentException. "Invalid file ID.")))
  (if (< stamp 1000000000)
    (throw (IllegalArgumentException. "Invalid stamp.")))
  (if-not (= (md5 body) record-id)
    (throw (IllegalArgumentException. (str "Invalid record ID: " record-id))))

  (try-times
    5
    (transaction
      (when (zero? (count (select records (fields :id) (where { :file_id file-id :stamp stamp :record_id record-id }))))
        (insert records
                (values {:file_id file-id
                         :stamp stamp
                         :record_id record-id
                         :record_short_id (second (re-find #"^([0-9a-f]{8})" record-id))
                         :body body
                         :time_created nil ; dirty flag ; (clj-time.coerce/to-sql-time (clj-time.core/now))
                         :size (+ 10 2 32 2 (count body) 1)
                         :deleted (if deleted true false)
                         :dat_file_line dat-file-line
                         :suffix suffix
                         :origin origin
                         :remote_address remote-address}))
        (add-anchor-in-post file-id body (second (re-find #"^([0-9a-f]{8})" record-id)))))))

(defn get-all-records-in-file
  [file-id]
  (select records
          (where {:file_id file-id
                  :deleted false})
          (order :stamp :ASC)))

(defn get-records-in-file-by-short-id
  [file-id short-id]
  (select records
          (where {:file_id file-id
                  :record_short_id short-id
                  :deleted false})))

(defn get-new-records-in-file
  [file-id time-last-accessed]
  (select records
          (where {:file_id file-id
                  :stamp [> time-last-accessed]
                  :deleted false})
          (order :stamp :ASC)))

(defn get-records-by-short-id
  [short-id]
  (select records
          (where {:record_short_id short-id
                  :deleted false})))

(defn get-deleted-records-by-short-id
  [short-id]
  (select records
          (where {:record_short_id short-id
                  :deleted true})))

(defn get-record-by-id
  [id]
  (nth
    (select records (where {:id id
                            :deleted false}))
    0 nil))

(defn get-record-in-file-by-record-id
  [file-id record-id]
  (nth
    (select records (where {:file_id file-id :record_id record-id :deleted false}))
    0 nil))

(defn get-record-by-record-id
  [record-id]
  (nth
    (select records (where {:record_id record-id :deleted false}))
    0 nil))

(defn get-record-in-file-by-short-id
  [file-id short-id]
  (nth
    (select records
            (where {:file_id file-id
                    :record_short_id short-id
                    :deleted false})
            (limit 1))
    0 nil))

(defn get-record-by-short-id
  [short-id]
  (nth
    (select records
            (where {:record_short_id short-id
                    :deleted false})
            (limit 1))
    0 nil))

(defn get-records-on-page
  [file-id page-size page-num]
  ;(timbre/debug "get-records-on-page" file-id page-size page-num)
  (let [num-records (:num-records (get-file-by-id file-id))
        num-pages (+ (quot num-records page-size) (if (pos? (rem num-records page-size)) 1 0))
        record-offset (- num-records page-size (* page-num page-size))
        record-offset (if (neg? record-offset) 0 record-offset)]
    ; The first version is faster on MySQL.
    (if (= (:subprotocol ju.db.schema/db-spec) "mysql")
      (->> (select records
                   (where {:file_id file-id
                           :deleted false
                           })
                   (fields :id :stamp))
           (sort-by #(:stamp %))
           (drop record-offset)
           (take (if (>= page-num (dec num-pages)) (- num-records (* (dec num-pages) page-size)) page-size))
           (map #(:id %))
           (map get-record-by-id))
      (select records
              (where {:file_id file-id
                      :deleted false})
              (order :stamp :ASC)
              (offset record-offset)
              (limit (if (>= page-num (dec num-pages)) (- num-records (* (dec num-pages) page-size)) page-size))))))

(defn get-records-in-file-with-range
  [file-id start end]
  (cond
    (nil? end)   (select records (where {:file_id file-id :deleted false}) (where {:stamp [>= start]})                            (order :stamp :ASC))
    (nil? start) (select records (where {:file_id file-id :deleted false})                              (where {:stamp [<= end]}) (order :stamp :ASC))
    :else         (select records (where {:file_id file-id :deleted false}) (where {:stamp [>= start]}) (where {:stamp [<= end]}) (order :stamp :ASC))))

(defn get-records-in-file-with-range-without-bodies
  [file-id start end]
  (cond
    (nil? end)
    (select records
            (fields :id :file_id :stamp :record_id :record_short_id :time_created :size :dat_file_line :suffix)
            (where {:file_id file-id :deleted false})
            (where {:stamp [>= start]})
            (order :stamp :ASC))

    (nil? start)
    (select records
            (fields :id :file_id :stamp :record_id :record_short_id :time_created :size :dat_file_line :suffix)
            (where {:file_id file-id :deleted false})
            (where {:stamp [<= end]})
            (order :stamp :ASC))

    :else
    (select records
            (fields :id :file_id :stamp :record_id :record_short_id :time_created :size :dat_file_line :suffix)
            (where {:file_id file-id :deleted false})
            (where {:stamp [>= start]})
            (where {:stamp [<= end]})
            (order :stamp :ASC))))

(defn count-records-in-file
  [file-id]
  (count-keyword (first (clojure.java.jdbc/query
                          ju.db.schema/db-spec
                          ["SELECT COUNT(*) FROM records WHERE file_id=? AND deleted=FALSE" file-id]))))

(defn count-new-records-in-file
  [file-id time-last-accessed]
  (count-keyword (first (clojure.java.jdbc/query
                          ju.db.schema/db-spec
                          ["SELECT COUNT(*) FROM records WHERE file_id=? AND stamp>? AND deleted=FALSE" file-id time-last-accessed]))))

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
            (fields :id :file_id :stamp :record_id :record_short_id :time_created :size :dat_file_line :suffix)
            (where {:file_id file-id :stamp stamp :record_id record-id :deleted false}))
    0
    nil))

(defn get-all-active-and-deleted-records-in-file-without-bodies
  [file-id]
  (select records
          (fields :id :file_id :stamp :record_id :record_short_id :time_created :size :dat_file_line :suffix)
          (where {:file_id file-id})))

(defn get-all-records-in-file-without-bodies
  [file-id]
  (select records
          (fields :id :file_id :stamp :record_id :record_short_id :time_created :size :dat_file_line :suffix)
          (where {:file_id file-id :deleted false})
          (order :stamp :ASC)))

(defn get-all-records-in-file-without-bodies
  [file-id]
  (select records
          (fields :id :file_id :stamp :record_id :record_short_id :time_created :size :dat_file_line :suffix)
          (where {:file_id file-id :deleted false})
          (order :stamp :ASC)))

(defn get-all-records-in-file-with-record-short-ids-only
  [file-id]
  (select records
          (fields :record_short_id)
          (where {:file_id file-id :deleted false})
          (order :stamp :ASC)))

(defn get-all-records-without-bodies
  []
  (select records
          (where {:deleted false})
          (fields :id :file_id :stamp :record_id :record_short_id :time_created :size :dat_file_line :suffix)))

(defn get-all-records
  []
  (select records
          (where {:deleted false})))

(defn get-all-records-with-ids-only
  []
  (select records
          (where {:deleted false})
          (fields :id)))

(defn mark-record-as-active
  [id]
  (update
    records
    (set-fields {:deleted false})
    (where {:id id})))

(defn mark-record-as-deleted
  [id]
  (transaction
    (let [record (get-record-by-id id)]
      (when record
        (update
          records
          (set-fields {:deleted true})
          (where {:stamp (:stamp record)
                  :record_id (:record-id record)}))
        (update
          images
          (set-fields {:deleted true})
          (where {:stamp (:stamp record)
                  :record_id (:record-id record)}))))))

(defn mark-record-in-file-with-record-id-as-deleted
  [file-id record-id]
  (update
    records
    (set-fields {:deleted true})
    (where {:file_id file-id
            :record_id record-id})))

(defn really-delete-record
  [id]
  (delete
    records
    (where {:id id})))

(defn update-dat-file-line
  [id dat-file-line suffix]
  (update
    records
    (set-fields {:dat_file_line dat-file-line
                 :suffix suffix})
    (where {:id id})))

(defn delete-duplicate-images
  [record-index]
  (let [record (first
                 (select records
                       (fields :stamp :record_id)
                       (where {:id record-index})))
        duplicates (drop 1 (sort #(< (:id %1) (:id %2))
                                 (select images
                                         (fields :id)
                                         (where {:stamp (:stamp record)
                                                 :record_id (:record-id record)}))))]
    (if (pos? (count duplicates))
      (timbre/info "Deleted" (count duplicates) "duplicate images."))
    (map (fn [image]
           (delete images
                   (where {:id (:id image)})))
         duplicates)))

(declare mark-file-as-dirty)
(declare update-file)
(defn remove-duplicate-records-in-file
  [file-id]
  (timbre/info "remove-duplicate-records-in-file:" file-id)
  (let [duplicate-lists (remove #(= (count %) 1)
                           (map (fn [record] (let [record (ju.db.core/get-record-by-id (:id record))]
                                               (map #(:id %) (remove #(or
                                                                       (not (= (:file-id %) (:file-id record)))
                                                                       (not (= (:stamp %) (:stamp record)))
                                                                       (not (= (:record-id %) (:record-id record))))
                                                                     (ju.db.core/get-records-by-short-id (:record-short-id record))))))
                                (sort #(< (:id %1) (:id %2)) (ju.db.core/get-all-records-in-file-without-bodies file-id))))
        duplicates (into #{} (apply concat (map #(drop 1 %) duplicate-lists)))]
    (dorun (map #(do
                  (ju.db.core/really-delete-record %)
                  (delete-duplicate-images %))
                duplicates))
    (when (pos? (count duplicates))
      (timbre/info "Deleted" (count duplicates) "duplicate record(s):" duplicate-lists)
      (mark-file-as-dirty file-id))
    (count duplicates)))

(defn remove-duplicate-records
  []
  (reduce + (map remove-duplicate-records-in-file (sort (map #(:id %) (get-all-files))))))

(defn remove-new-duplicate-records
  []
  ;(timbre/info "remove-new-duplicate-records")
  (let [new-records (sort #(< (:id %1) (:id %2))
                          (select records
                                  (fields :id :file_id :stamp :record_id :record_short_id)
                                  (where {:time_created nil})))
        duplicate-lists (remove #(= (count %) 1)
                                (map (fn [record] (let [record-list (map #(:id %)
                                                                         (remove #(or
                                                                                   (not (= (:file-id %) (:file-id record)))
                                                                                   (not (= (:stamp %) (:stamp record)))
                                                                                   (not (= (:record-id %) (:record-id record))))
                                                                                 (ju.db.core/get-records-by-short-id (:record-short-id record))))]
                                                    (update records
                                                            (where {:id (:id record)})
                                                            (set-fields {:time_created (clj-time.coerce/to-sql-time (clj-time.core/now))}))
                                                    record-list))
                                     new-records))
        duplicates (into #{} (apply concat (map #(drop 1 %) duplicate-lists)))]
    (dorun (map #(do
                  (ju.db.core/really-delete-record %)
                  (delete-duplicate-images %)
                  (mark-file-as-dirty (:file-id (select records (fields :file_id) (where {:id %})))))
                duplicates))
    (when (pos? (count duplicates))
      (timbre/info "Removed" (count duplicates) "new duplicate record(s)."))
    (pos? (count new-records))))

(defn clean-mikas-second-spill-
  [file-id]
  (timbre/info "clean-mikas-second-spill-:" file-id)
  (let [duplicate-lists (remove #(= (count %) 1)
                                (map (fn [record] (let [record (ju.db.core/get-record-by-id (:id record))]
                                                    (remove #(or
                                                              (not (= (:file-id %) (:file-id record)))
                                                              (not (= (mod (:stamp %) 32400) (mod (:stamp record) 32400)))
                                                              (not (= (:record-id %) (:record-id record))))
                                                            (ju.db.core/get-records-by-short-id (:record-short-id record)))))
                                     (sort #(< (:id %1) (:id %2)) (ju.db.core/get-all-records-in-file-without-bodies file-id))))
        sorted-duplicate-lists (map
                                 (fn [duplicate-list]
                                   (map #(:id %) (sort #(> (:stamp %1) (:stamp %2)) duplicate-list)))
                                 duplicate-lists)
        duplicates (into #{} (apply concat (map #(drop 1 %) sorted-duplicate-lists)))]
    (when (pos? (count duplicates))
      (timbre/info "clean-mikas-second-spill-:" (pr-str sorted-duplicate-lists))
      ;(dorun (map ju.db.core/mark-record-as-deleted duplicates))
      ;(update-file file-id))
      )
    (count duplicates)))

(defn clean-mikas-second-spill
  []
  (reduce + (map clean-mikas-second-spill- (sort (map #(:id %) (get-all-files))))))

(defn clean-mikas-first-spill
  "Removes records that are special cases of duplicates.
  Duplicate files were created in the following manner:
  e.g. NHK総合実況 -> nhk総合実況"
  []
  (reduce +
          (remove nil?
                  (map (fn [file]
                         (let [thread-title (file-name-to-thread-title (:file-name file))
                               thread-title-lower-case (clojure.string/lower-case thread-title)]
                           (if (and thread-title thread-title-lower-case (not (= thread-title thread-title-lower-case)))
                             (let [records-in-original-file (ju.db.core/get-all-active-and-deleted-records-in-file-without-bodies (:id file))
                                   file-lower-case (ju.db.core/get-file (thread-title-to-file-name thread-title-lower-case))
                                   records-in-duplicate-file (and file-lower-case (ju.db.core/get-all-records-in-file-without-bodies (:id file-lower-case)))
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
                                   (map #(mark-record-in-file-with-record-id-as-deleted (:id file-lower-case) %)
                                        duplicate-records))
                                 (update-file (:id file-lower-case))
                                 (count duplicate-records)
                                 )))))
                       (ju.db.core/get-all-files)))))



(defn add-blocked-record
  [file-name stamp record-id]
  (try-times
    5
    (transaction
      (when (zero? (count (select blocked_records (fields :id) (where { :file_name file-name :stamp stamp :record_id record-id }))))
        (insert blocked_records
                (values {:file_name file-name
                         :stamp stamp
                         :record_id record-id
                         :time_created (clj-time.coerce/to-sql-time (clj-time.core/now))
                         :origin nil}))))))

(defn add-blocked-record-with-origin
  [file-name stamp record-id origin]
  (try-times
    5
    (transaction
      (when (zero? (count (select blocked_records (fields :id) (where { :file_name file-name :stamp stamp :record_id record-id :origin origin}))))
        (insert blocked_records
                (values {:file_name file-name
                         :stamp stamp
                         :record_id record-id
                         :time_created (clj-time.coerce/to-sql-time (clj-time.core/now))
                         :origin origin}))))))

(defn is-record-blocked?
  [file-name stamp record-id origin]
  (or
    (pos? (count (select blocked_records
                         (fields :id)
                         (where
                           {:file_name file-name
                            :stamp stamp
                            :record_id record-id
                            :origin origin}))))
    (and
      origin
      (pos? (count (select blocked_records
                           (fields :id)
                           (where
                             {:file_name file-name
                              :stamp stamp
                              :record_id record-id
                              :origin nil})))))))

(defn get-all-blocked-records-in-file
  [file-name origin]
  (concat
    (select blocked_records
                         (where
                           {:file_name file-name
                            :origin origin}))
    (if origin
      (select blocked_records
                           (where
                             {:file_name file-name
                              :origin nil}))
      '())))




(defn update-file
  [file-id]
  ;(timbre/debug "update-file:" file-id)
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
                                                                          (fields :stamp)
                                                                          (where {:file_id file-id :deleted false})
                                                                          (order :stamp :DESC)
                                                                          (limit 1))
                                                                  0
                                                                  nil))))
                                 (catch Throwable t nil))})
    (where {:id file-id}))
  (update
    files
    (set-fields {:time_first_post (try
                                 (java.sql.Timestamp. (* 1000 (:stamp
                                                                (nth
                                                                  (select records
                                                                          (fields :stamp)
                                                                          (where {:file_id file-id :deleted false})
                                                                          (order :stamp :ASC)
                                                                          (limit 1))
                                                                  0
                                                                  nil))))
                                 (catch Throwable t nil))})
    (where {:id file-id}))
  (update
    files
    (set-fields {:size (reduce + (map :size (select records

                                                    (fields :size)
                                                    (where {:file_id file-id})
                                                    (fields :size))))})
    (where {:id file-id})))

(defn update-all-files
    []
  (dorun
    (map
      #(update-file %)
      (sort (map :id (select files (fields :id)))))))

(defn get-dirty-files
  []
  (select files
          (where {:dirty true})))


(defn mark-file-as-dirty
  [file-id]
  (update
    files
    (set-fields {:dirty true})
    (where {:id file-id})))

(defn mark-file-as-clean
  [file-id]
  (update
    files
    (set-fields {:dirty false})
    (where {:id file-id})))

(defn is-file-dirty?
  [file-id]
  (:dirty
    (first
      (select
        files
        (where {:id file-id})))))



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
    (when (zero? (count (select anchors (fields :id) (where { :file_id file-id :source source :destination destination}))))
      (insert anchors
              (values {:file_id file-id
                       :source source
                       :destination destination})))))

(defn get-anchors
  [file-id destination]
  (select anchors
          (where {:file_id file-id :destination destination})))




(defn add-image [image]
  (if (zero? (count (select files (fields :id) (where {:id (:file_id image)}))))
    (throw (IllegalArgumentException. (str "Invalid file ID: " (:file_id image)))))

  (try-times
    5
    (transaction
      (when (zero? (count (select images (where {
                                                 :file_id (:file_id image)
                                                 :record_id (:record_id image)
                                                 :stamp (:stamp image)
                                                 :md5_string (:md5_string image) }))))
        (insert images (values image))))))

(defn get-image [file-id record-id]
  (nth (select images (where {:file_id file-id
                              :record_id record-id
                              :deleted false}))
       0 nil))

(defn get-all-images-in-thread-with-record-ids-and-suffixes-only
  [file-id]
  (select images
          (where {:file_id file-id
                  :deleted false})
          (fields :record_id :suffix)
          (order :stamp :DESC)))



(defn start-database-monitor []
  (comment do
    (future
      (timbre/debug "Record Monitor started.")
      (while true
        (do
          (future
            (try
              (remove-duplicate-records)
              (clean-mikas-first-spill)
              (clean-mikas-second-spill)
              (catch Throwable t
                (timbre/error "Record Monitor:" t)))))
        (Thread/sleep (* 60 60 1000)))))

  (do
    (future
      (timbre/debug "New Record Monitor started.")
      (while true
        (try
          (while (remove-new-duplicate-records))
          (catch Throwable t
            ;(clojure.stacktrace/print-stack-trace t)
            (timbre/error "New Record Monitor:" t)))
        (Thread/sleep 100))))

  (do
    (future
      (timbre/info "File Monitor started.")
      (while true
        (try
          (let [dirty-files (get-dirty-files)]
            (when (pos? (count dirty-files))
              ;(timbre/info "File Monitor: Updating" (count dirty-files) "files.")
              (pmap
                (fn [file]
                  (when (is-file-dirty? (:id file))
                    (timbre/info "File Monitor: Updating file:" (:file-name file) (file-name-to-thread-title (:file-name file)))
                    (mark-file-as-clean (:id file))
                    (update-file (:id file))))
                dirty-files)))
          (catch Throwable t
            (timbre/error "File Monitor:" t)))
        (Thread/sleep 100)))))
