(ns ju.db.schema
  (:require
    [clojure.java.jdbc :as sql]
    [conman.core :as conman]
    [environ.core :refer [env]]
    [mount.core :refer [defstate]]

    ; Meriken
    [taoensso.timbre :as timbre]
    [korma.core :refer :all]
    [korma.db :refer [create-db default-connection]])
  (:import [java.sql
            BatchUpdateException
            PreparedStatement]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DATABASE SPECIFICATIONS ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def db-name "ju")
(def backup-db-name "ju")

; H2 embedded mode
(def h2-db-spec {:classname   "org.h2.Driver"
                 :subprotocol "h2"
                 :subname     (str "./" db-name
                                   ";MV_STORE=FALSE"
                                   ";MVCC=TRUE"
                                   ";MULTI_THREADED=FALSE"
                                   ";DEFRAG_ALWAYS=FALSE"
                                   ";RECOVER=TRUE"
                                   ";CACHE_SIZE=262144"
                                   ";LOCK_TIMEOUT=10000"
                                   ";TRACE_LEVEL_FILE=0"
                                   ";TRACE_LEVEL_SYSTEM_OUT=0")
                 :user        "sa"
                 :password    ""
                 :make-pool?  true
                 :naming      {:keys   clojure.string/lower-case
                               :fields clojure.string/upper-case}})

(def h2-backup-db-spec {:classname   "org.h2.Driver"
                        :subprotocol "h2"
                        :subname     (str "./" backup-db-name
                                          ";MV_STORE=FALSE"
                                          ";MVCC=TRUE"
                                          ";MULTI_THREADED=FALSE"
                                          ";DEFRAG_ALWAYS=FALSE"
                                          ";RECOVER=TRUE"
                                          ";CACHE_SIZE=262144"
                                          ";LOCK_TIMEOUT=10000"
                                          ";TRACE_LEVEL_FILE=0"
                                          ";TRACE_LEVEL_SYSTEM_OUT=0")
                        :user        "sa"
                        :password    ""
                        :make-pool?  true
                        :naming      {:keys   clojure.string/lower-case
                                      :fields clojure.string/upper-case}})

; H2 server mode
(comment def h2-server-db-spec {:classname   "org.h2.Driver"
                                :subprotocol "h2"
                                :subname     (str "tcp://localhost/" db-name)
                                :user        "sa"
                                :password    ""
                                :make-pool?  true
                                :naming      {:keys   clojure.string/lower-case
                                              :fields clojure.string/upper-case}})

; HyperSQL embedded mode
(def hsqldb-db-spec {:classname   "org.hsqldb.jdbc.JDBCDriver"
                     :subprotocol "hsqldb"
                     :subname     (str "file:" db-name ".hsqldb"
                                       ";hsqldb.tx=mvcc"
                                       ";hsqldb.lob_file_scale=1")
                     :user        "sa"
                     :password    ""
                     :make-pool?  true
                     :naming      {:keys   clojure.string/lower-case
                                   :fields clojure.string/upper-case}})

(def hsqldb-backup-db-spec {:classname   "org.hsqldb.jdbc.JDBCDriver"
                            :subprotocol "hsqldb"
                            :subname     (str "file:" backup-db-name ".hsqldb"
                                              ";hsqldb.tx=mvcc"
                                              ";hsqldb.lob_file_scale=1")
                            :user        "sa"
                            :password    ""
                            :make-pool?  true
                            :naming      {:keys   clojure.string/lower-case
                                          :fields clojure.string/upper-case}})

; MySQL
(def mysql-db-spec {:classname   "com.mysql.jdbc.Driver"
                    :subprotocol "mysql"
                    :subname     "//127.0.0.1:3306/merikens_2ch_browser" ; ?zeroDateTimeBehavior=convertToNull"
                    :delimiters  "`"
                    :user        "merikens_2ch_browser"
                    :password    ""
                    :make-pool?  true
                    :naming      {:keys   clojure.string/lower-case
                                  :fields clojure.string/lower-case}})

; PostgreSQL
(def postgresql-db-spec {:classname   "org.postgresql.Driver"
                         :subprotocol "postgresql"
                         :subname     "//127.0.0.1:5432/merikens_2ch_browser" ; ?zeroDateTimeBehavior=convertToNull"
                         :delimiters  ""
                         :user        "merikens_2ch_browser"
                         :password    ""
                         :make-pool?  true
                         :naming      {:keys   clojure.string/lower-case
                                       :fields clojure.string/lower-case}})

; default
(def db-spec        hsqldb-db-spec)
(def backup-db-spec hsqldb-backup-db-spec)



;;;;;;;;;;;;;;;;;;;;;
; UTILITY FUNCTIONS ;
;;;;;;;;;;;;;;;;;;;;;

(defn java-get-jdbc-url     [data-source]      (.getJdbcUrl data-source))

(defn db-types
  [db-spec]
  (cond
    (or (= (:subprotocol db-spec) "h2")
        (and (:datasource db-spec)
             (re-find #"^jdbc:h2:" (java-get-jdbc-url (:datasource db-spec)))))
    {:id                                 "BIGINT PRIMARY KEY AUTO_INCREMENT"
     :bigint                             "BIGINT"
     :varchar                            "VARCHAR"
     :varchar-unique                     "VARCHAR UNIQUE"
     :varchar-ignorecase                 "VARCHAR_IGNORECASE"
     :varchar-ignorecase-unique          "VARCHAR_IGNORECASE UNIQUE"
     :blob                               "BLOB"}

    (or (= (:subprotocol db-spec) "hsqldb")
        (and (:datasource db-spec)
             (re-find #"^jdbc:hsqldb:" (java-get-jdbc-url (:datasource db-spec)))))
    {:id                                 "BIGINT GENERATED BY DEFAULT AS IDENTITY (START WITH 1, INCREMENT BY 1)"
     :bigint                             "BIGINT"
     :varchar                            "VARCHAR(16777216)"
     :varchar-unique                     "VARCHAR(16777216) UNIQUE"
     :varchar-ignorecase                 "VARCHAR_IGNORECASE(16777216)"
     :varchar-ignorecase-unique          "VARCHAR_IGNORECASE(16777216) UNIQUE"
     :blob                               "BLOB"}

    (or (= (:subprotocol db-spec) "mysql")
        (and (:datasource db-spec)
             (re-find #"^jdbc:mysql:" (java-get-jdbc-url (:datasource db-spec)))))
    {:id                                 "SERIAL PRIMARY KEY"
     :bigint                             "BIGINT"
     :varchar                            "LONGTEXT CHARACTER SET UTF8 COLLATE 'utf8_bin'"
     :varchar-unique                     "LONGTEXT CHARACTER SET UTF8 COLLATE 'utf8_bin'" ; UNIQUE is not supported.
     :varchar-ignorecase                 "LONGTEXT CHARACTER SET UTF8 COLLATE 'utf8_general_ci'"
     :varchar-ignorecase-unique          "LONGTEXT CHARACTER SET UTF8 COLLATE 'utf8_general_ci'" ; UNIQUE is not supported.
     :blob                               "LONGBLOB"}

    (or (= (:subprotocol db-spec) "postgresql")
        (and (:datasource db-spec)
             (re-find #"^jdbc:postgresql:" (java-get-jdbc-url (:datasource db-spec)))))
    {:id                                 "BIGSERIAL PRIMARY KEY"
     :bigint                             "BIGINT"
     :varchar                            "TEXT"
     :varchar-unique                     "TEXT UNIQUE"
     :varchar-ignorecase                 "CITEXT"
     :varchar-ignorecase-unique          "CITEXT UNIQUE"
     :blob                               "BYTEA"}

    :else
    nil))

(defn initialized?
  "Checks to see if the database schema is present."
  []
  (try
    (sql/query db-spec "SELECT * FROM nodes")
    true

    (catch Throwable t
      (timbre/info "Database is not initialized:" (str t))
      ; (print-stack-trace t)
      false)))



(defn create-nodes-table
  [db-spec]
  (let [{:keys [id bigint blob varchar varchar-unique varchar-ignorecase-unique]} (db-types db-spec)]
    (sql/db-do-commands
      db-spec
      (sql/create-table-ddl
        :nodes
        [:id           id]
        [:node_name    varchar-unique "NOT NULL"]
        [:time_created "TIMESTAMP NULL"]
        [:time_active "TIMESTAMP NULL"]
        [:time_crawled "TIMESTAMP NULL"]))))

(defn create-files-table
  [db-spec]
  (let [{:keys [id bigint blob varchar varchar-unique varchar-ignorecase-unique]} (db-types db-spec)]
    (sql/db-do-commands
      db-spec
      (sql/create-table-ddl
        :files
        [:id           id]
        [:file_name    varchar-unique "NOT NULL"]
        [:application  varchar "NOT NULL"]
        [:time_created "TIMESTAMP NULL"]
        [:time_first_post "TIMESTAMP NULL"]
        [:time_updated "TIMESTAMP NULL"]
        [:num_records  bigint "DEFAULT 0"]
        [:num_deleted_records bigint "DEFAULT 0"]
        [:deleted "BOOLEAN DEFAULT FALSE"]
        [:size bigint "DEFAULT 0"]
        [:tags varchar "DEFAULT NULL"]
        [:suggested_tags varchar "DEFAULT NULL"]))))

(defn create-records-table
  [db-spec]
  (let [{:keys [id bigint blob varchar varchar-unique varchar-ignorecase-unique]} (db-types db-spec)]
    (sql/db-do-commands
      db-spec
      (sql/create-table-ddl
        :records
        [:id           id]
        [:file_id      bigint "NOT NULL"]
        [:stamp        bigint "NOT NULL"]
        [:record_id    varchar "NOT NULL"]
        [:record_short_id varchar "NOT NULL"]
        [:body         blob "NOT NULL"]
        [:time_created "TIMESTAMP NULL"]
        [:deleted "BOOLEAN DEFAULT FALSE"]
        [:size         bigint "NOT NULL"]
        [:tags varchar "DEFAULT NULL"]))))

(defn create-update-commands-table
  [db-spec]
  (let [{:keys [id bigint blob varchar varchar-unique varchar-ignorecase-unique]} (db-types db-spec)]
    (sql/db-do-commands
      db-spec
      (sql/create-table-ddl
        :update_commands
        [:id           id]
        [:file_name    varchar "NOT NULL"]
        [:stamp        bigint "DEFAULT 0"]
        [:record_id    varchar "NOT NULL"]))))



(defn create-indexes
  [db-spec]
  ; Some databases do not support CREATE INDEX IF EXISTS
  (try (sql/db-do-commands db-spec "CREATE INDEX files_index                     ON files   ( file_name                     );") (catch Throwable _ (timbre/info "Failed to create files_index")))
  (try (sql/db-do-commands db-spec "CREATE INDEX files_time_updated_index        ON files   ( time_updated                  );") (catch Throwable _ (timbre/info "Failed to create files_time_updated_index")))
  (try (sql/db-do-commands db-spec "CREATE INDEX records_index                   ON records ( file_id                       );") (catch Throwable _ (timbre/info "Failed to create records_index")))
  (try (sql/db-do-commands db-spec "CREATE INDEX records_stamp_index             ON records ( file_id, stamp                );") (catch Throwable _ (timbre/info "Failed to create records_stamp_index")))
  (try (sql/db-do-commands db-spec "CREATE INDEX records_record_id_index         ON records ( file_id, record_id            );") (catch Throwable _ (timbre/info "Failed to create records_record_id_index")))
  (try (sql/db-do-commands db-spec "CREATE INDEX records_record_short_id_index   ON records ( file_id, record_short_id      );") (catch Throwable _ (timbre/info "Failed to create records_record_short_id_index")))
  (try (sql/db-do-commands db-spec "CREATE INDEX update_commands_index           ON update_commands ( stamp                 );") (catch Throwable _ (timbre/info "Failed to create update_commands_index")))
  (try (sql/db-do-commands db-spec "CREATE INDEX update_commands_file_name_index ON update_commands ( file_name             );") (catch Throwable _ (timbre/info "Failed to create update_commands_file_name_index")))
  )

; TODO
(defn drop-indexes
  [db-spec])



(defn create-tables
  "Creates the database tables used by the application"
  [db-spec]
  (create-nodes-table db-spec)
  (create-files-table db-spec)
  (create-records-table db-spec)
  (create-update-commands-table db-spec)
  (create-indexes db-spec))

(defn drop-tables
  [db-spec]
  (sql/db-do-commands
    db-spec
    "DROP TABLE IF EXISTS nodes;"
    "DROP TABLE IF EXISTS files;"
    "DROP TABLE IF EXISTS records;"))
