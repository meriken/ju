(ns ju.test.db.core
  (:require [ju.db.core :as db]
            [ju.db.migrations :as migrations]
            [clojure.test :refer :all]
            [clojure.java.jdbc :as jdbc]
            [conman.core :refer [with-transaction]]
            [environ.core :refer [env]]
            [mount.core :as mount]))

(use-fixtures
  :once
  (fn [f]
    (mount/start #'myapp.db.core/*db*)
    (migrations/migrate ["migrate"])
    (f)))

(deftest test-users
  (with-transaction [t-conn db/*db*]
    (jdbc/db-set-rollback-only! t-conn)
    (is (= 1 (db/create-user!
               {:id         "1"
                :first_name "Sam"
                :last_name  "Smith"
                :email      "sam.smith@example.com"
                :pass       "pass"})))
    (is (= [{:id         "1"
             :first_name "Sam"
             :last_name  "Smith"
             :email      "sam.smith@example.com"
             :pass       "pass"
             :admin      nil
             :last_login nil
             :is_active  nil}]
           (db/get-user {:id "1"})))))
