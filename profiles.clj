; {:profiles/dev  {:env {:database-url "jdbc:mysql://localhost:3306/ju_dev?user=db_user_name_here&password=db_user_password_here"}}
;  :profiles/test {:env {:database-url "jdbc:mysql://localhost:3306/ju_test?user=db_user_name_here&password=db_user_password_here"}}}
{:profiles/dev  {:env {:database-url "jdbc:hsqldb:file:ju.hsqldb;user=sa;password=;hsqldb.tx=mvcc"}}
 :profiles/test {:env {:database-url "jdbc:hsqldb:file:ju.hsqldb;user=sa;password=;hsqldb.tx=mvcc"}}}
