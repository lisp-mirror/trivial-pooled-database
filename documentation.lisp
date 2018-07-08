;;;; Copyright (c) Eric Diethelm 2018 <ediethelm@yahoo.com>
;;;; This code is licensed under the MIT license.

(in-package :trivial-pooled-database)

(defsection @trivial-pooled-database-manual (:title "Trivial Pooled Database Manual")
  "trivial-pooled-database provides a multi-threaded DB connection pool."
  (initialize-connection-pool function)
  (shutdown-connection-pool function)
  (with-connection macro)
  (execute function)
  (execute-function function)
  (select function)
  (insert function)
  (update function))
