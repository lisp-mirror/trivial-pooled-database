;;;; Copyright (c) Eric Diethelm 2018 <ediethelm@yahoo.com>
;;;; This code is licensed under the MIT license.

(in-package :trivial-pooled-database)

(defsection @trivial-pooled-database-manual (:title "Trivial Pooled Database Manual")
  (@trivial-pooled-database-description section)
  (@trivial-pooled-database-exported section)
  (@trivial-pooled-database-license section)
  (@trivial-pooled-database-contributing section))

(defsection @trivial-pooled-database-description (:title "Description")
  "This library provides a multi-threaded DB connection pool.")

(defsection @trivial-pooled-database-exported (:title "Exported Symbols")
  (initialize-connection-pool function)
  (shutdown-connection-pool function)
  (with-connection macro)
  (execute function)
  (execute-function function)
  (select function)
  (insert function)
  (update function))

(defsection @trivial-pooled-database-license (:title "License Information")
  "This library is released under the MIT License. Please refer to the [LICENSE](https://gitlab.com/ediethelm/trivial-pooled-database/blob/master/LICENSE 'License') to get the full licensing text.")

(defsection @trivial-pooled-database-contributing (:title "Contributing to this project")
  "Please refer to the [CONTRIBUTING](https://gitlab.com/ediethelm/trivial-pooled-database/blob/master/CONTRIBUTING.md 'Contributing') document for more information.")
