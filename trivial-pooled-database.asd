;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem :trivial-pooled-database
  :name "trivial-pooled-database"
  :description "A DB multi-threaded connection pool."
  :version "0.1.7"
  :author "Eric Diethelm <ediethelm@yahoo.com>"
  :licence "MIT"
  :depends-on (:trivial-utilities
	       :trivial-object-lock
	       :log4cl
	       :bordeaux-threads
	       :iterate
	       :cl-dbi
	       :parse-number
	       :mgl-pax
	       :cl-annot)
  :components ((:file "package")
	       (:file "trivial-pooled-database")
	       (:file "documentation")))

