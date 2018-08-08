(mgl-pax:define-package #:trivial-pooled-database
  (:documentation "trivial-pooled-database provides a multi-threaded DB connection pool.")
  (:use #:common-lisp #:mgl-pax)
  (:export #:initialize-connection-pool
	   #:shutdown-connection-pool
	   #:execute
	   #:execute-function
	   #:select
	   #:insert
	   #:update))

