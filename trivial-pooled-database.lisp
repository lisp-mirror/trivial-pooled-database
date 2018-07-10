;;;; Copyright (c) Eric Diethelm 2018 <ediethelm@yahoo.com>
;;;; This code is licensed under the MIT license.

(in-package :trivial-pooled-database)
(annot:enable-annot-syntax)

(defparameter *username* "")
(defparameter *password* "")
(defparameter *schema* "")
(defparameter *host* "")

(defparameter *connection-pool-semaphore* nil)
(defparameter *connection-pool* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass dbi-connection-proxy ()
    ((dbi-connection :initform nil
		     :accessor dbi-connection)
     (dbi-pooled-connection :initform nil
			    :accessor dbi-pooled-connection))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass dbi-pooled-connection ()
    ((dbi-connection-proxy :initform (make-instance 'dbi-connection-proxy)
			   :accessor dbi-connection-proxy
			   :type dbi-connection-proxy)
     (semaphore :initform (bordeaux-threads:make-semaphore :count 1)
		:accessor semaphore)
     (connected-p :initform nil
		  :type boolean
		  :accessor connected-p)
     (owner-thread :initform nil
		   :accessor owner-thread))))

@export
(defun initialize-connection-pool (user pwd schema host &key (initial-pool-size 5) (max-pool-size 15))
  "Initialize the connection pool by providing access credentials to the database and additionaly pool size information.
USER - user name
PWD - user password
SCHEMA - database schema to use
HOST - database host name or IP address
INITIAL-POOL-SIZE - the number of connections to be created at initialization
MAX-POOL-SIZE - Maximal number of connections allowed "
  (declare (type trivial-utilities:positive-fixnum initial-pool-size max-pool-size)
	   (type (simple-array dbi-pooled-connection *) *connection-pool*))

  (unless (>= max-pool-size initial-pool-size)
    (error "Invalid arguments. 'max-pool-size' >= 'initial-pool-size'"))

  (setf *username* user)
  (setf *password* pwd)
  (setf *schema* schema)
  (setf *host* host)
  (setf *connection-pool-semaphore* (bordeaux-threads:make-semaphore :count max-pool-size))
  (setf *connection-pool* (make-array max-pool-size
				      :initial-contents (the list (loop repeat max-pool-size collect (make-instance 'dbi-pooled-connection)))
				      :element-type 'dbi-pooled-connection))

  (loop for pool-conn across *connection-pool*
     do (setf (dbi-pooled-connection (dbi-connection-proxy pool-conn)) pool-conn))

  (loop for n from 0 to (1- initial-pool-size)
     do (connect (dbi-connection-proxy (aref *connection-pool* n))
		 user
		 pwd
		 schema
		 host)

       (let* ((query (dbi:prepare (dbi-connection (dbi-connection-proxy (aref *connection-pool* n)))
				  "SET names 'utf8';"))
	      (result (dbi:execute query)))

	 ;; Consume the result
	 (loop for row = (dbi:fetch result)
	    while row))

       (setf (connected-p (aref *connection-pool* n)) t)))

@export
(defun shutdown-connection-pool ()
  "Disconnects all database connections and shuts down the connection pool."
  ;; @TODO Implementation needed.
  (error "Not implemented yet."))

@export
(defmacro with-connection ((connection) &body body)
  "Allows for safe acquisition and release of a pooled connection. In BODY a connection named be the symbol CONNECTION can be used."
  `(let ((,connection (acquire-connection)))
     (unwind-protect
	  (progn
	    ,@body)
       (when ,connection
	 (release-connection ,connection)))))

(defun acquire-connection ()
  (declare (type (simple-array dbi-pooled-connection *) *connection-pool*))

  (log:trace "Acquiring a connection...")
  (bordeaux-threads:wait-on-semaphore *connection-pool-semaphore*)

  (loop for conn across *connection-pool*
     if (connected-p conn)
     do (let ((semaphore (semaphore conn)))
	  (when (bordeaux-threads:wait-on-semaphore semaphore :timeout .0000001)
	    (log:trace "Re-using an existing DB connection.")
	    (check-connection-and-reconnect-if-necessary conn)
	    (setf (owner-thread conn) (bordeaux-threads:current-thread))
	    (return (dbi-connection-proxy conn))))
     else
     do (let ((semaphore (semaphore conn)))
	  (when (bordeaux-threads:wait-on-semaphore semaphore :timeout .0000001)
	    (log:trace "Initiating a new DB connection.")
	    (connect (dbi-connection-proxy conn) *username* *password* *schema* *host*)
	    (setf (owner-thread conn) (bordeaux-threads:current-thread))
	    (return (dbi-connection-proxy conn))))
     end
     finally (error "Could not acquire a DB connection!")))

(defun release-connection (connection)
  (declare (type dbi-connection-proxy connection))

  (unless (eq (owner-thread (dbi-pooled-connection connection)) (bordeaux-threads:current-thread))
    (error "Releasing thread is not the owner of the connection!"))

  (log:trace "Releasing a connection...")
  (setf (owner-thread (dbi-pooled-connection connection)) nil)
  (bordeaux-threads:signal-semaphore (semaphore (dbi-pooled-connection connection)))
  (bordeaux-threads:signal-semaphore *connection-pool-semaphore*))

(defun connect (connection-proxy username password schema host)
  (declare (type dbi-connection-proxy connection-proxy))

  (setf (dbi-connection connection-proxy)
	(dbi:connect :mysql
		     :username username
		     :password password
		     :host host
		     :database-name schema))

  (let* ((query (dbi:prepare (dbi-connection connection-proxy) "SET names 'utf8';"))
	 (result (dbi:execute query)))
    (loop for row = (dbi:fetch result)
       while row
       do (format t "~A~%" row))

    (setf (connected-p (dbi-pooled-connection connection-proxy)) t)))

(defun disconnect (pooled-connection)
  (declare (ignore pooled-connection))
  )

(defun check-connection-and-reconnect-if-necessary (connection)
  (declare (type (or null dbi-pooled-connection) connection))
  (unless (and connection (cl-dbi:ping (dbi-connection (dbi-connection-proxy connection))))
    (connect (dbi-connection-proxy connection) *username* *password* *schema* *host*))

  (unless connection
    (warn "Connection could not be established!")))

@export
(defun execute (cmd)
  "Allows for execution of a freely defined SQL command CMD."
  (log:trace "SQL command: '~a'." cmd)

  (with-connection (connection)
    (dbi:fetch-all
     (dbi:execute
      (dbi:prepare (dbi-connection connection) cmd)))))

@export
(defun execute-function (fn-name &rest parameters)
  "Executes a DB stored function identified by FN-NAME with the given PARAMETERS (if any) and returns it's value."

  (let ((cmd
	 ;;---------------------------------------- !! REVIEW !! ----------------------------------------
	 (format nil "SELECT ~a(~{~a~^, ~})" fn-name parameters)))

    (log:trace "SQL function ~a: '~a'." fn-name cmd)

    (with-connection (connection)
      (let* ((query (dbi:prepare (dbi-connection connection) cmd))
	     (result (dbi:execute query))
	     (row (dbi:fetch result)))

	(unless (and row (listp row) (eq (length row) 2))
	  (error "Could not verify the head revision."))

	(return-from  execute-function (cadr row))))))

@export
(defun select (table fields where &key (limit nil) (order-by nil))
  "Selects all entries from TABLE mathing the WHERE clause returning the FIELDS (might be '*' to select all fields of the table). Optionally LIMIT indicates the maximum number of entries to return and ORDER-BY defines the ordering of the result."
  (let ((cmd
	 ;;---------------------------------------- !! REVIEW !! ----------------------------------------
	 (format nil "SELECT ~{`~a`~^, ~} FROM `~a` ~a~a ~a~a ~a~a"
		 (trivial-utilities:mklist fields)
		 table
		 (if where "WHERE " "")
		 (if where where "")
		 (if order-by "ORDER BY " "")
		 (if order-by order-by "")
		 (if limit "LIMIT " "")
		 (if limit limit ""))))

    (log:trace "SQL command: '~a'." cmd)

    (with-connection (connection)
      (dbi:fetch-all
       (dbi:execute
	(dbi:prepare (dbi-connection connection) cmd))))))

@export
(defun insert (table fields values)
  "Inserts a new entry into the database TABLE assigning each element in VALUES to its corresponding FIELD."
  (let ((cmd
	 ;;---------------------------------------- !! REVIEW !! ----------------------------------------
	 (format nil "INSERT INTO `~a` (~{`~a`~^, ~}) VALUES (~{~a~^, ~})"
		 table
		 (trivial-utilities:mklist fields)
		 (loop for elm in (trivial-utilities:mklist values)
		    collect (ctypecase elm
			      (number elm)
			      (string (format nil "'~a'" elm)))))))

    (log:trace "SQL command: '~a'." cmd)

    (with-connection (connection)
      (dbi:do-sql (dbi-connection connection) cmd))))

@export
(defun update (table fields values where)
  "Updates an entry of the table TABLE matching the WHERE clause. Each element in VALUES is assigned to its corresponding FIELD."
  (let ((cmd
	 ;;---------------------------------------- !! REVIEW !! ----------------------------------------
	 (format nil "UPDATE `~a` SET (~{`~a`~^, ~}) VALUES (~{~a~^, ~}) WHERE ~a"
		 table
		 (trivial-utilities:mklist fields)
		 (loop for elm in (trivial-utilities:mklist values)
		    collect (ctypecase elm
			      (number elm)
			      (string (format nil "'~a'" elm))))
		 where)))

    (log:trace "SQL command: '~a'." cmd)

    (with-connection (connection)
      (dbi:do-sql (dbi-connection connection) cmd))))

