;;;; Copyright (c) Eric Diethelm 2018 <ediethelm@yahoo.com>
;;;; This code is licensed under the MIT license.

(in-package :trivial-pooled-database)


(defclass connection-parameters ()
  ((username :reader username
	     :initarg :username
	     :type string
	     :initform "")
   (password :reader password
	     :initarg :password
	     :type string
	     :initform "")
   (schema :reader schema
	   :initarg :schema
	   :type string
	   :initform "")
   (host :reader host
	 :initarg :host
	 :type string
	 :initform "")))

(defclass dbi-connection-proxy ()
  ((dbi-connection :initform nil
		   :accessor dbi-connection)
   (dbi-pooled-connection :initform nil
			  :accessor dbi-pooled-connection)))

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
		 :accessor owner-thread)
   (parameters :reader parameters
	       :type connection-parameters
	       :initarg :parameters)))


(defclass connection-pool ()
  ((connection-pool-semaphore :reader semaphore
			      :initarg :semaphore
			      :type bordeaux-threads:semaphore
			      :initform nil)
   (connection-pool :reader pool
		    :initarg :pool
		    :type (simple-array dbi-pooled-connection *)
		    :initform nil)))


(let ((connection-pool nil))
  (defun get-connection-pool ()
    (if connection-pool
	(return-from get-connection-pool connection-pool)
	(error "Connection pool not initialized!")))

  (defun set-connection-pool (pool)
    (if connection-pool
	(error "Connection pool already initialized!")
	(setf connection-pool pool))))

(defun initialize-connection-pool (user pwd schema host &key (initial-pool-size 5) (max-pool-size 15))
  "Initialize the connection pool by providing access credentials to the database and additionaly pool size information.
*USER* - user name
*PWD* - user password
*SCHEMA* - database schema to use
*HOST* - database host name or IP address
*INITIAL-POOL-SIZE* - the number of connections to be created at initialization
*MAX-POOL-SIZE* - Maximal number of connections allowed "
  (declare (type trivial-utilities:positive-fixnum initial-pool-size max-pool-size)
	   (type string user pwd schema host))

  (unless (>= max-pool-size initial-pool-size)
    (error "Invalid arguments. 'max-pool-size' >= 'initial-pool-size'"))

  (let* ((conn-params (make-instance 'connection-parameters
				     :username user
				     :password pwd
				     :host host
				     :schema schema))
	 (conn-pool (make-instance 'connection-pool
				   :semaphore (bordeaux-threads:make-semaphore :count max-pool-size)
				   :pool (make-array max-pool-size
						     :initial-contents (the list (loop repeat max-pool-size collect (make-instance 'dbi-pooled-connection :parameters conn-params)))
						     :element-type 'dbi-pooled-connection))))

    (iterate:iterate
     (iterate:for pooled-conn in-vector (pool conn-pool))
     (setf (dbi-pooled-connection (dbi-connection-proxy pooled-conn)) pooled-conn)
     (connect (dbi-connection-proxy pooled-conn))

     (let ((result (dbi:execute (dbi:prepare (dbi-connection (dbi-connection-proxy pooled-conn))
					     "SET names 'utf8';"))))

       ;; Consume the result
       (loop for row = (dbi:fetch result)
	  while row))

     (setf (connected-p pooled-conn) t))
    (set-connection-pool conn-pool)))

(defun shutdown-connection-pool (connection-pool)
  "Disconnects all database connections and shuts down the connection pool."
  ;; @TODO Implementation needed.
  (declare (ignore connection-pool)
	   (type connection-pool connection-pool))
  (error "Not implemented yet."))

(defmacro with-connection ((connection-name) connection-pool &body body)
  "Allows for safe acquisition and release of a pooled connection. In BODY a connection named be the symbol CONNECTION can be used."
  `(let ((,connection-name (acquire-connection ,connection-pool)))
     (unwind-protect
	  (progn
	    ,@body)
       (when ,connection-name
	 (release-connection ,connection-name ,connection-pool)))))

;; @TODO How to block the thread when the connection is broken and automatically resume when it becomes available. Consider time-out exceptions and background connection monitoring thread.
(defun acquire-connection (connection-pool)
  (declare (type connection-pool connection-pool))

  (log:trace "Acquiring a connection...")
  (bordeaux-threads:wait-on-semaphore (semaphore connection-pool))

  (loop for pooled-conn across (pool connection-pool)
     if (connected-p pooled-conn)
     do (let ((semaphore (semaphore pooled-conn)))
	  (when (bordeaux-threads:wait-on-semaphore semaphore :timeout .0000001)
	    (log:trace "Re-using an existing DB connection.")
	    (check-connection-and-reconnect-if-necessary pooled-conn)
	    (setf (owner-thread pooled-conn) (bordeaux-threads:current-thread))
	    (return (dbi-connection-proxy pooled-conn))))
     else
     do (let ((semaphore (semaphore pooled-conn)))
	  (when (bordeaux-threads:wait-on-semaphore semaphore :timeout .0000001)
	    (log:trace "Initiating a new DB connection.")
	    (connect (dbi-connection-proxy pooled-conn))
	    (setf (owner-thread pooled-conn) (bordeaux-threads:current-thread))
	    (return (dbi-connection-proxy pooled-conn))))
     end
     finally (error "Could not acquire a DB connection!")))

(defun release-connection (connection connection-pool)
  (declare (type dbi-connection-proxy connection))

  (unless (eq (owner-thread (dbi-pooled-connection connection)) (bordeaux-threads:current-thread))
    (error "Releasing thread is not the owner of the connection!"))

  (log:trace "Releasing a connection...")
  (setf (owner-thread (dbi-pooled-connection connection)) nil)
  (bordeaux-threads:signal-semaphore (semaphore (dbi-pooled-connection connection)))
  (bordeaux-threads:signal-semaphore (semaphore connection-pool)))

(defun connect (connection-proxy)
  (declare (type dbi-connection-proxy connection-proxy))

  (let ((params (parameters (dbi-pooled-connection connection-proxy))))
  (setf (dbi-connection connection-proxy)
	(dbi:connect :mysql
		     :username (username params)
		     :password (password params)
		     :host (host params)
		     :database-name (schema params))))

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
    (connect (dbi-connection-proxy connection)))

  (unless connection
    (warn "Connection could not be established!")))

(defun execute (cmd)
  "Allows for execution of a freely defined SQL command *CMD*."
  (log:trace "SQL command: '~a'." cmd)

  (with-connection (connection) (get-connection-pool)
    (dbi:fetch-all
     (dbi:execute
      (dbi:prepare (dbi-connection connection) cmd)))))

(defun execute-function (fn-name &rest parameters)
  "Executes a DB stored function identified by *FN-NAME* with the given *PARAMETERS* (if any) and returns it's value."

  (let ((cmd
	 ;;---------------------------------------- !! REVIEW !! ----------------------------------------
	 (format nil "SELECT ~a(~{~a~^, ~})" fn-name parameters)))

    (log:trace "SQL function ~a: '~a'." fn-name cmd)

    (with-connection (connection) (get-connection-pool)
      (let* ((row (dbi:fetch
		   (dbi:execute
		    (dbi:prepare (dbi-connection connection) cmd)))))

	(unless (and row (listp row) (eq (length row) 2))
	  (error "Could not verify the head revision."))

	(return-from  execute-function (cadr row))))))

(defun select (table fields where &key (limit nil) (order-by nil))
  "Selects all entries from *TABLE* matching the *WHERE* clause returning the *FIELDS* (might be '*' to select all fields of the table). Optionally *LIMIT* indicates the maximum number of entries to return and *ORDER-BY* defines the ordering of the result."
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

    (with-connection (connection) (get-connection-pool)
      (dbi:fetch-all
       (dbi:execute
	(dbi:prepare (dbi-connection connection) cmd))))))

(defun insert (table fields values)
  "Inserts a new entry into the database *TABLE* assigning each element in *VALUES* to its corresponding *FIELD*."
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

    (with-connection (connection) (get-connection-pool)
      (dbi:do-sql (dbi-connection connection) cmd))))

(defun update (table fields values where)
  "Updates an entry of the table *TABLE* matching the *WHERE* clause. Each element in *VALUES* is assigned to its corresponding *FIELD*."
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

    (with-connection (connection) (get-connection-pool)
      (dbi:do-sql (dbi-connection connection) cmd))))

