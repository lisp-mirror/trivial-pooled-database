# Trivial Pooled Database Manual

[![pipeline status](https://gitlab.com/ediethelm/trivial-pooled-database/badges/master/pipeline.svg)](https://gitlab.com/ediethelm/trivial-pooled-database/commits/master)
[![Quicklisp](http://quickdocs.org/badge/trivial-pooled-database.svg)](http://quickdocs.org/trivial-pooled-database/)

###### \[in package TRIVIAL-POOLED-DATABASE\]
## Description

This library provides a multi-threaded DB connection pool.

## Working Example

After loading the project one has to first initialize the connection pool by calling *INITIALIZE-CONNECTION-POOL*, passing it the access credentials and database information.

```lisp
(let ((username "sql_user")
      (passwd "password")
      (schema "minerva")
      (host "localhost"))
  (trivial-pooled-database:initialize-connection-pool username passwd schema host))
```

Once the pool is initialized, a SQL command can be executed and all connection handling is hidden inside the pool.

```lisp
(trivial-pooled-database:select "customers"
				'(:ID :NAME :ADDRESS :GOLD-MEMBER-P :LAST-VISIT)
				(format nil "`GOLD-MEMBER-P`= ~a" gold-member-status)
				:order-by "`NAME` DESC"
				:limit 1)
```


## Installing trivial-pooled-database

This project is available in the latest [QuickLisp](https://www.quicklisp.org/beta/ "QuickLisp") distribution, so installing it is reduced to calling:

```lisp
(ql:quickload :trivial-pooled-database)
```


**Note:** trivial-pooled-database depends on features from bordeaux-threads which are not yes available in [QuickLisp](https://www.quicklisp.org/beta/ "QuickLisp").  
Because of this, following step is necessary before loading trivial-pooled-database:
```bash
cd $HOME/quicklisp/local-projects
git clone https://github.com/sionescu/bordeaux-threads.git
```


## Exported Symbols

- [function] INITIALIZE-CONNECTION-POOL USER PWD SCHEMA HOST &KEY (INITIAL-POOL-SIZE 5) (MAX-POOL-SIZE 15)
    Initialize the connection pool by providing access credentials to the database and additionaly pool size information.
    *USER* - user name
    *PWD* - user password
    *SCHEMA* - database schema to use
    *HOST* - database host name or IP address
    *INITIAL-POOL-SIZE* - the number of connections to be created at initialization
    *MAX-POOL-SIZE* - Maximal number of connections allowed

- [function] SHUTDOWN-CONNECTION-POOL 
    Disconnects all database connections and shuts down the connection pool.

- [function] SELECT TABLE FIELDS WHERE &KEY (LIMIT NIL) (ORDER-BY NIL)

    Selects all entries from *TABLE* matching the *WHERE* clause returning the *FIELDS* (might be '\*' to select all fields of the table).  
	Optionally *LIMIT* indicates the maximum number of entries to return and *ORDER-BY* defines the ordering of the result.

- [function] SELECT TABLE FIELDS WHERE &KEY (LIMIT NIL) (ORDER-BY NIL)

    Selects all entries from *TABLE* matching the *WHERE* clause returning the *FIELDS* (might be '\*' to select all fields of the table).  
    Optionally *LIMIT* indicates the maximum number of entries to return and *ORDER-BY* defines the ordering of the result.

- [function] INSERT TABLE FIELDS VALUES

    Inserts a new entry into the database *TABLE* assigning each element in *VALUES* to its corresponding *FIELD*.

- [function] UPDATE TABLE FIELDS VALUES WHERE

    Updates an entry of the table *TABLE* matching the *WHERE* clause. Each element in *VALUES* is assigned to its corresponding *FIELD*.

- [function] EXECUTE-FUNCTION FN-NAME &REST PARAMETERS

    Executes a DB stored function identified by *FN-NAME* with the given *PARAMETERS* (if any) and returns it's value.

- [function] EXECUTE CMD

    Allows for execution of a freely defined SQL command *CMD*.

## License Information

This library is released under the MIT License. Please refer to the [LICENSE](https://gitlab.com/ediethelm/trivial-pooled-database/blob/master/LICENSE "License") to get the full licensing text.

## Contributing to this project

Please refer to the [CONTRIBUTING](https://gitlab.com/ediethelm/trivial-pooled-database/blob/master/CONTRIBUTING.md "Contributing") document for more information.
