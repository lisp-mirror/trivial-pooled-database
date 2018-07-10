# Trivial Pooled Database Manual

[![pipeline status](https://gitlab.com/ediethelm/trivial-pooled-database/badges/master/pipeline.svg)](https://gitlab.com/ediethelm/trivial-pooled-database/commits/master)

###### \[in package TRIVIAL-POOLED-DATABASE\]
## Description

This library provides a multi-threaded DB connection pool.


## Installing trivial-pooled-database

Since this project is not yet available in the latest [QuickLisp](https://www.quicklisp.org/beta/ "QuickLisp") distribution, it has to be copied to your local-projects folder:
```bash
cd $HOME/quicklisp/local-projects
git clone https://gitlab.com/ediethelm/trivial-pooled-database.git
```

After the files are copied, we can use [QuickLisp](https://www.quicklisp.org/beta/ "QuickLisp") to load trivial-pooled-database:
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
    *MAX-POOL-SIZE* - Maximum number of connections allowed

- [function] SHUTDOWN-CONNECTION-POOL

    Disconnects all database connections and shuts down the connection pool.

- [macro] WITH-CONNECTION (CONNECTION) &BODY BODY

    Allows for safe acquisition and release of a pooled connection. In *BODY* a connection named be the symbol *CONNECTION* can be used.

- [function] EXECUTE CMD

    Allows for execution of a freely defined SQL command *CMD*.

- [function] EXECUTE-FUNCTION FN-NAME &REST PARAMETERS

    Executes a DB stored function identified by *FN-NAME* with the given *PARAMETERS* (if any) and returns it's value.

- [function] SELECT TABLE FIELDS WHERE &KEY (LIMIT NIL) (ORDER-BY NIL)

    Selects all entries from *TABLE* matching the *WHERE* clause returning the *FIELDS* (might be '\*' to select all fields of the table).  
    Optionally *LIMIT* indicates the maximum number of entries to return and *ORDER-BY* defines the ordering of the result.

- [function] INSERT TABLE FIELDS VALUES

    Inserts a new entry into the database *TABLE* assigning each element in *VALUES* to its corresponding *FIELD*.

- [function] UPDATE TABLE FIELDS VALUES WHERE

    Updates an entry of the table *TABLE* matching the *WHERE* clause. Each element in *VALUES* is assigned to its corresponding *FIELD*.

## License Information

This library is released under the MIT License. Please refer to the [LICENSE](https://gitlab.com/ediethelm/trivial-pooled-database/blob/master/LICENSE "License") to get the full licensing text.

## Contributing to this project

Please refer to the [CONTRIBUTING](https://gitlab.com/ediethelm/trivial-pooled-database/blob/master/CONTRIBUTING.md "Contributing") document for more information.
