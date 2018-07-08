# Trivial Pooled Database Manual

###### \[in package TRIVIAL-POOLED-DATABASE\]
trivial-pooled-database provides a multi-threaded DB connection pool.

- [function] INITIALIZE-CONNECTION-POOL USER PWD SCHEMA HOST &KEY (INITIAL-POOL-SIZE 5) (MAX-POOL-SIZE 15)

- [function] SHUTDOWN-CONNECTION-POOL

- [macro] WITH-CONNECTION (CONNECTION) &BODY BODY



- [function] EXECUTE SQL

- [function] EXECUTE-FUNCTION FN-NAME &REST PARAMETERS

    Executes a DB stored function and returns it's value.

- [function] SELECT TABLE FIELDS WHERE &KEY (LIMIT NIL) (ORDER-BY NIL)

- [function] INSERT TABLE FIELDS VALUES

- [function] UPDATE TABLE FIELDS VALUES WHERE
