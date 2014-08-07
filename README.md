############################################
# FILE LOCK IMPLEMENTATION FOR CIAO PROLOG #
############################################

Author: Santiago Cervantes, IMDEA Software Institute.

* CONTENTS
  * 1. INTRODUCTION
  * 2. FILES
  * 3. USAGE
  * 4 BUGS AND KNOWN ISSUES

1 - INTRODUCTION

   This project is is basic semaphore implementation to synchronize
   concurrent process.  The predicates are implemented in C language
   using the Ciao module "foreign_interface".  New open and close
   predicates are implemented named lock_open and lock_close, they
   are, basically, a predicate with a semaphore created and ready to
   use with value 1.


2 - FILES
    
    semaphores.c -> File lock's implementation in C language programming.
    sempaphores.pl -> File lock's predicates declaration for Ciao (Prolog).
    tests.pl -> Some tests to prove the file lock.
    auxiliar.pl -> Auxiliar predicates used in the tests.

3 - USAGE

     sem_open(NAME,VALUE,SEM) -> Try to open a semaphore named NAME,
                        if the semaphore does not exists a new
                        semaphore is created with name NAME value
                        VALUE and returned in SEM.  NAME is an atom,
                        is the name of the semaphore.  VALUE is an
                        Integer, is thu value of the semaphore.  SEM
                        is a free variable, is a pointer to the
                        semaphore.

     sem_wait(SEM) -> Decrement in one the value of the semaphore SEM,
                        if the current value is 0, the process will
                        stop till the increment of the value SEM is an
                        address, is the semaphore to decrement.

     sem_post(SEM) -> Increment in one the value of the semaphore SEM,
                        If te current value is 0, then the new value
                        is 1 and stopped process will try catch the
                        semaphore.  SEM is an address, is the name
                        name to the semaphore to destroy
     
     sem_destroy(SEM) -> Destroy the semaphore SEM.
                        SEM is  address, is the name name to the semaphore
                        to destroy.

     lock_open(FILE,MODE,STREAM) -> Open the file FILE in mode MODE
                        and save it in the STREAM stream and a semaphore with
                        value 1 is created, named NAME and save it in the free
                        variable SEM.".  . 
                        Is equals to: 
                        ```
                        sem_open(FILE,1,Sem),sem_wait(SEM),open(FILE,MODE,STREAM).
                        ```
                        
                        File is an atom, is the name of the file to open and semaphore.
                        Mode is an atom, is the file open mode read, write, append.
                        Stream is a free variable, it will contain the Stream of the File.
                        Sem is a free variable, it will contain the semaphore.

     
      lock_close(STREAM,SEM) -> Release the semaphore SEM andclose the stream STREAM. 
                        Its equals to:
                        ```
                        sem_ppost(SEM),close(STREAM).
                        ```

                       Stream is the Stream of a file given by the lock_open predicate.
                       Sem is a semaphore.


4 - BUGS AND KNOWN ISSUES

      None