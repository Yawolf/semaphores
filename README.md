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

   This project is an easy implementation of one file lock for Ciao (Prolog). 
   The predicates are implemented in C language using the Ciao module "foreign_interface".
   New open and close predicates are implemented named lopen and lclose, they are, basically,
   a predicate with a lock created and ready to use.


2 - FILES
    
    file_lock.c -> File lock's implementation in C language programming.
    file_lock.pl -> File lock's predicates declaration for Ciao (Prolog).
    tests.pl -> Some tests to prove the file lock.
    auxiliar.pl -> Auxiliar predicates used in the tests.

3 - USAGE

     create_lock(FILE) -> This predicate create a lock for the file FILE.
                        FILE is an atom.

     file_lock(FILE) -> This predicate lock the file FILE,
     if it is free.
                       FILE is an atom.

     file_unlock(FILE) -> This predicate is the opposite of file_lock, unlock the file FILE.
                       FILE is an atom.

     destroy_lock(FILE) -> This predicate destroy the lock assigned to the file FILE 

     lopen(FILE,MODE,STREAM) -> lock open, is only a open called with locks. Is equals to:
                             ```
                             create_lock(FILE),lock_file(FILE),open(FILE,MODE,STREAM).
                             ```
                        
                       FILE is an atom.
                       MODE is an atom.
                       STREAM is a free var.
     
      lclose(FILE,STREAM) -> lock close, is a close predicated used with locks. Its equals to:
                             ```
                             file_unlock(FILE),close(STREAM).
                             ```

                       FILE is an atom.
                       STREAM is a stream of a opened file.

      Use destroy_lock(File) in a recursive predicate isn't a great idea.

4 - BUGS AND KNOWN ISSUES

    This lock only works for files in the concurrent directory.