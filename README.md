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
    
    file_lock.c -> file lock's implementation in C language programming.
    file_lock.pl -> file lock's predicates declaration for Ciao (Prolog).


3 - USAGE

     file_lock(file) -> This predicate creates and activate a lock assigned toa a file file,
     if it is free.
                       FILE is an atom.

     file_unlock(file) -> This predicate is the opposite of file_lock, it test if the lock was 
     created and if it is, delete it.
                       FILE is an atom.

     lopen(FILE,MODE,STREAM) -> lock open, is only a open called with locks. Is equals to:
                             ```
                             lock_file("FILE"),open(FILE,MODE,STREAM).
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



4 - BUGS AND KNOWN ISSUES

    None
