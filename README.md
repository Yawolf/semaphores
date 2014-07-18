############################################
# FILE LOCK IMPLEMENTATION FOR CIAO PROLOG #
############################################

Author: Santiago Cervantes, IMDEA Software Institute.

*CONTENTS:
        I - INTRODUCTION
        II - FILES
        III - USAGE
        IV - BUGS AND KNOWN ISSUES

I. INTRODUCTION

   This project is an easy implementation of one file lock for Ciao (Prolog). 
   The predicates are implemented in C language using the Ciao module "foreign_interface".
   New open and close predicates are implemented named lopen and lclose, they are, basically,
   a predicates with a lock created and ready to use.



II. FILES
    
    file_lock.c -> file lock's implementation in C language programming.
    file_lock.pl -> file lock's predicates declaration for Ciao (Prolog).



III. USAGE

     create_lock(FILE) -> This predicate creates a lock assigned to the file that you're passing as atom. 
     Actually, the lock is only a new file created in the current directory  with name .var{file}.lck. 
     The lock has inside one character, the value of this character can be 0 (UNLOCK) or 1 (LOCK).
     This predicate sets the value in UNLOCK.
                       FILE is an atom.

     file_lock(file) -> This predicate tests if the lock was created and, if it is free, 
     takes it overwitting the inside value to LOCK (1).
                       FILE is an atom.

     file_unlock(file) -> This predicate is the opposite of file_lock, it test if the lock was 
     created and, if it is in use, unlock it overwritting the inside value to UNLOCK (0).
                       FILE is an atom.

     clear_lock(FILE) -> This predicate delete the file assigned to a file if and only if the lock is free.
                       FILE is an atom.

     lopen(FILE,MODE,STREAM) -> lock open, is only a open called with locks. Is equals to:
                             ```
                             create_lock(FILE),lock_file("FILE"),open(FILE,MODE,STREAM).
                             ```
                        
                       FILE is an atom.
                       MODE is an atom.
                       STREAM is a free var.
     
      lclose(FILE,STREAM) -> lock close, is a close predicated used with locks. Its equals to:
                             ```
                             file_unlock(FILE),close(STREAM),clear_lock(FILE).
                             ```

                       FILE is an atom.
                       STREAM is a stream of a opened file.



IV. BUGS AND KNOWN ISSUES

    The locks can only be created in the current folder, in other words, if you current directory 
    is /home/$USER, create_lock(/home/$USER/my_folder/my_file.txt) will not work properly.

    create_lock does not test if the file FILE exists or not, this include that create_lock can 
    create a null file lock called ..lck

    These locks can only be used with lopen and lclose, with normal open and close predicates does not work.