:- module(file_lock, [create_lock/1, file_lock/1, file_unlock/1, clear_lock/1, lopen/3, lclose/2], [foreign_interface]).

:- true pred create_lock(in(FILE)) :: atm + (foreign).
:- true pred file_lock(in(FILE)) :: atm  + (foreign).
:- true pred file_unlock(in(FILE)) :: atm + (foreign).
:- true pred clear_lock(in(FILE)) :: atm + (foreign).

lopen(File,Mode,Stream) :- create_lock(File), file_lock(File), open(File,Mode,Stream).
lclose(File,Stream) :- file_unlock(File), close(Stream), clear_lock(File).

:- use_foreign_source(file_lock).
