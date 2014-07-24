:- module(file_lock, [file_lock/1, file_unlock/1, lopen/3, lclose/2], [foreign_interface]).

:- true pred file_lock(in(FILE)) :: atm  + (foreign).
:- true pred file_unlock(in(FILE)) :: atm + (foreign).

lopen(File,Mode,Stream) :- file_lock(File), open(File,Mode,Stream).
lclose(File,Stream) :- file_unlock(File), close(Stream).

:- use_foreign_source(file_lock).
