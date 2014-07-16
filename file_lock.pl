:- module(file_lock, [create_lock/1, file_lock/1, file_unlock/1, clear_lock/1], [foreign_interface]).

:- true pred create_lock(in(FILE)) :: atm + (foreign).
:- true pred file_lock(in(FILE)) :: atm  + (foreign).
:- true pred file_unlock(in(FILE)) :: atm + (foreign).
:- true pred clear_lock(in(FILE)) :: atm + (foreign).

:- use_foreign_source(file_lock).
