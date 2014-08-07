:- module(semaphores, [], [foreign_interface]).

:- doc(title, "Semaphores").

:- doc(author, "Santiago Cervantes").

:- doc(module, "This is basic semaphore implementation to synchronize
concurrent process.  ").

%% IDEA: Create a basic file lock that avoid the access to a file
%% while the lock exists.

:- export(sem_open/3).
:- pred sem_open(Name,Value,Sem) # "Try to
open a semaphore named @var{Name}, if the semaphore does not exists a
new semaphore is created with name @var{Name} value @var{Value} and
returned in @var{Sem}.".
%% `sem_open(+Name,+Value,-Sem)' Open or create a semaphore, actually,
%% the use of this predicate is not obligatory but it is completely
%% recommendable.
:- true pred sem_open(in(NAME),in(VALUE),go(SEM)) :: atm * int *
address + (foreign(prolog_sem_open), returns(SEM)).
%% NAME is an atom, is the name of the semaphore.
%% VALUE is an Integer, is thu value of the semaphore.
%% SEM is a free variable, is a pointer to the semaphore.

:- export(sem_wait/1).
:- pred sem_wait(Sem) # "Decrement in one the
value of the semaphore @var{Sem}, if the current value is 0, the
process will stop till the increment of the value.".
%% `sem_wait(+SEM)' Decrement the semaphero value, if the current
%% value is 0 the process waits.
:- true pred sem_wait(in(SEM)) :: address + (foreign(prolog_sem_wait)).
%% FILE is an atom, is the name of the file to lock.

:- export(sem_post/1).
:- pred sem_post(Sem) # "Increment in one the
value of the semaphore @var{Sem}, If te current value is 0, then the
new value is 1 and stopped process will try catch the semaphore.".
%% `file_unlock(+FILE)' unlock the file using fcntl and semaphore POSIX.
:- true pred sem_post(in(SEM)) :: address + (foreign(prolog_sem_post)).
%% FILE is an atom, is the name of the file to unlock.

:- export(sem_destroy/1).
:- pred sem_destroy(Sem) # "Destroy the semaphore @var{Sem}".
%% This predicate is highly recommendable to use becase, after using a
%% semaphore, it won't disappear, you need to destroy it.
:- true pred sem_destroy(in(SEM)) :: address + (foreign(prolog_sem_destroy)).
%% FILE is an atom, is the name of the file to unlock.

% TODO: this predicate can be improved.
%% lock_open: is an open predicate with a semaphore with value 1. 
:- export(lock_open/4).
:- pred lock_open(File,Mode,Stream,Sem) # "Open
the file @var{File} in mode @var{Mode} and save it in the @var{Stream}
stream and a semaphore with value 1 is created, name @var{File} and
save it in the free variable @var{Sem}.".
lock_open(File,Mode,Stream,Sem) :-
        sem_open(File,Mode,Sem),
        sem_wait(Sem),
        open(File,Mode,Stream).
%% File is an atom, is the name of the file to open and semaphore.
%% Mode is an atom, is the file open mode read, write, append.
%% Stream is a free variable, it will contain the Stream of the File.
%% Sem is a free variable, it will contain the semaphore.

% TODO: this predicate can be improved.
%% lock_close: this is a close predicate with the "releasing" of the semaphore.

:- pred lock_close(Stream,Sem) # "Release the semaphore @var{Sem} andclose
        the stream @var{Stream}.".
:- export(lock_close/2).
lock_close(Stream,Sem) :- 
        sem_post(Sem),
        close(Stream).
%% Stream is the Stream of a file given by the lock_open predicate.
%% Sem is a semaphore.

% TODO: Anonymous semaphores using sem_init could be implemented.

:- use_foreign_source(semaphores).
:- extra_compiler_opts(['-pthread', '-Wall']).