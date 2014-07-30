:- module(file_lock, [], [foreign_interface]).

:- doc(title, "File lock").

:- doc(author, "Santiago Cervantes").

:- doc(module, "This is basic file lock implementation as support of open and close predicate.
").

%% IDEA: Create a basic file lock that avoid the access to a file
%% while the lock exists.

% TODO: `open(File,Mode,Stream)` has a lock inside, need to test if this
%% lock actually works well.
:- export(file_lock/1).
:- pred file_lock(File) # "Create a lock assigned to the file @var{File}.".
%% `file_lock(+FILE)' Use fcntl and semaphore POSIX to lock a file, if the file is locked
%% the process waits.
:- true pred file_lock(in(FILE)) :: atm + (foreign).
%% FILE is an atom, is the name of the file to lock.

:- export(file_unlock/1).
:- pred file_unlock(File) # "unlock the file @var{File} locked by file_lock(File).".
%% `file_unlock(+FILE)' unlock the file using fcntl and semaphore POSIX.
:- true pred file_unlock(in(FILE)) :: atm + (foreign).
%% FILE is an atom, is the name of the file to unlock.

% TODO: this predicate can be improved. Implemented only for tests
%% lopen: is an open predicate with a lock ready to use. 
:- export(lopen/3).
:- pred lopen(File,Mode,Stream) # "Open and lock the file @var{File} in mode @var{Mode} and save it in the @var{Stream} stream".
lopen(File,Mode,Stream) :- file_lock(File), open(File,Mode,Stream).
%% File is an atom, is the name of the file to lock.
%% Mode is an atom, is the file open mode read, write, append.
%% Stream is a free variable, it will contain the Stream of the File.

% TODO: this predicate can be improved. Implemented only for tests.
%% lclose: this is a close predicate with the "releasing" of the lock.

:- pred lclose(File,Stream) # "Remove the lock assigned to the file @var{File} and close the stream @var{Stream} given by lopen(File,Mode,Stream)".
:- export(lclose/2).
lclose(File,Stream) :- file_unlock(File), close(Stream).
%% File is an atom, is the name of the file to unlock.
%% Stream is the Stream of a file given by the lopen predicate.

:- use_foreign_source(file_lock).
:- extra_compiler_opts(['-lpthread', '-Wall']).