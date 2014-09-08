:- module(_,[],[]).

:- use_module('../semaphores').
:- use_module(library(strings), [write_string/2]).
:- use_module(library(file_utils), [file_to_string/2]).
:- use_module(library(lists), [append/3]).

%% This is the substract process for the increment_and_decrement test
%% in tests.pl. The process takes the semaphore or wait, read the
%% number from the file, operate with it and write the new number
% in the same file, after that, the process release the semaphore.
:- export(main/0).
main :- sem_open(number,1,Sem),
        sem_wait(Sem),
        file_to_number('number',Number),
        Result is Number-1,
        open('number',write,Stream),
        number_to_file(Stream,Result),
        close(Stream),
        sem_post(Sem).

%%Auxiliar predicate: read a number from file and convert it to Int
:- export(file_to_number/2).
file_to_number(File,Number) :-
        file_to_string(File,String),
        append(ListNum,"\n",String),
        number_codes(Number,ListNum).

%%Auxiliar predicate: Write a number into a file
:- export(number_to_file/2).
number_to_file(Stream,Number) :-
        number_codes(Number,Code),
        write_string(Stream,Code),
        write_string(Stream,"\n").
