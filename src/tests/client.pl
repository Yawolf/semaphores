:- module(_,[],[]).

:- use_module('../semaphores').
:- use_module(library(random), [random/3]).
:- use_module(library(system), [pause/1]).
:- use_module(library(strings), [write_string/1]).
:- use_module(library(lists), [append/3]).

%% This is the "client" process for the test_server test in tests.pl file.
%% Thre process begin, takes the semaphore or wait, print a message and
%% is paused for a random time to simulate a working time, after that the
%% process "leave" the server and release the semaphore. 
:- export(main/1).
main([ARG1]) :-
        atom_codes(ARG1,Name),
        sem_open('sem_server',5,Sem),
        sem_wait(Sem),
        append(Name, ": Is in and working.\n",String),
        write_string(String),
        random(1,5,R),pause(R),
        append(Name, ": Leaving the server.\n",String2),
        write_string(String2),
        sem_post(Sem).
