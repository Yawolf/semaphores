:- module(client, []).

:- use_module('../semaphores').
:- use_module(library(random)).
:- use_module(library(system)).
:- use_module(library(strings)).

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