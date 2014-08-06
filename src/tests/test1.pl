:- module(test1, []).

:- use_module('../semaphores').
:- use_module(library(strings)).
:- use_module(library(system)).

:- export(main/0).
main([ARG1]) :-
        atom_number(ARG1,Number),
        number_codes(Number,String),
        sem_open('test1.txt',1,Sem),
        sem_wait(Sem),
        open('asdf.txt',append,Stream),
        write_string(Stream,String),
        write_string(Stream,"\n"),
        close(Stream),
        sem_post(Sem).