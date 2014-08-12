:- module(test_write, []).

:- use_module('../semaphores').
:- use_module(library(strings)).
        
:- export(recursive_writting/2).
recursive_writting(0,_). 
recursive_writting(Iter,Number) :-
        open('fdsa.txt',append,Stream),
        number_codes(Number,StrNumber),
        write_string(Stream,StrNumber),
        write_string(Stream,"\n"),
        close(Stream),
        Iter2 is Iter-1,
        recursive_writting(Iter2,Number).

:- export(main/1).
main([ARG1,ARG2]) :-
        atom_number(ARG1,Iter),
        atom_number(ARG2,Number),
        sem_open(test,1,Sem),
        sem_wait(Sem),
        recursive_writting(Iter,Number),
        sem_post(Sem).