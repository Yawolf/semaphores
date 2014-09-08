:- module(_,[],[]).

:- use_module('../semaphores').
:- use_module(library(strings)).
:- use_module(subs, [file_to_number/2]).

%% This is the test_summatory process for the test_summatory test
%% in test.pl. The process take the semaphore, read a number from a file
%% operate with it and write it back, after that the process release the
%% semaphore. This process is repeated several times.
:- export(main/1).
main([ARG1,ARG2]) :- %% ARG1 = Number of iterations, ARG2 = Semaphore Name.
        atom_number(ARG1,Iter),
        sem_open(ARG2,1,Sem),
        loop(Iter,Sem).

:- export(loop/2).
loop(0,_).
loop(Iter,Sem) :-
        Iter2 is Iter-1,
        sem_wait(Sem),
        file_to_number('number',Number),
        NewNumber is Number+1,
        open('number',write,Stream),
        number_codes(NewNumber,StrNumber),
        write_string(Stream,StrNumber),
        nl(Stream),
        close(Stream),
        sem_post(Sem),
        loop(Iter2,Sem).
