:- module(test_semaphores, []).

:- use_module(file_lock).
:- use_module('tests/auxiliar', [file_to_number/2, sumNumber/2, writeNumber/2]).

:- export(loop/2).
loop(0,_).
loop(Iter,Sem) :-
        Iter2 is Iter-1,
        sem_wait(Sem),
        file_to_number('number',Number),
        sumNumber(Number,Result),
        open('number',write,Stream),
        writeNumber(Stream,Result),
        close(Stream),
        sem_post(Sem),
        loop(Iter2,Sem).

:- export(main/1).
main([ARG1,ARG2]) :- %% ARG1 = Number of iterations, ARG2 = Semaphore Name.
        atom_number(ARG1,Iter),
        sem_open(ARG2,1,Sem),
        loop(Iter,Sem).
        