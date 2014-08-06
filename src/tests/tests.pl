:- module(tests, []).

:- use_module('../file_lock').
:- use_module(library(system)).
:- use_module(library(process)).
:- use_module(auxiliar, [writeLoop/3, foreachJoin/1, insert/3, fileArithm/2, prepareNumber/1, test8_aux/2, prepareFile/1]).

%% This test runs two Ciao proces, test1 writes a string into a file and test2 reads the string in the file.

%%Test: starts Number concurrent writters and one reader
:- export(test5/1).
test_1(Number) :-
        cd('/Users/santiago.cervantes/file_lock/src/tests'),
        writeLoop(test1,Number,[]),
        process_call(test2,[],[background(P1)]),
        process_join(P1).

%%Test: increments and decrement a number into a file Number times.
:- export(test8/2).
test_2(Number,List) :- 
        test8_aux(Number,List).

%%Test: Read lines from 2 different files, process them and then write them in a new file
:- export(testConcurrentWritting/0).
test_concurrent_writting :-
        cd('/Users/santiago.cervantes/file_lock/src/tests'),
        prepareFile('fdsa.txt'),
        process_call(testRW1,[],[background(P1)]),
        process_call(testRW2,[],[background(P2)]),
        process_join(P1),process_join(P2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% THIS IS THE IMPORTANT TEST %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- export(test_sem_aux/4).
test_sem_aux(0,_,_,_).
test_sem_aux(Number,Iter,Sem,List) :-
        atom_number(Atom,Iter),
        process_call(test_semaphores,[Atom,Sem],[background(P1)]),
        Number2 is Number-1,
        test_sem_aux(Number2,Iter,Sem,List2),
        insert(P1,List2,List).

:- export(test_sem/2).
test_sem(Number,Iter) :- %% Number = process, Iter = iterations by process
        prepareNumber(number),
        sem_open(number,1,Sem),
        test_sem_aux(Number,Iter,number,List),
        foreachJoin(List),
        sem_destroy(Sem).