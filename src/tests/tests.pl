:- module(tests, [test/0, test2/0, test3/0, test4/0, test5/1,testNoLock/1 ,writeLoop/3, foreachJoin/1, insert/3]).
:- use_module(library(system)).
:- use_module(library(process)).
:- use_module(library(strings)).
:- use_module('../file_lock').
%% This test runs two Ciao proces, test1 writes a string into a file and test2 reads the string in the file.
test :-
        cd('/home/santiago.cervantes/file_lock/src/tests'),
        system('./test1'),
        system('./test2').

%% Test: calling a python program
test2 :-
        cd('/home/santiago.cervantes/file_lock/src/tests'),
        system('python threads.py').

%Test: two concurrent writters and one reader
test3 :-
        cd('/home/santiago.cervantes/file_lock/src/tests'),
        process_call(test1,[],[background(P1)]),
        process_call(test3,[],[background(P3)]),
        process_join(P1),process_join(P3),
        process_call(test2,[],[background(P2)]),
        process_join(P2).

%%Test: Five concurrent writters and one reader
test4 :-
        cd('/home/santiago.cervantes/file_locksrc/tests'),
        process_call(test1,[],[background(P1)]),
        process_call(test3,[],[background(P3)]),
        process_call(test4,[],[background(P4)]),
        process_call(test5,[],[background(P5)]),
        process_call(test6,[],[background(P6)]),
        process_join(P1),
        process_join(P3),
        process_join(P4),
        process_join(P5),
        process_join(P6),
        process_call(test2,[],[background(P2)]),
        process_join(P2).

%%Test: starts Number concurrent writters and one reader
test5(Number) :-
        cd('/home/santiago.cervantes/file_lock/src/tests'),
        writeLoop(test1,Number,[]),
        process_call(test2,[],[background(P1)]),
        process_join(P1).

%%Test: starts Number concurrent writters without file_lock control and only one reader.
testNoLock(Number) :-
        cd('/home/santiago.cervantes/file_locksrc/tests'),
        writeLoop(testNoLock,Number,[]),
        process_call(testError,[],[background(P1)]),
        process_join(P1).

%%Auxiliar predicate: Starts Number process executing Test program and saving the "id" in List
writeLoop(_,0,List) :- foreachJoin(List).
writeLoop(Test,Num,List) :-
        Num2 is Num-1,
        process_call(Test,[],[background(P1)]),
        insert(P1,List,List2),
        writeLoop(Test,Num2,List2).

%%Auxiliar predicate: recursive join for all elements process saved in a list
foreachJoin([H]) :- process_join(H).
foreachJoin([H|T]) :-
        process_join(H),
        foreachJoin(T).

%%Auxiliar predicate: Insert elemenets into a list
insert(X,[],[X]).
insert(X,[A|L],[A|L1]) :- insert(X,L,L1).