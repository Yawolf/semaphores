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
        cd('/home/santiago.cervantes/file_lock/src/tests'),
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

:- export(test6/1).
test6(Number) :-
        cd('/home/santiago.cervantes/file_lock/src/tests'),
        fileArithm(Number,List),
        foreachJoin(List).

:- export(test7/0).
test7 :-
        cd('/home/santiago.cervantes/file_lock/src/tests'),
        prepareNumber(number),
        process_call(sum,[],[background(P1)]),
        process_call(subs,[],[background(P2)]),
        process_call(sum,[],[background(P3)]),
        process_call(subs,[],[background(P4)]),
        process_call(sum,[],[background(P5)]),
        process_call(subs,[],[background(P6)]),
        process_call(sum,[],[background(P7)]),
        process_call(subs,[],[background(P8)]),
        process_call(sum,[],[background(P9)]),
        process_call(subs,[],[background(P10)]),
        process_call(sum,[],[background(P11)]),
        process_call(subs,[],[background(P12)]),
        process_join(P1), process_join(P2),
        process_join(P3), process_join(P4),
        process_join(P5), process_join(P6),
        process_join(P7), process_join(P8),
        process_join(P9), process_join(P10),
        process_join(P11), process_join(P12).

:- export(test8/2).
test8(0,_).
test8(Number,List) :-
        cd('/home/santiago.cervantes/file_lock/src/tests'),
        process_call(sum,[],[background(P1)]),
        process_call(subs,[],[background(P2)]),
        Number2 is Number-1,
        test8(Number2,List),
        insert(P1,List,List2), insert(P2,List2,List3),
        foreachJoin(List3).

:-export(testNoLockNumber/2).
testNoLockNumber(0,_).
testNoLockNumber(Number,List) :-
        cd('/home/santiago.cervantes/file_lock/src/tests'),
        process_call(sumNoLock,[],[background(P1)]),
        process_call(subsNoLock,[],[background(P2)]),
        Number2 is Number-1,
        testNoLockNumber(Number2,List),
        insert(P1,List,List2), insert(P2,List2,List3),
        foreachJoin(List3).
        

%%%%%%%%%%%%%%%%%%%%%%%%%
%% AUXILIAR PREDICATES %%
%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%Auxiliar predicate: Recursive arithemtic op in file
fileArithm(0,_).
fileArithm(Num,List) :-
        Num2 is Num-1,
        process_call(sum,[],[background(P1)]),
        process_call(subs,[],[background(P2)]),
        fileArithm(Num2,Pids2),
        insert(P1,Pids2,Pids),
        insert(P2,Pids,List).

:- export(prepareNumber/1).
%%Auxiliar predicate: Prepare the file ``number
prepareNumber(Name) :- open(Name,write,Stream),write_string(Stream,"0\n"),close(Stream).