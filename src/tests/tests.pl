:- module(tests, []).

:- use_module('../file_lock').
:- use_module(library(system)).
:- use_module(library(process)).
:- use_module(auxiliar, [writeLoop/3, foreachJoin/1, insert/3, fileArithm/2, prepareNumber/1, test8_aux/2]).

%% This test runs two Ciao proces, test1 writes a string into a file and test2 reads the string in the file.
:- export(test/0).
test :-
        cd('/Users/santiago.cervantes/file_lock/src/tests'),
        system('./test1'),
        system('./test2').

%% Test: calling a python program
:- export(test2/0).
test2 :-
        cd('/Users/santiago.cervantes/file_lock/src/tests'),
        system('python threads.py').

%Test: two concurrent writters and one reader
:- export(test3/0).
test3 :-
        cd('/Users/santiago.cervantes/file_lock/src/tests'),
        process_call(test1,[],[background(P1)]),
        process_call(test3,[],[background(P3)]),
        process_join(P1),process_join(P3),
        process_call(test2,[],[background(P2)]),
        process_join(P2),
        destroy_lock('asdf.txt').

%%Test: Five concurrent writters and one reader
:- export(test4/0).
test4 :-
        cd('/Users/santiago.cervantes/file_lock/src/tests'),
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
        destroy_lock('asdf.txt'),
        process_call(test2,[],[background(P2)]),
        process_join(P2).

%%Test: starts Number concurrent writters and one reader
:- export(test5/1).
test5(Number) :-
        cd('/Users/santiago.cervantes/file_lock/src/tests'),
        writeLoop(test1,Number,[]),
        destroy_lock('asdf.txt'),
        process_call(test2,[],[background(P1)]),
        process_join(P1).

:- export(test6/1).
test6(Number) :-
        cd('/Users/santiago.cervantes/file_lock/src/tests'),
        create_lock('number'),
        prepareNumber(number),
        fileArithm(Number,List),
        foreachJoin(List),
        destroy_lock('number').

:- export(test7/0).
test7 :-
        cd('/Users/santiago.cervantes/file_lock/src/tests'),
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
        process_join(P11), process_join(P12),
        destroy_lock('number').

%%Test: increments and decrement a number into a file Number times.
:- export(test8/2).
test8(Number,List) :- 
        test8_aux(Number,List),
        destroy_lock('number').

%%Test: Like test8 but without locks
:-export(testNoLockNumber/2).
testNoLockNumber(0,_).
testNoLockNumber(Number,List) :-
        cd('/Users/santiago.cervantes/file_lock/src/tests'),
        prepareNumber(number),
        process_call(sumNoLock,[],[background(P1)]),
        process_call(subsNoLock,[],[background(P2)]),
        Number2 is Number-1,
        testNoLockNumber(Number2,List),
        insert(P1,List,List2), insert(P2,List2,List3),
        foreachJoin(List3).

%%Test: starts Number concurrent writters without file_lock control and only one reader.
:- export(testNoLock/1).
testNoLock(Number) :-
        cd('/Users/santiago.cervantes/file_lock/src/tests'),
        writeLoop(testNoLock,Number,[]),
        process_call(testError,[],[background(P1)]),
        process_join(P1).

%%Test: Read lines from 2 different files, process them and then write them in a new file
:- export(testConcurrentWritting/0).
testConcurrentWritting :-
        cd('/Users/santiago.cervantes/file_lock/src/tests'),
        process_call(testRW1,[],[background(P1)]),
        process_call(testRW2,[],[background(P2)]),
        process_join(P1),process_join(P2).