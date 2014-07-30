:- module(tests, []).

:- use_module(library(system)).
:- use_module(library(process)).
:- use_module(auxiliar, [writeLoop/3, foreachJoin/1, insert/3, fileArithm/2, prepareNumber/1]).

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
        process_join(P2).

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
        process_call(test2,[],[background(P2)]),
        process_join(P2).

%%Test: starts Number concurrent writters and one reader
:- export(test5/1).
test5(Number) :-
        cd('/Users/santiago.cervantes/file_lock/src/tests'),
        writeLoop(test1,Number,[]),
        process_call(test2,[],[background(P1)]),
        process_join(P1).

:- export(test6/1).
test6(Number) :-
        cd('/Users/santiago.cervantes/file_lock/src/tests'),
        fileArithm(Number,List),
        foreachJoin(List).

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
        process_join(P11), process_join(P12).

:- export(test8/2).
test8(0,_).
test8(Number,List) :-
        cd('/Users/santiago.cervantes/file_lock/src/tests'),
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

%%Test: starts Number concurrent writters without file_lock control and only one reader.
testNoLock(Number) :-
        cd('/home/santiago.cervantes/file_locksrc/tests'),
        writeLoop(testNoLock,Number,[]),
        process_call(testError,[],[background(P1)]),
        process_join(P1).

