:- module(tests, [test/0, test2/0, test3/0, test4/0]).
:- use_module(library(system)).
:- use_module(library(process)).

%% This test runs two Ciao proces, test1 writes a string into a file and test2 reads the string in the file.
test :-
        cd('/home/santiago.cervantes/file_lock'),
        system('./test1'),
        system('./test2').

test2 :-
        cd('/home/santiago.cervantes/file_lock'),
        system('python threads.py').

test3 :-
        cd('/home/santiago.cervantes/file_lock'),
        process_call(test1,[],[background(P1)]),
        process_call(test3,[],[background(P3)]),
        process_join(P1),process_join(P3),
        process_call(test2,[],[background(P2)]),
        process_join(P2).

test4 :-
        cd('/home/santiago.cervantes/file_lock'),
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

%%test5 :-
  %%      cd('/home/santiago.cervantes/file_lock').
