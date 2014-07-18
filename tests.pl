:- module(tests, [test/0]).
:- use_module(library(system)).

%% This test runs two Ciao proces, test1 writes a string into a file and test2 reads the string in the file.
test :-
        cd('/home/santiago.cervantes/file_lock'),
        system('./test1'),
        system('./test2').