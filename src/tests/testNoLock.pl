:- module(testNoLock, [main/0]).
:- use_module(library(strings)).

main :-
        open('fdsa.txt',append,Stream),
        write_string(Stream,":D\n"),
        close(Stream).