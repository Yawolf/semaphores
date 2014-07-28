:- module(test4, [main/0]).

:- use_module('../file_lock').
:- use_module(library(strings)).

main :-
        lopen('asdf.txt',append,Stream),
        write_string(Stream,"3\n"),
        lclose('asdf.txt',Stream).