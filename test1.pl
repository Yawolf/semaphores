:- module(test1, [main/0]).
:- use_module(file_lock).
:- use_module(library(strings)).
:- use_module(library(system)).

main :-
        file_lock('asdf.txt'),
        open('asdf.txt',append,Stream),
        write_string(Stream,"1\n"),
        file_unlock('asdf.txt'),
        close(Stream).