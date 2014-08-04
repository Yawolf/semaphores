:- module(test1, []).

:- use_module('../file_lock').
:- use_module(library(strings)).
:- use_module(library(system)).

:- export(main/0).
main :-
        create_lock('asdf.txt'),
        file_lock('asdf.txt'),
        open('asdf.txt',append,Stream),
        write_string(Stream,"1\n"),
        close(Stream),
        file_unlock('asdf.txt').