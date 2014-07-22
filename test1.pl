:- module(test1, [main/0]).
:- use_module(file_lock).
:- use_module(library(strings)).

main :-
        create_lock('asdf.txt'),
        file_lock('asdf.txt'),
        open('asdf.txt',append,Stream),
        write_string(Stream,"1\n"),
        file_unlock('asdf.txt'),
        clear_lock('asdf.txt'),
        close(Stream).