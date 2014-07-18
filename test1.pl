:- module(test1, [main/0]).
:- use_module(file_lock).
:- use_module(library(strings)).

main :-
        lopen('asdf.txt','append',Stream),
        write_string(Stream,"I am the process one, and I write into the file\n"),
        lclose('asdf.txt',Stream).