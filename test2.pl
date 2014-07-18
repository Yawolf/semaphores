:- module(test2, [main/0, read_file/2]).
:- use_module(file_lock).
:- use_module(library(strings)).

read_file(_,end_of_line).
read_file(Stream,String) :-
        get_lines(Stream,String),
        write_string(String),
        read_file(Stream,String).

main :-
        lopen('asdf.txt',read,Stream),
        read_file(Stream,String),
        lclose('asdf.txt',Stream).