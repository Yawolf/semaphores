:- module(test2, [main/0]).
:- use_module('../file_lock').
:- use_module(library(strings)).
:- use_module(library(file_utils)).
:- use_module(library(system)).

main :-
        file_lock('asdf.txt'),
        file_to_string('asdf.txt',String),
        write_string(String),
        file_unlock('asdf.txt').