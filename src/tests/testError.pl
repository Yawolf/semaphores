:- module(testError, [main/0]).
:- use_module(library(strings)).
:- use_module(library(file_utils)).

main :-
        file_to_string('fdsa.txt',String),
        write_string(String).