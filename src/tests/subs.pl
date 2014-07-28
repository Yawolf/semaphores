:- module(subs, []).

:- use_module('../file_lock').
:- use_module(library(strings)).
:- use_module(library(file_utils)).
:- use_module(library(system)).
:- use_module(tests, [insert/3]).
:- use_module(sum, [init/2, file_to_number/2, writeNumber/2]).

:- export(subNumber/2).
subNumber(Number,Number2) :-
        Number2 is Number-1.

:- export(main/0).
main :- file_lock('number'),
        file_to_number('number',Number),
        subNumber(Number,Result),
        open('number',write,Stream),
        writeNumber(Stream,Result),
        close(Stream),
        file_unlock('number').
