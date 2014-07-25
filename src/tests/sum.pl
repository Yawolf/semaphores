:- module(sum, []).

:- use_module('../file_lock').
:- use_module(library(strings)).
:- use_module(library(file_utils)).
:- use_module(library(system)).
:- use_module(tests, [insert/3]).

:- export(init/2).
init([X],List).
init([H|T],List) :- init(T,List2),insert(H,List2,List3),reverse(List3,List).

:- export(file_to_number/2).
file_to_number(File,Number) :-
        file_to_string(File,String),
        init(String,ListNum),
        number_codes(Number,ListNum).

:- export(sumNumber/2).
sumNumber(Number,Number2) :-
        Number2 is Number+1.

:- export(writeNumber/2).
writeNumber(Stream,Number) :-
        insert(Number,[],Number2),
        write_string(Stream,Number2).

:-export(loopWrite/1).
loopWrite(0).
loopWrite(Times) :-
        file_lock('number'),
        file_to_number('number',Number),
        sumNumber(Number,Result),
        open('number',write,Stream),
        writeNumber(Stream,Result),
        close(Stream),
        file_unlock('number'),
        Times2 is Times-1,
        loopWrite(Times2).