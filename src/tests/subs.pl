:- module(subs, []).

:- use_module('../file_lock').
:- use_module(auxiliar, [insert/3, file_to_number/2, writeNumber/2, subNumber/2]).

:- export(main/0).
main :- file_lock('number'),
        file_to_number('number',Number),
        subNumber(Number,Result),
        open('number',write,Stream),
        writeNumber(Stream,Result),
        close(Stream),
        file_unlock('number').
