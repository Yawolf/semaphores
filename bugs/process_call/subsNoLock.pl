:- module(subsNoLock, []).

:- use_module(main, [file_to_number/2, writeNumber/2, subNumber/2]).

:- export(main/0).
main :- file_to_number('number',Number),
        subNumber(Number,Result),
        open('number',write,Stream),
        writeNumber(Stream,Result),
        close(Stream).
