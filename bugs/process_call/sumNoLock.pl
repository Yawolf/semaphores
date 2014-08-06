:- module(sumNoLock, []).

:- use_module(main, [file_to_number/2, sumNumber/2, writeNumber/2]).

:- export(main/0).
main :- file_to_number('number',Number),
        sumNumber(Number,Result),
        open('number',write,Stream),
        writeNumber(Stream,Result),
        close(Stream).