:- module(subsNoLock, []).

:- use_module(test, [file_to_number/2, number_to_file/2]).

:- export(main/0).
main :- file_to_number('number',Number),
        Result is Number-1,
        open('number',write,Stream),
        number_to_file(Stream,Result),
        close(Stream).
