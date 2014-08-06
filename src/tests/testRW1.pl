:- module(testRW1, []).

:- use_module('../file_lock').
:- use_module(auxiliar, [loopTextF/2]).

:- export(main/0).
main :- 
        create_lock('fdsa.txt'),
        open('text1.txt',read,Stream),
        loopTextF(Stream,27),
        close(Stream),
        destroy_lock('fdsa.txt').
