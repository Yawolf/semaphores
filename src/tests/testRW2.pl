:- module(testRW2, []).

:- use_module('../file_lock').
:- use_module(auxiliar, [loopTextF/2]).

:- export(main/0).
main :- 
        create_lock('fdsa.txt'),
        open('text2.txt',read,Stream),
        loopTextF(Stream,13),
        close(Stream),
        destroy_lock('fdsa.txt').