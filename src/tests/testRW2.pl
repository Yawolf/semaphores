:- module(testRW2, []).

:- use_module('../semaphores').
:- use_module(auxiliar, [loopTextF/2]).

:- export(main/0).
main :- 
        open('text2.txt',read,Stream),
        loopTextF(Stream,13),
        close(Stream).