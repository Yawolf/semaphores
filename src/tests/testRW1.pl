:- module(testRW1, []).

:- use_module('../semaphores').
:- use_module(auxiliar, [loopTextF/2]).

:- export(main/0).
main :- 
        open('text1.txt',read,Stream),
        loopTextF(Stream,27),
        close(Stream).
