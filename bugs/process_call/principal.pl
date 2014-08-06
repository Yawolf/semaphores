:- module(main, []).

:- use_module(library(file_utils)).
:- use_module(library(process)).
:- use_module(library(strings)).

:- export(insert/3).
insert(X,[],[X]).
insert(X,[A|L],[A|L1]) :- insert(X,L,L1).

:- export(init/2).
init([_],_).
init([H|T],List) :- init(T,List2),insert(H,List2,List3),reverse(List3,List).

:- export(foreachJoin/1).
foreachJoin([H]) :- process_join(H).
foreachJoin([H|T]) :-
        process_join(H),
        foreachJoin(T).

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
        number_codes(Number,Code),
        write_string(Stream,Code),
        write_string(Stream,"\n").

:- export(subNumber/2).
subNumber(Number,Number2) :-
        Number2 is Number-1.

:-export(testNoLockNumber/2).
testNoLockNumber(0,_).
testNoLockNumber(Number,List) :-
        process_call(sumNoLock,[],[background(P1)]),
        process_call(subsNoLock,[],[background(P2)]),
        Number2 is Number-1,
        testNoLockNumber(Number2,List),
        insert(P1,List,List2), insert(P2,List2,List3),
        foreachJoin(List3).