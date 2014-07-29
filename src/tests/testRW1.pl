:- module(testRW1, []).

:- use_module('../file_lock').
:- use_module(library(strings)).
:- use_module(tests, [insert/3]).

:- export(replace_aux/4).
replace_aux([],_,_,_).
replace_aux([H|T],List,Elem,NewElem) :-
        replace_aux(T,List2,Elem,NewElem),
        (H =\= Elem,insert(H,List2,List));
        (H =:= Elem,insert(NewElem,List2,List)).

:- export(replace/4).
replace(List,NewList,Element,NewElement) :-
        replace_aux(List,List2,Element,NewElement),reverse(List2,NewList).