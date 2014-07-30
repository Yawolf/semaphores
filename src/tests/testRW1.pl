:- module(testRW1, []).

:- use_module('../file_lock').
:- use_module(library(strings)).
:- use_module(tests, [insert/3]).

:- export(head/2).
head([],none).
head([H|_],H).

:- export(body/2).
body([],none).
body([_],none).
body([_|T],T).

:- export(replace_aux/4). 
replace_aux([],_,_,_).
replace_aux(String,NewString,Elem,NewElem) :-
        body(String,T),
        replace_aux(T,String2,Elem,NewElem),
        head(String,H),
        (H =\= Elem,insert(H,String2,NewString));
        (H =:= Elem,insert(NewElem,String2,NewString)).

:- export(replace/4).
replace(List,NewList,Element,NewElement) :-
        replace_aux(List,List2,Element,NewElement),reverse(List2,NewList).