:- module(_,_,_).

:- use_module(library(file_utils), [file_to_string/2]).
:- use_module(library(strings), [write_string/2]).
:- use_module(library(lists), [append/3]).
:- use_module(library(process)).

:- export(test_error/1).
test_error(0) :- !,false.
test_error(Number) :-
        process_call(path(ciaoc),[subsNoLock],[]),
        process_call(path(ciaoc),[sumNoLock],[]),
        test_error_(Number,[]).
        
:-export(test_error_/2).
test_error_(0,_).
test_error_(Number,List) :-
        process_call(sumNoLock,[],[background(P1)]),
        process_call(subsNoLock,[],[background(P2)]),
        Number2 is Number-1,
        test_error_(Number2,List),
        insert(P1,List,List2), insert(P2,List2,List3),
        foreach_join(List3).

:- export(insert/3).
insert(X,[],[X]).
insert(X,[A|L],[A|L1]) :- insert(X,L,L1).

:- export(foreach_join/1).
foreach_join([H]) :- process_join(H).
foreach_join([H|T]) :-
        process_join(H),
        foreach_join(T).

:- export(file_to_number/2).
file_to_number(File,Number) :-
        file_to_string(File,String),
        append(ListNum,"\n",String),
        number_codes(Number,ListNum).

:- export(number_to_file/2).
number_to_file(Stream,Number) :-
        number_codes(Number,Code),
        write_string(Stream,Code),
        write_string(Stream,"\n").