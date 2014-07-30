:- module(auxiliar, []).

:- use_module(library(strings)).
:- use_module(library(file_utils)).
:- use_module(library(process)).

%%%%%%%%%%%%%%%%%%%%%%%%%
%% AUXILIAR PREDICATES %%
%%%%%%%%%%%%%%%%%%%%%%%%%

%% USED IN TESTS

%%Auxiliar predicate: Starts Number process executing Test program and saving the "id" in List
:- export(writeLoop/3).
writeLoop(_,0,List) :- foreachJoin(List).
writeLoop(Test,Num,List) :-
        Num2 is Num-1,
        process_call(Test,[],[background(P1)]),
        insert(P1,List,List2),
        writeLoop(Test,Num2,List2).

%%Auxiliar predicate: recursive join for all elements process saved in a list
:- export(foreachJoin/1).
foreachJoin([H]) :- process_join(H).
foreachJoin([H|T]) :-
        process_join(H),
        foreachJoin(T).

%%Auxiliar predicate: Insert elemenets into a list
:- export(insert/3).
insert(X,[],[X]).
insert(X,[A|L],[A|L1]) :- insert(X,L,L1).

%%Auxiliar predicate: Recursive arithemtic op in file
:- export(fileArithm/2).
fileArithm(0,_).
fileArithm(Num,List) :-
        Num2 is Num-1,
        process_call(sum,[],[background(P1)]),
        process_call(subs,[],[background(P2)]),
        fileArithm(Num2,Pids2),
        insert(P1,Pids2,Pids),
        insert(P2,Pids,List).

%%Auxiliar predicate: Prepare the file ``number
:- export(prepareNumber/1).
prepareNumber(Name) :- open(Name,write,Stream),write_string(Stream,"0\n"),close(Stream).

%% USED IN SUM AND SUBS

%%Auxiliar predicate: get all the elements in a lists but the last
:- export(init/2).
init([_],_).
init([H|T],List) :- init(T,List2),insert(H,List2,List3),reverse(List3,List).

%%Auxiliar predicate: read a number from file and convert it to Int
:- export(file_to_number/2).
file_to_number(File,Number) :-
        file_to_string(File,String),
        init(String,ListNum),
        number_codes(Number,ListNum).

%%Auxiliar predicate: Sum 1 to a number
:- export(sumNumber/2).
sumNumber(Number,Number2) :-
        Number2 is Number+1.

%%Auxiliar predicate: Write a number into a file
:- export(writeNumber/2).
writeNumber(Stream,Number) :-
        number_codes(Number,Code),
        write_string(Stream,Code),
        write_string(Stream,"\n").

%%Auxiliar predicate: substrat 1 to a number
:- export(subNumber/2).
subNumber(Number,Number2) :-
        Number2 is Number-1.