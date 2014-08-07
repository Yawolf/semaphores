:- module(auxiliar, []).

:- use_module('../semaphores').
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
        atom_number(Atom,Num),
        process_call(Test,[Atom],[background(P1)]),
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

%%Auxiliar predicate: Prepare the file ``number
:- export(prepareNumber/1).
prepareNumber(Name) :-
        open(Name,write,Stream),
        write_string(Stream,"0\n"),
        close(Stream).

%% USED IN SUM AND SUBS

%%Auxiliar predicate: read a number from file and convert it to Int
:- export(file_to_number/2).
file_to_number(File,Number) :-
        file_to_string(File,String),
        append(ListNum,"\n",String),
        number_codes(Number,ListNum).

%%Auxiliar predicate: Write a number into a file
:- export(writeNumber/2).
writeNumber(Stream,Number) :-
        number_codes(Number,Code),
        write_string(Stream,Code),
        write_string(Stream,"\n").

%%Auxiliar predicate: Recursive calls to sum and subs
:- export(test8_aux/2).
test8_aux(0,_).
test8_aux(Number,List) :-
        cd('/Users/santiago.cervantes/file_lock/src/tests'),
        prepareNumber(number),
        sem_open(number,1,Sem),
        process_call(sum,[],[background(P1)]),
        process_call(subs,[],[background(P2)]),
        Number2 is Number-1,
        test8_aux(Number2,List),
        insert(P1,List,List2), insert(P2,List2,List3),
        foreachJoin(List3),
        sem_destroy(Sem).

%% USED IN TESTRW1 AND TESTRW2

%%Auxiliar predicate: Replaces E by O
:- export(replace_aux/2).
replace_aux([],_).
replace_aux([101|T],NewList) :- replace_aux(T,List2),insert(111,List2,NewList).
replace_aux([H|T],NewList) :- replace_aux(T,List2),insert(H,List2,NewList).

%%Auxiliar predicate: Reverse replace_aux
:- export(replaceEO/2).
replaceEO(List,NewList) :-
        replace_aux(List,List2),reverse(List2,NewList).

%%Auxiliar predicate: Read from a file, process the line and write in a new file
:- export(loopTextF/2).
loopTextF(_,0).
loopTextF(Stream,Number) :-
        sem_open(fdsa,1,Sem),
        get_line(Stream,String),
        replaceEO(String,NewString),
        sem_wait(Sem),
        open('fdsa.txt',append,Stream2),
        write_string(Stream2,NewString),
        write_string(Stream2,"\n"),
        close(Stream2),
        sem_post(Sem),
        Number2 is Number-1,
        loopTextF(Stream,Number2).

:- export(prepareFile/1).
prepareFile(File) :- open(File,write,Stream),close(Stream).