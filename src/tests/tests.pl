:- module(tests, []).

:- use_module('../semaphores').
:- use_module(library(system)).
:- use_module(library(strings)).
:- use_module(library(file_utils)).
:- use_module(library(process)).

         %%%%%%%%%%%
         %% TESTS %%
         %%%%%%%%%%%

%% This test runs two Ciao proces, test1 writes a string into a file
%% and test2 reads the string in the file.

%% Test: write several lines in each process, check that lines are not
%% interleaved
:- export(test_exclusive_writing/1).
test_exclusive_writing(Number) :-
        process_call(path(ciaoc),['test_write'],[]),
        atom_number(Atom,Number),
        sem_open(test,1,Sem),
        process_call(test_write,[Atom,'1'],[background(P1)]),
        process_call(test_write,[Atom,'2'],[background(P2)]),
        process_join(P1),process_join(P2),
        sem_close(Sem).

%%Test: A number in a file, Number process incrementing that number
%%and Number process decrementing, the final result must be beginning
%%number. Avoid race conditions.
:- export(increment_and_decrement/2).
increment_and_decrement(0,_).
increment_and_decrement(Number,List) :-
        process_call(path(ciaoc),[sum],[]),
        process_call(path(ciaoc),[subs],[]),
        prepare_number_file(number),
        sem_open(number,1,Sem),
        process_call(sum,[],[background(P1)]),
        process_call(subs,[],[background(P2)]),
        Number2 is Number-1,
        increment_and_decrement(Number2,List),
        insert(P1,List,List2), insert(P2,List2,List3),
        foreach_join(List3),
        sem_close(Sem).

%% Test: Start Number process and each process increments Iter times a
%% number in a file, the final number must be the same as (Number *
%% Iter). Avoid race conditions.
:- export(test_sumatory/2).
test_sumatory(Number,Iter) :- %% Number = process, Iter = iterations by process
        prepare_number_file(number),
        process_call(path(ciaoc),[test_sumatory],[]),
        sem_open(number,1,Sem),
        test_sumatory_(Number,Iter,number,List),
        foreach_join(List),
        sem_close(Sem).

:- export(test_sumatory_/4).
test_sumatory_(0,_,_,_).
test_sumatory_(Number,Iter,Sem,List) :-
        atom_number(Atom,Iter),
        process_call(test_sumatory,[Atom,Sem],[background(P1)]),
        Number2 is Number-1,
        test_sumatory_(Number2,Iter,Sem,List2),
        insert(P1,List2,List).

         %%%%%%%%%%%%%%%%%%%%%%%%%%
         %% AUXILIARY PREDICATES %%
         %%%%%%%%%%%%%%%%%%%%%%%%%%

%% USED IN TESTS

%%Auxiliar predicate: recursive join for all elements process saved in a list
:- export(foreach_join/1).
foreach_join([H]) :- process_join(H).
foreach_join([H|T]) :-
        process_join(H),
        foreach_join(T).

%%Auxiliar predicate: Insert elemenets into a list
:- export(insert/3).
insert(X,[],[X]).
insert(X,[A|L],[A|L1]) :- insert(X,L,L1).

%%Auxiliar predicate: Prepare the file number.
:- export(prepare_number_file/1).
prepare_number_file(Name) :-
        open(Name,write,Stream),
        write_string(Stream,"0\n"),
        close(Stream).

%% Truncate the file File.
:- export(prepareFile/1).
prepareFile(File) :- open(File,write,Stream),close(Stream).