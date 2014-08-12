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

%%test_all :-
%%        test_1, ....

% TODO: write several lines in each process, check that lines are not
% interleaved

%  e.g.,
% This is OK:
%    process1
%    process1
%    process1
%    process1
%    process1
%    process2
%    process2
%    process2
%    process2
%    process2
% This is bad:
%    process1
%    process1
%    process1
%    process1
%    process2
%    process2
%    process1
%    process2
%    process2
%    process2

:- export(test_write_loop/1).
test_write_loop(Number) :-
        process_call(path(ciaoc),['test_write'],[]),
        atom_number(Atom,Number),
        sem_open(test,1,Sem),
        process_call(test_write,[Atom,'1'],[background(P1)]),
        process_call(test_write,[Atom,'2'],[background(P2)]),
        process_join(P1),process_join(P2),
        sem_destroy(Sem).

%%Test: start Number concurrent writters and one reader, the final
%%file must contain the same number of ones
:- export(multiple_write_and_read/1).
multiple_write_and_read(Number) :-
        process_call(path(ciaoc),[test1], []),
        loop_write(test1,Number,[]),
        process_call(test2,[],[background(P1)]),
        process_join(P1).

%%Test: A number in a file, Number process incrementing that number
%%and Number process decrementing, the final result must be beginning
%%number.
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
        sem_destroy(Sem).

% TODO: document that this is testing (e.g., avoid race conditions
% with semaphores)
:- export(test_sem_aux/4).
test_sem_aux(0,_,_,_).
test_sem_aux(Number,Iter,Sem,List) :-
        atom_number(Atom,Iter),
        process_call(test_semaphores,[Atom,Sem],[background(P1)]),
        Number2 is Number-1,
        test_sem_aux(Number2,Iter,Sem,List2),
        insert(P1,List2,List).

%% Test: Start Number process and each process increments Iter times a
%% number in a file, the final number must be the same as (Number *
%% Iter).
:- export(test_sem/2).
test_sumatory(Number,Iter) :- %% Number = process, Iter = iterations by process
        prepare_number_file(number),
        sem_open(number,1,Sem),
        test_sem_aux(Number,Iter,number,List),
        foreach_join(List),
        sem_destroy(Sem).


         %%%%%%%%%%%%%%%%%%%%%%%%%%
         %% AUXILIARY PREDICATES %%
         %%%%%%%%%%%%%%%%%%%%%%%%%%

%% USED IN TESTS

%%Auxiliar predicate: Starts Number process executing Test program and saving the "id" in List
:- export(loop_write/3).
loop_write(_,0,List) :- foreach_join(List).
loop_write(Test,Num,List) :-
        Num2 is Num-1,
        atom_number(Atom,Num),
        process_call(Test,[Atom],[background(P1)]),
        insert(P1,List,List2),
        loop_write(Test,Num2,List2).

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