:- module(tests, []).

:- use_module('../semaphores').
:- use_module(library(system)).
:- use_module(library(strings)).
:- use_module(library(file_utils)).
:- use_module(library(process)).

         %%%%%%%%%%%
         %% TESTS %%
         %%%%%%%%%%%

%% Test: write several lines in each process, check that lines are not
%% interleaved. In this test is proved that a process is blocked by 
%% the semaphore until the running process execute the sem_post.
%% The critical section is the file in which the process are writing,
%% The access of that file must be (and is) synchronized by the semaphore.
:- export(test_exclusive_writing/1).
test_exclusive_writing(Number) :-
        process_call(path(ciaoc),['test_write'],[]),
        atom_number(Atom,Number),
        sem_open(test,1,Sem),
        process_call(test_write,[Atom,'1'],[background(P1)]),
        process_call(test_write,[Atom,'2'],[background(P2)]),
        process_join(P1),process_join(P2),
        sem_close(Sem).

%% Test: A number in a file, Number process incrementing that number
%% and Number process decrementing, the final result must be beginning
%% number. The critical section in this test is reading and writing the number
%% In the file, each process do sem_wait,read,operation,write,sem_post. The
%% correct exclusión in each iteration is crucial, if a race condition
%% happen the final value can change so much and execution will end with
%% errors.
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
%% Iter). This test is very similar at last test but more intensive.
%% Like in the last test the critical section is the file, but instead
%% of to have 2 processes doing X iterations, in this test we have
%% Number processes doing Iter iterations so, the probability to have
%% a race condition is very high Sempahore with value 1 must
%% synchronize all that processes and avoid the race condition.
:- export(test_summatory/2).
test_summatory(Number,Iter) :- %% Number = process, Iter = iterations by process
        prepare_number_file(number),
        process_call(path(ciaoc),['test_summatory'],[]),
        sem_open(number,1,Sem),
        test_summatory_(Number,Iter,number,List),
        foreach_join(List),
        sem_close(Sem).

:- export(test_summatory_/4).
test_summatory_(0,_,_,_).
test_summatory_(Number,Iter,Sem,List) :-
        atom_number(Atom,Iter),
        process_call('test_summatory',[Atom,Sem],[background(P1)]),
        Number2 is Number-1,
        test_summatory_(Number2,Iter,Sem,List2),
        insert(P1,List2,List).

%% Test: A very simple test to verify the correct behaviour
%% in value > 1 sempahore. The critical section in this test is,
%% actually, the stdout, there cannot be more than 5 processes
%% "working" in the "server" so, to syncronize the processes and
%% stop them a semaphore with value 5 is used. When a processes
%% finish his "work" it leaves the "server" and a new process can
%% enter.
:- export(test_server/1).
test_server(Clients) :-
        process_call(path(ciaoc),[client],[]),
        sem_open('sem_server',5,Sem),
        test_server_(Clients,0,Pids),
        foreach_join(Pids),
        sem_close(Sem).

:- export(test_server_/3).
test_server_(0,_,_).
test_server_(Clients,Number,Pids) :-
        Client2 is Clients-1,
        Number2 is Number+1,
        number_codes(Number,StrNumber),
        append("Process ", StrNumber,Name),
        atom_codes(AtmName,Name),
        process_call(client,[AtmName],[background(P1)]),
        test_server_(Client2,Number2,List),
        insert(P1, List, Pids).

         %%%%%%%%%%%%%%%%%%%%%%%%%%
         %% AUXILIARY PREDICATES %%
         %%%%%%%%%%%%%%%%%%%%%%%%%%

%%Auxiliar predicate: recursive join for all processes pid saved in a list
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
        write_string(Stream,"0"),
        nl(Stream),
        close(Stream).

%% Truncate the file File.
:- export(prepareFile/1).
prepareFile(File) :- open(File,write,Stream),close(Stream).