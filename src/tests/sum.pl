:- module(_,[],[]).

:- use_module('../semaphores').
:- use_module(subs, [file_to_number/2, number_to_file/2]).

%% This is the sumatory process in the increment_and_decrement test
%% in tests.pl. As subs.pl, the process take the semaphore or wait,
%% read a number from a file, operate with it and write in the same
%% file again, after that the process release the semaphore.
:- export(main/0).
main :- sem_open(number,1,Sem),
        sem_wait(Sem),
        file_to_number('number',Number),
        Result is Number+1,
        open('number',write,Stream),
        number_to_file(Stream,Result),
        close(Stream),
        sem_post(Sem).
