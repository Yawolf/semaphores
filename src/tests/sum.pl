:- module(sum, []).

:- use_module(library(system)).
:- use_module('../semaphores').
:- use_module(subs, [file_to_number/2, number_to_file/2]).

:- export(main/0).
main :- sem_open(number,1,Sem),
        sem_wait(Sem),
        file_to_number('number',Number),
        Result is Number+1,
        open('number',write,Stream),
        number_to_file(Stream,Result),
        close(Stream),
        sem_post(Sem).