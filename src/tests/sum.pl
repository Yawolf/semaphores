:- module(sum, []).

:- use_module(library(system)).
:- use_module('../file_lock').
:- use_module(auxiliar, [insert/3, init/2, file_to_number/2, sumNumber/2, writeNumber/2]).

:- export(main/0).
main :- sem_open(number,1,Sem),
        sem_wait(Sem),
        file_to_number('number',Number),
        sumNumber(Number,Result),
        open('number',write,Stream),
        writeNumber(Stream,Result),
        close(Stream),
        sem_post(Sem).