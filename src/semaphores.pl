:- module(semaphores, [], [foreign_interface, regtypes, isomodes]).
:- use_package([assertions]).

:- doc(title, "Semaphores").

:- doc(author, "Santiago Cervantes").

:- doc(module, "Semaphore primitive implementation. Semaphores is a
   way of syncronize concurrent process. Each semaphore has a
   internal counter which can be incremented or decremented using
   sem_wait(Sem) and sem_post(Sem). The counter can never go below
   zero, if a process try a sem_wait(Sem) and find that the counter
   is zero, that process will wait until another process calls
   sem_post(Sem).").

:- doc(bug, "Write examples").

:- regtype semaphore(Sem) # "@var{Sem} is a semaphore".
semaphore('$semaphore'(Addr)) :- address(Addr).

:- export(sem_open/3).
:- pred sem_open(+Name,+Value,-Sem) :: atm * int * semaphore # "Try to
   open a semaphore named @var{Name}, if the semaphore does not exist
   a new semaphore is created with name @var{Name} value @var{Value}
   and returned in @var{Sem}.".

sem_open(Name,Value,Sem) :-
        check_sem_name(Name),
        sem_open_(Name,Value,Addr),
        Sem = '$semaphore'(Addr).

:- pred check_sem_name(+Name) :: atm # "Check if the Name of the
semaphore is valid o not.".
check_sem_name(Name) :-
        ( var(Name) -> throw(error(instantiation_error))
        ; atom(Name) = '' -> throw(error(domain_error))
        ; true
        ).

:- pred check_sem(+Sem) :: semaphore # "Test if the entry value is a
valid semaphore.".
check_sem(Sem) :-
        ( var(Sem) -> throw(error(isntantation_error))
        ; Sem = '$semaphore'(_) -> true 
        ; throw(error(domain_error))
        ).

:- true pred sem_open_(in(NAME),in(VALUE),go(SEM)) ::
        atm * int * address + (foreign(prolog_sem_open), returns(SEM)).

:- export(sem_wait/1).
:- pred sem_wait(+Sem) :: semaphore # "Decrement in one the value of
   the semaphore @var{Sem}, if the current value is 0, the process
   will stop till the increment of the value.".

sem_wait(Sem) :-
        check_sem(Sem),
        Sem = '$semaphore'(Addr),
        sem_wait_(Addr).

:- true pred sem_wait_(in(SEM)) :: address + (foreign(prolog_sem_wait)).

:- export(sem_post/1).
:- pred sem_post(+Sem) :: semaphore # "Increment in one the value of
   the semaphore @var{Sem}, If te current value is 0, then the new
   value is 1 and stopped process will try catch the semaphore.".

sem_post(Sem) :-
        check_sem(Sem),
        Sem = '$semaphore'(Addr),
        sem_post_(Addr).

:- true pred sem_post_(in(SEM)) :: address + (foreign(prolog_sem_post)).

:- export(sem_close/1).
:- pred sem_close(+Sem) :: semaphore # "Destroy the semaphore @var{Sem}".

sem_close(Sem) :-
        check_sem(Sem),
        Sem = '$semaphore'(Addr),
        sem_close_(Addr).

:- true pred sem_close_(in(SEM)) :: address + (foreign(prolog_sem_close)).

:- doc(appendix, "

@subsection{Examples}

This is a simple example of the semaphore usage with value 1. Two
processes write Number times in a file, one process writes the number 1
and the other writes 2. In this test can be only one process writting
at time, it means, at the end of the execution cannot be numbers
interleaved.

@bf{File} @em{test_writing.pl}:

@begin{verbatim}
:- module(test_writing, []).

:- use_module(semaphores).
:- use_module(library(strings)).
        
:- export(recursive_writing/2).
recursive_writing(0,_). 
recursive_writing(Iterations,Number) :-
        open('test.txt',append,Stream),
        number_codes(Number,StrNumber),
        write_string(Stream,StrNumber),
        nl(Stream),
        close(Stream),
        Iterations2 is Iterations-1,
        recursive_writing(Iterations2,Number).

:- export(main/1).
main([ARG1,ARG2]) :-
        atom_number(ARG1,Iterations),
        atom_number(ARG2,Number),
        sem_open(test,1,Sem),
        sem_wait(Sem),
        recursive_writing(Iterations,Number),
        sem_post(Sem).
@end{verbatim}

@bf{File} @em{test.pl}:

@begin{verbatim}
:- use_module(semaphores).
:- use_module(library(process)).

:- export(test_exclusive_writing/1).
test_exclusive_writing(Number) :-
        process_call(path(ciaoc),['test_writing'],[]),
        atom_number(Atom,Number),
        sem_open(test,1,Sem),
        process_call('test_writing',[Atom,'1'],[background(P1)]),
        process_call('test_writing',[Atom,'2'],[background(P2)]),
        process_join(P1),process_join(P2),
        sem_destroy(Sem).

@end{verbatim}
").
:- use_foreign_source(semaphores).
:- extra_compiler_opts(['-pthread', '-Wall']).