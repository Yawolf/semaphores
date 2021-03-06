:- module(semaphores, [], [foreign_interface, regtypes, isomodes]).


:- doc(title, "Semaphores").

:- doc(author, "Santiago Cervantes").

:- doc(module, "Semaphore primitive implementation. Semaphores is a
   way of syncronize concurrent process. Each semaphore has a
   internal counter which can be incremented or decremented using
   sem_wait(Sem) and sem_post(Sem). The counter can never go below
   zero, if a process try a sem_wait(Sem) and find that the counter
   is zero, that process will wait until another process calls
      sem_post(Sem).").

:- doc(bug, "Write examples value > 1").

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

This is a simple example of the semaphore usage. Two
processes write Number times in a file, one process writes the number 1
and the other writes 2. In this test can be only one process writting
at the time, it means, at the end of the execution cannot be numbers
interleaved.

@bf{File} @em{test_writing.pl}:

@begin{verbatim}
:- module(test_writing, []).

:- use_module(library(semaphores)).
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
:- use_module(library(semaphores)).
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

Example using a semaphore with value grater than 1 using a very simple
server-client test. In this test there are a server process and a
client process, the server has capacity for 5 clients so it's
necessary to control the access to the server:

@bf{File} @em{server.pl}:
@begin{verbatim}
:- module(server, []).

:- use_module(library(semaphores)).
:- use_module(library(process)).

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
        append(\"Process \", StrNumber,Name),
        atom_codes(AtmName,Name),
        process_call(client,[AtmName],[background(P1)]),
        test_server_(Client2,Number2,List),
        insert(P1, List2, Pids).
@end{verbatim}

@bf{File} @em{client.pl}
@begin{verbatim}
@includeverbatim{tests/client.pl}
@end{verbatim}

@subsection{Common Errors} 
The incorrect use of semaphores can result
in several errors that are very difficult to detect. 

In this case,
execute a sem_post before a sem_wait may allow other proccesses
execute a critical section simultaneously:

@begin{verbatim}
sem_open(semaphore,1,Sem),
sem_post(Sem),
%% CRITICAL SECTION
sem_wait(Sem).
@end{verbatim}

Another common error is execute two sem_wait consecutively in a
semaphore with value 1, the result is a deadlock and no process can continue executing:

@begin{verbatim}
sem_open(semaphore,1,Sem),
sem_wait(Sem),
%% CRITICAL SECTION
sem_wait(Sem).
@end{verbatim}

Take care when are you closing the semaphore, maybe you close a
semaphore at the end of a process but there are one or more processes
using the same semaphore, this can result in serious errors:

@em{process_1}:
@begin{verbatim}
sem_open(semaphore,1,Sem),
sem_wait(Sem),
%% CRITICAL SECTION
sem_post(Sem),
sem_close(Sem).
@end{verbatim}

@em{process_2}:
@begin{verbatim}
sem_open(semaphore,1,Sem),
sem_wait(Sem),
%% CRITICAL SECTION
sem_post(Sem),
sem_close(Sem).
@end{verbatim}

The correct way is:

@em{process_1}:
@begin{verbatim}
sem_open(semaphore,1,Sem),
sem_wait(Sem),
%% CRITICAL SECTION
sem_post(Sem).
@end{verbatim}

@em{process_2}:
@begin{verbatim}
sem_open(semaphore,1,Sem),
sem_wait(Sem),
%% CRITICAL SECTION
sem_post(Sem).
@end{verbatim}

@em{main_process}:
@begin{verbatim}
sem_open(semaphore,1,Sem),
process_call('process_1',[],[background(P1)]),
process_call('process_2',[],[background(P2)]),
process_join(P1), process_join(P2),
sem_close(Sem).
@end{verbatim}

Remember that sem_open create a new semaphore or open the semaphore if
it exists

").
:- use_foreign_source(semaphores).
:- extra_compiler_opts(['-pthread', '-Wall']).
