:- module(semaphores, [], [foreign_interface, regtypes, isomodes]).

:- doc(title, "Semaphores").

:- doc(author, "Santiago Cervantes").

:- doc(module, "Semaphore primitive implementation. Semaphores is a
   way of syncronize concurrent process. Each semaphore has a
   internal counter wich can be incremented or decremented using
   sem_wait(Sem) and sem_post(Sem). The counter can never go below
   zero, if a process trys a sem_wait(Sem) and finds that the counter
   is zero, that process will wait untill another process calls
   sem_post(Sem).").

:- doc(bug, "Implement anonymous semaphores (needs shared mem)").
:- doc(bug, "Write examples").
:- doc(bug, "Add term wrappers for semaphore addresses").

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

%% 'check_sem_name(+Name)` :: atm # Check if the Name of the semaphore
%% is valid o not.
check_sem_name(Name) :-
        ( var(Name) -> throw(error(instantiation_error))
        ; Name = '' -> throw(error(domain_error))
        ; true
        ).

%% 'check_sem(+Sem)` :: sempahore # Tests if the entry value is a
%% valid semaphore.
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

:- export(sem_destroy/1).
:- pred sem_destroy(+Sem) :: semaphore # "Destroy the semaphore @var{Sem}".

sem_destroy(Sem) :-
        check_sem(Sem),
        Sem = '$semaphore'(Addr),
        sem_destroy_(Addr).

:- true pred sem_destroy_(in(SEM)) :: address + (foreign(prolog_sem_destroy)).

:- use_foreign_source(semaphores).
:- extra_compiler_opts(['-pthread', '-Wall']).