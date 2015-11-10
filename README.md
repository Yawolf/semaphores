<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#orgheadline1">1. How it works</a></li>
<li><a href="#orgheadline2">2. How to use it</a></li>
<li><a href="#orgheadline3">3. Predicates</a></li>
<li><a href="#orgheadline4">4. Examples</a></li>
<li><a href="#orgheadline5">5. Common Errors</a></li>
</ul>
</div>
</div>


# How it works<a id="orgheadline1"></a>

Semaphore primitive implementation. Semaphores is a
way of syncronize concurrent process. Each semaphore has a
internal counter which can be incremented or decremented using
sem<sub>wait</sub>(Sem) and sem<sub>post</sub>(Sem). The counter can never go below
zero, if a process try a sem<sub>wait</sub>(Sem) and find that the counter
is zero, that process will wait until another process calls
sem<sub>post</sub>(Sem).

# How to use it<a id="orgheadline2"></a>

Just clone the repository and include the module using the repo path:

    :- use_module(/home/user/git/semaphores/src/semaphores).

# Predicates<a id="orgheadline3"></a>

**pred** **sem<sub>open</sub>(+Name,+Value,-Sem)** **::** **atm** \* **int** \* **semaphore**
Tries to open a semaphore named *Name*, if the semaphore does not exist
a new semaphore is created with name *Name* value *Value* and returned in *Sem*.

**pred** **sem<sub>wait</sub>(+Sem)** **::** **semaphore** 
Decrements in one the value of the semaphore *Sem*, if the current value is 0, 
the process will stop till the increment of the value.

**pred** **sem<sub>post</sub>(+Sem)** **::** **semaphore**
Increments in one the value of the semaphore *Sem*, If te current value is 0, 
then the new value is 1 and stopped process will try catch the semaphore.

**pred** **sem<sub>close</sub>(+Sem)** **::** **semaphore** 
Destroys the semaphore *Sem*.

# Examples<a id="orgheadline4"></a>

This is a simple example of the semaphore usage with value 1. Two
processes write Number times in a file, one process writes the number 1
and the other writes 2. In this test can be only one process writting
at time, it means, at the end of the execution cannot be numbers
interleaved.

\*File: test<sub>writing.pl</sub> \*

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

**File: test.pl**

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

# Common Errors<a id="orgheadline5"></a>

The incorrect use of semaphores can result
in several errors that are very difficult to detect. 

In this case, execute a *sem<sub>post</sub>* before a *sem<sub>wait</sub>* may allow other proccesses
execute a critical section simultaneously:

    sem_open(semaphore,1,Sem),
    sem_post(Sem),
    %% CRITICAL SECTION
    sem_wait(Sem).

Another common error is execute two *sem<sub>wait</sub>* consecutively in a semaphore with value 1,
the result is a deadlock and no process can continue executing:

    sem_open(semaphore,1,Sem),
    sem_wait(Sem),
    %% CRITICAL SECTION
    sem_wait(Sem).

Take care when are you closing the semaphore, maybe you close a
semaphore at the end of a process but there are one or more processes
using the same semaphore, this can result in serious errors:

**Process 1**

    sem_open(semaphore,1,Sem),
    sem_wait(Sem),
    %% CRITICAL SECTION
    sem_post(Sem),
    sem_close(Sem).

**Process 2**

    sem_open(semaphore,1,Sem),
    sem_wait(Sem),
    %% CRITICAL SECTION
    sem_post(Sem),
    sem_close(Sem).

The correct way is:

**Process 1**

    sem_open(semaphore,1,Sem),
    sem_wait(Sem),
    %% CRITICAL SECTION
    sem_post(Sem).

**Process 2**

    sem_open(semaphore,1,Sem),
    sem_wait(Sem),
    %% CRITICAL SECTION
    sem_post(Sem).

**Main process**

    sem_open(semaphore,1,Sem),
    process_call('process_1',[],[background(P1)]),
    process_call('process_2',[],[background(P2)]),
    process_join(P1), process_join(P2),
    sem_close(Sem).

Remember that sem<sub>open</sub> create a new semaphore or open the semaphore if
it exists
