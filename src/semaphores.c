/*
 *  semaphores.c CAMBIAR!
 *
 *  Semaphores handling primitives.
 *
 *  Copyright (C) 2014 Santiago Cervantes
 */

#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <sys/stat.h>
#include <semaphore.h>

/* Create a semaphore and set it with the value 1*/
sem_t *prolog_sem_open(char * name, int value) {
    sem_t *sem; /* semaphore address */
    
    if (strlen(name) == 0) {
        fprintf(stderr,"Invalid lock name");
        exit(-1);
    }
    
    /* Create the semaphore, the value of the semaphore is 1 because
       there can be only one process working in the locked file*/
    if ((sem = sem_open(name, O_CREAT, 0644, value)) == SEM_FAILED) {
        perror("sem_open");
        return NULL;
    }
    return sem;
}

/* Lock a file */
void prolog_sem_wait(sem_t *sem) {
    if (sem_wait(sem) == -1)
        if (errno == EINVAL) {
            perror("sem_wait");
            return;
        } /* decrement the semaphore value, now the semaphore
                      has value 0, any process can continue */
}

/* Unlock a file */
void prolog_sem_post(sem_t *sem) {
    if (sem_post(sem) == -1) /* increment the semaphore value, other
                                process can continue */
        if (errno == EINVAL) {
            perror("sem_wait");
            return;
        }          
}

/* Destroy the semaphore created before */
void prolog_sem_destroy(sem_t *sem) {
    if (sem_close(sem) == -1) { /* destroy the semaphore */
        perror("sem_unlink");
        return;
    }
}
