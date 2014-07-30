#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <sys/stat.h>
#include <semaphore.h>

struct flock fl; /* struct for fcntl */
int fd; /*File descriptor for the file to lock*/
sem_t * sem; /* semaphore address */
char SEM_NAME[255]; /* Name of the semaphore */

void file_lock (char * file) {
    sprintf(SEM_NAME,"/sem_%s",file); /* all the semaphores are named /sem_${file_to_lock} */
    
    /*Set fcntl params*/
    fl.l_type = F_WRLCK;
    fl.l_whence = SEEK_SET;
    fl.l_start = 0;
    fl.l_len = 0;

    /*try to open the file to lock, if it fails is because the file does not exist*/
    if ((fd = open(file,O_RDWR)) == -1) {
        perror("open");
        return;
    }

    /* Create the semaphore, the value of the semaphore is 1 because there can be only
       one process working in the locked file*/
    sem = sem_open(SEM_NAME, O_CREAT, 0644, 1);

    /* if the creation of the semaphore fails, the program fails*/
    if(sem == SEM_FAILED) {
        perror("sem_open"); /* Shouldn't print this error never */
        return;
    }
    sem_wait(sem); /* decrement the semaphore value, now the semaphore has value 0, any process can continue */
    if (fcntl(fd, F_SETLK, &fl) == -1) { /* lock the file using fcntl*/
        if (errno == EACCES || errno == EAGAIN) {
            printf("ERROR,SHOULDN'T PRINT THIS!\n"); /* shouldn't print this error */
        } else {
            printf("ERROR FCNTL\n");
        }
    }
}

void file_unlock (char * file) {
    /* Set the fcntl params */
    fl.l_type = F_UNLCK;
    fl.l_whence = SEEK_SET;
    fl.l_start = 0;
    fl.l_len = 0;
    fl.l_pid = getpid();

    if (fcntl(fd, F_SETLK, &fl) == -1) { /* Unlock the file using fcntl */
        perror("fcntl unlock");
        return;
    }
    sem_post(sem); /* increment the semaphore value, other process can continue */
    sem_close(sem); /* close the semaphore */
}
