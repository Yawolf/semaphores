#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <sys/stat.h>
#include <semaphore.h>

int fd; /*File descriptor for the file to lock*/
sem_t * sem; /* semaphore address */
struct flock fl;

/* Create a semaphore and set it with the value 1*/
void create_lock (char * file) {
    char SEM_NAME[245]; /* Name of the semaphore */

    if (strlen(file) == 0) {
        fprintf(stderr,"Invlaid lock name");
        exit(-1);
    }
    
    sprintf(SEM_NAME,"/sem_%s",file);
    
    /* Create the semaphore, the value of the semaphore is 1 because there can be only
       one process working in the locked file*/
    if ((sem = sem_open(SEM_NAME, O_CREAT, 0644, 1)) == SEM_FAILED) {
        perror("sem_open");
        exit(-1);
    }
    //printf("SEMAPHORE VALUE: %p\n",sem);
    
}

/* Lock a file */
void file_lock (char * file) {
    // int value;
    
    /*Set fcntl params*/
    fl.l_type = F_WRLCK;
    fl.l_whence = SEEK_SET;
    fl.l_start = 0;
    fl.l_len = 0;
    fl.l_pid = getpid();

    create_lock(file);

    sem_wait(sem); /* decrement the semaphore value, now the semaphore has value 0, any process can continue */

    // printf("SEMAPHORE VALUE: %p\n",sem);

    /*try to open the file to lock, if it fails is because the file does not exist*/
    if ((fd = open(file,O_RDWR)) == -1) {
        perror("open");
        return;
    }

    //sem_getvalue(sem,&value);
    //    printf("SEM en memoria: %p\n", sem);
    // printf("value before sem: %d\n",*value);

    if (fcntl(fd, F_SETLK, &fl) == -1) { /* lock the file using fcntl*/
        if (errno == EACCES || errno == EAGAIN) {
            //            sem_getvalue(sem,&value);
            //      printf("Value after sem: %d\n",value);
            printf("ERROR,SHOULDN'T PRINT THIS!\n"); /* shouldn't print this error */
        } else {
            printf("ERROR FCNTL\n");
        }
    }
}

/* Unlock a file */
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
}

/* Destroy the semaphore created before */
void destroy_lock (char * file) {
    char SEM_NAME[245]; /* Name of the semaphore */

    if (strlen(file) == 0) { /* the file name cannot be empty */
        fprintf(stderr,"Invlaid lock name");
        exit(-1);
    }
    
    sprintf(SEM_NAME,"/sem_%s",file); /* concat /sem_ to the file name */

    //    printf("LOCK NAME: %s\n",SEM_NAME);
    
    if (sem_unlink(SEM_NAME) == -1) { /* destroy the semaphore */
        perror("sem_unlink");
        exit(-1);
    }
}
