#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <sys/stat.h>
#include <semaphore.h>

struct flock fl;
int fd;
sem_t * sem;
char SEM_NAME[255];

void file_lock (char * file) {
    sprintf(SEM_NAME,"/sem_%s",file);
    
    fl.l_type = F_WRLCK;
    fl.l_whence = SEEK_SET;
    fl.l_start = 0;
    fl.l_len = 0;

    if ((fd = open(file,O_RDWR)) == -1) {
        perror("open");
        return;
    }

    sem = sem_open(SEM_NAME, O_CREAT, 0644, 1);

    if(sem == SEM_FAILED) {
        perror("sem_open");
        return;
    }
    sem_wait(sem);
    if (fcntl(fd, F_SETLK, &fl) == -1) {
        if (errno == EACCES || errno == EAGAIN) {
            printf("ERROR,SHOULDN'T PRINT THIS!\n");
        } else {
            printf("ERROR FCNTL\n");
        }
    }
}

void file_unlock (char * file) {
    fl.l_type = F_UNLCK;
    fl.l_whence = SEEK_SET;
    fl.l_start = 0;
    fl.l_len = 0;
    fl.l_pid = getpid();

    if (fcntl(fd, F_SETLK, &fl) == -1) {
        perror("fcntl unlock");
        return;
    }
    sem_post(sem);
    sem_close(sem);
}
