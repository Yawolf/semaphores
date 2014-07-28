#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <time.h>
#include <errno.h>
#include <string.h>

struct flock fl;
int fd;

void file_lock (char * file, char * mode) {
    struct timespec timer = {0, 100000000};
    
    fl.l_type = F_WRLCK;
    fl.l_whence = SEEK_SET;
    fl.l_start = 0;
    fl.l_len = 0;
    fl.l_pid = getpid();
    
    if ((fd = open(file,O_RDWR)) == -1) {
        perror("open");
        return;
    }
    
    while (fcntl(fd, F_SETLK, &fl) == -1) {
        if (errno == EACCES || errno == EAGAIN) {
            printf("sleeping!\n");
            nanosleep(&timer,&timer);
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
}
