#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <time.h>
#include <errno.h>

struct flock fl;
int fd;

void file_lock (char * file) {
    struct timespec timer = {0, 100000000};

    fl.l_type = F_RDLCK;
    fl.l_whence = SEEK_SET;
    fl.l_start = 0;
    fl.l_len = 0;
    
 retry:
    if ((fd = open(file,O_RDWR)) == -1) {
        perror("open");
        return;
    } 
    if (fcntl(fd, F_SETLK, &fl) == -1) {
        if (errno == EACCES || errno == EAGAIN) {
            nanosleep(&timer,&timer);
            close(fd);
            goto retry;
        } else {
            perror("fcntl create");
            return;
        } 
    }
}

void file_unlock (char * file) {
    fl.l_type = F_UNLCK;
    fl.l_whence = SEEK_SET;
    fl.l_start = 0;
    fl.l_len = 0;
    
    if (fcntl(fd, F_SETLK, &fl) == -1) {
        perror("fcntl unlock");
        return;
    }
}
