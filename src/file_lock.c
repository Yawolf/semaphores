#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <dirent.h>
#include <time.h>

#define SMAX 255

void file_lock (char * file) {
    FILE * fd;
    char lock_path[SMAX];
    struct timespec timer = {0, 100000000};
test:
    if (exists_lock(file) == 0) {
        nanosleep (&timer, &timer); goto test;
    } else {
        sprintf(lock_path, "/tmp/%s.lck", file);
        fd = fopen (lock_path,"a");

        if (fd < 0) {
            perror("open");
            return;
        }
    }
}

void file_unlock (char * file) {
    char lock_path[SMAX];
    
    if (exists_lock(file) == 0) {
        sprintf(lock_path,"/tmp/%s.lck",file);
        if (remove (lock_path) < 0) {
            perror("remove");
            return;
        }
    }
}

int exists_lock (char * file) {
    DIR * dir;
    char lock_name[SMAX];
    struct dirent * entry;
    sprintf(lock_name, "%s.lck",file);

    if ((dir = opendir ("/tmp/")) == NULL) {
        perror("opendir");
        return -1;
    } 

    while ((entry = readdir(dir)) != NULL) {
        if (!strcmp(entry->d_name,lock_name)) return 0;
        else continue;
    }
    return 1;
}
