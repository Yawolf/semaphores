#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <unistd.h>
#include <fcntl.h>
#include <time.h>

#define LOCK '1'
#define UNLOCK '2'

char lock_name [100];

int create_lock (char * file_name) {
  FILE * fd;
  DIR * curr_dir;
  char path_cdir [100]; /* a name with size 100 chars */
  struct dirent *dir;  

  snprintf(lock_name,100,".%s.lock",file_name);

  if (getcwd(path_cdir,100) == NULL) {
    perror("getcwd");
    return -1;
  }

  if ((curr_dir = opendir(path_cdir)) == NULL) {
    perror("opendir");
    return -2;
  }
  
  while ((dir = readdir(curr_dir)) != NULL) {
    if (dir->d_name[0] != '.') continue;
    else 
      if (!strcmp(dir->d_name,lock_name)) { printf ("%s\n",dir->d_name); return 0; }
      else continue;
  }
  
  fd = fopen (lock_name,"ab+");

  if (fd == NULL) return -4;
  else fputc(UNLOCK,fd);

  fclose (fd);
  
  return 0;
}

/*At first, this function comproves if exists a file lock, if it exists try to take it.
If the lock is free can take it, in another way the process waits till the lock is released*/

void file_lock (char * file) {
  FILE * fd;
  int lock_value;
  struct timespec timer;
  timer.tv_sec = 0;
  timer.tv_nsec = 100000000;
  int ret = create_lock(file);

  if (ret < 0) {
    fprintf (stderr,"cannot create lock");
    return;
  } 
  
  fd = fopen (lock_name, "r+");
  
  if (fd == NULL) {
    perror("open");
    return;
  }
prove:
  lock_value = fgetc(fd);
  if (lock_value == UNLOCK) { fd = freopen(NULL,"w+",fd); fputc(LOCK,fd); }
  else { printf("Nanos!!\n"); nanosleep (&timer, &timer); goto prove; }

  fclose(fd);
}

/** This function is so easy, the process release the lock */

void file_unlock (char * file) {
  FILE * fd;
  int lock_value;
  
  fd = fopen (lock_name,"r+");
  if (fd == NULL) {
    perror("open");
    return;
  }
  lock_value = fgetc(fd);
  if (lock_value == LOCK) { fd = freopen(NULL,"w+",fd); fputc(UNLOCK,fd); }
  else fprintf(stderr,"lock already released");
}

void clear_lock (char * file) {
  FILE * fd;
  int lock_value;
  if ((fd = fopen (lock_name,"r+")) == NULL) {
    perror("open");
    return;
  }
  lock_value = fgetc(fd);
  printf("%c",lock_value);
  (lock_value == UNLOCK) ? remove (lock_name) : fprintf(stderr,"cannot remove lock, still in use");
}
