#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <unistd.h>
#include <fcntl.h>
#include <time.h>

#define LOCK '1'
#define UNLOCK '0'

char lock_name [100];

/** This function creates the lock named .$file_to_lock.lck  */

void create_lock (char * file_name) {
  FILE * fd;
  snprintf(lock_name,100,".%s.lck",file_name); /* save in lock_name the name of the lock */

  fd = fopen (lock_name,"ab+"); /* open the lock with the flags ab+, it means, Create or append */
  if (fd == NULL) { fprintf(stderr,"cannot create lock\n"); return; } /* if fopen fails, cannot create the lock */
  else fputc(UNLOCK,fd); /* otherway set free the lock writing 0 inside */

  fclose (fd); /* close lock */
}

/*At first, this function comproves if exists a file lock, if it exists try to take it.
If the lock is free can take it, in another way the process waits till the lock is released*/

void file_lock (char * file) {
  FILE * fd;
  int lock_value;
  struct timespec timer; /* struct necessary for nanosleep */
  timer.tv_sec = 0;
  timer.tv_nsec = 100000000; /* 100000000 nsec = 0.1 sec */
  int ret = exists_lock(file); /* comproves if the lock exists */

  if (ret < 0) {
    fprintf (stderr,"this lock does not exists\n"); /* a return value < 0 means that the file doesn't exist */ 
    return;
  } 
  
  fd = fopen (lock_name, "r+"); /* Open the lock in READ/WRITE mode */
  
  if (fd == NULL) {
    perror("open");
    return;
  }
prove:
  lock_value = fgetc(fd); /* take the first character */
  if (lock_value == UNLOCK) { fd = freopen(NULL,"w+",fd); fputc(LOCK,fd); }
  /* if the car == 0 then the process can take the lock setting it at 1 */
  else { nanosleep (&timer, &timer); goto prove; }
  /* if the lock is not free the process waits and trys again after 0.1 seconds */

  fclose(fd); /* close the file */
}

/** This function is so easy, the process release the lock */

void file_unlock (char * file) {
  FILE * fd;
  int lock_value;
  int ret = exists_lock(file); /* comproves if the lock exists */

  if (ret < 0) {
    fprintf (stderr,"this lock does not exists.\n"); /* a return value < 0 means that the file doesn't exist */ 
    return;
  }

  fd = fopen (lock_name,"r+");
  
  if (fd == NULL) {
    perror("open");
    return;
  }
  lock_value = fgetc(fd);
  if (lock_value == LOCK) { fd = freopen(NULL,"w+",fd); fputc(UNLOCK,fd); }
  /* If the lock is taken, to release it, the process overwrites the inside value, 1 to 0 */
  else fprintf(stderr,"lock already released");
  
  fclose(fd);
}

/* This function delete the lock assigned to a file */

void clear_lock (char * file) {
  FILE * fd;
  int lock_value;
  int ret = exists_lock(lock_name);

  if (ret < 0) {
    fprintf (stderr,"this lock does not exist.\n");
    return;
  }
  
  if ((fd = fopen (lock_name,"r+")) == NULL) {
    perror("open");
    return;
  }
  
  lock_value = fgetc(fd); 
  /* comprove if the lock has been released  and delete it*/
  (lock_value == UNLOCK) ? remove (lock_name) : fprintf(stderr,"cannot remove lock, still in use");
}

int exists_lock (char * file_name) {
  DIR * curr_dir;
  char path_cdir [100]; /* a name with size 100 chars */
  struct dirent *dir;  

  if (getcwd(path_cdir,100) == NULL) {
    perror("getcwd");
    return -1;
  }
  
  if ((curr_dir = opendir(path_cdir)) == NULL) {
    perror("opendir");
    return -1;
  }
  
  while ((dir = readdir(curr_dir)) != NULL) {
    if (dir->d_name[0] != '.') continue;
    else 
      if (!strcmp(dir->d_name,lock_name)) return 0;
      else continue;
  }
  return -2;
}