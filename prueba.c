#include <stdio.h>

int main (void) {
  FILE * fd = fopen ("fdsa.txt","ab+");
  fputc('4',fd);
  fclose (fd);
}
