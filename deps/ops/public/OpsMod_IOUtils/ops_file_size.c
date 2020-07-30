#include <sys/stat.h>
#include <stdio.h>

long ops_file_size(const char *file_name) {
  struct stat st;
  long s;
  int rc;
  rc = stat(file_name, &st);
  if (rc == 0) {
    return st.st_size;
  } else {
    perror ("Error in stat call:");
    return -1;
  }
}
