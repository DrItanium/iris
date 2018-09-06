#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdbool.h>
#include "util.h"

struct _reent* _impure_ptr ;

int errStorage;

void openfw(FileWrapper* fw) {
   FILE* tmp;
   tmp = fopen(fw->line, fw->permissions);
   if(!tmp) {
      fprintf(stderr, "couldn't open %s\n", fw->line);
      exit(errno);
   }
   fw->needsclosing = 1;
   fw->fptr = tmp;
}

void closefw(FileWrapper* fw) {
   if(fw->needsclosing && fclose(fw->fptr) != 0) {
      fprintf(stderr, "couldn't close %s\n", fw->line); 
      exit(errno);
   }
}

void exit(int code) {
   // do nothing
}

int* __errno() {
   return &errStorage;
}

int fgetc(FILE* fp) {
   return EOF;
}

size_t strlen(const char* str) {
   size_t count = 0;
   while (*str != 0) {
      if (*str == 0) {
         break;
      } 
      ++count;
   }
   return count;
}

int fprintf(FILE* fp, const char* str, ...) {
   return 0;
}
int printf(const char* str, ...) {
   return 0;
}
int sprintf(char* str, const char* fmt, ...) {
   return 0;
}

FILE* fopen(const char* name, const char* type) {
   errno = EINVAL;
   return NULL;
}

int fclose(FILE* type) {
   // TODO set errno
   errno = EINVAL;
   return EOF;
}

int __srget(FILE* fp) {
   errno = EINVAL;
   return EOF;
}

int __swbuf(int c, FILE* stream) {
   return EOF;
}

void* sbrk(ptrdiff_t incr) {
   return NULL;
}


/* vim: set expandtab tabstop=3 shiftwidth=3: */
