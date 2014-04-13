/* sim.c - the iris simulator */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "iris.h"

static void usage(char* arg0);
static int execute(FILE* file);
static void startup(void);
static void shutdown(void);
static void installprogram(FILE* file);
static core proc;

int main(int argc, char* argv[]) {
   /* initialize the core */
   FILE* target;
   int code;
   code = 0;
   startup();
   if(argc == 1) {
      /* assume standard input */
      code = execute(stdin);
   } else if (argc == 2) {
      if(strcmp(argv[1], "-h") == 0) {
         usage(argv[0]);
         code = 1;
      } else {
         /* a file */
         target = fopen(argv[1], "r");
         if(target != 0) {
            code = execute(target);
            if(fclose(target) != 0) {
               fprintf(stderr, "Couldn't close file: %s\n", argv[1]);
               code = 1;
            }
         } else {
            fprintf(stderr, "Couldn't open file: %s\n", argv[1]);
            code = 1;
         }
      }
   } else {
      usage(argv[0]);
      code = 1;
   }
   /*
      datum d;
      instruction tmp;
      d.group = InstructionGroupCompare;
      tmp.compare.op = CompareOpEq;
      tmp.compare.reg0 = 7;
      tmp.compare.reg1 = 5;
      tmp.compare.combinebits = CombineBitsOpNil;
      d.rest = tmp.value;
      decode(&proc, d.value);
      printf("equality = %d\n", proc.predicateregister);
      printf("sizeof(instruction) = %ld\n", sizeof(instruction));
      */
   shutdown();
   return code;
}

void usage(char* arg0) {
   fprintf(stderr, "usage: %s -h | <file> | <pipe>\n", arg0);
}
int execute(FILE* file) {
   /* install the program to memory */
   installprogram(file); 
   /* TODO: Install code */
   do {
      decode(&proc, &proc.code[proc.pc]);
      if(proc.advancepc) {
         proc.pc++;
      }
   } while(!proc.terminateexecution);
   return 0;
}
void startup() {
   int i;
   proc.pc = 0;
   proc.terminateexecution = 0;
   proc.advancepc = 1;
   for(i = 0; i < RegisterCount; i++) {
      proc.gpr[i] = 0;
   }
   for(i = 0; i < ImplicitRegisterPredicate; i++) {
      proc.impliedregisters[i] = 0;
   }
   for(i = 0; i < MemorySize; i++) {
      proc.data[i] = 0;
      proc.code[i].full = 0;
   }
}

void shutdown() {
   /* nothing to do at this point */
}

void installprogram(FILE* file) {
   /* read up to 64k of program information */
   int count, i;
   datum cell;
   instruction contents; /* theres a reason for this */
   i = 0;
   cell = 0;
   count = fread(&contents, sizeof(contents), 1, file);
   while(count > 0) {
      if(i < MemorySize) {
         proc.code[i] = contents;
         i++; 
         count = fread(&contents, sizeof(contents), 1, file);
      } else {
         break;
      }
   }
   count = fread(&cell, sizeof(cell), 1, file);
   while(count > 0) {
      if(i < MemorySize) {
         proc.data[i] = cell;
         i++;
         count = fread(&contents, sizeof(contents), 1, file);
      } else {
         printf("warning: Input file still has: %d cells left.\n", count);
         break;
      }
   }
}
