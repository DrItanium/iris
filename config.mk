LIBS = -lc -lm -nostdlib -lgcc -lc -lm -lnosys -lgcc -lgcc

CC = i960-elf-gcc
LEX = flex
YACC = bison
CFLAGS = -g3 -ansi -std=c99 -Wall -Iinclude/ -mkb
LDFLAGS = ${LIBS} -mnumerics -mkb -Wl,--entry=_main -mnumerics -mkb
PREFIX = /usr/local
