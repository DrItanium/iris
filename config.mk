LIBS = -lc -lm

CC = /home/jscoggins/sys/toolchains/i960-elf-gcc-3.4.6/bin/i960-elf-gcc
LEX = flex
YACC = bison
CFLAGS = -g3 -ansi -std=c99 -Wall -Iinclude/
LDFLAGS = ${LIBS} 
PREFIX = /usr/local
