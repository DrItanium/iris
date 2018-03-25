LIBS = -lc -lm -lpthread

CC := gcc
CXX := g++
GENFLAGS = -Wall -g3 -Imisc/PEGTL/include 
CXXFLAGS = -std=c++17 ${GENFLAGS}
LDFLAGS = ${LIBS}
PREFIX = /usr/local
