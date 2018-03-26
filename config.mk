LIBS = -lc -lm -lpthread

CC := gcc
CXX := g++
GENFLAGS = -Wall -g3 -Imisc/PEGTL/include 
CXXFLAGS = -std=c++17 ${GENFLAGS}
LDFLAGS = -std=c++17 ${LIBS}
PREFIX = /usr/local
