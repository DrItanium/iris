LIBS = -lc -lm -lpthread

CC := gcc
CXX := g++-7.3.0
GENFLAGS = -Wall -g3 -Og
CXXFLAGS = -std=c++17 ${GENFLAGS}
LDFLAGS = -std=c++17 ${LIBS}
PREFIX = /usr/local
