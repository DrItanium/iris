CXX := g++
LD := ${CXX}

GENFLAGS := -Wall -Wextra 
#LIBS :=
OPTIMIZATION_FLAGS := -Og
CXXFLAGS := -std=c++17 ${GENFLAGS} ${OPTIMIZATION_FLAGS} ${DEBUGGING_FLAGS}
LDFLAGS := ${LIBS} ${OPTIMIZATION_FLAGS}
