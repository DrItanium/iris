include config.mk

CORE_OBJS := iris.o \
	exceptions.o

IRIS_ARCHIVE := libiris.a

OBJS := $(CORE_OBJS) 
PROGS := $(IRIS_ARCHIVE) 


all: options $(PROGS)

options:
	@echo Build Options
	@echo ------------------
	@echo CXXFLAGS = ${CXXFLAGS}
	@echo LDFLAGS = ${LDFLAGS}
	@echo ------------------

$(IRIS_ARCHIVE): $(CORE_OBJS)
	@echo AR ${IRIS_ARCHIVE}
	@${AR} rcs ${IRIS_ARCHIVE} ${CORE_OBJS}

.cc.o :
	@echo CXX $<
	@${CXX} -I. ${CXXFLAGS} -c $< -o $@

clean: 
	@echo Cleaning...
	@rm -f ${OBJS} ${PROGS}



.PHONY: options

# generated via g++ -MM -std=c++17 *.cc

exceptions.o: exceptions.cc exceptions.h types.h
iris.o: iris.cc iris.h types.h opcodes.h InstructionFormats.def \
 exceptions.h
