include config.mk

CORE_OBJS := iris.o \
	exceptions.o \
	invoke.o \
	register.o \
	logic.o \
	IODevices.o \
	opcodes.o
	

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
invoke.o: invoke.cc types.h iris.h exceptions.h IODevices.h register.h \
 InstructionFormats.def opcodes.h
IODevices.o: IODevices.cc IODevices.h types.h exceptions.h
iris.o: iris.cc iris.h types.h exceptions.h IODevices.h register.h \
 InstructionFormats.def
logic.o: logic.cc iris.h types.h exceptions.h IODevices.h register.h \
 InstructionFormats.def opcodes.h
opcodes.o: opcodes.cc opcodes.h types.h InstructionFormats.def
opcode_tester.o: opcode_tester.cc iris.h types.h exceptions.h IODevices.h \
 register.h InstructionFormats.def
register.o: register.cc register.h types.h
