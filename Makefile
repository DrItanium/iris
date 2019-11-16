include config.mk

CORE_OBJS := iris.o \
	exceptions.o \
	register.o \
	IODevices.o \
	opcodes.o \
	encoding.o \
	mem_core.o
	

IRIS_ARCHIVE := libiris.a

IRIS_TESTER_OBJS := opcode_tester.o

IRIS_TESTER := iris_test

OBJS := $(CORE_OBJS) \
		$(IRIS_TESTER_OBJS)
PROGS := $(IRIS_ARCHIVE) \
	     $(IRIS_TESTER)


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

$(IRIS_TESTER): $(IRIS_ARCHIVE) $(IRIS_TESTER_OBJS)
	@echo LD $@
	@${CXX} -o ${IRIS_TESTER} ${LDFLAGS} ${IRIS_TESTER_OBJS} ${IRIS_ARCHIVE}

.cc.o :
	@echo CXX $<
	@${CXX} -I. ${CXXFLAGS} -c $< -o $@

clean: 
	@echo Cleaning...
	@rm -f ${OBJS} ${PROGS}



.PHONY: options

# generated via g++ -MM -std=c++17 *.cc


encoding.o: encoding.cc encoding.h opcodes.h types.h \
 InstructionFormats.def exceptions.h
exceptions.o: exceptions.cc exceptions.h types.h
IODevices.o: IODevices.cc IODevices.h types.h exceptions.h mem_bank.h
iris.o: iris.cc types.h iris.h exceptions.h register.h mem_bank.h \
 opcodes.h InstructionFormats.def
mem_core.o: mem_core.cc types.h mem_core.h iris.h exceptions.h register.h \
 mem_bank.h opcodes.h InstructionFormats.def IODevices.h
opcodes.o: opcodes.cc opcodes.h types.h InstructionFormats.def
opcode_tester.o: opcode_tester.cc iris.h types.h exceptions.h register.h \
 mem_bank.h opcodes.h InstructionFormats.def mem_core.h IODevices.h \
 encoding.h
register.o: register.cc register.h types.h mem_bank.h
