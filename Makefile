include config.mk

CORE_OBJS := iris.o

SIM_OBJS := backends/memcore/mem_core.o \
	backends/memcore/exceptions.o
	

IRIS_ARCHIVE := libiris.a
IRIS_SIM_ARCHIVE := libiris_memcore.a

IRIS_TESTER_OBJS := cmd/opcode_tester.o
IRIS_CLP_GEN_CMD_OBJS := cmd/generate_clp_code.o

IRIS_TESTER := iris_test
IRIS_CLP_GEN_CMD := iris_generate_clp_code

OBJS := $(CORE_OBJS) \
		$(IRIS_TESTER_OBJS) \
		$(SIM_OBJS) \
		$(IRIS_CLP_GEN_CMD_OBJS)
PROGS := $(IRIS_ARCHIVE) \
	     $(IRIS_TESTER) \
		 $(IRIS_SIM_ARCHIVE) \
		 $(IRIS_CLP_GEN_CMD)


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

$(IRIS_SIM_ARCHIVE): $(SIM_OBJS)
	@echo AR ${IRIS_SIM_ARCHIVE}
	@${AR} rcs ${IRIS_SIM_ARCHIVE} ${SIM_OBJS}

$(IRIS_TESTER): $(IRIS_ARCHIVE) $(IRIS_SIM_ARCHIVE) $(IRIS_TESTER_OBJS)
	@echo LD $@
	@${CXX} -o ${IRIS_TESTER} ${LDFLAGS} ${IRIS_TESTER_OBJS} ${IRIS_ARCHIVE} ${IRIS_SIM_ARCHIVE}

$(IRIS_CLP_GEN_CMD): $(IRIS_CLP_GEN_CMD_OBJS)
	@echo LD $@
	@${CXX} -o ${IRIS_CLP_GEN_CMD} ${LDFLAGS} ${IRIS_CLP_GEN_CMD_OBJS}

.cc.o :
	@echo CXX $<
	@${CXX} -I. -Ilib ${CXXFLAGS} -c $< -o $@

clean: 
	@echo Cleaning...
	@rm -f ${OBJS} ${PROGS}



.PHONY: options

# generated via g++ -MM -std=c++17 *.cc


exceptions.o: backends/memcore/exceptions.cc \
 backends/memcore/exceptions.h types.h
mem_core.o: backends/memcore/mem_core.cc types.h \
 backends/memcore/mem_core.h iris.h types.h register.h mem_bank.h \
 opcodes.h InstructionFormats.def mem_bank.h \
 register.h opcodes.h
iris.o: iris.cpp types.h iris.h register.h mem_bank.h opcodes.h \
 InstructionFormats.def
opcode_tester.o: cmd/opcode_tester.cc iris.h types.h register.h mem_bank.h \
 opcodes.h InstructionFormats.def backends/memcore/mem_core.h iris.h \
 types.h mem_bank.h register.h encoding.h
