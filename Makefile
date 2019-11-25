include config.mk

CORE_OBJS := iris.o \
	exceptions.o \
	opcodes.o

SIM_OBJS := backends/memcore/IODevices.o \
	backends/memcore/register.o \
	backends/memcore/mem_core.o
	

IRIS_ARCHIVE := libiris.a
IRIS_SIM_ARCHIVE := libiris_memcore.a

IRIS_TESTER_OBJS := opcode_tester.o

IRIS_TESTER := iris_test

OBJS := $(CORE_OBJS) \
		$(IRIS_TESTER_OBJS) \
		$(SIM_OBJS)
PROGS := $(IRIS_ARCHIVE) \
	     $(IRIS_TESTER) \
		 $(IRIS_SIM_ARCHIVE)


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

.cc.o :
	@echo CXX $<
	@${CXX} -I. ${CXXFLAGS} -c $< -o $@

clean: 
	@echo Cleaning...
	@rm -f ${OBJS} ${PROGS}



.PHONY: options

# generated via g++ -MM -std=c++17 *.cc

