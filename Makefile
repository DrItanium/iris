# syn - a series of virtual cpus and other such things as I learn more about
# processor simulation
# See LICENSE file for copyright and license details.

include config.mk
	
ARCH_OBJECTS = Core.o
ASSEMBLER_OBJECTS = Assembler.o \
					AssemblerBase.o


SIMULATOR = simiris

SIMULATOR_OBJECTS = ${ARCH_OBJECTS} \
					Simulator.o

ALL_BINARIES = ${SIMULATOR}

ALL_OBJECTS = ${ARCH_OBJECTS} \
			  ${ASSEMBLER_OBJECTS} \
			  ${SIMULATOR_OBJECTS} \
			  ${ALL_BINARIES}


all: options  ${ALL_BINARIES}

docs: 
	@echo "running doxygen"
	@doxygen

${SIMULATOR}: ${SIMULATOR_OBJECTS}
	@echo LD $@
	@${CXX} ${LDFLAGS} -o ${SIMULATOR} ${SIMULATOR_OBJECTS}

options:
	@echo iris build options:
	@echo "CXXFLAGS = ${CXXFLAGS}"
	@echo "LDFLAGS  = ${LDFLAGS}"
	@echo "CC       = ${CC}"
	@echo "CXX      = ${CXX}"


%.o: %.cc
	@echo CXX $<
	@${CXX} ${CXXFLAGS} -c $< -o $@


clean:
	@echo Cleaning...
	@rm -f ${ALL_OBJECTS} 

.PHONY: all options clean docs

include deps.make
