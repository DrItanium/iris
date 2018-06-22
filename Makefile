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

LINKER = linkiris

LINKER_OBJECTS = ${ARCH_OBJECTS} \
				 Linker.o

ALL_BINARIES = ${SIMULATOR} \
			   ${LINKER}

ALL_OBJECTS = ${ARCH_OBJECTS} \
			  ${ASSEMBLER_OBJECTS} \
			  ${SIMULATOR_OBJECTS} \
			  ${LINKER_OBJECTS} \
			  ${ALL_BINARIES}


all: options ${ALL_BINARIES}

docs: 
	@echo "running doxygen"
	@doxygen

${SIMULATOR}: ${SIMULATOR_OBJECTS}
	@echo LD $@
	@${CXX} ${LDFLAGS} -o ${SIMULATOR} ${SIMULATOR_OBJECTS}

${LINKER}: ${LINKER_OBJECTS}
	@echo LD $@
	@${CXX} ${LDFLAGS} -o ${LINKER} ${LINKER_OBJECTS}


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

AssemblerStructures.o: AssemblerStructures.cc Assembler.h Problem.h \
 AssemblerStructures.h Types.h Core.h Opcodes.def
Core.o: Core.cc Core.h Types.h Problem.h Opcodes.def
Linker.o: Linker.cc Types.h Core.h Problem.h Opcodes.def
Simulator.o: Simulator.cc Core.h Types.h Problem.h Opcodes.def
