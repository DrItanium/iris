# syn - a series of virtual cpus and other such things as I learn more about
# processor simulation
# See LICENSE file for copyright and license details.

include config.mk
	
ARCH_OBJECTS = Core.o
ASSEMBLER_OBJECTS = Assembler.o \
					AssemblerBase.o

ALL_OBJECTS = ${ARCH_OBJECTS} \
			  ${ASSEMBLER_OBJECTS}


all: options 

docs: 
	@echo "running doxygen"
	@doxygen

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
