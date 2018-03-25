# syn - a series of virtual cpus and other such things as I learn more about
# processor simulation
# See LICENSE file for copyright and license details.

include config.mk
	
COMMON_THINGS = Core.o

REPL_FINAL_OBJECTS = Repl.o \
					 RegisteredExternalAddressAssemblers.o \
					 IrisCoreAssemblerStateWrapper.o \
					 IrisCoreWrapper.o \
					 ${COMMON_THINGS} \
					 ${ARCH_OBJECTS} \
					 ${ASM_PARSERS_OBJECTS} \

DEFINE_OBJECTS = defines_iris.h

ALL_OBJECTS = ${COMMON_THINGS} \
			  ${ARCH_OBJECTS} \
			  ${REPL_OBJECTS} \
			  ${REPL_FINAL_OBJECTS}


all: options ${ALL_BINARIES}

docs: ${ALL_BINARIES}
	@echo "running doxygen"
	@doxygen

options:
	@echo syn build options:
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

.PHONY: all options clean install uninstall docs





include deps.make
