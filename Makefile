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

ALL_BINARIES = ${REPL_FINAL_BINARY}

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
	@echo "CFLAGS   = ${CFLAGS}"
	@echo "CXXFLAGS = ${CXXFLAGS}"
	@echo "LDFLAGS  = ${LDFLAGS}"
	@echo "CC       = ${CC}"
	@echo "CXX      = ${CXX}"


%.o: %.c
	@echo CC $<
	@${CC} ${CFLAGS} -c $< -o $@

%.o: %.cc
	@echo CXX $<
	@${CXX} ${CXXFLAGS} -c $< -o $@


clean:
	@echo Cleaning...
	@rm -f ${ALL_OBJECTS} ${ALL_BINARIES}

nuke: clean

install: ${ALL_BINARIES}
	@echo installing executables to ${DESTDIR}${PREFIX}/bin
	@mkdir -p ${DESTDIR}${PREFIX}/bin
	@for n in $(ALL_BINARIES); do \
		cp $$n ${DESTDIR}${PREFIX}/bin/$$n; \
		chmod 755 ${DESTDIR}${PREFIX}/bin/$$n; \
	done

uninstall:
	@echo removing executables from ${DESTDIR}${PREFIX}/bin
	@for n in $(ALL_BINARIES); do \
		rm -f ${DESTDIR}${PREFIX}/bin/$$n ; \
	done




.PHONY: all options clean install uninstall docs nuke





#define generateFields
#	./deffield.sh -f2 $(1) -f2 lib/reset-run-exit.clp > $(2).h
#endef
#
#define generateDefines
#	echo "Generating encoders, decoders, and enumerations for $(1)..."
#	$(call generateFields,def/$(1)/instruction.clp,defines_$(1))
#endef
#
#define generateDefinesRule
#
#defines_$(1).h: ${COMMON_GEN_ENCODER_DECODER_FILES} def/$(1)/instruction.clp
#	@$(call generateDefines,$(1))
#
#endef
#
#$(foreach i,iris,$(eval $(call generateDefinesRule,$(i))))

include deps.make
