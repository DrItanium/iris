AssemblerStructures.o: AssemblerStructures.cc Assembler.h Problem.h \
 AssemblerStructures.h Types.h Core.h Opcodes.def
Core.o: Core.cc Core.h Types.h Problem.h Opcodes.def
Linker.o: Linker.cc Types.h Core.h Problem.h Opcodes.def
Simulator.o: Simulator.cc Core.h Types.h Problem.h Opcodes.def
