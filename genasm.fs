{enum
#define X(c, t, w) enum: Asm ## c 
#define FirstX(c, t, w) X(c,t,w) 
#include "Opcodes.def"
#undef X
#undef FirstX
enum}

#define X(c, t, w) : ! ## w ( args* -- n ) t Asm ## c or ;
#define FirstX(c, t, w) X(c, t, w) 
#include "Opcodes.def"
#undef FirstX
#undef X

