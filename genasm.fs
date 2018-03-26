enum-start
#define X(c, t, w) enum-next : Asm ## c ( -- n ) literal ; 
#define FirstX(c, t, w) : Asm ## c ( -- n ) literal ;
#include "Opcodes.def"
#undef X
#undef FirstX
enum-done

#define X(c, t, w) : ! ## w ( args* -- n ) t Asm ## c bitwise-oru ;
#define FirstX(c, t, w) X(c, t, w) 
#include "Opcodes.def"
#undef FirstX
#undef X

