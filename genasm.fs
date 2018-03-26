enum-start
#define X(c, t, w) enum-next : w ( -- n ) literal ; 
#define FirstX(c, t, w) : w ( -- n ) literal ;
#include "Opcodes.def"
#undef X
#undef FirstX
enum-done

