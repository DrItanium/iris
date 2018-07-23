\ iris assembler for gforth
require code.fs
: addr8 ( i -- i8 ) 0xFF and ; 
: addr4 ( i -- i4 ) 0x0F and ;
: num>reg ( i -- ri ) 0xF and ;
get-current
also assembler definitions
include ./opcodes.fs
: do-asm0: ( -- ) 0 c, ;
: do-asm1: ( dest -- ) num>reg addr8 c, ;
: do-asm2: ( src dest -- ) num>reg swap num>reg 4 lshift or addr8 c, ;
: do-asm3: ( src2 src dest -- ) do-asm2: do-asm1: ;
: do-asm4: ( s3 s2 s d -- ) do-asm2: do-asm2: ;
: do-asmi16: ( imm16 -- ) dup addr8 c, 8 rshift addr8 c, ;
: do-asm1i16: ( imm16 dest -- ) do-asm1: do-asmi16: ;
: do-asm2i16: ( imm16 src dest -- ) do-asm2: do-asmi16: ;
: stash-opcode ( n -- ) c@ addr8 c, ;
: asm0: ( n -- ) create c, does> stash-opcode do-asm0: ;
: asm1: ( n -- ) create c, does> stash-opcode do-asm1: ;
: asm2: ( n -- ) create c, does> stash-opcode do-asm2: ;
: asm3: ( n -- ) create c, does> stash-opcode do-asm3: ;
: asm4: ( n -- ) create c, does> stash-opcode do-asm4: ;
: asmi16: ( n -- ) create c, does> stash-opcode do-asmi16: ;
: asm1i16: ( n -- ) create c, does> stash-opcode do-asm1i16: ;
: asm2i16: ( n -- ) create c, does> stash-opcode do-asm2i16: ;
include ./asmops.fs
include ./registers.fs

previous set-current
