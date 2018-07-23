\ simple iris disassembler for gforth
\ according to the documentation this should be done first before other actions
\ uses arch/mips/disasm.fs as a base
get-current 
vocabulary disassembler
also disassembler definitions
\ instruction fields
: disasm-op ( w -- u )
  \ lowest eight bits
  0xFF and ;
: disasm-register ( w shift -- u ) rshift 0x0F and ;
: disasm-rdest ( w -- u )  8 disasm-register ;
: disasm-rsrc ( w -- u )  12 disasm-register ;
: disasm-rsrc2 ( w -- u ) 16 disasm-register ;
: disasm-rsrc3 ( w -- u ) 20 disasm-register ;
: disasm-imm16 ( w -- u ) 16 rshift 0xFFFF and ;
: disasm-imm16-noreg ( w -- u ) 12 rshift 0xFFFF and ;
: disasm-imm8-with-reg ( w -- u ) 16 rshift 0xFF and ;

\ decode tables

: disasm-illegal ( addr w -- )
  \ disassemble illegal/unknown instruction w at addr 
  hex. ." , ( illegal inst ) " drop ;

: disasm-table ( n "name" -- )
  \ initialize table with n entries with disasm-illegal
  create 0 ?do
  ['] disasm-illegal ,
  loop
does> ( u -- addr )
    swap cells + ;
    
0x100 disasm-table instruction-table \ instruction decode table

\ disassembler central decode cascade
dup set-current
: disasm-inst ( addr w -- ) 
  \G disassemble instruction w at addr (addr is used for computing 
  \G branch targets)
  instruction-table @ execute ;

: disasm ( addr u -- ) \ gforth
  \G disassemble u aus starting at addr
  bounds u+do
  cr ." ( " i hex. ." ) " i i c@ disasm-inst
  1+ +loop 
  cr ;

' disasm IS discode

definitions

: disasm-instruction ( addr w -- )
  \ disassemble instruction with no arguments
  dup disasm-op instruction-table @ execute ;

\ disassemble various formats
: disasm-noargs ( addr -- 1 ) drop 1 ;
: disasm-1reg ( addr -- n ) 1+ c@ disasm-rdest . 1 ;
: disasm-2reg ( addr -- n ) 
  1+ c@ dup  
  disasm-rsrc .
  disasm-rdest . 1 ;
: disasm-3reg ( addr -- n ) 
  1+ dup ( addr+1 addr+1 )
  1+ ( addr+1 addr+2 )
  c@ disasm-rsrc2 . ( addr+1 )
  c@ dup ( l u )
  disasm-rsrc .
  disasm-rdest . 2 ;
: disasm-4reg ( addr -- n ) 
  1+ dup ( addr+1 addr+1 )
  1+ ( addr+1 addr+2 )
  c@ dup ( addr+1 l u ) 
  disasm-rsrc3 . ( addr+1 l )
  disasm-rsrc2 . ( addr+1 )
  c@ dup ( l u )
  disasm-rsrc .
  disasm-rdest . 2 ;
: disasm-1reg-imm16 ( addr -- n )
  1+ dup ( a+1 a+1 )
  1+ dup c@ ( a+1 a+2 v ) 
  swap 1+ c@ ( a+1 v v2 )
  8 lshift ( a+1 v v2<<8 )
  or ( a+1 imm )
  0xFFFF and hex . ( a+1 )
  c@ disasm-rdest . 3 ;
: disasm-imm16-only ( addr -- n )
  1+ dup ( a+1 a+1 )
  c@ swap ( v a+1 )
  1+ ( v a+2 )
  c@ 8 lshift or 0xFFFF and hex . 2 ;

: disasm-2reg-imm16 ( addr -- n )
  1+ dup ( a+1 a+1 )
  1+ dup c@ ( a+1 a+2 v ) 
  swap 1+ c@ ( a+1 v v2 )
  8 lshift ( a+1 v v2<<8 )
  or ( a+1 imm )
  0xFFFF and hex . ( a+1 )
  c@ dup disasm-rsrc .
  disasm-rdest . 3 ;

: disasm-2wreg ( addr w -- ) disasm-2reg ;
: disasm-3wreg ( addr w -- ) disasm-3reg ;


\ meta-definining word for instruction format disassembling definitions

\ the following word defines instruction format words, which in turn define
\ anonymous words for disassembling specific instructions and
\ put them int he appropriate decode table.

: define-format ( disasm-xt table-xt -- )
  \ define an instruction format that uses disasm-xt for disassembling and 
  \ enters the defined instructions into table table-xt
  create 2, 
does> 
    \ defines an anonymous word for disassembling instruction inst,
    \ and enters it as u-th entry into table-xt
    2@ swap here name string, ( u table-xt disasm-xt c-addr ) \ remember string
    noname create 2, \ define anonymous word 
    execute lastxt swap ! \ enter execution token of defined word into table-xt
does> ( addr w -- )
    \ disassemble instruction w at addr
    2@ >r ( addr w disasm-xt R: c-addr )
    execute ( R: caddr ) \ disassemble operands
    r> count type ; \ print name


\ all of the following words have the stack effect ( u "name" )
' disasm-noargs ' instruction-table define-format  asm0:
' disasm-1reg ' instruction-table define-format asm1:
' disasm-2reg ' instruction-table define-format asm2:
' disasm-3reg ' instruction-table define-format asm3:
' disasm-4reg ' instruction-table define-format asm4:
' disasm-1reg-imm16 ' instruction-table define-format asm1i16:
' disasm-2reg-imm16 ' instruction-table define-format asm2i16:
' disasm-2wreg ' instruction-table define-format asm2w: 
' disasm-3wreg ' instruction-table define-format asm3w:
' disasm-imm16-only ' instruction-table define-format asmi16:

include ./opcodes.fs
include ./asmops.fs

previous set-current
