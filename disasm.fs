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
  dup disasm-op instruction-table @ execute ;

: disasm ( addr u -- ) \ gforth
  \G disassemble u aus starting at addr
  bounds u+do
  cr ." ( " i hex. ." ) " i i @ disasm-inst
  1 cells +loop
  cr ;

' disasm IS discode

definitions

: disasm-instruction ( addr w -- )
  \ disassemble instruction with no arguments
  dup disasm-op instruction-table @ execute ;

\ disassemble various formats
: disasm-noargs ( addr w -- ) 2drop ;
: disasm-1reg ( addr w -- ) disasm-rdest . drop ; 
: disasm-2reg ( addr w -- ) 
  dup disasm-rdest .
  disasm-rsrc . drop ;
: disasm-3reg ( addr w -- ) 
  dup disasm-rdest .
  dup disasm-rsrc .
  disasm-rsrc2 . drop ;
: disasm-4reg ( addr w  -- )
  dup disasm-rdest .
  dup disasm-rsrc .
  dup disasm-rsrc2 .
  disasm-rsrc3 . drop ;

: disasm-1reg-imm16 ( addr w -- )
  dup disasm-rdest .
  disasm-imm16 . drop ;
: disasm-imm16-only ( addr w -- )
  disasm-imm16-noreg . drop ;

: disasm-2reg-imm16 ( addr w -- )
  dup disasm-rdest .
  dup disasm-rsrc .
  disasm-imm16 . drop ;

: disasm-1reg-imm8 ( addr w -- )
  dup disasm-rdest .
  disasm-imm8-with-reg . drop ;

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
' disasm-noargs ' instruction-table define-format asm-noargs 
' disasm-1reg ' instruction-table define-format asm-1reg
' disasm-2reg ' instruction-table define-format asm-2reg
' disasm-3reg ' instruction-table define-format asm-3reg
' disasm-4reg ' instruction-table define-format asm-4reg
' disasm-1reg-imm16 ' instruction-table define-format asm-1reg-imm16
' disasm-2reg-imm16 ' instruction-table define-format asm-2reg-imm16
' disasm-1reg-imm8 ' instruction-table define-format asm-1reg-imm8
' disasm-2wreg ' instruction-table define-format asm-2wreg 
' disasm-3wreg ' instruction-table define-format asm-3wreg 

\ todo update opcodes.fs so that it can be used in both execution, assembly, and disassembly
\ : {opcode ( -- 0 ) 0 ;
\ : opcode} ( n -- ) drop ;
\ : opcode: ( n "name" "body" "encoder" "decoder" -- n+1 )
\   create addr8 dup , ( n8 n8 )
\   ' over ( n8 n8 "body" n8 ) bodies ! ( n8 )
\   ' over ( n8 n8 "encoder" n8 ) encoders ! ( n8 )
\   ' over ( n8 "decoder" n8 ) decoders ! ( n8 )
\   1+ 
\   does> ( args* -- encoded-args* control count )
\   ( args* addr )
\   @ ( args* c ) dup ( args* c c ) >r ( args* c count )
\   encoders @ execute 1+ ( advance the counter first ) r> addr8 swap ;
\ include ./opcodes.fs
include ./asmops.fs

 previous set-current
