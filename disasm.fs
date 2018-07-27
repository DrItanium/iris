\ iris
\ Copyright (c) 2013-2018, Joshua Scoggins and Contributors
\ All rights reserved.
\ 
\ Redistribution and use in source and binary forms, with or without
\ modification, are permitted provided that the following conditions are met:
\     * Redistributions of source code must retain the above copyright
\       notice, this list of conditions and the following disclaimer.
\     * Redistributions in binary form must reproduce the above copyright
\       notice, this list of conditions and the following disclaimer in the
\       documentation and/or other materials provided with the distribution.
\ 
\ THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
\ ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
\ WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
\ DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
\ ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
\ (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
\ LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
\ ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
\ (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
\ SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
\ simple iris disassembler for gforth
\ according to the documentation this should be done first before other actions
\ uses arch/mips/disasm.fs as a base
get-current 
vocabulary disassembler
also disassembler definitions
\ instruction fields
: addr4 ( w -- w ) 0x0F and ;
: addr8 ( w -- w ) 0xFF and ;
: addr16 ( w -- w16 ) 0xFFFF and ;
: disasm-op ( w -- u )
  \ lowest eight bits
  0xFF and ;
: print-reg ( index -- ) 
  addr4
  case
    0 of ." x0 " endof 1 of ." x1 " endof 2 of ." x2 " endof 3 of ." x3 " endof
    4 of ." x4 " endof 5 of ." x5 " endof 6 of  ." x6 " endof 7 of ." x7 " endof
    8 of ." x8 " endof 9 of ." x9 " endof 10 of ." x10 " endof 11 of ." x11 " endof
    12 of ." x12 " endof 13 of ." x13 " endof 14 of ." x14 " endof 15 of ." x15 " endof
    abort" Illegal Register!"
  endcase 
  ;
: disasm-register ( w shift -- ) rshift print-reg ;
: disasm-rdest ( w -- )  0 disasm-register ;
: disasm-rsrc ( w -- )  4 disasm-register ;
: disasm-rsrc2 ( w -- ) 0 disasm-register ;
: disasm-rsrc3 ( w -- ) 4 disasm-register ;
: disasm-imm16 ( w -- u ) 16 rshift addr16 ;
: disasm-imm16-noreg ( w -- u ) 12 rshift addr16 ;
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
: disasm-1reg ( addr -- n ) 1+ c@ disasm-rdest  1 ;
: disasm-2reg ( addr -- n )
  1+ c@ dup 
  disasm-rsrc 
  disasm-rdest 1 ;
: disasm-3reg ( addr -- n )
  1+ dup ( addr+1 addr+1 )
  1+ ( addr+1 addr+2 )
  c@ disasm-rsrc2 ( addr+1 )
  c@ dup ( l u )
  disasm-rsrc 
  disasm-rdest  2 ;
: disasm-4reg ( addr -- n ) 
  1+ dup ( addr+1 addr+1 )
  1+ ( addr+1 addr+2 )
  c@ dup ( addr+1 l u ) 
  disasm-rsrc3  ( addr+1 l )
  disasm-rsrc2  ( addr+1 )
  c@ dup ( l u )
  disasm-rsrc 
  disasm-rdest  2 ;
: disasm-1reg-imm16 ( addr -- n )
  1+ dup ( a+1 a+1 )
  1+ dup c@ ( a+1 a+2 v ) 
  swap 1+ c@ ( a+1 v v2 )
  8 lshift ( a+1 v v2<<8 )
  or ( a+1 imm )
  addr16 hex . ( a+1 )
  c@ disasm-rdest 3 ;
: disasm-imm16-only ( addr -- n )
  1+ dup ( a+1 a+1 )
  c@ swap ( v a+1 )
  1+ ( v a+2 )
  c@ 8 lshift or addr16 hex . 2 ;

: disasm-2reg-imm16 ( addr -- n )
  1+ dup ( a+1 a+1 )
  1+ dup c@ ( a+1 a+2 v ) 
  swap 1+ c@ ( a+1 v v2 )
  8 lshift ( a+1 v v2<<8 )
  or ( a+1 imm )
  addr16 hex . ( a+1 )
  c@ dup disasm-rsrc 
  disasm-rdest 3 ;

: disasm-2wreg ( addr w -- ) disasm-2reg ;
: disasm-3wreg ( addr w -- ) disasm-3reg ;
: disasm-1reg-imm4 ( addr -- n ) 
  1+ c@ dup 
  4 rshift addr4 hex . 
  disasm-rdest 1 ;
: disasm-1reg-imm8 ( addr -- n )
  1+ dup ( a+1 a+1 )
  1+ c@ addr8 hex .
  c@ dup disasm-rsrc 
  disasm-rdest 2 ;
  


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
' disasm-1reg-imm4 ' instruction-table define-format asm1i4:
' disasm-1reg-imm8 ' instruction-table define-format asm1i8:

include ./opcodes.fs
include ./asmops.fs

previous set-current
