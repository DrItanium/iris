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
\ iris assembler for gforth
require code.fs
require ./core.fs
: addr8 ( i -- i8 ) 0xFF and ; 
: addr4 ( i -- i4 ) 0x0F and ;
: num>reg ( i -- ri ) 0xF and ;
: addr16 ( i -- ri ) 0xFFFF and ;
variable memory_base
variable memory_offset
: byte, ( i -- ) memory_offset @ addr16 memory_base @ + c! 
                 memory_offset @ 1+ addr16 memory_offset ! ;
: word, ( imm -- ) addr16 dup byte, 8 rshift addr8 byte, ;
: .mloc ( -- addr ) memory_offset @ ;
: .org ( value -- ) addr16 memory_offset ! ;
: label: ( "name" -- ) .mloc constant ;
get-current
also assembler definitions
_memory memory_base !
0 memory_offset !
include ./opcodes.fs
: do-asm0: ( -- ) 0 byte, ;
: do-asm1: ( dest -- ) num>reg addr8 byte, ;
: do-asm2: ( src dest -- ) num>reg swap num>reg 4 lshift or addr8 byte, ;
: do-asm3: ( src2 src dest -- ) do-asm2: do-asm1: ;
: do-asm4: ( s3 s2 s d -- ) do-asm2: do-asm2: ;
: do-asmi16: ( imm16 -- ) dup addr8 byte, 8 rshift addr8 byte, ;
: do-asm1i16: ( imm16 dest -- ) do-asm1: do-asmi16: ;
: do-asm2i16: ( imm16 src dest -- ) do-asm2: do-asmi16: ;
: do-asm1i4: ( imm4 dest -- ) do-asm2: ;
: do-asm1i8: ( imm8 dest -- ) do-asm1: addr8 byte, ;
: stash-opcode ( n -- ) c@ addr8 byte, ;
: asm0: ( n -- ) create c, does> stash-opcode do-asm0: ;
: asm1: ( n -- ) create c, does> stash-opcode do-asm1: ;
: asm2: ( n -- ) create c, does> stash-opcode do-asm2: ;
: asm3: ( n -- ) create c, does> stash-opcode do-asm3: ;
: asm4: ( n -- ) create c, does> stash-opcode do-asm4: ;
: asmi16: ( n -- ) create c, does> stash-opcode do-asmi16: ;
: asm1i16: ( n -- ) create c, does> stash-opcode do-asm1i16: ;
: asm2i16: ( n -- ) create c, does> stash-opcode do-asm2i16: ;
: asm1i4: ( n -- ) create c, does> stash-opcode do-asm1i4: ;
: asm1i8: ( n -- ) create c, does> stash-opcode do-asm1i8: ;
include ./asmops.fs
include ./registers.fs
x15 constant xtmp
: ?same-registers ( a b -- a b f ) 2dup = ; 
: irisdis ( offset count -- ) swap addr16 memory_base @ + swap disasm ;
: move, ( src dest -- ) ?same-registers if 2drop else move, endif ;
: zero, ( dest -- ) 0 swap set4, ;
: set4, ( imm dest -- ) 
  >r dup 0= 
  if 
     drop r> zero, 
  else
    addr4 r> set4, 
  endif ;
: set8, ( imm dest -- )
  >r dup dup addr4 = 
  if \ is 4 bits wide
     r> set4, 
  else \ it is 8 bits wide
     addr8 r> set8,
  endif ;
: set16, ( imm dest -- ) ( always do 4 byte output ) swap addr16 swap set, ;
: set, ( imm dest -- ) 
  >r dup dup addr8 = 
  if ( 8 bits wide )
     r> set8, \ this will check to see if we should do set4
  else 
     r> set, 
  endif ;
: set-tmp, ( imm -- xtmp ) xtmp set, xtmp ;

: rshifti, ( imm src dest -- ) 2>r dup 0= if drop 2r> move, else 2r> rshifti, endif ;
: lshifti, ( imm src dest -- ) 2>r dup 0= if drop 2r> move, else 2r> lshifti, endif ;
: sub, ( src2 src dest -- ) 
  >r ?same-registers
  if \ if the two registers are the same then it will be a zeroing operation
    2drop r> zero, 
  else
    r> sub, 
  endif
;

: or, ( src2 src dest -- )
  >r ?same-registers 
  if 
    drop r> move, 
  else
    r> or, 
  endif ;

: ori, ( imm src dest -- ) 
  2>r dup 0= 
  if
    drop 2r> move, 
  else
    set-tmp, 2r> or, 
  endif ;

: xor, ( src2 src dest -- ) 
  >r ?same-registers 
  if 
     2drop r> zero, 
  else 
     r> xor, 
  endif ;
: xori, ( imm src dest -- ) 
  2>r dup 0= 
  if 
    drop 2r> move, 
  else 
    set-tmp, 2r> xor, 
  endif ;

: and, ( src2 src dest -- )
  >r ?same-registers
  if 
    drop r> move, 
  else
    r> and, 
  endif ;

: andi, ( imm src dest -- )
  2>r dup 0= 
  if 
    drop 2r> nip zero, 
  else 
    2r> andi, 
  endif ;
 
\ can perform checks to simplify logic such as eq, with src and src2 being the same
\ register always yielding true
: addi, ( imm src dest -- )
  2>r dup
  case
    0 of drop 2r> move, endof
    1 of drop 2r> incr, endof
    2r> addi, 
  endcase ;

: subi, ( imm src dest -- )
  2>r dup
  case
    0 of drop 2r> move, endof
    1 of drop 2r> decr, endof
    2r> subi, 
  endcase ;
: muli, ( imm src dest -- )
  2>r dup 
    case
    0 of drop 2r> drop zero, endof
    1 of drop 2r> move, endof
    2 of drop 2r> over swap add, endof
    4 of drop 2 2r> lshifti, endof
    8 of drop 3 2r> lshifti, endof
    16 of drop 4 2r> lshifti, endof
    32 of drop 5 2r> lshifti, endof
    64 of drop 6 2r> lshifti, endof
    128 of drop 6 2r> lshifti, endof
    2r> muli, ( imm )
    endcase ;
: divi, ( imm src dest -- )
  2>r dup
    case 
    0 of drop 2r> move, ( divide by zero causes a move ) endof
    1 of drop 2r> move, endof
    2 of drop 1 2r> rshifti, endof
    4 of drop 2 2r> rshifti, endof
    8 of drop 3 2r> rshifti, endof
    16 of drop 4 2r> rshifti, endof
    32 of drop 5 2r> rshifti, endof
    64 of drop 6 2r> rshifti, endof
    128 of drop 6 2r> rshifti, endof
    2r> divi,
    endcase ;
: stopi, ( imm -- ) set-tmp, stop, ;
: ldi, ( imm dest -- ) >r set-tmp, r> ld, ;
: sti, ( src imm -- ) set-tmp, st, ; 
: square, ( src dest -- ) over swap mul, ;
: indld, ( src dest -- ) 
  >r xtmp ld,
  xtmp r> ld, ;
: indst, ( src dest -- )
  \ indirect store
  swap >r xtmp ld,
  r> xtmp st, ;

: pushi4, ( imm sp -- ) swap addr4 swap pushi4, ; 
: pushi8, ( imm sp -- ) 
  swap addr8 dup dup addr4 = 
  if swap pushi4, 
  else swap pushi8, endif ;
: pushi, ( imm sp -- ) 
  swap addr16 dup dup addr8 = 
  if 
    swap pushi8, 
  else
    swap pushi, 
  endif ;
: emiti, ( imm -- ) set-tmp, emit, ;

: 1+, ( dest -- ) dup incr, ;
: 1-, ( dest -- ) dup decr, ;

previous set-current
