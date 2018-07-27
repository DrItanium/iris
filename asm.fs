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
: .org ( value -- ) addr16 memory_offset ! ;
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
: stash-opcode ( n -- ) c@ addr8 byte, ;
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
x15 constant xtmp
: set-tmp, ( imm -- xtmp ) xtmp set, xtmp ;
: irisdis ( offset count -- ) swap addr16 memory_base @ + swap disasm ;
: move, ( src dest -- ) 2dup = if 2drop else move, endif ;
: set, ( imm dest -- ) 
  >r dup 0= 
  if 
    drop r> zero, 
  else
    r> set, 
  endif ;

: rshifti, ( imm src dest -- ) 2>r dup 0= if drop 2r> move, else 2r> rshifti, endif ;
: lshifti, ( imm src dest -- ) 2>r dup 0= if drop 2r> move, else 2r> lshifti, endif ;
: sub, ( src2 src dest -- ) 
  >r 2dup = 
  if \ if the two registers are the same then it will be a zeroing operation
    2drop r> zero, 
  else
    r> sub, 
  endif
;

: or, ( src2 src dest -- )
  >r 2dup = 
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

: xor, ( src2 src dest -- ) >r 2dup = if 2drop r> zero, else r> xor, endif ;
: xori, ( imm src dest -- ) 
  2>r dup 0= 
  if 
    drop 2r> move, 
  else 
    set-tmp, 2r> xor, 
  endif ;

: and, ( src2 src dest -- )
  >r 2dup = 
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
previous set-current
