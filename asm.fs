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
require ./iris.fs
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

previous set-current
