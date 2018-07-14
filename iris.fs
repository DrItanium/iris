\ 
\  iris
\  Copyright (c) 2013-2018, Joshua Scoggins and Contributors
\  All rights reserved.
\ 
\  Redistribution and use in source and binary forms, with or without
\  modification, are permitted provided that the following conditions are met:
\      * Redistributions of source code must retain the above copyright
\        notice, this list of conditions and the following disclaimer.
\      * Redistributions in binary form must reproduce the above copyright
\        notice, this list of conditions and the following disclaimer in the
\        documentation and/or other materials provided with the distribution.
\ 
\  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
\  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
\  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
\  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
\  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
\  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
\  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
\  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
\  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
\  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
\  


get-current vocabulary iris also iris definitions

: addr16 ( a -- b ) 0xFFFF and ;
: addr8 ( a -- b ) 0x00FF and ;
: {opcode ( -- 0 ) 0 ;
: opcode} ( n -- ) drop ;
{opcode
opcode: #illegal,
opcode: #add 
opcode: #sub 
opcode: #mul 
opcode: #div 
opcode: #rem 
opcode: #lshift
opcode: #rshift
opcode: #and 
opcode: #or 
opcode: #negate
opcode: #xor 
opcode: #min
opcode: #max
opcode: #eq
opcode: #neq
opcode: #lt
opcode: #gt
opcode: #le
opcode: #ge
opcode: #set
opcode: #ld
opcode: #st
opcode: #push
opcode: #pop
opcode: #br
opcode: #brl
opcode: #bcr
opcode: #bcrl
opcode: #ueq
opcode: #uneq
opcode: #ult
opcode: #ugt
opcode: #ule
opcode: #uge
opcode: #uand
opcode: #uor
opcode: #unegate
opcode: #uxor
opcode: #umin
opcode: #umax
opcode: #uadd
opcode: #usub
opcode: #umul
opcode: #udiv
opcode: #urem
opcode: #ulshift
opcode: #urshift
opcode: #incr
opcode: #decr
opcode: #uincr
opcode: #udecr
opcode: #call
opcode: #condb
opcode: #addi
opcode: #subi
opcode: #rshifti
opcode: #lshifti
opcode: #ldtincr
opcode: #lti
opcode: #move
opcode: #sttincr
opcode: #addw
opcode: #subw
opcode: #pushw
opcode: #popw
opcode: #return
opcode: #creturn
opcode: #negatew
opcode: #umsmod
opcode: #msmod
opcode: #umstar
opcode: #mstar
opcode: #stw
opcode: #ldw
opcode: #ldbu
opcode: #stbu
opcode: #ldbl
opcode: #stbl
opcode: #setb
opcode: #bi
opcode: #eqz
opcode: #neqz
opcode: #ltz
opcode: #gtz
opcode: #lez
opcode: #gez
opcode: #andi
opcode: #uandi
opcode: #muli
opcode: #divi
opcode: #pushi
opcode: #memincr
opcode: #memdecr
opcode}

: hardwired-register ( value "name" -- ) constant ;
: register ( "name" -- ) variable 0 latest name>int execute ! ;

register x0
register x1 
register x2
register x3
register x4
register x5
register x6
register x7
register x8
register x9
register x10
register x11
register x12
register x13
register x14
register x15
register pc

0x10000 constant memory-size
memory-size 1 cells / constant memory-size-in-cells
create data-memory memory-size-in-cells cells allot
: clear-data-memory ( -- )
  memory-size-in-cells 0 ?do
    0 I cells data-memory + ! 
  loop ;
: dump-data-memory ( -- )
  data-memory memory-size-in-cells cells dump ;
: upper-half ( a -- b ) addr16 8 rshift 0x00FF and ;
: store-byte ( v a -- ) addr16 data-memory + c! ;
: store-word ( value addr -- ) 2dup store-byte 1+ swap 8 rshift swap store-byte ;
: load-byte ( addr -- value ) addr16 data-memory + c@ ;
: load-word ( addr -- value ) 
  dup load-byte 0xFF and ( addr l )
  swap 1+ load-byte ( l h )
  8 lshift  0xFF00 and ( l h<<8 )
  or addr16 ;
: set-register ( value dest -- ) swap addr16 swap ! ;
: get-register ( reg -- value ) @ addr16 ;
: set-pc ( value -- ) pc set-register ;
: get-pc ( -- value ) pc get-register ;
: advance-pc ( -- ) pc get-register 1+ set-register ;
: load-byte ( addr -- value ) 
: idx>reg ( idx -- addr )
  0xF and 
  case
    0 of x0 endof 1 of x1 endof 2 of x2 endof 3 of x3 endof
    4 of x4 endof 5 of x5 endof 6 of x6 endof 7 of x7 endof
    8 of x8 endof 9 of x9 endof 10 of x10 endof 11 of x11 endof
    12 of x12 endof 13 of x13 endof 14 of x14 endof 15 of x15 endof
    abort" Illegal Register!"
  endcase 
  ;
: reg>idx ( addr -- idx ) 
  case
    x0 of 0 endof x1 of 1 endof x2 of 2 endof x3 of 3 endof
    x4 of 4 endof x5 of 5 endof x6 of 6 endof x7 of 7 endof
    x8 of 8 endof x9 of 9 endof x10 of 10 endof x11 of 11 endof
    x12 of 12 endof x13 of 13 endof x14 of 14 endof x15 of 15 endof
    abort" Illegal Register Address!"
  endcase
  ;
\ execution logic
: binary-op-exec ( src2 src dest op -- ) 
  swap
  >r  \ dest
  >r  \ op
  get-register swap 
  get-register swap 
  r> execute 
  r> set-register ;

: defbinaryop ( "name" "action" -- ) 
  create ' ,
  does> ( src2 src dest -- ) 
  @ binary-op-exec ;
defbinaryop add; +
defbinaryop sub; -
defbinaryop mul; *
defbinaryop div; /
defbinaryop rem; mod

: 1+; ( src dest -- ) >r get-register 1+ r> set-register ;
: 1-; ( src dest -- ) >r get-register 1- r> set-register ;
