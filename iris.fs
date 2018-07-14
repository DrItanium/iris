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
: opcode: dup addr8 constant 1+ ;
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

: register ( "name" -- ) variable 0 latest name>int execute ! ;

register r0
register r1 
register r2
register r3
register r4
register r5
register r6
register r7
register r8
register r9
register r10
register r11
register r12
register r13
register r14
register r15
register pc

: idx>reg ( idx -- addr )
  0xF and 
  case
    0 of r0 endof 1 of r1 endof 2 of r2 endof 3 of r3 endof
    4 of r4 endof 5 of r5 endof 6 of r6 endof 7 of r7 endof
    8 of r8 endof 9 of r9 endof 10 of r10 endof 11 of r11 endof
    12 of r12 endof 13 of r13 endof 14 of r14 endof 15 of r15 endof
    abort" Illegal Register!"
  endcase 
  ;
: reg>idx ( addr -- idx ) 
  case
    r0 of 0 endof r1 of 1 endof r2 of 2 endof r3 of 3 endof
    r4 of 4 endof r5 of 5 endof r6 of 6 endof r7 of 7 endof
    r8 of 8 endof r9 of 9 endof r10 of 10 endof r11 of 11 endof
    r12 of 12 endof r13 of 13 endof r14 of 14 endof r15 of 15 endof
    abort" Illegal Register Address!"
  endcase
  ;

: set-register ( value dest -- ) swap addr16 swap ! ;
: get-register ( reg -- value ) @ addr16 ;
: 1+-register ( reg -- ) dup get-register 1+ swap set-register ;
: 2+-register ( reg -- ) dup get-register 2 + swap set-register ;
: 1--register ( reg -- ) dup get-register 1- swap set-register ;
: set-pc ( value -- ) pc set-register ;
: get-pc ( -- value ) pc get-register ;
: advance-pc ( -- ) pc get-register 1+ set-register ;

\ memory block
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
: load-double-word ( addr -- value ) 
  dup 2 + addr16 load-word \ get the upper half
  16 lshift 0xFFFF0000 and \ finish setting up the upper half
  swap 
  addr16 load-word \ get the lower half
  0x0000FFFF and  \ make sure
  or \ combine it 
  0xFFFFFFFF and ;
: push-byte ( value addr -- addr-1 )
  \ move the address down one addr and then store the value there
  1 - swap over store-byte ;
: pop-byte ( addr -- value addr+1 )
  dup load-byte swap 1+ ;
: push-word ( value addr -- addr-2 ) 
  2 - dup 
  -rot 
  store-word ;
: pop-word ( addr -- value addr+2 ) 
  dup load-word 
  swap 2 + ; 

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
defbinaryop div; / \ todo add support for denominator checking
defbinaryop rem; mod \ todo add support for denominator checking
defbinaryop and; and
defbinaryop or; or
defbinaryop xor; xor
defbinaryop lshift; lshift
defbinaryop rshift; rshift
defbinaryop =; =
defbinaryop <>; <>
defbinaryop <; <
defbinaryop >; >
defbinaryop <=; <=
defbinaryop >=; >=
defbinaryop min; min
defbinaryop max; max

: 1+; ( src dest -- ) >r get-register 1+ r> set-register ;
: 1-; ( src dest -- ) >r get-register 1- r> set-register ;
: set; ( constant dest -- ) set-register ;
: ld; ( src dest -- ) 
  swap ( dest src )
  get-register ( dest src-value )
  load-word ( dest value ) 
  swap ( value dest ) 
  set-register ; 
: st; ( src dest -- ) 
  swap get-register 
  swap get-register 
  store-word ;
: pushi; ( imm dest -- ) 
  swap over 
  get-register 
  push-word ( n ) 
  set-register ;
: push; ( src dest -- ) 
  swap ( dest src )
  get-register swap pushi; ;

: pop; ( src dest -- )
  over get-register pop-word ( src dest value addr+2 )
  >r ( src dest value )
  swap set-register r>
  swap set-register ;
: ibranch; ( addr -- ) set-pc ;
: call; ( imm dest -- )
  get-pc swap pushi;
  ibranch; ;
: rbranch; ( dest -- ) get-register ibranch; ;
: rbranch-link; ( src dest -- ) 
  \ branch to the dest register and push _pc onto the stack at src
  get-register swap ( imm src )
  call; ;


: ?rbranch; ( src dest -- ) 
  \ conditional register branch
  swap 
  get-register 
  if 
    rbranch;
  else
    drop
  endif ;

: ?rbranch-link; ( src2 src dest -- )
  >r
  get-register 
  if 
    r> rbranch-link;
  else
    drop r> drop 
  endif ;
: ?branch; ( imm dest -- ) 
  get-register 
  if 
    set-pc
  else
    drop
  endif ;

: move; ( src dest -- ) swap get-register swap set-register ; 
: return; ( dest -- ) 
  \ pop the top element from the stack pointer and store it in _pc
  dup pop-word ( dest value addr ) 
  swap ( dest addr value )
  set-pc ( dest addr )
  swap set-register ;
: ?return; ( src dest -- )
  swap get-register 
  if 
     return;
  else
    drop
  endif ;

: load-then-increment; ( src dest -- ) 
  \ perform a load operation then increment source to the next word address
  over >r 
  ld; 
  r> 2+-register ;

: store-then-increment; ( src dest -- ) 
  \ perform a store operation then increment dest to the next word address
  dup >r
  st;
  r> 2+-register ;

set-current
r0 constant x0 
r1 constant x1 
r2 constant x2 
r3 constant x3 
r5 constant x5 
r6 constant x6 
r7 constant x7 
r8 constant x8 
r9 constant x9 
r10 constant x10
r11 constant x11
r12 constant x12
r13 constant x13
r14 constant x14
r15 constant x15
: print-pc ( -- ) ." pc: " get-pc . cr ;
: examine-memory ( -- ) data-memory memory-size-in-cells cells dump ;
: examine-word ( address -- ) dup load-word swap . ." : " . cr ;
: execute-core ( -- ) ;

previous 
