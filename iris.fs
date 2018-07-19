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
: illegal-instruction ( -- ) 1 abort" Illegal Instruction!" ;
: generic-load ( addr -- value ) drop 0 ;
: generic-store ( value addr -- ) drop drop ;
: dispatch-table ( n "name" -- )
    \ initialize table with n entries with disasm-illegal
    create 0 ?do
    ['] illegal-instruction ,
    loop
does> ( u -- addr )
    swap cells + ; 
\ : devspace-define ( n "name" -- ) 
\   create 2 / 0 ?do 
\   ['] generic-load ,
\   ['] generic-store ,
\   loop
\ does> ( u -- addr )
\   swap 2* cells + ;



0x100 dispatch-table decoders 
0x100 dispatch-table bodies
0x100 dispatch-table encoders
: addr32 ( a -- b ) 0xFFFFFFFF and ;
: addr16 ( a -- b ) 0xFFFF and ;
: addr8 ( a -- b ) 0x00FF and ;
: addr4 ( a -- b ) 0x000F and ;
: addr12 ( a -- b ) 0x0FFF and ; 
: register ( "name" -- ) variable 0 latest name>int execute ! ;

register _r0
register _r1 
register _r2
register _r3
register _r4
register _r5
register _r6
register _r7
register _r8
register _r9
register _r10
register _r11
register _r12
register _r13
register _r14
register _r15
register pc
register imm
register ?running
false ?running !


: use-imm ( value -- imm ) imm ! imm ; 
: idx>reg ( idx -- addr )
  addr4
  case
    0 of _r0 endof 1 of _r1 endof 2 of _r2 endof 3 of _r3 endof
    4 of _r4 endof 5 of _r5 endof 6 of _r6 endof 7 of _r7 endof
    8 of _r8 endof 9 of _r9 endof 10 of _r10 endof 11 of _r11 endof
    12 of _r12 endof 13 of _r13 endof 14 of _r14 endof 15 of _r15 endof
    abort" Illegal Register!"
  endcase 
  ;
: reg>idx ( addr -- idx ) 
  case
    _r0 of 0 endof _r1 of 1 endof     _r2 of 2 endof   _r3 of 3 endof
    _r4 of 4 endof _r5 of 5 endof     _r6 of 6 endof   _r7 of 7 endof
    _r8 of 8 endof _r9 of 9 endof     _r10 of 10 endof _r11 of 11 endof
    _r12 of 12 endof _r13 of 13 endof _r14 of 14 endof _r15 of 15 endof
    abort" Illegal Register Address!"
  endcase
  ;

: set-reg ( value dest -- ) swap addr16 swap ! ;
: get-reg ( reg -- value ) @ addr16 ;
: 1+-register ( reg -- ) dup get-reg 1+ swap set-reg ;
: 2+-register ( reg -- ) dup get-reg 2 + swap set-reg ;
: 1--register ( reg -- ) dup get-reg 1- swap set-reg ;
: set-pc ( value -- ) pc set-reg ;
: get-pc ( -- value ) pc get-reg ;
: advance-pc ( -- ) get-pc 1+ set-pc ;

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
: store-byte ( v a -- ) addr8 data-memory + c! ;
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

: @pc@ ( -- value ) get-pc load-byte ;
: @pc@1+ ( -- value )
  \ load and then advance the pc
  @pc@ advance-pc ;

\ execution logic
: binary-op-exec ( src2 src dest op -- ) 
  swap ( src2 src op dest )
  >r  ( src2 src op )
  >r  ( src2 src )
  get-reg ( src2 sreg )
  swap ( sreg src2 )
  get-reg ( sreg s2reg )
  r> execute 
  r> set-reg ;

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
defbinaryop eq; =
defbinaryop neq; <>
defbinaryop lt; <
defbinaryop gt; >
defbinaryop le; <=
defbinaryop ge; >=
defbinaryop min; min
defbinaryop max; max
defbinaryop ult; u<
defbinaryop ugt; u>
defbinaryop ule; u<=
defbinaryop uge; u>=
defbinaryop uneq; u<>
defbinaryop umin; umin

: 1+; ( src dest -- ) >r get-reg 1+ r> set-reg ;
: 1-; ( src dest -- ) >r get-reg 1- r> set-reg ;
: set; ( constant dest -- ) set-reg ;
: ld; ( src dest -- ) 
  swap ( dest src )
  get-reg ( dest src-value )
  load-word ( dest value ) 
  swap ( value dest ) 
  set-reg ; 
: st; ( src dest -- ) 
  swap get-reg 
  swap get-reg 
  store-word ;
: pushi; ( imm dest -- ) 
  swap over 
  get-reg 
  push-word ( n ) 
  set-reg ;
: push; ( src dest -- ) 
  swap ( dest src )
  get-reg swap pushi; ;

: pop; ( src dest -- )
  over get-reg pop-word ( src dest value addr+2 )
  >r ( src dest value )
  swap set-reg r>
  swap set-reg ;
: branch; ( addr -- ) set-pc ;
: call; ( imm dest -- )
  get-pc swap pushi;
  branch; ;
: rbranch; ( dest -- ) get-reg branch; ;
: rbranch-link; ( src dest -- ) 
  \ branch to the dest register and push _pc onto the stack at src
  get-reg swap ( imm src )
  call; ;


: ?rbranch; ( src dest -- ) 
  \ conditional register branch
  swap 
  get-reg 
  if 
    rbranch;
  else
    drop
  endif ;

: ?rbranch-link; ( src2 src dest -- )
  >r
  get-reg 
  if 
    r> rbranch-link;
  else
    drop r> drop 
  endif ;
: ?branch; ( imm dest -- ) 
  get-reg 
  if 
    set-pc
  else
    drop
  endif ;

: move; ( src dest -- ) swap get-reg swap set-reg ; 
: swap; ( src dest -- ) 
  \ swap the contents of the top two registers
  2dup ( src dest src dest )
  get-reg >r \ load dest onto the return stack
  get-reg \ get the src value
  swap set-reg \ set dest
  r> \ get dest value back
  swap set-reg ;


: return; ( dest -- ) 
  \ pop the top element from the stack pointer and store it in _pc
  dup pop-word ( dest value addr ) 
  swap ( dest addr value )
  set-pc ( dest addr )
  swap set-reg ;
: ?return; ( src dest -- )
  swap get-reg 
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

: decode-lower-half ( v -- reg ) addr4 idx>reg ;
: decode-upper-half ( v -- reg ) 4 rshift decode-lower-half ;
: get-dest ( v -- reg ) decode-lower-half ;
: get-src ( v -- reg ) decode-upper-half ;
: get-dest-src ( v -- src dest ) 
  dup ( v v )
  get-src ( v src ) 
  swap ( src v ) 
  get-dest ( src dest ) ;
: get-src2-src3 ( v -- src3 src2 ) get-dest-src ;
: get-src2 ( v -- src2 ) decode-lower-half ;
: decode-no-register ( -- ) advance-pc ;
: decode-1reg ( -- dest ) @pc@1+ get-dest ; 
: decode-2reg ( -- src dest ) 
  @pc@1+ get-dest-src ;
: decode-3reg ( -- src2 src dest ) 
  @pc@1+ ( b1 )
  get-dest-src ( src dest )
  @pc@1+ ( src dest b2 )
  get-src2 ( src dest src2 )
  -rot ;
: make-imm16 ( u l -- v ) 
  @pc@1+ @pc@1+ ( l u ) 8 rshift swap or addr16 ;
: decode-1reg-imm16 ( -- imm dest )
  @pc@1+ ( b1 )
  get-dest >r 
  make-imm16 r> ;
: decode-2reg-imm16 ( -- imm src dest ) 
  @pc@1+ ( b1 )
  get-dest-src 2>r 
  make-imm16 2r> ;
: get-upper-register ( position -- reg-addr )
  1+ addr4 idx>reg ;
: compute-reg-pair ( n -- u l ) 
  dup ( n n )
  get-upper-register ( n u )
  swap idx>reg ( u l ) ;
: decode-wide-2reg ( -- srcu srcl destu destl )
  @pc@1+ ( b1 ) 
  dup addr4 ( b1 dest )
  >r ( b1 )
  4 rshift addr4 ( srcl )
  compute-reg-pair ( srcu srcl )
  r> ( srcu srcl dest )
  compute-reg-pair ( srcu srcl destu destl ) ;
: decode-wide-3reg ( -- src2u src2l srcu srcl destu destl ) 
  @pc@1+ ( b1 ) >r 
  @pc@1+ ( b2 ) addr4
  compute-reg-pair ( src2u src2l ) r> ( src2u src2l b1 )
  dup ( s2u s2l b1 b1 ) >r ( s2u s2l b1 )
  4 rshift addr4 ( s2u s2l src )
  compute-reg-pair ( s2u s2l su sl )
  r> ( s2u s2l su sl b1 )
  addr4 compute-reg-pair ; 

: decode-imm16 ( -- imm ) make-imm16 ;
: decode-one-reg-imm8 ( -- imm8 dest ) 
  @pc@1+ ( b1 )
  @pc@1+ ( b1 b2 )
  addr8 swap ( imm8 b1 )
  addr4 idx>reg ;
: decode-two-reg-imm8 ( -- imm8 dest ) 
  @pc@1+ ( b1 )
  @pc@1+ ( b1 b2 )
  addr8 swap ( imm8 b1 )
  dup 4 rshift addr4 idx>reg swap 
  addr4 idx>reg ;


: invert; ( src dest -- ) 
  swap get-reg 
  invert swap set-reg ;
: break-apart-num32 ( s -- u l )
  addr32 
  dup ( s s )
  addr16 ( s l )
  swap ( l s )
  16 rshift addr16 swap ( u l ) ;
: make-num32 ( ru rl -- n )
  get-reg swap get-reg ( l u )
  16 lshift ( l u<<16 )
  or ( s )
  addr32 ;
: set-reg32 ( u l du dl -- ) 
  swap ( u l dl du ) >r ( u l dl )
  set-reg ( u ) r> set-reg ;
: invertw; ( srcu srcl destu destl ) 
  2>r ( srcu srcl ) 
  make-num32 
  invert ( s' )
  break-apart-num32 ( su sl )
  2r> set-reg32 ;

: ueq; ( src2 src dest -- ) 
  dup >r ( src2 src dest ) uneq; 
  r> dup invert; ;
: {opcode ( -- 0 ) 0 ;
: opcode} ( n -- ) drop ;
: skip-opcode ( n -- n+1 ) 1+ ;
: decode-instruction ( control -- args* )
  decoders @ execute ;
: execute-instruction ( args* control -- )
  bodies @ execute ;
: decode-and-execute-instruction ( -- ) 
  @pc@1+ dup ( control control )
  >r ( control )
  decode-instruction 
  r> execute-instruction ;
: defimmop ( "name" "operation" ) 
  create ' , 
  does> ( imm src dest -- )
        swap 2>r ( imm src )
        swap use-imm swap ( immreg srcreg )
        2r> ( immreg srcreg destreg addr ) @ execute ;

defimmop andi; and;
defimmop addi; add;
defimmop subi; sub;
defimmop _muli; mul;
defimmop _divi; div;

defimmop rshifti; rshift;
defimmop lshifti; lshift;
defimmop lti; lt;
defimmop eqi; eq;
defimmop neqi; neq;
defimmop gti; gt;
defimmop lei; le;
defimmop gei; ge;
: divi; ( imm src dest -- )
  2>r dup 2 = if 
      drop 1 2r> rshifti;
      else
      2r> _divi;
      endif ;
: muli; ( imm src dest -- )
  2>r dup 2 = if 
      drop 1 2r> lshifti;
      else
      2r> _muli;
      endif ;
: def0src2op ( "name" "op" -- )
  create ' , 
  does> ( src dest -- )
  >r 0 -rot r> @ execute ;
def0src2op eqz; eqi; 
def0src2op neqz; neqi; 
def0src2op ltz; lti; 
def0src2op gtz; gti; 
def0src2op gez; gei; 
def0src2op lez; lei; 

: umax ( a b -- a | b ) 
  2dup u< 
  if swap
  then 
  drop ; 
defbinaryop umax; umax 

: memincr; ( dest -- ) 
  dup imm ld; ( dest ) \ load into the imm register
  imm imm 1+; imm ( dest imm ) \ increment the contents
  swap ( imm dest ) st;  \ store it back into memory
  ;
: memdecr; ( dest -- ) 
  dup imm ld; ( dest ) \ load into the imm register
  imm imm 1-; imm ( dest imm ) \ increment the contents
  swap ( imm dest ) st;  \ store it back into memory
  ;

: ldtincr; ( src dest -- ) 
  \ load into dest and then increment src
  over >r ld; \ perform the load
  r> dup 1+; \ go to the next location
  ;
: sttincr; ( src dest -- ) 
  \ store into dest and then increment dest
  dup >r st; \ perform the store
  r> dup 1+; \ perform the increment 
  ;
: extract-2wide ( src2u src2l srcu srcl -- n n2 ) 
  make-num32 ( src2u src2l n )
  r>  ( src2u src2l )
  make-num32  ( n2 )
  >r  swap ( n n2 ) ;
: addw; ( src2u src2l srcu srcl destu destl -- ) 
  2>r ( s2u s2l s1u s1l )
  extract-2wide + 
  break-apart-num32 ( u l )
  2r> ( u l destu destl ) set-reg32 ;
: subw; ( s2u s2l su sl du dl -- )
  2>r ( s2u s2l su sl )
  extract-2wide - 
  break-apart-num32 ( u l )
  2r> set-reg32 ;
: pushw; ( su sl du dl -- ) 
  swap drop ( su sl dl )
  -rot ( dl su sl ) 
  get-reg swap ( dl l su )
  get-reg rot swap over ( l dl u dl )
  pushi;
  pushi; ;

: popw; ( su sl du dl -- )
  2>r swap drop dup 2r> ( sl sl du dl ) \ get rid of the double source registers 
  swap >r ( sl sl dl ) 
  pop; ( sl ) 
  r> ( sl du ) 
  pop; ;

: stop; ( dest -- ) get-reg ?running ! ;

: encode-2reg ( src dest -- v ) reg>idx addr4 swap reg>idx 4 lshift or addr8 ;
: encode-1reg ( dest -- v ) 0 swap encode-2reg ;
: encode-0reg ( -- v ) 0 ;
: encode-3reg ( src2 src dest -- v2 v1 ) 
  2>r encode-1reg 2r> encode-2reg ; 
: encode-4reg ( src3 src2 src dest -- v2 v1 )
  2>r encode-2reg 2r> encode-2reg ; 
: encode-imm16 ( imm16 -- u l ) 
  dup >r 
  rshift 8 addr8 
  r> addr8 ;
: encode-1reg-imm16 ( imm16 dest -- v3 v2 v1 )
  encode-1reg ( imm16 v1 ) >r ( imm16 )
  encode-imm16 ( v3 v2 ) r> ( v3 v2 v1 ) ;
: encode-2reg-imm16 ( imm16 src dest -- v3 v2 v1 ) 
  encode-2reg ( imm16 v1 ) >r ( imm16 )
  encode-imm16 ( v3 v2 ) r> ( v3 v2 v1 ) ;
: execute-latest ( -- * ) latest name>int execute ;
: opcode: ( n "name" "body" "encoder" "decoder" -- n+1 )
  addr8 dup ( n8 n8 n8 )
  constant ( n8 n8 ) 
  ' over ( n8 n8 "body" n8 ) bodies ! ( n8 )
  ' over ( n8 n8 "encoder" n8 ) encoders ! ( n8 )
  ' over ( n8 "decoder" n8 ) decoders ! ( n8 )
  1+ ;

: set-memory ( value address -- ) swap addr8 swap store-byte ;
set-current
{opcode
opcode: #illegal  illegal-instruction illegal-instruction illegal-instruction
opcode: #add      add;                encode-3reg         decode-3reg
opcode: #sub      sub;                encode-3reg         decode-3reg
opcode: #mul      mul;                encode-3reg         decode-3reg
opcode: #div      div;                encode-3reg         decode-3reg
opcode: #rem      rem;                encode-3reg         decode-3reg
opcode: #lshift   lshift;             encode-3reg         decode-3reg
opcode: #rshift   rshift;             encode-3reg         decode-3reg
opcode: #and      and;                encode-3reg         decode-3reg
opcode: #or       or;                 encode-3reg         decode-3reg
opcode: #invert   invert;             encode-2reg         decode-2reg
opcode: #xor      xor;                encode-3reg         decode-3reg
opcode: #min      min;                encode-3reg         decode-3reg
opcode: #max      max;                encode-3reg         decode-3reg
opcode: #eq       eq;                 encode-3reg         decode-3reg
opcode: #neq      neq;                encode-3reg         decode-3reg
opcode: #lt       lt;                 encode-3reg         decode-3reg
opcode: #gt       gt;                 encode-3reg         decode-3reg
opcode: #le       le;                 encode-3reg         decode-3reg
opcode: #ge       ge;                 encode-3reg         decode-3reg
opcode: #set      set;                encode-1reg-imm16   decode-1reg-imm16
opcode: #ld       ld;                 encode-2reg         decode-2reg
opcode: #st       st;                 encode-2reg         decode-2reg
opcode: #push     push;               encode-2reg         decode-2reg
opcode: #pop      pop;                encode-2reg         decode-2reg
opcode: #br       rbranch;            encode-1reg         decode-1reg
opcode: #brl      rbranch-link;       encode-2reg         decode-2reg
opcode: #bcr      ?rbranch;           encode-2reg         decode-2reg
opcode: #bcrl     ?rbranch-link;      encode-3reg         decode-3reg
opcode: #ueq      ueq;                encode-3reg         decode-3reg
opcode: #uneq     uneq;               encode-3reg         decode-3reg
opcode: #ult      ult;                encode-3reg         decode-3reg
opcode: #ugt      ugt;                encode-3reg         decode-3reg
opcode: #ule      ule;                encode-3reg         decode-3reg
opcode: #uge      uge;                encode-3reg         decode-3reg
opcode: #uand     and;                encode-3reg         decode-3reg
opcode: #uor      or;                 encode-3reg         decode-3reg
opcode: #uinvert  invert;             encode-2reg         decode-2reg
opcode: #uxor     xor;                encode-3reg         decode-3reg
opcode: #umin     umin;               encode-3reg         decode-3reg
opcode: #umax     umax;               encode-3reg         decode-3reg
opcode: #uadd     add;                encode-3reg         decode-3reg
opcode: #usub     sub;                encode-3reg         decode-3reg
opcode: #umul     mul;                encode-3reg         decode-3reg
opcode: #udiv     div;                encode-3reg         decode-3reg
opcode: #urem     rem;                encode-3reg         decode-3reg
opcode: #ulshift  lshift;             encode-3reg         decode-3reg
opcode: #urshift  rshift;             encode-3reg         decode-3reg
opcode: #incr     1+;                 encode-2reg         decode-2reg
opcode: #decr     1-;                 encode-2reg         decode-2reg
opcode: #uincr    1+;                 encode-2reg         decode-2reg
opcode: #udecr    1-;                 encode-2reg         decode-2reg
opcode: #call     call;               encode-1reg-imm16   decode-1reg-imm16
opcode: #condb    ?branch;            encode-1reg-imm16   decode-1reg-imm16
opcode: #addi     addi;               encode-2reg-imm16   decode-2reg-imm16
opcode: #subi     subi;               encode-2reg-imm16   decode-2reg-imm16
opcode: #rshifti  rshifti;            encode-2reg-imm16   decode-2reg-imm16
opcode: #lshifti  lshifti;            encode-2reg-imm16   decode-2reg-imm16
opcode: #ldtincr  ldtincr;            encode-2reg         decode-2reg
opcode: #lti      lti;                encode-2reg-imm16   decode-2reg-imm16
opcode: #move     move;               encode-2reg         decode-2reg
opcode: #sttincr  sttincr;            encode-2reg         decode-2reg
opcode: #addw     addw;               encode-3reg         decode-wide-3reg
opcode: #subw     subw;               encode-3reg         decode-wide-3reg
opcode: #pushw    pushw;              encode-2reg         decode-wide-2reg
opcode: #popw     popw;               encode-2reg         decode-wide-2reg
opcode: #return   return;             encode-1reg         decode-1reg
opcode: #creturn  ?return;            encode-2reg         decode-2reg
opcode: #invertw  invertw;            encode-2reg         decode-wide-2reg
opcode: #bi       branch;             encode-imm16        decode-imm16
opcode: #eqz      eqz;                encode-2reg         decode-2reg
opcode: #neqz     neqz;               encode-2reg         decode-2reg
opcode: #ltz      ltz;                encode-2reg         decode-2reg
opcode: #gtz      gtz;                encode-2reg         decode-2reg
opcode: #lez      lez;                encode-2reg         decode-2reg
opcode: #gez      gez;                encode-2reg         decode-2reg
opcode: #andi     andi;               encode-2reg-imm16   decode-2reg-imm16
opcode: #uandi    andi;               encode-2reg-imm16   decode-2reg-imm16
opcode: #muli     muli;               encode-2reg-imm16   decode-2reg-imm16
opcode: #divi     divi;               encode-2reg-imm16   decode-2reg-imm16
opcode: #pushi    pushi;              encode-1reg-imm16   decode-1reg-imm16
opcode: #memincr  memincr;            encode-1reg         decode-1reg
opcode: #memdecr  memdecr;            encode-1reg         decode-1reg
opcode: #stop     stop;               encode-1reg         decode-1reg \ stop execution
opcode}
_r0 constant x0 
_r1 constant x1 
_r2 constant x2 
_r3 constant x3 
_r4 constant x4 
_r5 constant x5 
_r6 constant x6 
_r7 constant x7 
_r8 constant x8 
_r9 constant x9 
_r10 constant x10 
_r11 constant x11 
_r12 constant x12 
_r13 constant x13 
_r14 constant x14 
_r15 constant x15 
: print-pc ( -- ) ." pc: " get-pc . cr ;
: examine-memory ( -- ) data-memory memory-size-in-cells cells dump ;
: examine-word ( address -- ) dup load-word swap . ." : " . cr ;
: i>x ( idx -- reg ) idx>reg ;
: x>i ( reg -- idx ) reg>idx ;
: x@ ( reg -- value ) get-reg ;
: x! ( value reg -- ) set-reg ;
: pc@ ( -- value ) pc get-reg ;
: pc! ( value -- ) set-pc ;
: pc1+ ( value -- ) advance-pc ;
: print-registers ( -- ) cr
  base @ >r
  ." pc: 0x" pc@ hex . cr
  ." x0: 0x" x0 x@ hex . cr 
  ." x1: 0x" x1 x@ hex . cr 
  ." x2: 0x" x2 x@ hex . cr
  ." x3: 0x" x3 x@ hex . cr 
  ." x4: 0x" x4 x@ hex . cr 
  ." x5: 0x" x5 x@ hex . cr
  ." x6: 0x" x6 x@ hex . cr 
  ." x7: 0x" x7 x@ hex . cr 
  ." x8: 0x" x8 x@ hex . cr
  ." x9: 0x" x9 x@ hex . cr 
  ." x10: 0x" x10 x@ hex . cr
  ." x11: 0x" x11 x@ hex . cr
  ." x12: 0x" x12 x@ hex . cr 
  ." x13: 0x" x13 x@ hex . cr
  ." x14: 0x" x14 x@ hex . cr
  ." x15: 0x" x15 x@ hex . cr
  r> base ! ;
: invoke-instruction ( args* control -- ) execute-instruction ;

  
: execute-core ( -- ) 
  true ?running ! \ mark that we are indeed executing
  begin
  decode-and-execute-instruction \ cycle execution
  ?running @ invert until ;

previous 
