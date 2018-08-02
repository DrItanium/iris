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


get-current 
vocabulary iris 
also iris definitions
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

0x100 dispatch-table decoders 
0x100 dispatch-table bodies
: addr32 ( a -- b ) 0xFFFFFFFF and ;
: addr16 ( a -- b ) 0xFFFF and ;
: addr8 ( a -- b ) 0x00FF and ;
: addr4 ( a -- b ) 0x000F and ;
: addr12 ( a -- b ) 0x0FFF and ; 
: register ( "name" -- ) variable 0 latest name>int execute ! ;

register  x0 register x1  register x2  register x3
register  x4 register x5  register x6  register x7
register  x8 register x9  register x10 register x11
register x12 register x13 register x14 register x15
register pc
register imm
register ?running
false ?running !

include ./opcodes.fs

: use-imm ( value -- imm ) imm ! imm ; 
: idx>reg ( idx -- addr )
  addr4
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
    x0 of 0 endof x1 of 1 endof     x2 of 2 endof   x3 of 3 endof
    x4 of 4 endof x5 of 5 endof     x6 of 6 endof   x7 of 7 endof
    x8 of 8 endof x9 of 9 endof     x10 of 10 endof x11 of 11 endof
    x12 of 12 endof x13 of 13 endof x14 of 14 endof x15 of 15 endof
    abort" Illegal Register Address!"
  endcase
  ;

: set-reg ( value dest -- ) swap addr16 swap ! ;
: get-reg ( reg -- value ) @ addr16 ;
: 1+-register ( reg -- ) dup get-reg 1+ swap set-reg ;
: 2+-register ( reg -- ) dup get-reg 2 + swap set-reg ;
: 1--register ( reg -- ) dup get-reg 1- swap set-reg ;
: get-pc ( -- value ) pc get-reg ;
: set-pc ( value -- ) pc set-reg ;
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
: store-byte ( v a -- ) 
  addr16 data-memory + swap addr8 swap c! ;
: store-word ( value addr -- ) 
  addr16
  2dup ( v a v a )
  store-byte ( v a )
  1+ addr16 ( v a+1 )
  swap ( a+1 v )
  8 rshift ( a+1 v>>8 ) 
  addr8 ( a+1 v8 )
  swap store-byte ;
: load-byte ( addr -- value ) addr16 data-memory + c@ ;
: load-word ( addr -- value ) 
  dup load-byte 0xFF and ( addr l )
  swap 1+ load-byte ( l h )
  8 lshift  0xFF00 and ( l h<<8 )
  or addr16 ;
: push-byte ( value addr -- addr-1 )
  \ move the address down one addr and then store the value there
  1 - swap over store-byte ;
: pop-byte ( addr -- value addr+1 )
  dup load-byte swap 1+ ;
: push-word ( value addr -- addr-2 ) 
  2 - dup ( value addr-2 addr-2 )
  -rot ( addr-2 value addr-2 )
  store-word ( addr-2 ) ;
: pop-word ( addr -- value addr+2 ) 
  dup ( addr addr ) 
  load-word ( addr value ) 
  swap 2 + ( value addr+2 ) 
  ;

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
  dup >r ( imm dest )
  get-reg ( imm dreg )
  push-word ( addr ) r>
  set-reg ;
: push; ( src dest -- ) 
  swap ( dest src )
  get-reg swap pushi; ;

: pop; ( src dest -- )
  over ( src dest src )
  get-reg ( src dest a )
  pop-word ( src dest value addr+2 )
  -rot ( src addr+2 dest value ) 
  swap ( s a2 v d )
  set-reg ( s a2 )
  swap ( a2 s )
  set-reg ;
  
: branch; ( addr -- ) set-pc ;
: call; ( imm dest -- )
  get-pc swap pushi; ( imm )
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
  \ dest is where to go
  \ src is the conditional
  \ src2 is the stack pointer
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
  @pc@1+ @pc@1+ ( l u ) 
  8 lshift swap or addr16 ;
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

: decode-1reg-imm4 ( -- imm dest )
  @pc@1+ dup ( b1 b1 )
  get-dest swap ( dest b1 )
  4 rshift addr4 swap ( imm4 dest ) ;

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
	    >r ( imm dest src )
	    2>r ( imm )
	    use-imm 2r> r> @ execute ;

defimmop andi; and;
defimmop addi; add;
defimmop subi; sub;
defimmop _muli; mul;
defimmop _divi; div;

defimmop rshifti; rshift;
defimmop lshifti; lshift;
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

: umax ( a b -- a | b ) 
  2dup u< 
  if swap
  then 
  drop ; 
defbinaryop umax; umax 



: stop; ( dest -- ) get-reg ?running ! ;
: emit; ( dest -- ) get-reg addr8 emit ;

: ?key; ( dest -- ) ekey? addr16 swap set-reg ;

: key;  ( dest -- ) 
  ekey ekey>char 
  if 
     addr16 swap set-reg
  else
     ekey>fkey 
     if ( kid )
        case 
            k-left   of 0x100 endof
            k-right  of 0x101 endof
            k-up     of 0x102 endof
            k-down   of 0x103 endof
            k-home   of 0x104 endof
            k-end    of 0x105 endof
            k-prior  of 0x106 ( page up ) endof
            k-next   of 0x107 ( page down ) endof
            k-insert of 0x108 endof
            k-delete of 0x109 endof
            k-f1     of 0x10A endof
            k-f2     of 0x10B endof
            k-f3     of 0x10C endof
            k-f4     of 0x10D endof
            k-f5     of 0x10E endof
            k-f6     of 0x10F endof
            k-f7     of 0x110 endof
            k-f8     of 0x111 endof
            k-f9     of 0x112 endof
            k-f10    of 0x113 endof
            k-f11    of 0x114 endof
            k-f12    of 0x115 endof
            \ shift+
            k-left   k-shift-mask or of 0x120 endof
            k-right  k-shift-mask or of 0x121 endof
            k-up     k-shift-mask or of 0x122 endof
            k-down   k-shift-mask or of 0x123 endof
            k-home   k-shift-mask or of 0x124 endof
            k-end    k-shift-mask or of 0x125 endof
            k-prior  k-shift-mask or of 0x126 ( page up ) endof
            k-next   k-shift-mask or of 0x127 ( page down ) endof
            k-insert k-shift-mask or of 0x128 endof
            k-delete k-shift-mask or of 0x129 endof
            k-f1     k-shift-mask or of 0x12A endof
            k-f2     k-shift-mask or of 0x12B endof
            k-f3     k-shift-mask or of 0x12C endof
            k-f4     k-shift-mask or of 0x12D endof
            k-f5     k-shift-mask or of 0x12E endof
            k-f6     k-shift-mask or of 0x12F endof
            k-f7     k-shift-mask or of 0x130 endof
            k-f8     k-shift-mask or of 0x131 endof
            k-f9     k-shift-mask or of 0x132 endof
            k-f10    k-shift-mask or of 0x133 endof
            k-f11    k-shift-mask or of 0x134 endof
            k-f12    k-shift-mask or of 0x135 endof
            \ ctrl+
            k-left   k-ctrl-mask or of 0x140 endof
            k-right  k-ctrl-mask or of 0x141 endof
            k-up     k-ctrl-mask or of 0x142 endof
            k-down   k-ctrl-mask or of 0x143 endof
            k-home   k-ctrl-mask or of 0x144 endof
            k-end    k-ctrl-mask or of 0x145 endof
            k-prior  k-ctrl-mask or of 0x146 ( page up ) endof
            k-next   k-ctrl-mask or of 0x147 ( page down ) endof
            k-insert k-ctrl-mask or of 0x148 endof
            k-delete k-ctrl-mask or of 0x149 endof
            k-f1     k-ctrl-mask or of 0x14A endof
            k-f2     k-ctrl-mask or of 0x14B endof
            k-f3     k-ctrl-mask or of 0x14C endof
            k-f4     k-ctrl-mask or of 0x14D endof
            k-f5     k-ctrl-mask or of 0x14E endof
            k-f6     k-ctrl-mask or of 0x14F endof
            k-f7     k-ctrl-mask or of 0x150 endof
            k-f8     k-ctrl-mask or of 0x151 endof
            k-f9     k-ctrl-mask or of 0x152 endof
            k-f10    k-ctrl-mask or of 0x153 endof
            k-f11    k-ctrl-mask or of 0x154 endof
            k-f12    k-ctrl-mask or of 0x155 endof
            \ ctrl + shift + 
            k-left   k-ctrl-mask or k-shift-mask or of 0x160 endof
            k-right  k-ctrl-mask or k-shift-mask or of 0x161 endof
            k-up     k-ctrl-mask or k-shift-mask or of 0x162 endof
            k-down   k-ctrl-mask or k-shift-mask or of 0x163 endof
            k-home   k-ctrl-mask or k-shift-mask or of 0x164 endof
            k-end    k-ctrl-mask or k-shift-mask or of 0x165 endof
            k-prior  k-ctrl-mask or k-shift-mask or of 0x166 ( page up ) endof
            k-next   k-ctrl-mask or k-shift-mask or of 0x167 ( page down ) endof
            k-insert k-ctrl-mask or k-shift-mask or of 0x168 endof
            k-delete k-ctrl-mask or k-shift-mask or of 0x169 endof
            k-f1     k-ctrl-mask or k-shift-mask or of 0x16A endof
            k-f2     k-ctrl-mask or k-shift-mask or of 0x16B endof
            k-f3     k-ctrl-mask or k-shift-mask or of 0x16C endof
            k-f4     k-ctrl-mask or k-shift-mask or of 0x16D endof
            k-f5     k-ctrl-mask or k-shift-mask or of 0x16E endof
            k-f6     k-ctrl-mask or k-shift-mask or of 0x16F endof
            k-f7     k-ctrl-mask or k-shift-mask or of 0x170 endof
            k-f8     k-ctrl-mask or k-shift-mask or of 0x171 endof
            k-f9     k-ctrl-mask or k-shift-mask or of 0x172 endof
            k-f10    k-ctrl-mask or k-shift-mask or of 0x173 endof
            k-f11    k-ctrl-mask or k-shift-mask or of 0x174 endof
            k-f12    k-ctrl-mask or k-shift-mask or of 0x175 endof
            \ alt + 
            k-left   k-alt-mask or of 0x180 endof
            k-right  k-alt-mask or of 0x181 endof
            k-up     k-alt-mask or of 0x182 endof
            k-down   k-alt-mask or of 0x183 endof
            k-home   k-alt-mask or of 0x184 endof
            k-end    k-alt-mask or of 0x185 endof
            k-prior  k-alt-mask or of 0x186 ( page up ) endof
            k-next   k-alt-mask or of 0x187 ( page down ) endof
            k-insert k-alt-mask or of 0x188 endof
            k-delete k-alt-mask or of 0x189 endof
            k-f1     k-alt-mask or of 0x18A endof
            k-f2     k-alt-mask or of 0x18B endof
            k-f3     k-alt-mask or of 0x18C endof
            k-f4     k-alt-mask or of 0x18D endof
            k-f5     k-alt-mask or of 0x18E endof
            k-f6     k-alt-mask or of 0x18F endof
            k-f7     k-alt-mask or of 0x190 endof
            k-f8     k-alt-mask or of 0x191 endof
            k-f9     k-alt-mask or of 0x192 endof
            k-f10    k-alt-mask or of 0x193 endof
            k-f11    k-alt-mask or of 0x194 endof
            k-f12    k-alt-mask or of 0x195 endof
            \ alt + shift + 
            k-left   k-alt-mask or k-shift-mask or of 0x1A0 endof
            k-right  k-alt-mask or k-shift-mask or of 0x1A1 endof
            k-up     k-alt-mask or k-shift-mask or of 0x1A2 endof
            k-down   k-alt-mask or k-shift-mask or of 0x1A3 endof
            k-home   k-alt-mask or k-shift-mask or of 0x1A4 endof
            k-end    k-alt-mask or k-shift-mask or of 0x1A5 endof
            k-prior  k-alt-mask or k-shift-mask or of 0x1A6 ( page up ) endof
            k-next   k-alt-mask or k-shift-mask or of 0x1A7 ( page down ) endof
            k-insert k-alt-mask or k-shift-mask or of 0x1A8 endof
            k-delete k-alt-mask or k-shift-mask or of 0x1A9 endof
            k-f1     k-alt-mask or k-shift-mask or of 0x1AA endof
            k-f2     k-alt-mask or k-shift-mask or of 0x1AB endof
            k-f3     k-alt-mask or k-shift-mask or of 0x1AC endof
            k-f4     k-alt-mask or k-shift-mask or of 0x1AD endof
            k-f5     k-alt-mask or k-shift-mask or of 0x1AE endof
            k-f6     k-alt-mask or k-shift-mask or of 0x1AF endof
            k-f7     k-alt-mask or k-shift-mask or of 0x1B0 endof
            k-f8     k-alt-mask or k-shift-mask or of 0x1B1 endof
            k-f9     k-alt-mask or k-shift-mask or of 0x1B2 endof
            k-f10    k-alt-mask or k-shift-mask or of 0x1B3 endof
            k-f11    k-alt-mask or k-shift-mask or of 0x1B4 endof
            k-f12    k-alt-mask or k-shift-mask or of 0x1B5 endof
            \ alt + ctrl + 
            k-left   k-alt-mask or k-ctrl-mask or of 0x1C0 endof
            k-right  k-alt-mask or k-ctrl-mask or of 0x1C1 endof
            k-up     k-alt-mask or k-ctrl-mask or of 0x1C2 endof
            k-down   k-alt-mask or k-ctrl-mask or of 0x1C3 endof
            k-home   k-alt-mask or k-ctrl-mask or of 0x1C4 endof
            k-end    k-alt-mask or k-ctrl-mask or of 0x1C5 endof
            k-prior  k-alt-mask or k-ctrl-mask or of 0x1C6 ( page up ) endof
            k-next   k-alt-mask or k-ctrl-mask or of 0x1C7 ( page down ) endof
            k-insert k-alt-mask or k-ctrl-mask or of 0x1C8 endof
            k-delete k-alt-mask or k-ctrl-mask or of 0x1C9 endof
            k-f1     k-alt-mask or k-ctrl-mask or of 0x1CA endof
            k-f2     k-alt-mask or k-ctrl-mask or of 0x1CB endof
            k-f3     k-alt-mask or k-ctrl-mask or of 0x1CC endof
            k-f4     k-alt-mask or k-ctrl-mask or of 0x1CD endof
            k-f5     k-alt-mask or k-ctrl-mask or of 0x1CE endof
            k-f6     k-alt-mask or k-ctrl-mask or of 0x1CF endof
            k-f7     k-alt-mask or k-ctrl-mask or of 0x1D0 endof
            k-f8     k-alt-mask or k-ctrl-mask or of 0x1D1 endof
            k-f9     k-alt-mask or k-ctrl-mask or of 0x1D2 endof
            k-f10    k-alt-mask or k-ctrl-mask or of 0x1D3 endof
            k-f11    k-alt-mask or k-ctrl-mask or of 0x1D4 endof
            k-f12    k-alt-mask or k-ctrl-mask or of 0x1D5 endof
            \ alt + ctrl + shift + 
            k-left   k-alt-mask or k-ctrl-mask or k-shift-mask or of 0x1E0 endof
            k-right  k-alt-mask or k-ctrl-mask or k-shift-mask or of 0x1E1 endof
            k-up     k-alt-mask or k-ctrl-mask or k-shift-mask or of 0x1E2 endof
            k-down   k-alt-mask or k-ctrl-mask or k-shift-mask or of 0x1E3 endof
            k-home   k-alt-mask or k-ctrl-mask or k-shift-mask or of 0x1E4 endof
            k-end    k-alt-mask or k-ctrl-mask or k-shift-mask or of 0x1E5 endof
            k-prior  k-alt-mask or k-ctrl-mask or k-shift-mask or of 0x1E6 ( page up ) endof
            k-next   k-alt-mask or k-ctrl-mask or k-shift-mask or of 0x1E7 ( page down ) endof
            k-insert k-alt-mask or k-ctrl-mask or k-shift-mask or of 0x1E8 endof
            k-delete k-alt-mask or k-ctrl-mask or k-shift-mask or of 0x1E9 endof
            k-f1     k-alt-mask or k-ctrl-mask or k-shift-mask or of 0x1EA endof
            k-f2     k-alt-mask or k-ctrl-mask or k-shift-mask or of 0x1EB endof
            k-f3     k-alt-mask or k-ctrl-mask or k-shift-mask or of 0x1EC endof
            k-f4     k-alt-mask or k-ctrl-mask or k-shift-mask or of 0x1ED endof
            k-f5     k-alt-mask or k-ctrl-mask or k-shift-mask or of 0x1EE endof
            k-f6     k-alt-mask or k-ctrl-mask or k-shift-mask or of 0x1EF endof
            k-f7     k-alt-mask or k-ctrl-mask or k-shift-mask or of 0x1F0 endof
            k-f8     k-alt-mask or k-ctrl-mask or k-shift-mask or of 0x1F1 endof
            k-f9     k-alt-mask or k-ctrl-mask or k-shift-mask or of 0x1F2 endof
            k-f10    k-alt-mask or k-ctrl-mask or k-shift-mask or of 0x1F3 endof
            k-f11    k-alt-mask or k-ctrl-mask or k-shift-mask or of 0x1F4 endof
            k-f12    k-alt-mask or k-ctrl-mask or k-shift-mask or of 0x1F5 endof
            >r 0xFFFF r> 
        endcase
        swap set-reg
     else
        drop 0xFFFF swap set-reg 
     endif
  endif ;
                
: stb; ( value dest -- ) swap get-reg addr8 swap get-reg store-byte ;
: ldb; ( addr dest -- ) swap get-reg load-byte swap set-reg ;
: tdims; ( rows cols -- ) 
  form ( rows cols urows ucols )
  >r ( rows cols ucols )
  swap set-reg 
  r> swap set-reg ;
: setcur; ( column row -- ) get-reg swap get-reg swap at-xy ;
: clrscr; ( -- ) page ;


: opcode ( n body decoder -- ) 
  rot addr8 ( body decoder n ) 
  swap over ( body n decoder n )
  decoders ! 
  bodies ! ;
: opcode0 ( n body -- ) ['] decode-no-register opcode ;
: opcode1 ( n body -- ) ['] decode-1reg opcode ;
: opcode2 ( n body -- ) ['] decode-2reg opcode ;
: opcode3 ( n body -- ) ['] decode-3reg opcode ;
: opcode1i16 ( n body -- ) ['] decode-1reg-imm16 opcode ;
: opcode2i16 ( n body -- ) ['] decode-2reg-imm16 opcode ;
: opcodei16 ( n body -- ) ['] decode-imm16 opcode ;
: opcode1i4 ( n body -- ) ['] decode-1reg-imm4 opcode ;
: opcode1i8 ( n body -- ) ['] decode-one-reg-imm8 opcode ;
\ wiring 
#illegal  ' illegal-instruction ' illegal-instruction opcode
#add      ' add;           opcode3
#sub      ' sub;           opcode3
#mul      ' mul;           opcode3
#div      ' div;           opcode3
#rem      ' rem;           opcode3
#lshift   ' lshift;        opcode3
#rshift   ' rshift;        opcode3
#and      ' and;           opcode3
#or       ' or;            opcode3
#invert   ' invert;        opcode2
#xor      ' xor;           opcode3
#min      ' min;           opcode3
#max      ' max;           opcode3
#eq       ' eq;            opcode3
#neq      ' neq;           opcode3
#lt       ' lt;            opcode3
#gt       ' gt;            opcode3
#le       ' le;            opcode3
#ge       ' ge;            opcode3
#set      ' set;           opcode1i16
#ld       ' ld;            opcode2
#st       ' st;            opcode2
#push     ' push;          opcode2
#pop      ' pop;           opcode2
#br       ' rbranch;       opcode1
#brl      ' rbranch-link;  opcode2
#bcr      ' ?rbranch;      opcode2
#bcrl     ' ?rbranch-link; opcode3
#ueq      ' ueq;           opcode3
#uneq     ' uneq;          opcode3
#ult      ' ult;           opcode3
#ugt      ' ugt;           opcode3
#ule      ' ule;           opcode3
#uge      ' uge;           opcode3
#umin     ' umin;          opcode3
#umax     ' umax;          opcode3
#incr     ' 1+;            opcode2
#decr     ' 1-;            opcode2
#call     ' call;          opcode1i16 
#condb    ' ?branch;       opcode1i16 
#addi     ' addi;          opcode2i16 
#subi     ' subi;          opcode2i16 
#rshifti  ' rshifti;       opcode2i16 
#lshifti  ' lshifti;       opcode2i16 
#move     ' move;          opcode2
#bi       ' branch;        opcodei16
#andi     ' andi;          opcode2i16
#muli     ' muli;          opcode2i16
#divi     ' divi;          opcode2i16
#pushi    ' pushi;         opcode1i16
#stop     ' stop;          opcode1 \ stop execution
#set4     ' set;           opcode1i4
#set8     ' set;           opcode1i8
#pushi4   ' pushi;         opcode1i4
#pushi8   ' pushi;         opcode1i8
#emit     ' emit;          opcode1
#?key	  ' ?key;		   opcode1
#key 	  ' key; 		   opcode1
#stb      ' stb;           opcode2
#ldb      ' ldb;           opcode2
#clrscr   ' clrscr;        opcode0
#setcur   ' setcur;        opcode2
#tdims    ' tdims;         opcode2

: set-memory ( value address -- ) swap addr8 swap store-byte ;
set-current
: i>x ( idx -- reg ) idx>reg ;
: x>i ( reg -- idx ) reg>idx ;
: x@ ( reg -- value ) get-reg ;
: x! ( value reg -- ) set-reg ;
: pc@ ( -- value ) pc get-reg ;
: pc! ( value -- ) set-pc ;
: pc1+ ( value -- ) advance-pc ;
: mem@ ( addr -- value ) load-byte ;
: inst! ( args* length addr -- length ) 
  over >r 
  addr16 swap over + addr16 swap ( args* end addr )
  ?do 
    I store-byte 
  loop r> ;
: inst@ ( addr -- args* ) 
  pc@ >r \ save pc
  dup 1+ pc! \ goto the next location
  mem@ dup >r decode-instruction 
  r> 
  r> pc! \ restore pc 
  ;
: inst!->pc ( args* length -- length ) 
  pc@ inst! pc@ + pc! ;
: opcode>idx ( opcode -- idx ) 2 cells + @ ;
\ used for the purposes of data definitions
: print-pc ( -- ) ." pc: " get-pc . cr ;
: examine-memory ( -- ) data-memory memory-size-in-cells cells dump ;
: examine-word ( address -- ) dup load-word swap . ." : " . cr ;
: examine-byte ( address -- ) dup load-byte swap . ." : " . cr ;
data-memory constant _memory
: inspect-registers ( -- ) cr
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
