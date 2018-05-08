get-current ( wid )
vocabulary iris also iris definitions
: addr-mask ( mask "name" -- )
  CREATE , 
  does> @ and ;
0x3f addr-mask addr6
0xFFFF addr-mask addr16
0x0FFF addr-mask addr12
variable mloc \ current memory location
: loc@ ( -- n ) mloc @ ;
: loc! ( n -- ) addr16 mloc ! ; \ make sure that it doesn't go out of bounds
: loc1+ ( -- ) loc@ 1+ loc! ;
: reg-pos ( shift "name" -- ) 
  create , 
  does> swap addr6 swap @ lshift ;
8 reg-pos dest-reg
14 reg-pos src-reg
20 reg-pos src2-reg
26 reg-pos src3-reg
: 1reg ( dest -- value ) dest-reg ;
: 2reg ( src dest -- value ) 1reg swap src-reg or ;
: 3reg ( src2 src dest -- value ) 2reg swap src2-reg or ;
: 4reg ( src3 src2 src dest -- value ) 3reg swap src3-reg or ;
: imm16 ( imm16 -- value ) addr16 16 lshift ;
: imm12 ( imm12 -- value ) addr12 20 lshift ;
: imm6 ( imm6 -- value ) addr6 26 lshift ;

: 1reg-with-imm ( imm16 dest -- value )
  1reg swap imm16 or ;
: 2reg-with-imm ( imm12 src dest -- value )
  2reg swap imm12 or ;
: 3reg-with-imm ( imm6 src2 src dest -- value )
  3reg swap imm6 or ;
: cconstant ( byte "name" -- ) create c, does> c@ ;
: xop& ( n a -- k ) c@ or ;
: inst-1reg ( opcode-index "name" -- )
  create c, \ embed opcode
  does> >r 
        1reg
        r> xop& ;
: inst-2reg ( opcode-index "name" -- )
  create c, \ embed opcode
  does> >r
        2reg
        r> xop& ;
: inst-3reg ( opcode-index "name" -- )
  create c, \ embed opcode
  does> >r 
        3reg 
		r> xop& ;
: inst-4reg ( opcode-index "name" -- )
  create c, \ embed opcode
  does> >r
        4reg
        r> xop& ;
: inst-1reg-with-imm ( opcode-index "name" -- )
  create c, \ embed opcode
  does> >r
        1reg-with-imm
        r> xop& ;
: inst-imm16 ( opcode-index "name" -- )
  create c, \ embed opcode
  does> >r
        imm16
        r> xop& ;


\ registers
set-current \ go back
0 cconstant zero
1 cconstant error-code
2 cconstant terminator
3 cconstant num-base
4 cconstant dsp
5 cconstant rsp
6 cconstant vmsp
7 cconstant cv
8 cconstant lr
9 cconstant at0
10 cconstant at1
11 cconstant at2
12 cconstant at3
13 cconstant io
14 cconstant ci \ core index number

0  inst-3reg add, 
1  inst-3reg sub, 
2  inst-3reg mul, 
3  inst-3reg div, 
4  inst-3reg rem, 
5  inst-3reg shl, 
6  inst-3reg shr, 
7  inst-3reg and, 
8  inst-3reg or, 
9  inst-2reg not, 
10 inst-3reg xor, 
11 inst-3reg nand, 
12 inst-3reg nor,
13 inst-3reg min,
14 inst-3reg max,
15 inst-3reg lxor,
16 inst-2reg lnot,
17 inst-3reg land,
18 inst-3reg lor,
19 inst-3reg lnand,
20 inst-3reg lnor,
21 inst-3reg eq,
22 inst-3reg neq,
23 inst-3reg lt,
24 inst-3reg gt,
25 inst-3reg le,
26 inst-3reg ge,
27 inst-1reg-with-imm set,
28 inst-2reg ld,
29 inst-2reg st,
30 inst-2reg push,
31 inst-2reg pop,
32 inst-2reg ldc,
33 inst-2reg stc,
34 inst-2reg ldio,
35 inst-2reg stio,
36 inst-1reg br,
37 inst-2reg brl,
38 inst-2reg bcr,
39 inst-3reg bcrl,
40 inst-3reg ueq,
41 inst-3reg uneq,
42 inst-3reg ult,
43 inst-3reg ugt,
44 inst-3reg ule,
45 inst-3reg uge,
46 inst-3reg uand,
47 inst-3reg uor,
48 inst-2reg unot,
49 inst-3reg uxor,
50 inst-3reg unand,
51 inst-3reg unor,
52 inst-3reg umin,
53 inst-3reg umax,
54 inst-3reg uadd,
55 inst-3reg usub,
56 inst-3reg umul,
57 inst-3reg udiv,
58 inst-3reg urem,
59 inst-3reg ushl,
60 inst-3reg ushr,
61 inst-2reg readtok,
62 inst-3reg number,
: move, ( src dest -- n ) zero swap uor, ;
: -> ( src dest -- n ) move, ;
: $-> ( imm dest -- n ) set, ;
: $->at0 ( imm -- n ) at0 $-> ;
: $->at0-3arg ( imm a b -- at0 a b n ) 
  rot $->at0 >r 
  at0 ( a b at0 ) -rot ( at0 a b ) 
  r> ( at0 a b n ) ;


: addi, ( imm src dest -- n set-op ) 
  $->at0-3arg ( at0 a b so )
  >r 
  add,
  r> ;

: subi, ( imm src dest -- n set-op ) 
  $->at0-3arg ( at0 a b so )
  >r 
  sub,
  r> ;
: muli, ( imm src dest -- n set-op ) 
  $->at0-3arg ( at0 a b so )
  >r 
  mul,
  r> ;

: divi, ( imm src dest -- n set-op ) 
  $->at0-3arg ( at0 a b so )
  >r 
  div,
  r> ;

: .org ( n -- ) loc! ;
: .data16 ( n -- v ) addr16 ;
: .data32 ( n -- vlower vupper ) dup addr16 swap 16 rshift addr16 ;
: .data64 ( n -- vlowest vlower vhigher vhighest ) dup 
>r .data32 \ lower half 
r> 32 rshift .data32 ;
previous
( The idea is to write a simple interpreter and perform the memory encoding
  within gforth and then dump it out to disk in such a way as to make it easy
  to load into the iris interpreter. The simplest way is to dump it out in the
  core format as a series of 16 bit numbers with each core section being 64kb
  in size. )
