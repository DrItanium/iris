get-current ( wid )
vocabulary iris also iris definitions

: nout ( num id -- ) swap s>d <<# #s #> rot write-line #>> throw ;
: addr-mask ( mask "name" -- )
  CREATE , 
  does> @ and ;
0x3f addr-mask addr6
0xFFFF addr-mask addr16
0x0FFF addr-mask addr12
0xFFFFFFFF addr-mask addr32
variable mloc \ current memory location
: loc@ ( -- n ) mloc @ ;
: loc! ( n -- ) addr16 mloc ! ; \ make sure that it doesn't go out of bounds
: loc1+ ( -- ) loc@ 1+ loc! ;
: loc2+ ( -- ) loc@ 2+ loc! ;
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
0 constant RegisterValueSpace
1 constant MemorySpace
2 constant CoreMemorySpace
3 constant InstructionSpace 

: linker-entry ( kind address value -- n ) 
  addr32 0x20 lshift ( k a v<<32 )
  swap ( k v3 a )
  addr16 0x10 lshift 
  rot ( v3 a k )
  0xFF and ( v3 a1 k8 )
  or ( v3 n )
  or ( n ) ;
: def-space-entry ( value "name" -- )
  create c, 
  does> ( addr value -- n )
  @ -rot linker-entry ;


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
: move, ( src dest -- n ) zero swap add, ;
: nop, ( -- n ) zero zero zero add, ;
: -> ( src dest -- n ) move, ;
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
15 inst-3reg eq,
16 inst-3reg neq,
17 inst-3reg lt,
18 inst-3reg gt,
19 inst-3reg le,
20 inst-3reg ge,
21 inst-1reg-with-imm set,
: $-> ( imm dest -- n ) set, ;
: $->at0 ( imm -- n ) at0 $-> ;
: $->at1 ( imm -- n ) at1 $-> ;
: $->at0-3arg ( imm a b -- at0 a b n ) 
  rot $->at0 >r 
  at0 ( a b at0 ) -rot ( at0 a b ) 
  r> ( at0 a b n ) ;

: $->at0-2arg ( imm b -- at0 b n )
  swap ( b imm )
  $->at0 at0 -rot ( at0 b n ) ;
: $->at0-1arg ( imm -- at0 ) $->at0 at0 ;
22 inst-2reg ld,
23 inst-2reg st,
24 inst-2reg push,
25 inst-2reg pop,
26 inst-2reg ldc,
27 inst-2reg stc,
28 inst-2reg ldio,
29 inst-2reg stio,
30 inst-1reg br,
31 inst-2reg brl,
32 inst-2reg bcr,
33 inst-3reg bcrl,
34 inst-3reg ueq,
35 inst-3reg uneq,
36 inst-3reg ult,
37 inst-3reg ugt,
38 inst-3reg ule,
39 inst-3reg uge,
40 inst-3reg uand,
41 inst-3reg uor,
42 inst-2reg unot,
43 inst-3reg uxor,
44 inst-3reg unand,
45 inst-3reg unor,
46 inst-3reg umin,
47 inst-3reg umax,
48 inst-3reg uadd,
49 inst-3reg usub,
50 inst-3reg umul,
51 inst-3reg udiv,
52 inst-3reg urem,
53 inst-3reg ushl,
54 inst-3reg ushr,
55 inst-2reg readtok,
56 inst-3reg number,

: def2argi ( "name" "op" -- )
  create ' , 
  does> ( imm dest -- n set-op )
  >r \ stash the address for now
  $->at0-2arg
  r> \ get the top back
  swap
  >r \ stash the set op for now
  @ execute \ execute the stashed operation
  r> \ restore the top
  ;

: def3argi ( "name" "op" -- )
  create ' , 
  does> ( imm src dest -- n set-op )
  >r \ stash the address for now
  $->at0-3arg
  r> \ get the top back
  swap
  >r \ put the new value back onto the stack
  @ execute \ we should now have the argument correctly setup
  r> \ get the set back as well
  ;

: b, ( imm -- n s ) $->at0 >r at0 br, r> ;
: bl, ( imm lr -- n s ) swap $->at0 >r at0 brl, r> ;
: bc, ( imm cond -- n s ) swap $->at0 >r at0 bcr, r> ;
: bcl, ( imm link cond -- n s ) rot $->at0 >r at0 bcrl, r> ;
def3argi addi, add,
def3argi subi, sub,
def3argi muli, mul,
def3argi divi, div,
def3argi remi, rem,
def3argi andi, and,
def3argi ori, or,
def3argi xori, xor,
def3argi nori, nor,
def3argi nandi, nand,
def2argi noti, not,
def3argi eqi, eq,
def3argi neqi, neq,
def3argi gti, gt,
def3argi lti, lt,
def3argi gei, ge,
def3argi lei, le,

def3argi uaddi, uadd,
def3argi usubi, usub,
def3argi umuli, umul,
def3argi udivi, udiv,
def3argi uremi, urem,
def3argi uandi, uand,
def3argi uori, uor,
def3argi uxori, uxor,
def3argi unori, unor,
def3argi unandi, unand,
def2argi unoti, unot,
def3argi ueqi, ueq,
def3argi uneqi, uneq, 
def3argi ulti, ult,
def3argi ugti, ugt,
def3argi ulei, ule,
def3argi ugei, uge,

RegisterValueSpace def-space-entry register-entry
MemorySpace def-space-entry memory-entry
CoreMemorySpace def-space-entry core-entry
InstructionSpace def-space-entry instruction-entry
: <<linker ( entry id -- ) 
  hex
  dup >r
  nout
  s" " r> write-line throw ;
: <<inst ( inst id -- ) 
  >r
  @loc swap instruction-entry
  r> <<linker 
  loc2+ \ each instruction is two entries
  ;
: <<mem ( value id -- )
  >r
  @loc swap memory-entry
  r> <<linker loc1+ ;
: <<core ( inst id -- )
  >r 
  @loc swap core-entry
  r> <<linker loc1+ ;
: <<register ( index value id -- ) 
  >r
  register-entry 
  r> <<linker ; 



: .label ( -- ) loc@ constant ;
: .org ( n -- ) loc! ;
: .data16 ( n -- v ) addr16 ;
: .data32 ( n -- vlower vupper ) dup .data16 swap 0x10 rshift .data16 ;


( linker format routines as well )


previous
( The idea is to write a simple interpreter and perform the memory encoding
  within gforth and then dump it out to disk in such a way as to make it easy
  to load into the iris interpreter. The simplest way is to dump it out in the
  core format as a series of 16 bit numbers with each core section being 64kb
  in size. )
