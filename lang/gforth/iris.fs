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
variable labelIndex
variable mloc \ current memory location
: 2+ ( n -- 2+n) 2 + ;
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
  create dup c, 1+ \ embed opcode
  does> >r 
        1reg
        r> xop& ;
: inst-2reg ( opcode-index "name" -- )
  create dup c, 1+ \ embed opcode
  does> >r
        2reg
        r> xop& ;
: inst-3reg ( opcode-index "name" -- )
  create dup c, 1+ \ embed opcode
  does> >r 
        3reg 
		r> xop& ;
: inst-4reg ( opcode-index "name" -- )
  create dup c, 1+ \ embed opcode
  does> >r
        4reg
        r> xop& ;
: inst-1reg-with-imm ( opcode-index "name" -- )
  create dup c, 1+ \ embed opcode
  does> >r
        1reg-with-imm
        r> xop& ;
0 constant RegisterValueSpace
1 constant MemorySpace
2 constant CoreMemorySpace
3 constant InstructionSpace 
4 constant LabelSpace
5 constant IndirectInstructionSpace


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
: {registers ( -- 0 ) 0 ;
: registers} ( n -- ) drop ;
: register: dup cconstant 1+ ;
: {opcode ( -- 0 ) {registers ;
: opcode} ( n -- ) registers} ;

\ registers
set-current \ go back

{registers
register: r0
register: r1
register: r2
register: r3
register: r4
register: r5
register: r6
register: r7
register: r8
register: r9
register: r10
register: r11
register: r12
register: r13
register: r14
register: r15
register: r16
register: r17
register: r18
register: r19
register: r20
register: r21
register: r22
register: r23
register: r24
register: r25
register: r26
register: r27
register: r28
register: r29
register: r30
register: r31
register: r32
register: r33
register: r34
register: r35
register: r36
register: r37
register: r38
register: r39
register: r40
register: r41
register: r42
register: r43
register: r44
register: r45
register: r46
register: r47
register: r48
register: r49
register: r50
register: r51
register: r52
register: r53
register: r54
register: r55
register: r56
register: r57
register: r58
register: r59
register: r60
register: r61
register: r62
register: r63
registers}
r0 cconstant zero
r1 cconstant error-code
r2 cconstant terminator
r3 cconstant num-base
r4 cconstant dsp
r5 cconstant rsp
r6 cconstant vmsp
r7 cconstant cv
r8 cconstant lr
r9 cconstant at0
r10 cconstant at1
r11 cconstant at2
r12 cconstant at3
r13 cconstant io
r14 cconstant ci \ core index number

: sanity-check-opcode ( op expected -- ) <> ABORT" opcode does not match expected!" ;
{opcode
inst-3reg add, 
: move, ( src dest -- n ) zero swap add, ;
: nop, ( -- n ) zero zero zero add, ;
: -> ( src dest -- n ) move, ;
inst-3reg sub, 
inst-3reg mul, 
inst-3reg div, 
inst-3reg rem, 
inst-3reg shl, 
inst-3reg shr, 
inst-3reg and, 
inst-3reg or, 
inst-2reg not, 
inst-3reg xor, 
inst-3reg nand, 
inst-3reg nor,
inst-3reg min,
inst-3reg max,
inst-3reg eq,
inst-3reg neq,
inst-3reg lt,
inst-3reg gt,
inst-3reg le,
inst-3reg ge,
inst-1reg-with-imm set,
\ sanity check

0 zero set, 21 sanity-check-opcode 
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
 inst-2reg ld,
 inst-2reg st,
 inst-2reg push,
 inst-2reg pop,
 inst-2reg ldc,
 inst-2reg stc,
 inst-2reg ldio,
 inst-2reg stio,
 inst-1reg br,
 inst-2reg brl,
 inst-2reg bcr,
 inst-3reg bcrl,
 inst-3reg ueq,
 inst-3reg uneq,
 inst-3reg ult,
 inst-3reg ugt,
 inst-3reg ule,
 inst-3reg uge,
 inst-3reg uand,
 inst-3reg uor,
 inst-2reg unot,
 inst-3reg uxor,
 inst-3reg unand,
 inst-3reg unor,
 inst-3reg umin,
 inst-3reg umax,
 inst-3reg uadd,
 inst-3reg usub,
 inst-3reg umul,
 inst-3reg udiv,
 inst-3reg urem,
 inst-3reg ushl,
 inst-3reg ushr,
 inst-2reg readtok,
 inst-3reg number,
opcode}

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
LabelSpace def-space-entry label-entry 
IndirectInstructionSpace def-space-entry indirect-instruction-entry 
: <<linker ( entry id -- ) 
  hex
  dup >r
  nout
  s" " r> write-line throw ;
: <<inst ( inst id -- ) 
  >r
  loc@ swap instruction-entry
  r> <<linker 
  loc2+ \ each instruction is two entries
  ;
: <<iinst ( inst id -- ) 
  >r
  loc@ swap indirect-instruction-entry
  r> <<linker 
  loc2+ \ each instruction is two entries
  ;
: <<mem ( value id -- )
  >r
  loc@ swap memory-entry
  r> <<linker loc1+ ;
: <<core ( inst id -- )
  >r 
  loc@ swap core-entry
  r> <<linker loc1+ ;
: <<register ( index value id -- ) 
  >r
  register-entry 
  r> <<linker ; 
: <<label ( index id -- ) 
  >r
  loc@ swap label-entry
  r> <<linker ;
\ labels must be defined ahead of time before first reference
: reset-labels ( -- ) 0 labelIndex ! ;
: deflabel ( "name" -- ) create labelIndex @ addr32 , labelIndex @ 1+ labelIndex ! does> @ ;


: .label ( label id -- ) <<label ; 
: .org ( n -- ) loc! ;
: .data16 ( n id -- ) swap addr16 swap <<mem ;
: .data32 ( n id -- )  swap addr32 swap <<inst ;
\ : dumpable-inst ( "name" "name2" -- ) 
\  create ' does> swap >r @ execute r> <<inst ;

previous
