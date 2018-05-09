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
: imm16 ( imm16 -- value ) addr16 16 lshift ;

: cconstant ( byte "name" -- ) create c, does> c@ ;
: xop& ( n a -- k ) c@ or ;
0 constant RegisterValueSpace
1 constant MemorySpace
2 constant CoreMemorySpace
3 constant InstructionSpace 
4 constant LabelSpace
5 constant IndirectInstructionSpace
6 constant IndirectMemorySpace


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
: opcode: dup cconstant 1+ ;
: opcode} ( n -- ) registers} ;

{opcode
opcode: #add 
opcode: #sub 
opcode: #mul 
opcode: #div 
opcode: #rem 
opcode: #shl 
opcode: #shr 
opcode: #and 
opcode: #or 
opcode: #not 
opcode: #xor 
opcode: #nand 
opcode: #nor
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
opcode: #ldc
opcode: #stc
opcode: #ldio
opcode: #stio
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
opcode: #unot
opcode: #uxor
opcode: #unand
opcode: #unor
opcode: #umin
opcode: #umax
opcode: #uadd
opcode: #usub
opcode: #umul
opcode: #udiv
opcode: #urem
opcode: #ushl
opcode: #ushr
opcode: #readtok
opcode: #number
opcode}

\ registers
set-current \ go back


RegisterValueSpace def-space-entry register-entry
MemorySpace def-space-entry memory-entry
\ CoreMemorySpace def-space-entry core-entry
InstructionSpace def-space-entry instruction-entry
LabelSpace def-space-entry label-entry 
IndirectInstructionSpace def-space-entry indirect-instruction-entry 
IndirectMemorySpace def-space-entry indirect-memory-entry
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
: <<imem ( value id -- )
  >r
  loc@ swap indirect-memory-entry
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


: .org ( n -- ) loc! ;

variable CurrentAssemblyFile

: curasm@ ( -- file ) CurrentAssemblyFile @ ;
: curasm! ( n -- ) CurrentAssemblyFile ! ;
: clearasm ( -- ) 0 curasm! ;
: <<imminst ( n set -- ) curasm@ swap over <<iinst <<inst ;
: <<inst ( n -- ) curasm@ <<inst ;
: .label ( label -- ) curasm@ <<label ;
: .data16 ( n -- ) curasm@ swap addr16 swap <<mem ;
: .data32 ( n -- ) curasm@ swap addr32 swap <<inst ;
: .idata32 ( n -- ) curasm@ swap addr16 swap <<imem ;
: .register ( index value -- ) curasm@ <<register ;
: {asm ( path -- ) 
  w/o create-file throw curasm! 
  reset-labels ;
: asm} ( -- ) curasm@ close-file throw clearasm ;

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
: inst-1reg ( opcode-index "name" -- )
  create c, \ embed opcode
  does> >r 
        1reg
        r> xop& <<inst ;
: inst-2reg ( opcode-index "name" -- )
  create c, \ embed opcode
  does> >r
        2reg
        r> xop& <<inst ;
: inst-3reg ( opcode-index "name" -- )
  create c, \ embed opcode
  does> >r 
        3reg 
		r> xop& <<inst ;

#add inst-3reg add, 
: move, ( src dest -- n ) zero swap add, ;
: nop, ( -- n ) zero zero zero add, ;
: -> ( src dest -- n ) move, ;
\ constant tagging version
\ #, is a constant version
: #, ( -- 0 ) 0 ; 
\ !, is an indirect instruction
: !, ( -- 1 ) 1 ;

#sub inst-3reg sub, 
#mul inst-3reg mul, 
#div inst-3reg div, 
#rem inst-3reg rem, 
#shl inst-3reg shl, 
#shr inst-3reg shr, 
#and inst-3reg and, 
#or inst-3reg or, 
#not inst-2reg not, 
#xor inst-3reg xor, 
#nand inst-3reg nand, 
#nor inst-3reg nor,
#min inst-3reg min,
#max inst-3reg max,
#eq inst-3reg eq,
#neq inst-3reg neq,
#lt inst-3reg lt,
#gt inst-3reg gt,
#le inst-3reg le,
#ge inst-3reg ge,
: set, ( imm imm-type dest -- )
  1reg #set
  or ( imm it v ) rot ( it v imm ) 
  addr16 16 lshift or
  swap ( v it )
  0= if ( constant ) <<inst else <<iinst endif ;
: #set, ( imm dest -- ) #, swap set, ;
: !set, ( imm dest -- ) !, swap set, ;

: $-> ( imm id dest -- n ) set, ;
: $->at0 ( imm id -- n ) at0 $-> ;
: $->at1 ( imm id -- n ) at1 $-> ;
: $->at0-3arg ( imm id a b -- at0 a b ) >r >r $->at0 at0 r> r> swap ;
: $->at0-2arg ( imm id b -- at0 b )
  -rot ( b imm id )
  $->at0 ( b )
  at0 swap ( at0 b ) ;
#ld inst-2reg ld,
#st inst-2reg st,
#push inst-2reg push,
#pop inst-2reg pop,
#ldc inst-2reg ldc,
#stc inst-2reg stc,
#ldio inst-2reg ldio,
#stio inst-2reg stio,
#br inst-1reg br,
#brl inst-2reg brl,
#bcr inst-2reg bcr,
#bcrl inst-3reg bcrl,
#ueq inst-3reg ueq,
#uneq inst-3reg uneq,
#ult inst-3reg ult,
#ugt inst-3reg ugt,
#ule inst-3reg ule,
#uge inst-3reg uge,
#uand inst-3reg uand,
#uor inst-3reg uor,
#unot inst-2reg unot,
#uxor inst-3reg uxor,
#unand inst-3reg unand,
#unor inst-3reg unor,
#umin inst-3reg umin,
#umax inst-3reg umax,
#uadd inst-3reg uadd,
#usub inst-3reg usub,
#umul inst-3reg umul,
#udiv inst-3reg udiv,
#urem inst-3reg urem,
#ushl inst-3reg ushl,
#ushr inst-3reg ushr,
#readtok inst-2reg readtok,
#number inst-3reg number,

: def2argi ( "name" "op" -- )
  create ' , 
  does> ( imm dest -- n set-op )
  >r \ stash the address for now
  $->at0-2arg 
  r> \ get the top back
  @ execute \ execute the stashed operation
  ;

: def3argi ( "name" "op" -- )
  create ' , 
  does> ( imm src dest -- n set-op )
  >r \ stash the address for now
  $->at0-3arg
  r> \ get the top back
  @ execute \ we should now have the argument correctly setup
  ;

: b, ( imm id -- ) $->at0 at0 br, ;
: bl, ( imm id lr -- ) -rot $->at0 at0 brl, ;
: bc, ( imm id cond -- ) -rot $->at0 at0 bcr, ;
: bcl, ( imm id link cond -- ) >r >r $->at0 r> r> swap at0 bcrl, ;
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
previous
