get-current ( wid )
vocabulary iris also iris definitions

: nout ( num id -- ) swap s>d 
\ get rid of sign extension nonsense
dup 0< if 0 and endif 
<<# #s #> rot write-file #>> throw ;
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
: imm12 ( imm12 -- value ) addr12 20 lshift ;
: imm16 ( imm16 -- value ) addr16 16 lshift ;
: 2reg-imm12 ( imm12 src dest -- value ) 2reg swap imm12 or ;
: 1reg-imm16 ( imm16 dest -- value ) 1reg swap imm16 or ;

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
: {ioaddr ( -- 0 ) {registers ;
: ioaddr} ( n -- ) registers} ;
: ioaddr: ( n -- k ) dup constant 1+ ;

{opcode
opcode: #add 
opcode: #sub 
opcode: #mul 
opcode: #div 
opcode: #rem 
opcode: #lshift
opcode: #rshift
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
opcode: #ulshift
opcode: #urshift
\ opcode: #readtok
\ opcode: #number
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
  s" " r> write-line throw decimal ;
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
: <<iinst ( n -- ) curasm@ <<iinst ;
: <<inst ( n -- ) curasm@ <<inst ;
: .label ( label -- ) curasm@ <<label ;
: .data16 ( imm id -- ) swap addr16 swap curasm@ swap 0= if <<mem else <<imem endif ;
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
r4 cconstant separator
r5 cconstant vmsp
r6 cconstant cv
r7 cconstant lr
r8 cconstant at0
r9 cconstant at1
r10 cconstant at2
r11 cconstant cmd \ command buffer register
r12 cconstant io
r13 cconstant arg0
r14 cconstant arg1
r15 cconstant arg2
r16 cconstant arg3
r17 cconstant loc0
r18 cconstant loc1
r19 cconstant loc2
r20 cconstant loc3
r21 cconstant loc4
r22 cconstant loc5
r23 cconstant loc6
r24 cconstant loc7
r25 cconstant out0
r26 cconstant out1
r27 cconstant out2
r28 cconstant out3
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
: #, ( imm -- imm16 0 ) addr16 0 ; 
\ !, is an indirect instruction
: !, ( -- 1 ) 1 ;

#sub inst-3reg sub, 
#mul inst-3reg mul, 
#div inst-3reg div, 
#rem inst-3reg rem, 
#rshift inst-3reg rshift,
#lshift inst-3reg lshift, 
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
: ?choose-instruction-kind ( v id -- ) 0= if ( c ) <<inst else <<iinst endif ;
: set, ( imm imm-type dest -- )
  1reg #set or ( imm it v ) 
  rot ( it v imm )  
  imm16 or swap ( v it )
  ?choose-instruction-kind ;
\ only known constants here!
: ?imm0 ( imm v -- imm v f ) over 0= ;
: #set, ( imm dest -- ) 
  ?imm0
  if 
    \ emit a move zero if it turns out we are looking at a constant zero
    \ zero and 0 translate to the same thing
    move,
  else 
    #, swap set, \ otherwise emit as normal
  endif ;
: !set, ( imm dest -- ) !, swap set, ;
: $-> ( imm id dest -- n ) set, ;
: $->at0 ( imm id -- n ) at0 $-> ;
: $->at0-3arg ( imm id a b -- at0 a b ) 2>r $->at0 at0 2r> ;
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
#ulshift inst-3reg ulshift,
#urshift inst-3reg urshift, 
\ #readtok inst-2reg readtok,
\ #number inst-3reg number,
#incr inst-2reg incr,
#decr inst-2reg decr,
#uincr inst-2reg uincr,
#udecr inst-2reg udecr,
: bl, ( imm imm-type dest -- )
  1reg #call
  or ( imm it v ) 
  rot ( it v imm )  
  imm16
  or 
  swap ( v it )
  ?choose-instruction-kind ;
: ?imm0 ( imm v -- imm v f ) over 0= ;
: #bl, ( imm dest -- ) 
  ?imm0
  if 
    \ emit a move zero if it turns out we are looking at a constant zero
    \ zero and 0 translate to the same thing
    move,
  else 
    #, swap bl, \ otherwise emit as normal
  endif ;
: !bl, ( imm dest -- ) !, swap bl, ;

: bc, ( imm imm-type dest -- )
  1reg #condb
  or ( imm it v ) 
  rot ( it v imm )  
  imm16
  or 
  swap ( v it )
  ?choose-instruction-kind ;
: ?imm0 ( imm v -- imm v f ) over 0= ;
: #bc, ( imm dest -- ) 
  ?imm0
  if 
    \ emit a move zero if it turns out we are looking at a constant zero
    \ zero and 0 translate to the same thing
    move,
  else 
    #, swap bc, \ otherwise emit as normal
  endif ;
: !bc, ( imm dest -- ) !, swap bc, ;

: def2argi ( "name" "op" -- )
  create ' , 
  does> ( imm id dest -- n set-op )
  >r \ stash the address for now
  $->at0-2arg 
  r> \ get the top back
  @ execute \ execute the stashed operation
  ;
: def3argi ( "name" "op" -- )
  create ' , 
  does> ( imm id src dest -- n set-op )
  >r \ stash the address for now
  $->at0-3arg
  r> \ get the top back
  @ execute \ we should now have the argument correctly setup
  ;

: b, ( imm id -- ) zero bl, ;
: bcl, ( imm id link cond -- ) 2>r $->at0 2r> at0 bcrl, ;
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
: zero-instead, ( src dest -- ) swap drop zero swap -> ;
: emit-2reg-imm12 ( imm12 src dest opcode -- ) >r 2reg-imm12 r> or <<inst ;
: ?not-imm12 ( imm id -- imm id f ) over 0x0FFF > ;
: 3insert# ( imm src dest -- imm # src dest ) 2>r #, 2r> ;
: 3insert! ( imm src dest -- imm # src dest ) 2>r !, 2r> ;
: addi16, ( imm id src dest -- ) 2>r at0 set, at0 2r> add, ;
: subi16, ( imm id src dest -- ) 2>r at0 set, at0 2r> sub, ;
: lshifti16, ( imm id src dest -- ) 2>r at0 set, at0 2r> lshift, ;
: rshifti16, ( imm id src dest -- ) 2>r at0 set, at0 2r> rshift, ;
: addi12, ( imm src dest -- ) #addi emit-2reg-imm12 ;
: subi12, ( imm src dest -- ) #subi emit-2reg-imm12 ;
: rshifti4, ( imm src dest -- ) 2>r 0xF and 2r> #rshifti emit-2reg-imm12 ;
: rshifti12, ( imm src dest -- ) 
  2>r 
  dup 0xF > 
  if \ we are going to zero stuff out
    drop 2r> zero-instead,
  else
    2r> rshifti4, 
  endif ;
: rshifti, ( imm id src dest -- ) 2>r dup 0= if drop 2r> rshifti12, else 2r> rshifti16, endif ;
: #rshifti, ( imm src dest -- ) 2>r #, 2>r rshifti, ;
: !rshifti, ( imm src dest -- ) 2>r !, 2>r rshifti, ;

: lshifti4, ( imm src dest -- ) 2>r 0xF and 2r> #lshifti emit-2reg-imm12 ;
: lshifti12, ( imm src dest -- ) 
  2>r 
  dup 0xF > 
  if \ we are going to zero stuff out
    drop 2r> zero-instead,
  else
    2r> lshifti4, 
  endif ;

: lshifti, ( imm id src dest -- ) 2>r dup 0= if drop 2r> lshifti12, else 2r> lshifti16, endif ;
: #lshifti, ( imm src dest -- ) 2>r #, 2>r lshifti, ;
: !lshifti, ( imm src dest -- ) 2>r !, 2>r lshifti, ;

: #muli, ( imm src dest -- )
  rot dup 
  case 
    0 of drop zero-instead, endof 
    1 of drop -> endof
    2 of drop 1 -rot lshifti4, endof \ << 1
    4 of drop 2 -rot lshifti4, endof \ << 2
    8 of drop 3 -rot lshifti4, endof \ << 3
    16 of drop 4 -rot lshifti4, endof \ << 4
    32 of drop 5 -rot lshifti4, endof \ << 5
    64 of drop 6 -rot lshifti4, endof \ << 6
    128 of drop 7 -rot lshifti4, endof \ << 7
    256 of drop 8 -rot lshifti4, endof \ << 8
    512 of drop 9 -rot lshifti4, endof \ << 9
    1024 of drop 10 -rot lshifti4, endof \ << 10
    2048 of drop 11 -rot lshifti4, endof \ << 11
    4096 of drop 12 -rot lshifti4, endof \ << 12
    8192 of drop 13 -rot lshifti4, endof \ << 13
    16384 of drop 14 -rot lshifti4, endof \ << 14
    32768 of drop 15 -rot lshifti4, endof \ << 15
    -rot 3insert# muli, 
    endcase ;


: addi, ( imm id src dest -- ) 
  2>r dup 0= 
      if \ check and make sure to only do this if we encounter 12-bit number
         ?not-imm12
         if 
           2r> addi16, 
         else 
           drop dup ( imm ) 
           case 
              0 of drop 2r> -> endof
              1 of drop 2r> incr, endof
              2r> addi12,
           endcase
         endif
      else 
        2r> addi16,
      endif ;
: #addi, ( imm src dest -- ) 3insert# addi, ;
: !addi, ( imm src dest -- ) 3insert! addi, ;

: subi, ( imm id src dest -- ) 
  2>r dup 0= 
      if \ check and make sure to only do this if we encounter 12-bit number
         ?not-imm12
         if 
           2r> subi16,
         else 
           drop dup ( imm ) 
           case 
              0 of drop 2r> -> endof
              1 of drop 2r> decr, endof
              2r> subi12,
           endcase
         endif
      else 2r> subi16,
      endif ;
: #subi, ( imm src dest -- ) 3insert# subi, ;
: !subi, ( imm src dest -- ) 3insert! subi, ;

{ioaddr
ioaddr: /dev/null 
ioaddr: /dev/console0
ioaddr: /dev/console1
ioaddr: /dev/core-dump
ioaddr: /dev/core-load
ioaddr: /dev/dump-vm
ioaddr: /dev/terminate-vm
ioaddr: /dev/register
ioaddr}
: $->io ( imm id -- ) io $-> ;
: #->io ( imm -- ) #, $->io ;
: !->io ( imm -- ) !, $->io ;

: ret, ( -- ) lr br, ;
: io-write ( src -- ) io stio, ;
: io-read ( dest -- ) io swap ldio, ;
: inspect-register ( reg -- )
  /dev/register #, at0 set,
  #, at1 set,
  at1 at0 stio, ;

\ core routines
: call, ( dest -- ) lr bl, ;
: callr, ( reg -- ) lr swap brl, ;
: @-> ( a b -- ) 
  \ the contents of the memory location word whose address is in register A
  \ are loaded into register B (a 16-bit indirect fetch from A to B )
  ld, ;

: pop-> ( s a -- )
  \ the S push down stack top entry is loaded to register A and the stack pointer
  \ is adjusted
  pop, ;

: psh-> ( a s -- )
\ the A register contents are loaded to the S pushdown stack and the stack 
\ pointer is adjusted
push, ;

: jmp ( addr id -- ) b, ;
: #jmp ( addr -- ) #, jmp ;
: !jmp ( addr -- ) !, jmp ;
: ldi, ( imm id dest -- ) >r $->at0 at0 r> ld, ;
: #ldi, ( imm dest -- ) ?imm0 if ld, else #, swap ldi, endif ;
: !ldi, ( imm dest -- ) !, swap ldi, ;

: sti, ( imm id addr -- ) >r $->at0 at0 r> st, ;
: #sti, ( imm addr -- ) ?imm0 if st, else #, swap sti, endif ;
: !sti, ( imm addr -- ) !, swap sti, ;

: pushi, ( imm id sp -- ) >r $->at0 at0 r> push, ;
: #pushi, ( imm sp -- ) ?imm0 if push, else #, swap pushi, endif ;
: !pushi, ( imm sp -- ) !, swap pushi, ;
: swap, ( src dest -- ) 
  2dup = if 
  \ ignore the operation if they are equal and don't dump anything out
    2drop 
  else
    dup ( src dest dest )
    at0 -> ( stash dest in at0 )
    over swap ( r0 r0 r1 )
    -> 
    at0 swap ( at0 r0 )
    -> 
  endif ;

: !def3i ( "name" "op" -- ) create ' , does> ( imm src dest addr -- ) 2>r !, swap 2r> @ execute ;

!def3i !muli, muli,


: #divi, ( imm src dest -- ) >r #, swap r> divi, ;
: #remi, ( imm src dest -- ) >r #, swap r> remi, ;

!def3i !divi, divi,
!def3i !remi, remi, 

: andi, ( imm id src dest -- ) 2>r $->at0 at0 2r> and, ;
: ori, ( imm id src dest -- ) 2>r $->at0 at0 2r> or, ;
: xori, ( imm id src dest -- ) 2>r $->at0 at0 2r> xor, ;
: nandi, ( imm id src dest -- ) 2>r $->at0 at0 2r> nand, ;
: nori, ( imm id src dest -- ) 2>r $->at0 at0 2r> nor, ;

: eqz, ( value dest -- ) zero -rot eq, ;
: neqz, ( value dest -- ) zero -rot neq, ;
: gtz, ( value dest -- ) zero -rot gt, ;
: ltz, ( value dest -- ) zero -rot lt, ;
: gez, ( value dest -- ) zero -rot ge, ;
: lez, ( value dest -- ) zero -rot le, ;

!def3i !andi, andi, 
!def3i !ori, ori, 
!def3i !xori, xori, 
!def3i !nandi, nandi, 
!def3i !nori, nori, 
: 1+, ( reg -- ) dup incr, ;
: 1-, ( reg -- ) dup decr, ;
: 2+, ( reg -- ) 2 swap dup #addi, ;
: 2-, ( reg -- ) 2 swap dup #subi, ;
: 2*, ( dest -- ) dup dup add, ; \ just add the register with itself
: 2/, ( dest -- ) 1 swap dup #rshifti, ;
: 4*, ( dest -- ) 2 swap dup #lshifti, ;
: 4/, ( dest -- ) 2 swap dup #rshifti, ;
: next-address ( -- imm id ) loc@ 1+ #, ;
: !.data16 ( imm -- ) !, .data16 ;
: #.data16 ( imm -- ) #, .data16 ;
: save-register ( reg -- ) vmsp psh-> ;
: restore-register ( reg -- ) vmsp swap pop-> ;
: save-lr ( -- ) lr save-register ;
: restore-lr ( -- ) lr restore-register ;
: 1save-loc ( -- ) loc0 save-register ;
: 2save-loc ( -- ) 1save-loc loc1 save-register ;
: 3save-loc ( -- ) 2save-loc loc2 save-register ;
: 4save-loc ( -- ) 3save-loc loc3 save-register ;
: 5save-loc ( -- ) 4save-loc loc4 save-register ;
: 6save-loc ( -- ) 5save-loc loc5 save-register ;
: 7save-loc ( -- ) 6save-loc loc6 save-register ;
: 1restore-loc ( -- ) loc0 restore-register ;
: 2restore-loc ( -- ) loc1 restore-register 1restore-loc ;
: 3restore-loc ( -- ) loc2 restore-register 2restore-loc ;
: 4restore-loc ( -- ) loc3 restore-register 3restore-loc ;
: 5restore-loc ( -- ) loc4 restore-register 4restore-loc ;
: 6restore-loc ( -- ) loc5 restore-register 5restore-loc ;
: 7restore-loc ( -- ) loc6 restore-register 6restore-loc ;
: save-locals ( count -- )
  case 
  1 of 1save-loc endof
  2 of 2save-loc endof
  3 of 3save-loc endof
  4 of 4save-loc endof
  5 of 5save-loc endof
  6 of 6save-loc endof
  7 of 7save-loc endof
  endcase ;
: restore-locals ( count -- ) 
  case 
  1 of 1restore-loc endof
  2 of 2restore-loc endof
  3 of 3restore-loc endof
  4 of 4restore-loc endof
  5 of 5restore-loc endof
  6 of 6restore-loc endof
  7 of 7restore-loc endof
  endcase ;
: (leafn ( label -- ) .label ;
: leafn) ( -- ) ret, ;
: (fn ( label -- ) .label save-lr ;
: fn) ( -- ) restore-lr ret, ;
: mask-lower-half, ( src dest -- ) 2>r 0x00FF #, 2r> andi, ;
: mask-upper-half, ( src dest -- ) 2>r 0xFF00 #, 2r> andi, ;

#ldtincr inst-2reg ldtincr,
: lti16, ( imm id src dest -- ) 2>r at0 set, at0 2r> lt, ;
: lti12, ( imm src dest -- ) #lti emit-2reg-imm12 ;
: lti, ( imm id src dest -- ) 
  2>r dup 0= 
      if \ check and make sure to only do this if we encounter 12-bit number
         ?not-imm12
         if 
           2r> lti16,  
         else 
           drop dup ( imm ) 
           0= if drop 2r> ltz, else 2r> lti12, endif 
         endif
      else 
        2r> lti16, 
      endif ;
: bclti, ( dest id imm id src dest -- ) dup >r lti, r> bc, ;
: bcgti, ( dest id imm id src dest -- ) dup >r gti, r> bc, ;
: bceq, ( dest id src2 src dest -- ) dup >r eq, r> bc, ;
: bcneq, ( dest id src2 src dest -- ) dup >r neq, r> bc, ;
: bcneqi, ( dest id imm id src dest -- ) dup >r neqi, r> bc, ;
: bcgt, ( imm id src2 src dest -- ) dup >r gt, r> bc, ;
: bceqz, ( imm id src dest -- ) dup >r eqz, r> bc, ;
: bclt, ( imm id src2 src dest -- ) dup >r lt, r> bc, ;
: bcltz, ( imm id src dest -- ) dup >r ltz, r> bc, ;
