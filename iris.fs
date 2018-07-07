get-current ( wid )
vocabulary iris also iris definitions

variable iris-debug 
false iris-debug !
: nout ( num id -- ) swap s>d 
\ get rid of sign extension nonsense
dup 0< if 0 and endif 
<<# #s #> rot write-file #>> throw ;
: addr-mask ( mask "name" -- )
  CREATE , 
  does> @ and ;
0x3f addr-mask addr6
0x0F addr-mask addr4
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
  does> swap addr4 swap @ lshift ;
0 reg-pos lower-reg
4 reg-pos upper-reg 
: 1reg ( dest -- value ) lower-reg ;
: 2reg ( src dest -- value ) 1reg swap upper-reg or ;
: 3reg ( src2 src dest -- r2 r1 ) 2reg swap lower-reg swap ;
: 4reg ( src3 src2 src dest -- r2 r1 ) 
  3reg ( src3 r2 r1 ) 
  >r ( src3 r2 )
  swap ( r2 src3 )
  upper-reg ( r2 value ) 
  or ( r2 )
  r> ( r2 r1 )
  ;
: imm16 ( imm16 -- H L ) dup >r
  8 rshift 0xFF and \ compute the high piece 
  r>
  0xFF and \ compute the low piece 
  ;
: 2reg-imm16 ( imm16 src dest -- h l regs ) 2reg >r imm16 r> ;
: 1reg-imm16 ( imm16 dest -- h l regs ) 1reg >r imm16 r> ;

: cconstant ( byte "name" -- ) create c, does> c@ ;


: linker-entry ( kind address value -- n ) 
  addr32 0x20 lshift ( k a v<<32 )
  swap ( k v3 a )
  addr16 0x10 lshift 
  rot ( v3 a k )
  0xFF and ( v3 a1 k8 )
  or ( v3 n )
  or ( n ) ;
: def-linker-action ( value "name" -- )
  create c, 
  does> ( addr value -- n )
  @ -rot linker-entry ;
0 constant ByteWrite
1 constant WordWrite
2 constant IndirectEntry
3 constant IndirectWordWrite


: {registers ( -- 0 ) 0 ;
: registers} ( n -- ) drop ;
: register: dup cconstant 1+ ;
: {opcode ( -- 0 ) {registers ;
: opcode: dup cconstant 1+ ;
: opcode} ( n -- ) registers} ;
: {ioaddr ( -- 0 ) 0xFF00 ;
: ioaddr} ( n -- ) drop ;
: ioaddr: ( n -- k ) dup constant 1+ ;

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
opcode: #nop
opcode: #setb
opcode}
\ registers
set-current \ go back

ByteWrite def-linker-action action:write-byte
WordWrite def-linker-action action:write-word
IndirectEntry def-linker-action action:indirect-entry
IndirectWordWrite def-linker-action action:write-indirect-word

variable CurrentAssemblyFile

: curasm@ ( -- file ) CurrentAssemblyFile @ ;
: curasm! ( n -- ) CurrentAssemblyFile ! ;
: clearasm ( -- ) 0 curasm! ;

: <<linker ( entry id -- ) 
  iris-debug @ if ." address: " hex loc@ . ." : " over hex . cr decimal then
  hex dup >r nout s" " r> write-line throw decimal ;
: <<byte ( value id -- )
	loc@ swap action:write-byte
	curasm@ <<linker
	loc1+ ;
: <<word ( value id -- )
	loc@ swap action:write-word
	curasm@ <<linker loc2+ ;
: <<ientry ( value id -- )
	loc@ swap action:indirect-entry
	currasm@ <<linker ;
: <<iword ( value id -- )
	loc@ swap action:write-indirect-word
	currasm@ <<linker loc2+ ;
\ labels must be defined ahead of time before first reference
: reset-labels ( -- ) 0 labelIndex ! ;
: print-latest ( -- ) latest name>string type ;
: print-new-label ( -- ) print-latest ." : " loc@ hex . ." , index#: " labelindex @ . cr ;
: deflabel ( "name" -- ) create 
labelIndex @ addr32 , labelIndex @ 1+ labelIndex ! 
iris-debug @ if print-new-label then
does> @ ;

: .org ( n -- ) loc! ;
: .label ( label -- ) iris-debug @ if dup >r then curasm@ <<label iris-debug @ if r> ." emit label: " . cr then ;
: execute-latest ( -- * ) latest name>int execute ;
: deflabel-here ( "name" -- ) deflabel execute-latest .label ;
: .data16 ( imm id -- ) swap addr16 swap curasm@ swap 0= if <<mem else <<imem endif ;
: {asm ( path -- ) 
  w/o create-file throw curasm! 
  reset-labels ;
: asm} ( -- ) curasm@ close-file throw clearasm ;

{registers
register: x0
register: x1
register: x2
register: x3
register: x4
register: x5
register: x6
register: x7
register: x8
register: x9
register: x10
register: x11
register: x12
register: x13
register: x14
register: x15
registers}
16 constant num-registers 
: too-many-registers-defined ( addr -- ) num-registers >= ABORT" To many registers used!" ;
: 1+cconstant ( n "name" -- ) dup cconstant 1+ ;
x0 1+cconstant zero
1+cconstant cv
1+cconstant at0
1+cconstant io
1+cconstant unused-start
too-many-registers-defined
: emit-opcode ( addr -- ) @ <<byte ;
: inst-0reg ( opcode-index "name" -- )
  create c, \ embed opcode
  does> emit-opcode ;
: inst-1reg ( opcode-index "name" -- )
  create c, \ embed opcode
  does> ( dest addr -- )
	emit-opcode
	1reg <<byte ;
: inst-2reg ( opcode-index "name" -- )
  create c, \ embed opcode
  does> ( src dest -- )
	emit-opcode
	2reg <<byte ;
: inst-3reg ( opcode-index "name" -- )
  create c, \ embed opcode
  does> ( src2 src dest addr -- )
  	emit-opcode
	3reg ( r2 r1 )
	<<byte ( r2 )
	<<byte ;
: inst-4reg ( opcode-index "name" -- )
  create c, \ embed opcode
  does> ( src3 src2 src dest addr -- )
		emit-opcode
		4reg ( r2 r1 )
		<<byte
		<<byte ;
       

#add inst-3reg add, 
#move inst-2reg move,
#nop inst-0reg nop,
: -> ( src dest -- ) move, ;
\ constant tagging version
\ #, is a constant version
: #, ( imm -- imm16 0 ) addr16 0 ; 
\ ??, is an indirect instruction
: ??, ( -- 1 ) 1 ;

#sub inst-3reg sub, 
#mul inst-3reg mul, 
#div inst-3reg div, 
#rem inst-3reg rem, 
#rshift inst-3reg rshift,
#lshift inst-3reg lshift, 
#and inst-3reg and, 
#or inst-3reg or, 
#negate inst-2reg negate, 
#xor inst-3reg xor, 
\ #nand inst-3reg nand, 
\ #nor inst-3reg nor,
#min inst-3reg min,
#max inst-3reg max,
#eq inst-3reg eq,
#neq inst-3reg neq,
#lt inst-3reg lt,
#gt inst-3reg gt,
#le inst-3reg le,
#ge inst-3reg ge,
: ?choose-instruction-kind ( v id -- ) 
0= 
if 
( c ) 
	<<inst 
	else 
	<<iinst 
	endif 
	;
: set, ( imm imm-type dest -- )
	swap #, = 
	if 
		\ if we get a zero then do a move instead
		over 0= if 
				nip zero swap move, 
			else
				\ check and see if the value is less than or equal to 0xFF
				over 0xFF <= if 
					\ emit set byte instead of a full set to save space
					#setb <<byte
					1reg <<byte
					0xFF and <<byte
				else
					\ its not a zero so instead we should do the normal set action
					#set <<byte \ first emit a set operation, just do it
					1reg <<byte \ emit the destination
					<<word \ emit the word operation
				then
			then
	else
		#set <<byte \ first emit a set operation, just do it
		1reg <<byte \ emit the destination
		<<iword \ emit the indirect word operation
	then ;
: #set, ( imm dest -- ) #, swap set, ;
: ??set, ( imm dest -- ) ??, swap set, ;
\ only known constants here!
: ?imm0 ( imm v -- imm v f ) over 0= ;
: $->at0 ( imm id -- n ) at0 set, ;
: $->at0-3arg ( imm id a b -- at0 a b ) 2>r $->at0 at0 2r> ;
: $->at0-2arg ( imm id b -- at0 b )
  -rot ( b imm id )
  $->at0 ( b )
  at0 swap ( at0 b ) ;
#ld inst-2reg ld,
#st inst-2reg st,
#push inst-2reg push,
#pop inst-2reg pop,
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
#unegate inst-2reg unegate,
#uxor inst-3reg uxor,
#umin inst-3reg umin,
#umax inst-3reg umax,
#uadd inst-3reg uadd,
#usub inst-3reg usub,
#umul inst-3reg umul,
#udiv inst-3reg udiv,
#urem inst-3reg urem,
#ulshift inst-3reg ulshift,
#urshift inst-3reg urshift, 
#incr inst-2reg incr,
#decr inst-2reg decr,
#uincr inst-2reg uincr,
#udecr inst-2reg udecr,
: <<?word ( imm type -- ) #, = if <<word else <<iword then ; 
: call, ( imm imm-type dest -- )
	#call <<byte
	1reg <<byte
	<<?word ;
: #call, ( imm dest -- ) #, swap call, ;
: ??call, ( imm dest -- ) ??, swap call, ;
: bc, ( imm imm-type cond -- )
	#condb <<byte \ conditional
	1reg <<byte \ conditional
	<<?word ;
: #bc, ( imm dest -- ) #, swap bc, ;
: ??bc, ( imm dest -- ) ??, swap bc, ;
: b, ( imm id -- ) zero bl, ;
: #b, ( imm -- ) #, b, ;
: ??b, ( imm -- ) ??, b, ;
: bcl, ( imm id link cond -- ) 2>r $->at0 2r> at0 bcrl, ;
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

def3argi muli, mul,
def3argi divi, div,
def3argi remi, rem,
def2argi negatei, negate,
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
def2argi unegatei, unegate,
def3argi ueqi, ueq,
def3argi uneqi, uneq, 
def3argi ulti, ult,
def3argi ugti, ugt,
def3argi ulei, ule,
def3argi ugei, uge,



{ioaddr
ioaddr: /dev/null 
ioaddr: /dev/console0
ioaddr: /dev/console1
ioaddr: /dev/console2
ioaddr: /dev/core-dump
ioaddr: /dev/core-load
ioaddr: /dev/dump-vm
ioaddr: /dev/terminate-vm
ioaddr: /dev/register
ioaddr: /dev/hexprint
ioaddr: /dev/decprint
ioaddr: /dev/octprint
ioaddr}


\ core routines

: ldi, ( imm id dest -- ) >r $->at0 at0 r> ld, ;
: #ldi, ( imm dest -- ) ?imm0 if ld, else #, swap ldi, endif ;
: ??ldi, ( imm dest -- ) ??, swap ldi, ;

: sti, ( imm id addr -- ) >r $->at0 at0 r> st, ;
: #sti, ( imm addr -- ) ?imm0 if st, else #, swap sti, endif ;
: ??sti, ( imm addr -- ) ??, swap sti, ;

: pushi, ( imm id sp -- ) >r $->at0 at0 r> push, ;
: #pushi, ( imm sp -- ) ?imm0 if push, else #, swap pushi, endif ;
: ??pushi, ( imm sp -- ) ??, swap pushi, ;

: ??def3i ( "name" "op" -- ) create ' , does> ( imm src dest addr -- ) 2>r ??, swap 2r> @ execute ;

??def3i ??muli, muli,

: #divi, ( imm src dest -- ) >r #, swap r> divi, ;
: #remi, ( imm src dest -- ) >r #, swap r> remi, ;

??def3i ??divi, divi,
??def3i ??remi, remi, 

: andi, ( imm id src dest -- ) 2>r $->at0 at0 2r> and, ;
: ori, ( imm id src dest -- ) 2>r $->at0 at0 2r> or, ;
: xori, ( imm id src dest -- ) 2>r $->at0 at0 2r> xor, ;
\ : nandi, ( imm id src dest -- ) 2>r $->at0 at0 2r> nand, ;
\ : nori, ( imm id src dest -- ) 2>r $->at0 at0 2r> nor, ;

: eqz, ( value dest -- ) zero -rot eq, ;
: neqz, ( value dest -- ) zero -rot neq, ;
: gtz, ( value dest -- ) zero -rot gt, ;
: ltz, ( value dest -- ) zero -rot lt, ;
: gez, ( value dest -- ) zero -rot ge, ;
: lez, ( value dest -- ) zero -rot le, ;

??def3i ??andi, andi, 
??def3i ??ori, ori, 
??def3i ??xori, xori, 
\ ??def3i ??nandi, nandi, 
\ ??def3i ??nori, nori, 
: 1+, ( reg -- ) dup incr, ;
: 1-, ( reg -- ) dup decr, ;
: 2+, ( reg -- ) 2 swap dup #addi, ;
: 2-, ( reg -- ) 2 swap dup #subi, ;
: 2*, ( dest -- ) dup dup add, ; \ just add the register with itself
: 2/, ( dest -- ) 1 swap dup #rshifti, ;
: 4*, ( dest -- ) 2 swap dup #lshifti, ;
: 4/, ( dest -- ) 2 swap dup #rshifti, ;
: next-address ( -- imm id ) loc@ 1+ #, ;
: ??.data16 ( imm -- ) ??, .data16 ;
: #.data16 ( imm -- ) #, .data16 ;
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
#sttincr inst-2reg sttincr,
: .cell ( addr id -- ) .data16 ;
#addw inst-3reg addw,
#subw inst-3reg subw,
#pushw inst-2reg pushw,
#popw inst-2reg popw,
#return inst-1reg ret,
#creturn inst-2reg cret,
: reteqz, ( reg sp -- ) 
  swap cv eqz, 
  cv swap cret, ;
#negatew inst-2reg negatew,
: retgez, ( reg sp -- )
  swap cv gez,
  cv swap cret, ;
: retgt, ( src2 src sp -- )
  >r cv gt, 
  cv r> cret, ;
#umsmod inst-3reg um/mod,
#msmod inst-3reg m/mod,
#umstar inst-3reg um*,
#mstar inst-3reg m*,
#stw inst-2reg stw,
#ldw inst-2reg ldw,
#stbl inst-2reg stbl,
#stbu inst-2reg stbu,
#ldbl inst-2reg ldbl,
#ldbu inst-2reg ldbu,
