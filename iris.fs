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
0x0F addr-mask addr4
0xFF addr-mask addr8
0xFFFF addr-mask addr16
0xFFFFFFFF addr-mask addr32
variable labelIndex
variable mloc \ current memory location
: 2+ ( n -- 2+n) 2 + ;
: loc@ ( -- n ) mloc @ ;
: loc! ( n -- ) addr16 mloc ! ; \ make sure that it doesn't go out of bounds
: loc+ ( value -- ) loc@ + loc! ;
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
  r> ( r2 r1 ) ;

: cconstant ( byte "name" -- ) create c, does> c@ ;
: ?debug ( -- f ) iris-debug @ ;

: linker-entry ( value address kind -- n ) 
	addr8 ?debug if dup ." kind: " hex . endif swap
	addr16 ?debug if dup ." address: " hex . endif 0x10 lshift or swap 
	addr32 ?debug if dup ." value: " hex . endif 0x20 lshift or 
	?debug if dup ." result: " hex . cr endif ;
: def-linker-action ( value "name" -- )
  create c, 
  does> ( value addr -- n )
  c@ linker-entry ;
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
0xFF00 constant io-start
: {ioaddr ( -- 0 ) io-start ;
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
: reset-labels ( -- ) 0 labelIndex ! ;
: print-latest ( -- ) latest name>string type ;
: print-new-label ( -- ) print-latest ." : " loc@ hex . ." , index#: " labelindex @ . cr ;
: deflabel ( "name" -- ) 
create 
labelIndex @ ?debug if dup print-new-label endif addr32 , labelIndex @ 1+ labelIndex !  does> @ ;

: def2label ( "name" "name2" -- ) deflabel deflabel ;
: def3label ( "name" "name2" "name3" -- ) def2label deflabel ;

: <<linker ( entry id -- ) hex dup >r nout s" " r> write-line throw decimal ;
: <<byte ( value -- ) addr8 loc@ action:write-byte curasm@ <<linker loc1+ ;
: <<word ( value -- ) addr16 loc@ action:write-word curasm@ <<linker loc2+ ;
: <<iword ( value -- ) loc@ action:write-indirect-word curasm@ <<linker loc2+ ;
: <<ientry ( value -- ) loc@ action:indirect-entry curasm@ <<linker ;
\ labels must be defined ahead of time before first reference
: .org ( n -- ) loc! ;
: .label ( label -- ) <<ientry ;
: execute-latest ( -- * ) latest name>int execute ;
: deflabel-here ( "name" -- ) deflabel execute-latest .label ;
\ constant tagging version
0 constant #, \ #, is a constant version
1 constant ??, \ ??, is an indirect instruction
: <<?word ( imm type -- ) #, = if <<word else <<iword then ; 
: .data16 ( imm id -- ) <<?word ;
: .data8 ( imm -- ) <<byte ;
: {asm ( path -- ) w/o create-file throw curasm! reset-labels ;
: asm} ( -- ) curasm@ close-file throw clearasm ;

{registers
register:  x0 register:  x1 register:  x2 register:  x3 
register:  x4 register:  x5 register:  x6 register:  x7 
register:  x8 register:  x9 register: x10 register: x11
register: x12 register: x13 register: x14 register: x15
registers}
decimal 16 constant num-registers 
: too-many-registers-defined ( addr -- ) num-registers >= ABORT" To many registers used!" ;
: 1+cconstant ( n "name" -- ) dup cconstant 1+ ;
x0 1+cconstant zero
1+cconstant cv
1+cconstant at0
1+cconstant io
1+cconstant unused-start
too-many-registers-defined
: <<opcode ( value -- ) <<byte ;
: <<@opcode ( addr -- ) @ <<opcode ;
: inst-0reg ( opcode-index "name" -- )
  create c, \ embed opcode
  does> <<@opcode ;
: inst-1reg ( opcode-index "name" -- )
  create c, \ embed opcode
  does> ( dest addr -- )
	<<@opcode
	1reg <<byte ;
: inst-2reg ( opcode-index "name" -- )
  create c, \ embed opcode
  does> ( src dest -- )
	<<@opcode
	2reg <<byte ;
: inst-3reg ( opcode-index "name" -- )
  create c, \ embed opcode
  does> ( src2 src dest addr -- )
  	<<@opcode
	3reg ( r2 r1 )
	<<byte ( r2 )
	<<byte ;
: inst-4reg ( opcode-index "name" -- )
  create c, \ embed opcode
  does> ( src3 src2 src dest addr -- )
		<<@opcode
		4reg ( r2 r1 )
		<<byte
		<<byte ;
: inst-2reg-with-imm16 ( opcode-index "name" -- )
	create c, \ embed opcode
	does> ( imm id src dest -- )
	<<@opcode
	2reg <<byte 
	<<?word ;

#add inst-3reg add, 
#move inst-2reg move,
#nop inst-0reg nop,

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
#eqz inst-2reg eqz,
#neqz inst-2reg neqz,
#ltz inst-2reg ltz,
#gtz inst-2reg gtz,
#lez inst-2reg lez,
#gez inst-2reg gez,
: set, ( imm imm-type dest -- )
	swap #, = 
	if 
		\ if we get a zero then do a move instead
		over 0= 
		if 
			nip zero swap move, 
		else
			\ check and see if the value is less than or equal to 0xFF
			over 0xFF <= 
			if 
				\ emit set byte instead of a full set to save space
				#setb <<opcode
				1reg <<byte
				0xFF and <<byte
			else
				\ its not a zero so instead we should do the normal set action
				#set <<opcode \ first emit a set operation, just do it
				1reg <<byte \ emit the destination
				<<word \ emit the word operation
			then
		then
	else
		#set <<opcode \ first emit a set operation, just do it
		1reg <<byte \ emit the destination
		<<iword \ emit the indirect word operation
	then ;
: #set, ( imm dest -- ) #, swap set, ;
: ??set, ( imm dest -- ) ??, swap set, ;
\ only known constants here!
: $->at0 ( imm id -- at0 ) at0 set, at0 ;
: $->at0-3arg ( imm id a b -- at0 a b ) 2>r $->at0 2r> ;
: $->at0-2arg ( imm id b -- at0 b )
  -rot ( b imm id )
  $->at0 ( b )
  swap ( at0 b ) ;
#ld inst-2reg ld,
: ldi, ( imm id dest -- ) >r $->at0 r> ld, ; 
: #ldi, ( imm dest -- ) #, swap ldi, ;
: ??ldi, ( imm dest -- ) ??, swap ldi, ;
#st inst-2reg st,
: sti, ( imm id addr -- ) >r $->at0 r> st, ;
: #sti, ( imm addr -- ) #, swap sti, ;
: ??sti, ( imm addr -- ) ??, swap sti, ;

#push inst-2reg push,

#pop inst-2reg pop,

#br inst-1reg br,
#brl inst-2reg brl,
#bcr inst-2reg bcr,
#bcrl inst-3reg bcrl,

: call, ( imm imm-type dest -- )
	#call <<byte
	1reg <<byte
	<<?word ;
: #call, ( imm dest -- ) #, swap call, ;
: ??call, ( imm dest -- ) ??, swap call, ;
: bc, ( imm imm-type cond -- )
	#condb <<opcode \ conditional
	1reg <<byte     \ conditional
	<<?word ;
: #bc, ( imm dest -- ) #, swap bc, ;
: ??bc, ( imm dest -- ) ??, swap bc, ;
: b, ( imm id -- ) 
  #bi <<opcode
  <<?word ;
: #b, ( imm -- ) #, b, ;
: ??b, ( imm -- ) ??, b, ;

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
{ioaddr
ioaddr: /dev/null 
ioaddr: /dev/console0
ioaddr: /dev/console1
ioaddr: /dev/console2
ioaddr: /dev/terminate-vm
ioaddr: /dev/debug-enable
ioaddr}

: addi, ( imm id src dest -- ) 
  #addi <<opcode
  2reg <<byte
  <<?word ;
: #addi, ( imm src dest -- ) 2>r #, 2r> addi, ;
: ??addi, ( imm src dest -- ) 2>r ??, 2r> addi, ;
: subi, ( imm id src dest -- )
  #subi <<opcode
  2reg <<byte
  <<?word ;
: #subi, ( imm src dest -- ) 2>r #, 2r> subi, ;
: ??subi, ( imm src dest -- ) 2>r ??, 2r> subi, ;

\ core routines


#incr inst-2reg incr,
#decr inst-2reg decr,
#uincr inst-2reg uincr,
#udecr inst-2reg udecr,
: 1+, ( reg -- ) dup incr, ;
: 1-, ( reg -- ) dup decr, ;
: 2+, ( reg -- ) 2 swap dup #addi, ;
: 2-, ( reg -- ) 2 swap dup #subi, ;
	
#ldtincr inst-2reg ldtincr,
#sttincr inst-2reg sttincr,
: .cell ( addr id -- ) .data16 ;
: #cell, ( addr -- ) #, .cell ;
: ??cell, ( addr -- ) ??, .cell ;
: constant, ( addr -- ) #cell, ;

#addw inst-3reg addw,
#subw inst-3reg subw,
#pushw inst-2reg pushw,
#popw inst-2reg popw,
#return inst-1reg ret,
#creturn inst-2reg cret,
#negatew inst-2reg negatew,
: retgez, ( reg sp -- )
  swap cv gez,
  cv swap cret, ;
: retgt, ( src2 src sp -- )
  >r cv gt, 
  cv r> cret, ;
: reteqz, ( reg sp -- )
  swap cv eqz,
  cv swap cret, ;

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
#andi inst-2reg-with-imm16 andi,
#uandi inst-2reg-with-imm16 uandi,
#rshifti inst-2reg-with-imm16 rshifti,
#lshifti inst-2reg-with-imm16 lshifti,
#lti inst-2reg-with-imm16 lti,

: stb, ( value addr -- ) stbl, ;
: ldb, ( addr dest -- ) ldbl, ;
: spdrop, ( sp -- ) zero pop, ;
: beq, ( imm id src2 src -- ) 
  cv eq,
  cv bc, ;
: beqz, ( imm id src -- ) 
  cv eqz,
  cv bc, ;
: bneqz, ( imm id src -- )
  cv neqz,
  cv bc, ;
: bgt, ( imm id src2 src -- )
    cv gt, 
    cv bc, ;
: blt, ( imm id src2 src -- )
    cv lt,
    cv bc, ;
: set-io, ( imm id -- ) io set, ;
: #set-io, ( imm -- ) #, set-io, ;
: ??set-io, ( imm -- ) ??, set-io, ;
: iost, ( reg -- ) io st, ;
: iold, ( reg -- ) io swap ld, ;
: iostb, ( reg -- ) io stb, ;
: ioldb, ( reg -- ) io swap ldb, ;
: putc, ( reg -- ) /dev/console0 #set-io, iostb, ;
: getc, ( reg -- ) /dev/console0 #set-io, ioldb, ;
: terminate, ( -- ) /dev/terminate-vm #set-io, zero iostb, ;
: muli, ( imm id src dest -- ) 
    2>r 
    #, = if 
       dup 
       case 
            0 of drop zero 2r> swap drop move, endof
            1 of drop 2r> move, endof
            2 of drop 1 #, 2r> lshifti, endof
            4 of drop 2 #, 2r> lshifti, endof
            8 of drop 3 #, 2r> lshifti, endof
            16 of drop 4 #, 2r> lshifti, endof
            32 of drop 5 #, 2r> lshifti, endof
            64 of drop 6 #, 2r> lshifti, endof
            128 of drop 7 #, 2r> lshifti, endof
            256 of drop 8 #, 2r> lshifti, endof
            swap 
            2r>
            #muli <<opcode
            2reg <<byte
            <<word
           endcase
    else
       2r>
       #muli <<opcode
       2reg <<byte
       <<iword 
    endif
;
: divi, ( imm id src dest -- )
    2>r 
    #, = if 
       dup 
       case 
            0 of drop zero 2r> swap drop move, endof
            1 of drop 2r> move, endof
            2 of drop 1 #, 2r> rshifti, endof
            4 of drop 2 #, 2r> rshifti, endof
            8 of drop 3 #, 2r> rshifti, endof
            16 of drop 4 #, 2r> rshifti, endof
            32 of drop 5 #, 2r> rshifti, endof
            64 of drop 6 #, 2r> rshifti, endof
            128 of drop 7 #, 2r> rshifti, endof
            256 of drop 8 #, 2r> rshifti, endof
            swap 
            2r>
            #divi <<opcode
            2reg <<byte
            <<word
           endcase
    else
       2r>
       #divi <<opcode
       2reg <<byte
       <<iword 
    endif
    ;
: pushi, ( imm id sp -- ) 
    swap ( imm sp id ) 
    #, = 
    if
        over 0= 
        if 
            swap drop
            zero swap push,
        else
            #pushi <<opcode
            1reg <<byte
            <<word
        endif
    else
        #pushi <<opcode
        1reg <<byte
        <<iword
    endif ;
: #pushi, ( imm sp -- ) #, swap pushi, ;
: ??pushi, ( imm sp -- ) ??, swap pushi, ;

0xFFFF constant ram-end
0x0000 constant ram-start
: ?debug, ( dest -- )
  /dev/debug-enable #set-io,
  ioldb, ;
: enable-debug, ( -- ) 
  /dev/debug-enable #set-io,
  0xFFFF #, at0 set, 
  at0 iostb, ;
: disable-debug, ( -- )
  /dev/debug-enable #set-io,
  zero iostb, ;

