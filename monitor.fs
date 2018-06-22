include iris.fs
\ contains all of the registers and pieces used for the monitor itself
s" monitor.o" {asm
: save-register ( reg -- ) vmsp psh-> ;
: restore-register ( reg -- ) vmsp swap pop-> ;
: save-lr ( -- ) lr save-register ;
: restore-lr ( -- ) lr restore-register ;
: (leafn ( label -- ) .label ;
: leafn) ( -- ) ret, ;
: (fn ( label -- ) .label save-lr ;
: fn) ( -- ) restore-lr ret, ;
0xFFFF constant monitor-memory-end
0xFF00 constant monitor-program-end
0xF500 constant monitor-program-start
0xF400 constant monitor-variables-end
0xF300 constant monitor-variables-start
0xF300 constant monitor-input-end
0xF200 constant monitor-input-start
0xF100 constant monitor-stack-start 
0xF000 constant monitor-stack-end \ 512 elements
0xF000 constant monitor-memory-start
unused-start 1+cconstant cmd \ command buffer register
1+cconstant num-base
1+cconstant terminator
1+cconstant separator
1+cconstant error-code
1+cconstant vmsp
1+cconstant in0
1+cconstant out0
1+cconstant in1
1+cconstant out1
1+cconstant loc0
1+cconstant loc1
1+cconstant loc2
1+cconstant loc3
1+cconstant loc4
1+cconstant loc5
1+cconstant loc6
1+cconstant loc7
drop
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
\ use the upper stack elements as 
: check-overflow ( -- ) loc@ monitor-memory-start < ABORT" routines are too large!" ;

deflabel monitor-loop-start
deflabel FixCaseRoutine 
deflabel PrintCharactersRoutine
deflabel WriteRangeToIOAddressRoutine
deflabel $ECHO 
deflabel $NEWLINERoutine
deflabel $->HEX
deflabel printline
deflabel readline 
deflabel SwitchCore
deflabel DumpCore
deflabel LoadCore
deflabel SwapCore
deflabel IOWrite
deflabel PRINT-NUMBER 
deflabel DontTerminateExecution
deflabel $HEX->KEY 
: $KEY ( -- ) 
  /dev/console0 #->io
  out0 io-read ;
: $TERMINATE ( -- )
   zero in0 ->
  /dev/terminate-vm #->io
  IOWrite !jmp ;
: #ECHO ( a -- )
  #, in0 set,
  $ECHO !, call, ;
: $SPACE ( -- ) 0x20 #ECHO ;
: $PROMPT ( -- ) 0x2D #ECHO $SPACE ;
: $NEWLINE ( -- ) $NEWLINERoutine !, call, ;
0x0000 .org monitor-program-start #, jmp

monitor-program-start .org
    0xA #, terminator set,
    monitor-stack-start #, vmsp set,
    0x10 #, num-base set,
monitor-loop-start .label
	\ print out the set of registers before accepting input
	deflabel DISPLAY_REGISTERS_LOOP
	deflabel DISPLAY-REGISTER8-LOOP
	$NEWLINE
	$NEWLINE
	zero loc1 ->
	DISPLAY_REGISTERS_LOOP .label
	0x8 #, loc1 loc2 addi,
	DISPLAY-REGISTER8-LOOP .label
	/dev/register #->io
	loc1 io-write
	$SPACE
	loc1 1+, 
	DISPLAY-REGISTER8-LOOP !, loc2 loc1 cv bclt, 
	$NEWLINE
	DISPLAY_REGISTERS_LOOP !, 0x40 #, loc1 cv bclti,
	$NEWLINE
	\ print the prompt out
	$PROMPT
    readline !, call,
	\ if we fail the check then see if the front of 
	\ input is M for terMinate
	\ precompute the offset
    monitor-input-start 1+ #, loc0 set,
    loc0 loc0 ld,
	DontTerminateExecution !, 0x4D #, loc0 cv bcneqi,
	\ terminate execution if we get in here
	$TERMINATE
	DontTerminateExecution .label
	\ end TERMINATE
    out0 loc0 ->
    monitor-loop-start !, 5 #, loc0 cv bclti, 
    monitor-input-start 1+ #, loc0 set,
\ $HEX
    loc0 in0 ldtincr, \ first character
    $->HEX !, call,
    out0 loc1 ->
    loc0 in0 ldtincr, \ second character
    $->HEX !, call,
    4 #, loc1 loc1 lshifti,
    out0 loc1 loc1 add,
    loc0 in0 ldtincr, \ third character
    $->HEX !, call,
    4 #, loc1 loc1 lshifti,
    out0 loc1 loc1 add,
    loc0 in0 ldtincr, \ fourth character
    $->HEX !, call,
    4 #, loc1 loc1 lshifti,
    out0 loc1 in0 add,
    PRINT-NUMBER !, call,
	$NEWLINE
    monitor-input-start #, in0 set,
	in0 in1 ldtincr,
    printline !, call,
    monitor-loop-start !, b,
\ this must always be first!
IOWrite (leafn in0 io-write leafn)
WriteRangeToIOAddressRoutine (leafn
	\ in0 - starting point in memory
	\ in1 - length
	deflabel WriteRangeToIOAddress_Done
	deflabel WriteRangeToIOAddress_Loop
    2 save-locals
	zero loc0 ->
	WriteRangeToIOAddress_Done !, in1 cv bceqz,
	WriteRangeToIOAddress_Loop .label
	in0 loc0 loc1 uadd, \ uadd bro!?
	loc1 loc1 ld,
	loc1 io-write
	loc0 1+,
	WriteRangeToIOAddress_Loop !, loc0 in1 cv bcgt,
	WriteRangeToIOAddress_Done .label
    2 restore-locals
    leafn)
$->HEX (fn
    deflabel $->HEX_Done
    deflabel $->HEX_IsDigit
    \ in0 - value to hexify
    0x30 #, in0 in0 subi,
    $->HEX_Done !, in0 cv bcltz,
    $->HEX_IsDigit !, 0x0A #, in0 cv bclti, 
    $->HEX_Done !, 0x0A #, in0 cv bclti, 
    0x7 #, in0 in0 subi,
    $->HEX_IsDigit .label
    $->HEX_Done .label
    fn)

    \ reads the next four characters in as hexadecimal characters and converts them to hexidecimal numbers
        
$NEWLINERoutine .label
      0xA #, in0 set,
$ECHO (leafn
      \ in0, the character to write
      /dev/console0 #->io
      in0 io-write
      leafn)

$HEX->KEY (leafn
    deflabel $HEX->KEY_DONE
    \ go backwards from what we originally got in
    \ in0 - contains lowest 4 bits to convert
    0x000F #, in0 in0 andi,
    0xA #, in0 cv lti,
    0x30 #, in0 in0 addi, \ always want to do this
    $HEX->KEY_DONE !, cv bc,
    0x7 #, in0 in0 addi,
    $HEX->KEY_DONE .label
    leafn)
: print-number-$hex->key ( index -- ) 
  #, loc0 in0 rshifti, 
  $HEX->KEY !, call,
  $ECHO !, call, ;

PRINT-NUMBER (fn
    deflabel print-number-done
    \ in0 - number to print
    1 save-locals
    in0 loc0 ->
	$NEWLINE
    0xC print-number-$hex->key
    8 print-number-$hex->key
    4 print-number-$hex->key
    loc0 in0 ->
    $HEX->KEY !, call,
    $ECHO !, call,
    print-number-done .label
    1 restore-locals
    fn)
readline (fn
deflabel readline_loop
deflabel readline_done
deflabel readline_consume_rest_of_line 
deflabel FixCaseRoutineDone
    1 save-locals 
    monitor-input-start 1+ #, loc0 set,
readline_loop .label 
	$KEY \ get the key
    FixCaseRoutineDone !, 0x61 #, out0 cv bclti,
    FixCaseRoutineDone !, 0x7a #, out0 cv bcgti, 
    0x20 #, out0 out0 subi, 
FixCaseRoutineDone .label
    readline_done !, out0 terminator cv bceq,
    out0 loc0 sttincr,
    readline_loop !, monitor-input-end #, loc0 cv bclti,
    readline_done !, out0 terminator cv bceq,
readline_consume_rest_of_line .label
	$KEY \ get the key
    readline_consume_rest_of_line !, out0 terminator cv bcneq,
readline_done .label 
    \ save the length in memory
    monitor-input-start #, at0 set,
    at0 loc0 out0 usub,
    out0 at0 st,
    \ terminator is used to terminate early
    1 restore-locals
    fn)

printline (fn
    \ in0 - start address
    \ in1 - length
    /dev/console0 #->io
    WriteRangeToIOAddressRoutine !, call,
	$NEWLINE
fn)
cr 
." routines stop at " loc@ hex . cr
." words free in routine area: " hex monitor-program-end loc@ - hex . cr


check-overflow
asm}
bye
