include iris.fs
\ contains all of the registers and pieces used for the monitor itself
s" monitor.o" {asm
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
\ use the upper stack elements as 
: check-overflow ( -- ) loc@ monitor-memory-start < ABORT" routines are too large!" ;

deflabel monitor-loop-start
deflabel FixCaseRoutine 
deflabel PrintCharactersRoutine
deflabel WriteRangeToIOAddressRoutine
deflabel $ECHO 
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
: $NEWLINE ( -- ) 0xA #ECHO ;
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
    loc0 loc1 ldtincr, \ second character
    loc0 loc2 ldtincr, \ third character
    loc0 loc3 ldtincr, \ fourth character
    $->HEX !, call,
    loc1 in0 out0 loc4 move2, 
    $->HEX !, call,
    4 #, loc4 loc4 lshifti,
    out0 loc4 loc4 add,
    loc2 in0 ->
    $->HEX !, call,
    4 #, loc4 loc4 lshifti,
    out0 loc4 loc4 add,
    loc3 in0 ->
    $->HEX !, call,
    4 #, loc4 loc4 lshifti,
    out0 loc4 loc4 add,
    loc4 in0 ->
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
    at0 loc0 at1 usub,
    at1 at0 st,
    at1 out0 ->
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
