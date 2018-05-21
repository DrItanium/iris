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
deflabel $NEWLINE
deflabel $SPACE
deflabel printline
deflabel printbuf
deflabel printinput
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
  arg0 io-read ;
: $TERMINATE ( -- )
   zero arg0 ->
  /dev/terminate-vm #->io
  IOWrite !jmp ;
: #ECHO ( a -- )
  #, arg0 set,
  $ECHO !, call, ;
: $SPACE ( -- ) 0x20 #ECHO ;
: $PROMPT ( -- ) 0x2D #ECHO $SPACE ;
0x0000 .org monitor-program-start #, jmp

monitor-program-start .org
    0xA #, terminator set,
    monitor-stack-start #, vmsp set,
    0x10 #, num-base set,
monitor-loop-start .label
	\ print out the set of registers before accepting input
	deflabel DISPLAY_REGISTERS_LOOP
	deflabel DISPLAY-REGISTER8-LOOP
	$NEWLINE !, loc0 set,
	loc0 callr,
	loc0 callr,
	zero loc1 ->
	DISPLAY_REGISTERS_LOOP .label
	0x8 #, loc1 loc2 addi,
	DISPLAY-REGISTER8-LOOP .label
	/dev/register #->io
	loc1 io-write
	$SPACE
	loc1 1+, 
	loc2 loc1 cv lt, 
	DISPLAY-REGISTER8-LOOP !, cv bc,
	$NEWLINE !, call,
	0x40 #, loc1 cv lti,
	DISPLAY_REGISTERS_LOOP !, cv bc,
	loc0 callr,
	\ print the prompt out
	$PROMPT
    readline !, call,
	\ if we fail the check then see if the front of 
	\ input is M for terMinate
	\ precompute the offset
    monitor-input-start 1+ #, loc0 set,
    loc0 loc0 ld,
    0x4D #, loc0 cv neqi,
	DontTerminateExecution !, cv bc,
	\ terminate execution if we get in here
	$TERMINATE
	DontTerminateExecution .label
	\ end TERMINATE
    out0 loc0 ->
    5 #, loc0 cv lti, 
    monitor-loop-start !, cv bc,
    monitor-input-start 1+ #, arg0 set,
\ $HEX
    deflabel $HEX_DONE
    arg0 loc0 ld, \ first character
    arg0 1+,
    arg0 loc1 ld, \ second character
    arg0 1+,
    arg0 loc2 ld, \ third character
    arg0 1+,
    arg0 loc3 ld, \ fourth character
    loc0 arg0 ->
    $->HEX !, call,
    out0 loc4 ->
    loc1 arg0 ->
    $->HEX !, call,
    4 #, loc4 loc4 lshifti,
    out0 loc4 loc4 add,
    loc2 arg0 ->
    $->HEX !, call,
    4 #, loc4 loc4 lshifti,
    out0 loc4 loc4 add,
    loc3 arg0 ->
    $->HEX !, call,
    4 #, loc4 loc4 lshifti,
    out0 loc4 loc4 add,
    loc4 arg0 ->
    PRINT-NUMBER !, call,
    $NEWLINE !, call,
    printinput !, call,
    monitor-loop-start !, b,
\ this must always be first!
DumpCore .label
    \ dump the contents of core memory to disk using the given arg address
    \ arg0 - core id to dump to ( can easily make a copy by dumping to a different id )
	/dev/core-dump #->io
	IOWrite !jmp
LoadCore .label 
    \ load a given core segment into memory
    \ arg0 - the core segment index to load 
	/dev/core-load #->io
IOWrite (leafn arg0 io-write leafn)
SwapCore (fn
    \ save the current state to the target core fragment and then load a new target
    \ arg0 - core index to save current memory to
    \ arg1 - core index to load after save is complete
    DumpCore !, call,
    arg1 arg0 ->
    LoadCore !, call,
    fn)

WriteRangeToIOAddressRoutine (leafn
	\ arg0 - starting point in memory
	\ arg1 - length
	deflabel WriteRangeToIOAddress_Done
	deflabel WriteRangeToIOAddress_Loop
    2 save-locals
	zero loc0 ->
	arg1 cv eqz,
	WriteRangeToIOAddress_Done !, cv bc,
	WriteRangeToIOAddress_Loop .label
	arg0 loc0 loc1 uadd, \ uadd bro!?
	loc1 loc1 ld,
	loc1 io-write
	loc0 1+,
	loc0 arg1 cv gt,  
	WriteRangeToIOAddress_Loop !, cv bc,
	WriteRangeToIOAddress_Done .label
    2 restore-locals
    leafn)
$->HEX (fn
    deflabel $->HEX_Done
    \ arg0 - value to hexify
    1 save-locals 
    0x30 #, arg0 loc0 subi,
    loc0 cv ltz,
    $->HEX_Done !, cv bc,
    0x09 #, loc0 cv lei, 
    deflabel $->HEX_IsDigit
    $->HEX_IsDigit !, cv bc,
    0x0A #, loc0 cv lti, 
    $->HEX_Done !, cv bc,
    0x7 #, loc0 loc0 subi,
    $->HEX_IsDigit .label
    $->HEX_Done .label
    loc0 out0 ->
    1 restore-locals
    fn)

    \ reads the next four characters in as hexadecimal characters and converts them to hexidecimal numbers
        
$NEWLINE .label
      0xA #, arg0 set,
$ECHO (leafn
      \ arg0, the character to write
      /dev/console0 #->io
      arg0 io-write
      leafn)

$HEX->KEY (leafn
    deflabel $HEX->KEY_DONE
    \ go backwards from what we originally got in
    \ arg0 - contains lowest 4 bits to convert
    0x000F #, arg0 arg0 andi,
    0xA #, arg0 cv lti,
    0x30 #, arg0 arg0 addi, \ always want to do this
    $HEX->KEY_DONE !, cv bc,
    0x7 #, arg0 arg0 addi,
    $HEX->KEY_DONE .label
    arg0 out0 ->
    leafn)
PRINT-NUMBER (fn
    deflabel print-number-done
    \ arg0 - number to print
    1 save-locals
    arg0 loc0 ->
    $NEWLINE !, call,
    0xC #, loc0 arg0 rshifti,
    $HEX->KEY !, call,
    out0 arg0 ->
    $ECHO !, call,
    8 #, loc0 arg0 rshifti,
    $HEX->KEY !, call,
    out0 arg0 ->
    $ECHO !, call,
    4 #, loc0 arg0 rshifti,
    $HEX->KEY !, call,
    out0 arg0 ->
    $ECHO !, call,
    loc0 arg0 ->
    $HEX->KEY !, call,
    out0 arg0 ->
    $ECHO !, call,
    print-number-done .label
    1 restore-locals
    fn)
readline (fn
deflabel readline_loop
deflabel readline_done
deflabel readline_consume_rest_of_line 
deflabel FixCaseRoutineDone
    2 save-locals 
    monitor-input-start 1+ #, loc0 set,
    zero loc1 -> \ current
readline_loop .label 
	$KEY \ get the key
    0x61 #, arg0 cv lti,
    FixCaseRoutineDone !, cv bc,
    0x7a #, arg0 cv gti, 
    FixCaseRoutineDone !, cv bc,
    0x20 #, arg0 arg0 subi, 
FixCaseRoutineDone .label
    arg0 loc1 ->
    loc1 terminator cv eq,
    readline_done !, cv bc,
    loc1 loc0 st,
    loc0 1+,
    monitor-input-end #, loc0 cv lti,
    readline_loop !, cv bc,
    loc1 terminator cv eq, 
    readline_done !, cv bc,
readline_consume_rest_of_line .label
	$KEY \ get the key
    out0 terminator cv neq,
    readline_consume_rest_of_line !, cv bc,
readline_done .label 
    \ save the length in memory
    monitor-input-start #, at0 set,
    at0 loc0 at1 usub,
    at1 at0 st,
    at1 out0 ->
    \ terminator is used to terminate early
    2 restore-locals
    fn)

printinput .label
    monitor-input-start #, arg0 set,
printbuf .label
	arg0 arg1 ld,
	arg0 1+,
printline (fn
    \ arg0 - start address
    \ arg1 - length
    /dev/console0 #->io
    WriteRangeToIOAddressRoutine !, call,
    $NEWLINE !, call,
fn)
cr 
." routines stop at " loc@ hex . cr
." words free in routine area: " hex monitor-program-end loc@ - 1+ hex . cr


check-overflow
asm}
bye
