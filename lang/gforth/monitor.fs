include iris.fs
\ contains all of the registers and pieces used for the monitor itself
s" monitor.o" {asm
0xFFFF constant monitor-memory-end
0xFF00 constant command-table-start
0xFEFF constant monitor-routines-end
0xFA00 constant monitor-routines-start
0xF600 constant monitor-data-stack-start
0xF500 constant monitor-data-stack-end
0xF400 constant monitor-input-end
0xF300 constant monitor-input-start
0xF200 constant monitor-stack-start 
0xF100 constant monitor-stack-end \ 512 elements
0xF000 constant monitor-memory-start
\ use the upper stack elements as 
monitor-memory-start constant monitor-loop
: check-overflow ( -- ) loc@ monitor-routines-end > ABORT" routines are too large!" ;
0x0000 .org monitor-loop #, jmp

deflabel monitor-loop-start
deflabel TerminateExecutionRoutine
deflabel FixCaseRoutine 
deflabel PrintCharactersRoutine
deflabel WriteRangeToIOAddressRoutine
deflabel $KEY 
deflabel $ECHO 
deflabel $HEX
deflabel $->HEX
deflabel $NEWLINE
deflabel $SPACE
deflabel $PROMPT
deflabel printline
deflabel printbuf
deflabel printinput
deflabel readline 
deflabel SwitchCore
deflabel DumpCore
deflabel LoadCore
deflabel SwapCore

monitor-routines-start .org
\ this must always be first!
DumpCore (leafn
    \ dump the contents of core memory to disk using the given arg address
    \ arg0 - core id to dump to ( can easily make a copy by dumping to a different id )
    /dev/core-dump #->io
    arg0 io-write
    leafn)
LoadCore (leafn
    \ load a given core segment into memory
    \ arg0 the index to load from memory
    /dev/core-load #->io
    arg0 io-write
    leafn)
SwapCore (fn
    \ save the current state to the target core fragment and then load a new target
    \ arg0 - core index to save current memory to
    \ arg1 - core index to load after save is complete
    2 save-locals
    DumpCore !, call,
    arg1 arg0 ->
    LoadCore !, call,
    2 restore-locals
    fn)

TerminateExecutionRoutine .label
    /dev/terminate-vm #->io
    arg0 io-write \ this will not return

FixCaseRoutine (leafn 
deflabel FixCaseRoutineDone
    \ arg0 - character to fix case of
    0x61 #, arg0 cv lti,
    FixCaseRoutineDone !, cv bc,
    0x7a #, arg0 cv gti, 
    FixCaseRoutineDone !, cv bc,
    0x20 #, arg0 arg0 subi, 
    FixCaseRoutineDone .label
    arg0 out0 ->
    leafn)
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
$KEY (fn 
    \ resets the keyboard and then awaits the next keyboard input
    \ The next input is return in a known register (in0)
    /dev/console0 #->io
    arg0 io-read
    FixCaseRoutine !, call,
    fn)
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

$HEX (fn
    \ arg0 - starting location
    deflabel $HEX_DONE
    5 save-locals
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
    loc4 out0 ->
    5 restore-locals
    fn)
    \ reads the next four characters in as hexidecimal characters and converts them to hexidecimal numbers
        
$PROMPT (fn
      0x2D #, arg0 set,
      $ECHO !, call,
      $SPACE !, call,
      fn)
$SPACE .label
      0x20 #, arg0 set,
      $ECHO !, jmp
$NEWLINE .label
      0xA #, arg0 set,
$ECHO (leafn
      \ arg0, the character to write
      /dev/console0 #->io
      arg0 io-write
      leafn)

deflabel $HEX->KEY 
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
deflabel PRINT-NUMBER 
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
    2 save-locals 
    monitor-input-start #, loc0 set,
    loc0 1+,
    zero loc1 -> \ current
readline_loop .label 
    $KEY !, call,  \ get the key
    out0 loc1 ->
    loc1 terminator cv eq,
    readline_done !, cv bc,
    loc1 loc0 st,
    loc0 1+,
    monitor-input-end #, loc0 cv lti,
    readline_loop !, cv bc,
    loc1 terminator cv eq, 
    readline_done !, cv bc,
readline_consume_rest_of_line .label
    $KEY !, call,  \ get the key
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
deflabel DISPLAY_REGISTER8
DISPLAY_REGISTER8 (fn
		deflabel DISPLAY-REGISTERS-LOOP
		\ arg0 start address
		2 save-locals
		arg0 loc0 ->
		0x8 #, arg0 loc1 addi,
		DISPLAY-REGISTERS-LOOP .label
		/dev/register #->io
		loc0 io-write
		$SPACE !, call,
		loc0 1+, 
		loc1 loc0 cv lt, 
		DISPLAY-REGISTERS-LOOP !, cv bc,
		$NEWLINE !, call,
		2 restore-locals
fn)
deflabel DISPLAY_REGISTERS
DISPLAY_REGISTERS (fn
	deflabel DISPLAY_REGISTERS_LOOP
	2 save-locals
	$NEWLINE !, loc0 set,
	loc0 callr,
	loc0 callr,
	zero loc1 ->
	DISPLAY_REGISTERS_LOOP .label
	loc1 arg0 -> 
	DISPLAY_REGISTER8 !, call,
	0x8 #, loc1 loc1 addi, 
	0x40 #, loc1 cv lti,
	DISPLAY_REGISTERS_LOOP !, cv bc,
	loc0 callr,
	2 restore-locals
    fn)
deflabel CHECK_TERMINATE
CHECK_TERMINATE (fn
    2 save-locals 
	arg0 loc1 ->
    monitor-input-start #, loc0 set,
    loc0 1+,
    loc0 loc0 ld,
    0x4D #, loc0 cv eqi,
	zero arg0 ->
	TerminateExecutionRoutine !, cv bc,
	loc1 arg0 ->
    2 restore-locals
fn)
check-overflow
cr 
." routines stop at " loc@ hex . cr
." words free in routine area: " hex monitor-routines-end loc@ - 1+ hex . cr


monitor-loop .org
    0xA #, terminator set,
    monitor-stack-start #, vmsp set,
    0x10 #, num-base set,
monitor-loop-start .label
    DISPLAY_REGISTERS !, call,
    $PROMPT !, call,
    readline !, call,
    CHECK_TERMINATE !, call,
    \ if we fail the check then see if the front of input is M for terMinate
    out0 loc0 ->
    5 #, loc0 cv lti, 
    monitor-loop-start !, cv bc,
    monitor-input-start #, arg0 set,
    arg0 1+,
    $HEX !, call,
    out0 arg0 -> 
    PRINT-NUMBER !, call,
    $NEWLINE !, call,
    printinput !, call,
    monitor-loop-start !, b,

asm}
bye
