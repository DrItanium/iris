include iris.fs
\ contains all of the registers and pieces used for the monitor itself
s" monitor.o" {asm
0xFFFF constant monitor-memory-end
0xFA00 constant monitor-routines-start
0xF400 constant monitor-input-end
0xF300 constant monitor-input-start
0xF200 constant monitor-stack-start 
0xF100 constant monitor-stack-end \ 512 elements
0xF000 constant monitor-memory-start
monitor-memory-start constant monitor-loop

0x0000 .org monitor-loop #, jmp

deflabel TerminateExecutionRoutine
deflabel StoreMemory
deflabel RetrieveMemory
deflabel StoreCore
deflabel RetrieveCore
deflabel StoreIO
deflabel RetrieveIO
deflabel FixCaseRoutine 
deflabel PrintCharactersRoutine
deflabel WriteRangeToIOAddressRoutine
deflabel $KEY 
deflabel $ECHO 
deflabel $HEX
deflabel $->HEX
deflabel $NEWLINE
monitor-routines-start .org
TerminateExecutionRoutine .label
    /dev/terminate-vm #->io
    arg0 io-write \ this will not return

StoreMemory (leafn 
    \ arg0 - data to store
    \ arg1 - address to store to
    arg0 arg1 st, 
    leafn)
RetrieveMemory (leafn
    \ arg0 - address to load from
    arg0 out0 ld, 
    leafn)
StoreCore (leafn
    \ arg0 - data to store in core
    \ arg1 - core address
    arg0 arg1 stc, 
    leafn)
RetrieveCore (leafn
    \ arg0 - address of core to load
    arg0 out0 ldc,
    leafn)
StoreIO (leafn
    \ arg0 - the value to store at the address contained in io
    arg0 io-write
    leafn)
RetrieveIO (leafn
    out0 io-read
    leafn)
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
 FixCaseRoutine
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
        
$ECHO (leafn
      \ arg0, the character to write
      /dev/console0 #->io
      arg0 io-write
      leafn)
$NEWLINE (fn
    0xA #, arg0 set,
    $ECHO !, call,
    fn)
    

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
deflabel readline 
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
monitor-loop .org
deflabel monitor-loop-start
    0xA #, terminator set,
    monitor-stack-start #, vmsp set,
    0x10 #, num-base set,
monitor-loop-start .label
    readline !, call,
    4 #, out0 cv lti, 
    monitor-loop-start !, cv bc,
    monitor-input-start #, arg0 set,
    arg0 1+,
    $HEX !, call,
    out0 arg0 -> 
    PRINT-NUMBER !, call,
    $NEWLINE !, call,
    monitor-input-start #, arg0 set,
    arg0 arg1 ld, 
    arg1 1-,
    arg0 1+,
    /dev/console0 #->io
    WriteRangeToIOAddressRoutine !, call,
    $NEWLINE !, call,
    monitor-loop-start !, b,
asm}
bye
