include iris.fs
\ contains all of the registers and pieces used for the monitor itself
s" monitor.o" {asm
0xFFFF constant monitor-memory-end
0xFA00 constant monitor-routines-start
0xF200 constant monitor-stack-start 
0xF100 constant monitor-stack-end \ 512 elements
0xF000 constant monitor-memory-start
monitor-memory-start constant monitor-loop

0x0000 .org monitor-loop #, jmp

deflabel TerminateExecutionRoutine
deflabel CallRoutine
deflabel SetIORegister
deflabel StoreMemory
deflabel RetrieveMemory
deflabel StoreCore
deflabel RetrieveCore
deflabel StoreIO
deflabel RetrieveIO
deflabel FixCaseRoutine 
deflabel PrintCharactersRoutine
deflabel WriteRangeToIOAddressRoutine
deflabel ReadRangeFromIOAddressRoutine 
deflabel ?VMStackFull
deflabel ?VMStackEmpty
deflabel $KEY 
deflabel $ECHO 
deflabel $HEX
deflabel $KEY->HEX
monitor-routines-start .org
TerminateExecutionRoutine .label
    /dev/terminate-vm #->io
    arg0 io-write \ this will not return
?VMStackFull (leafn monitor-stack-end #, vmsp out0 eqi, leafn)
?VMStackEmpty (leafn monitor-stack-start #, vmsp out0 eqi, leafn)
CallRoutine (fn arg0 callr, fn)
SetIORegister (leafn arg0 io -> leafn)

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
	loc0 arg1 cv neq,
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
$KEY->HEX (fn
    1 save-locals 
    deflabel $KEY->HEX_Bad
    $KEY !, call,
    out0 loc0 ->
    deflabel $KEY->HEX_Bad
    0x30 #, loc0 loc0 subi,
    loc0 cv ltz,
    $KEY->HEX_Bad !, cv bc,
    0x09 #, loc0 cv lei, 
    deflabel $KEY->HEX_IsDigit
    $KEY->HEX_IsDigit !, cv bc,
    0x0A #, loc0 cv lti, 
    $KEY->HEX_Bad !, cv bc,
    0x7 #, loc0 loc0 subi,
    $KEY->HEX_IsDigit .label
    $KEY->HEX_Bad .label
    loc0 out0 ->
    1 restore-locals
    fn)

$HEX (fn
    deflabel $HEX_DONE
    1 save-locals
    zero loc0 ->
    $KEY->HEX !, call,
    0xF #, out0 cv ugti, 
    $HEX_DONE !, cv bc,
    out0 loc0 ->
    $KEY->HEX !, call,
    0xF #, out0 cv ugti, 
    $HEX_DONE !, cv bc,
    4 #, loc0 loc0 lshifti, 
    out0 loc0 loc0 add,
    $KEY->HEX !, call,
    0xF #, out0 cv ugti, 
    $HEX_DONE !, cv bc,
    4 #, loc0 loc0 lshifti, 
    out0 loc0 loc0 add,
    $KEY->HEX !, call,
    0xF #, out0 cv ugti, 
    $HEX_DONE !, cv bc,
    4 #, loc0 loc0 lshifti, 
    out0 loc0 loc0 add,
    $HEX_DONE .label
    loc0 out0 ->
    1 restore-locals
    fn)
    \ reads the next four characters in as hexidecimal characters and converts them to hexidecimal numbers
        
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
    0xA #, arg0 set,
    $ECHO !, call,
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

monitor-loop .org
deflabel monitor-loop-start
monitor-loop-start .label
    $HEX !, call,
    out0 arg0 -> 
    PRINT-NUMBER !, call,
    monitor-loop-start !, b,
asm}
bye
