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
deflabel FixCaseRoutine_Done
FixCaseRoutine (leafn 
    \ arg0 - character to fix case of
    0x97 #, arg0 cv lti,
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
    4 save-locals 
    deflabel $KEY->HEX_Done
    $KEY !, call,
    0x30 #, loc0 set,
    0x09 #, loc1 set,
    out0 loc2 ->
    loc0 loc2 loc2 subi,
    loc2 cv ltz,
    $KEY->HEX_Done !, cv bc,
    loc1 loc2 cv le, 
    deflabel $KEY->HEX_IsDigit
    $KEY->HEX_IsDigit !, cv bc,
    \ TODO keep writing this routine
    $KEY->HEX_IsDigit .label
    loc2 out0 ->
    $KEY->HEX_Done .label
    4 restore-locals
    fn)

$HEX (fn
    1 save-locals
    $KEY->HEX !, call,
    out0 loc0 ->
    $KEY->HEX !, call,
    4 #, loc0 lshift, 
    out0 loc0 loc0 add,
    $KEY->HEX !, call,
    4 #, loc0 lshift, 
    out0 loc0 loc0 add,
    $KEY->HEX !, call,
    4 #, loc0 lshift, 
    out0 loc0 out0 add,
    \ TODO keep implementing here
    1 restore-locals
    fn)
    \ reads the next four characters in as hexidecimal characters and converts them to hexidecimal numbers
        
$ECHO (leafn
      \ arg0, the character to write
      /dev/console0 #->io
      arg0 io-write
      leafn)


PrintCharactersRoutine (leafn
    /dev/console0 #->io
    leafn)

ReadLine .fn
	\ arg0 - start location
	\ arg1 - length
	deflabel ReadLineLoop
	deflabel ReadLineLoopDone
	3 save-locals
	zero t0 ->
	arg0 t2 ->
	/dev/console0 #->io
	\ if length is zero then do nothing
	t2 cv eqz,
	ReadLineLoopDone !, cv bc,
	ReadLineLoop .label
	t2 t0 t1 add, 
	arg0 io-read
	FixCaseRoutine !, call,
	ret0 t1 st,
	t0 1+,
	ret0 terminator cv eq,
	ReadLineLoopDone !, cv bc,
	t0 arg1 cv neq,
	ReadLineLoop !, cv bc,
	ReadLineLoopDone .label
	t0 ret1 ->
	t2 ret0 ->
	3 restore-locals
	.fnret
PrintCharacters .fn
	/dev/console0 #->io
	WriteRangeToIOAddress !, call,
	.fnret
PrintLine .fn
	PrintCharacters !, call,
	/dev/console0 #->io
	0xA #, $->at0
	at0 io-write
	.fnret
WriteRangeToIOAddress .fn
	.fnret
asm}
bye
