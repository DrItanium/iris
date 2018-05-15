include iris.fs
\ contains all of the registers and pieces used for the monitor itself
s" monitor.o" {asm
0xFFFF constant monitor-memory-end
0xFA00 constant monitor-routines-start
0xF300 constant monitor-stack-start 
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
monitor-routines-start .org
TerminateExecutionRoutine .label
    /dev/terminate-vm #->io
    arg0 io-write \ this will not return
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
ReadRangeFromIOAddressRoutine (leafn
    deflabel ReadRangeFromIOAddress_Done
    deflabel ReadRangeFromIOAddress_Loop


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
