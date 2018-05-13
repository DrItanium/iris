include iris.fs
s" monitor.o" {asm
0xF000 constant input-buffer-start
0xF100 constant input-buffer-end
0xD000 constant dictionary-start
0xE800 constant data-stack-start \ 2048 entries
0xE000 constant data-stack-full 
0xF000 constant call-stack-start \ 2048 entries
0xE800 constant call-stack-full
0xA000 constant routines-start
0xE000 constant vmstack-start \ 2048 entries
0xD800 constant vmstack-full 
deflabel InputRoutine
deflabel TerminateExecutionRoutine
deflabel Start
deflabel Restart
deflabel CoreDictionaryStart
deflabel FixCaseRoutine
deflabel ReadLine
deflabel WriteRangeToIOAddress
deflabel PrintCharacters
deflabel PrintLine
deflabel SetBase
deflabel UnknownWord
r63 constant dp
r62 constant arg0
r61 constant arg1
r60 constant ret0
r59 constant ?sysinit
r58 constant t0
r57 constant t1
r56 constant t2
r55 constant ret1
\ input buffer variables
r54 constant ibcurr
r53 constant ibend
r52 constant keep-executing
r51 constant iblen
: save-register ( reg -- ) vmsp psh-> ;
: restore-register ( reg -- ) vmsp swap pop-> ;
: save-lr ( -- ) lr save-register ;
: restore-lr ( -- ) lr restore-register ;
routines-start .org
TerminateExecutionRoutine .label
	/dev/terminate-vm #->io
	arg0 io-write
	ret, 

FixCaseRoutine .label
	deflabel DoneFixCaseRoutine
	\ look in a given range
	0x97 #, arg0 cv lti,
	DoneFixCaseRoutine !, cv bc,
	0x7a #, arg0 cv gti,
	DoneFixCaseRoutine !, cv bc,
	0x20 #, arg0 arg0 subi,
	DoneFixCaseRoutine .label
	arg0 ret0 ->
	ret,
ReadLine .label
	\ arg0 - start location
	\ arg1 - length
	deflabel ReadLineLoop
	deflabel ReadLineLoopDone
	save-lr
	t0 save-register
	t1 save-register
	t2 save-register
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
	t2 restore-register
	t1 restore-register
	t0 restore-register
	restore-lr
	ret,
PrintCharacters .label
	save-lr
	/dev/console0 #->io
	WriteRangeToIOAddress !, call,
	restore-lr
	ret,
PrintLine .label
	save-lr
	PrintCharacters !, call,
	/dev/console0 #->io
	0xA #, $->at0
	at0 io-write
	restore-lr
	ret,
WriteRangeToIOAddress .label	
	\ arg0 - starting point in memory
	\ arg1 - length
	deflabel WriteRangeToIOAddress_Done
	deflabel WriteRangeToIOAddress_Loop
	save-lr
	t0 save-register
	t1 save-register
	zero t0 ->
	arg1 cv eqz,
	WriteRangeToIOAddress_Done !, cv bc,
	WriteRangeToIOAddress_Loop .label
	arg0 t0 t1 uadd, \ uadd bro!?
	t1 t1 ld,
	t1 io-write
	t0 1+,
	t0 arg1 cv neq,
	WriteRangeToIOAddress_Loop !, cv bc,
	WriteRangeToIOAddress_Done .label
	t1 restore-register
	t0 restore-register
	restore-lr
	ret,
SetBase .label 
	\ arg0 - new base
	arg0 num-base ->
	ret,
UnknownWord .label
	\ TODO make sure that th
	dp arg0 ->
	dp arg1 ld,
	arg0 1+,
	arg0 arg1 t0 add,
	0x3f t0 #sti,
	t0 1+,
	0xA t0 #sti,
	arg1 2+,
	PrintCharacters !, call,
	Start !jmp
deflabel LoadMemory 
LoadMemory .label
	\ arg0 - address to load from memory
	arg0 ret0 ld,
	ret,
deflabel StoreMemory
	StoreMemory .label
	\ arg0 - address to store to
	\ arg1 - value to store at given address
	arg1 arg0 st,
	ret,
deflabel LoadCore
	LoadCore .label
	\ arg0 - address in core
	arg0 ret0 ldc,
	ret,
deflabel StoreCore
	StoreCore .label
	\ arg0 - address to store at in core
	\ arg1 - value to store at given address
	arg1 arg0 stc,
	ret,
deflabel ReadTokenRoutine
	ReadTokenRoutine .label
	\ arg0 - dictionary pointer to start at
	\ arg1 - current address location
	arg1 arg0 readtok,
	arg0 ret0 ->
	arg1 ret1 ->
	ret,

dictionary-start .org
CoreDictionaryStart .label
0x0000 .org
	zero ?sysinit ->
\ initialization code goes here
Start .label
	zero ?sysinit cv neq,
	Restart !, cv bc,
\ any init once code goes here!
	0xFFFF #, ?sysinit $->
Restart .label
	vmstack-start #, vmsp $->
	data-stack-start #, dsp $->
	call-stack-start #, rsp $->
	CoreDictionaryStart !, dp $->
	0x20 #, separator $->
	0x10 #, num-base $->
	zero error-code ->
	zero ci ->
	0xFFFF #, keep-executing $->
	0xFFFF #, error-code $->
	0xA #, terminator $->
InputRoutine .label
	input-buffer-start #, arg0 $->
	arg0 1+,
	0x50 #, arg1 $->
	ReadLine !, call,
	ret0 ibcurr ->
	ret1 iblen ->
	ibcurr iblen ibend add,
deflabel InputTokenLoop
	InputTokenLoop .label
	dp arg0 ->
	ibcurr arg1 ->
	ReadTokenRoutine !, call,
	ret0 dp ->
	ret1 ibcurr ->
	keep-executing cv eqz, 
	TerminateExecutionRoutine !, cv bc,
	error-code cv neqz, 
	UnknownWord !, cv bc, 
	ret0 dsp psh->
	ibcurr ibend cv neq,
	InputTokenLoop !, cv bc, 
	InputRoutine !, b,
	TerminateExecutionRoutine !, b,
asm}
bye
