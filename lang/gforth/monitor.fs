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
r50 constant t3
r49 constant t4
r48 constant t5
: save-register ( reg -- ) vmsp psh-> ;
: restore-register ( reg -- ) vmsp swap pop-> ;
: save-lr ( -- ) lr save-register ;
: restore-lr ( -- ) lr restore-register ;
: .leafn ( label -- ) .label ;
: .fn ( label -- ) .label save-lr ;
: .fnret ( -- ) restore-lr ret, ;
: .leafret ( -- ) ret, ;

\ keep overriding it in successive impls of the same function

: save-locals ( count -- )
	case
		1 of t0 save-register endof
		2 of t0 save-register 
			 t1 save-register endof
		3 of t0 save-register 
			 t1 save-register 
			 t2 save-register endof
		4 of t0 save-register 
			 t1 save-register 
			 t2 save-register 
			 t3 save-register endof
		5 of t0 save-register 
			 t1 save-register 
			 t2 save-register 
			 t3 save-register 
			 t4 save-register endof
		6 of t0 save-register 
			 t1 save-register 
			 t2 save-register 
			 t3 save-register 
			 t4 save-register 
			 t5 save-register endof
		endcase ;
: restore-locals ( count -- )
	case
		1 of t0 restore-register endof
		2 of t1 restore-register 
			 t0 restore-register endof
		3 of 
		  t2 restore-register 
		  t1 restore-register 
		  t0 restore-register endof
		4 of 
		  t3 restore-register
		  t2 restore-register 
		  t1 restore-register 
		  t0 restore-register endof
		5 of 
		  t4 restore-register
		  t3 restore-register
		  t2 restore-register 
		  t1 restore-register 
		  t0 restore-register endof
		6 of 
		  t5 restore-register
		  t4 restore-register
		  t3 restore-register
		  t2 restore-register 
		  t1 restore-register 
		  t0 restore-register endof
	endcase ;

routines-start .org
TerminateExecutionRoutine .label
	/dev/terminate-vm #->io
	arg0 io-write

deflabel JumpTo
	JumpTo .label
	\ arg0 - place to jump to and never return
	arg0 br,


FixCaseRoutine .leafn
	deflabel DoneFixCaseRoutine
	\ look in a given range
	0x97 #, arg0 cv lti,
	DoneFixCaseRoutine !, cv bc,
	0x7a #, arg0 cv gti,
	DoneFixCaseRoutine !, cv bc,
	0x20 #, arg0 arg0 subi,
	DoneFixCaseRoutine .label
	arg0 ret0 ->
	.leafret
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
	\ arg0 - starting point in memory
	\ arg1 - length
	deflabel WriteRangeToIOAddress_Done
	deflabel WriteRangeToIOAddress_Loop
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
	.fnret
SetBase .leafn
	\ arg0 - new base
	arg0 num-base ->
	.leafret
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
LoadMemory .leafn
	\ arg0 - address to load from memory
	arg0 ret0 ld,
	.leafret
deflabel StoreMemory
	StoreMemory .leafn
	\ arg0 - address to store to
	\ arg1 - value to store at given address
	arg1 arg0 st,
	.leafret
deflabel LoadCore
	LoadCore .leafn
	\ arg0 - address in core
	arg0 ret0 ldc,
	.leafret
deflabel StoreCore
	StoreCore .fn
	\ arg0 - address to store at in core
	\ arg1 - value to store at given address
	arg1 arg0 stc,
	.leafret
: #true ( -- imm id ) 0xFFFF #, ;
: #false ( -- imm id ) 0x0000 #, ;
deflabel ReadTokenRoutine
	ReadTokenRoutine .leafn
	\ arg0 - dictionary pointer to start at
	\ arg1 - current address location
	arg1 arg0 readtok,
	arg0 ret0 ->
	arg1 ret1 ->
	.leafret



deflabel NumberRoutine
	NumberRoutine .fn
	deflabel HandleSign 
	deflabel SignHandled
	deflabel NumberRoutineLoop
	deflabel NumberRoutineDone
	deflabel NumberRoutine_TerminateEarly
	deflabel NumberRoutine_GreaterThanNine
	deflabel NumberRoutine_ContinueHere
	\ arg0 - base address to read from
	6 save-locals
	\ t0 - flag
	\ t1 - p1
	\ t2 - count
	\ t3 - current char
	\ t4 - sgn
	\ t5 - terminatedEarly
	\ ret0 - result
	\ ret1 - another temporary
	zero ret0 ->
	#false t0 $->
	#false t5 $->
	arg0 t1 ld, \ load the beginning of the string
	t1 t2 ld, \ count
	t1 1+, \ go forward one
	t1 t3 ld, \ first character
	0x00FF #, t3 t3 andi, 
	0x2D #, t3 t4 eqi, \ compare against - sign to see if we're looking at a
					   \ negative number
	HandleSign !, t4 bc,
	SignHandled !, b,
	HandleSign .label
	t1 1+,
	\ found a minus sign
	SignHandled .label
	NumberRoutineLoop .label
		\ while ( count != 0 )
		zero t2 cv eq,
		NumberRoutineDone !, cv bc,
		t1 t3 ld,
		0x00FF #, t3 t3 andi,
		0x30 #, t3 t3 subi,
		t3 cv ltz,
		NumberRoutine_TerminateEarly !, cv bc,
		0x9 #, t3 cv gti,
		NumberRoutine_GreaterThanNine !, cv bc,
		NumberRoutine_ContinueHere .label
		num-base ret1 ->
		ret1 1-,
		ret1 t3 cv ge, \ invert logic
		NumberRoutine_TerminateEarly !, cv bc,
		num-base ret0 ret0 mul,
		t3 ret0 ret0 add,
		t2 1-,
		t1 1+,
		NumberRoutineLoop !, b,
	NumberRoutine_GreaterThanNine .label
		0x11 #, t3 cv lei, \ invert the logic here
		NumberRoutine_TerminateEarly !, cv bc,
		0x7 #, t3 t3 subi,
		NumberRoutine_ContinueHere !, b,
	NumberRoutine_TerminateEarly .label
		#false t0 $->
		#true t5 $->
	NumberRoutineDone .label
	zero ret1 ->
	6 restore-locals
	.fnret

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
	#true keep-executing $->
	#true error-code $->
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
	3 save-locals
	3 restore-locals
asm}
bye
